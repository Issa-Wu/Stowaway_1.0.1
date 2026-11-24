# [Content_Types].xml: MIME table, Override injection ---------------------------

# Extension -> content type. Anything not listed falls back to
# application/octet-stream, which is always legal.
zip_content_types <- c(
  csv = "text/csv",
  tsv = "text/tab-separated-values",
  txt = "text/plain",
  md = "text/markdown",
  json = "application/json",
  geojson = "application/geo+json",
  xml = "application/xml",
  html = "text/html",
  css = "text/css",
  js = "text/javascript",
  py = "text/x-python",
  r = "text/plain",
  rnw = "text/plain",
  bib = "text/plain",
  svg = "image/svg+xml",
  png = "image/png",
  jpg = "image/jpeg",
  jpeg = "image/jpeg",
  gif = "image/gif",
  webp = "image/webp",
  tif = "image/tiff",
  tiff = "image/tiff",
  bmp = "image/bmp",
  ico = "image/vnd.microsoft.icon",
  pdf = "application/pdf",
  eps = "application/postscript",
  gz = "application/gzip",
  zip = "application/zip",
  tar = "application/x-tar",
  xlsx = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
  xlsm = "application/vnd.ms-excel.sheet.macroEnabled.12",
  docx = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
  docm = "application/vnd.ms-word.document.macroEnabled.12",
  pptx = "application/vnd.openxmlformats-officedocument.presentationml.presentation",
  pptm = "application/vnd.ms-powerpoint.presentation.macroEnabled.12",
  odt = "application/vnd.oasis.opendocument.text",
  ods = "application/vnd.oasis.opendocument.spreadsheet",
  odp = "application/vnd.oasis.opendocument.presentation",
  sav = "application/x-spss-sav",
  dta = "application/x-stata-dta",
  parquet = "application/vnd.apache.parquet",
  sqlite = "application/vnd.sqlite3"
)

#' Guess an OPC content type from an entry name
#'
#' Maps the extension of `entry` to a MIME content type suitable for an
#' `<Override/>` in `[Content_Types].xml`. Unknown extensions map to
#' "application/octet-stream", which the OPC spec always allows.
#'
#' @param entry character vector of ZIP-internal entry names.
#' @return A character vector of content types, same length as `entry`.
#' @examples
#' guess_content_type(c("ppt/media/table.csv", "code/mycode.R", "data.bin"))
#' @export
guess_content_type <- function(entry) {
  if (!is.character(entry)) stop("`entry` must be a character vector")
  ext <- tolower(tools::file_ext(entry))
  vapply(ext, function(e) {
    if (e %in% names(zip_content_types)) zip_content_types[[e]] else "application/octet-stream"
  }, character(1), USE.NAMES = FALSE)
}

# XML-escape a string for use inside a double-quoted attribute value.
zip_xml_escape <- function(s) {
  s <- enc2utf8(s)
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  s <- gsub(">", "&gt;", s, fixed = TRUE)
  s <- gsub('"', "&quot;", s, fixed = TRUE)
  s <- gsub("'", "&apos;", s, fixed = TRUE)
  s
}

# Build the bytes of a new [Content_Types].xml with <Override/> elements for
# `entries` inserted before the final </Types>. Idempotent: parts that
# already have an Override, or whose extension is already covered by a
# <Default/> with the same content type, are skipped. Returns a list with
# the new bytes and counters, or NULL when there is nothing to insert.
zip_ct_with_overrides <- function(sc, ct_idx, entries, cts, verbose = TRUE) {
  s <- rawToChar(zip_entry_bytes(sc, ct_idx))
  if (!validUTF8(s)) {
    stop("[Content_Types].xml is not valid UTF-8; refusing to modify it")
  }
  Encoding(s) <- "UTF-8"
  ends <- gregexpr("</Types>", s, fixed = TRUE)[[1L]]
  if (length(ends) == 1L && ends == -1L) {
    stop("[Content_Types].xml has no </Types> closing tag; unexpected layout")
  }
  last_end <- ends[length(ends)]
  inserts <- character(0)
  skipped <- 0L
  for (i in seq_along(entries)) {
    already <- grepl(sprintf('PartName="/%s"', entries[i]), s, fixed = TRUE) ||
      grepl(sprintf('PartName="/%s"', zip_xml_escape(entries[i])), s, fixed = TRUE) ||
      grepl(sprintf("PartName='/%s'", entries[i]), s, fixed = TRUE)
    if (already) {
      skipped <- skipped + 1L
      next
    }
    ext <- tolower(tools::file_ext(entries[i]))
    covered <- nzchar(ext) &&
      grepl(sprintf('<Default Extension="%s" ContentType="%s"', ext, cts[i]), s, fixed = TRUE)
    if (covered) {
      skipped <- skipped + 1L
      next
    }
    inserts <- c(
      inserts,
      sprintf('<Override PartName="/%s" ContentType="%s"/>',
              zip_xml_escape(entries[i]), cts[i])
    )
  }
  if (!length(inserts)) {
    if (verbose) {
      message("stowaway: all parts are already registered in [Content_Types].xml; nothing to do")
    }
    return(NULL)
  }
  s_new <- paste0(substring(s, 1L, last_end - 1L),
                  paste0(inserts, collapse = ""),
                  substring(s, last_end))
  if (requireNamespace("xml2", quietly = TRUE)) {
    tryCatch(
      xml2::read_xml(s_new),
      error = function(e) {
        stop("modified [Content_Types].xml is not well-formed XML: ",
             conditionMessage(e))
      }
    )
  }
  list(bytes = charToRaw(s_new), n_inserted = length(inserts), n_skipped = skipped)
}

#' Declare content types for parts in an OOXML package
#'
#' Rewrites `[Content_Types].xml` so that the given parts carry an explicit
#' `<Override/>` content type, making the package fully conformant for
#' strict OPC validators (this is "level B" compliance for stowed parts:
#' the parts themselves are already in the package, they just get a
#' declared type). Idempotent: parts that already have an Override, or whose
#' extension is already covered by an equivalent `<Default/>`, are skipped.
#'
#' The rewrite is done with the same append machinery as [stow]: the old
#' `[Content_Types].xml` is dropped and a fresh one appended in one rebuild
#' pass, with a temp-file-then-rename write, so no external zip tool and no
#' full re-compression of the package are ever needed. ISO/IEC 29500-2
#' requires only that the content-types stream be *named*
#' `[Content_Types].xml` at the package root; its physical position in the
#' ZIP is irrelevant (Office, LibreOffice, python-pptx and the OpenXML SDK
#' all locate it through the central directory).
#'
#' @param src path to an OOXML container (.pptx/.docx/.xlsx or any package
#'   with a `[Content_Types].xml`).
#' @param entries character vector of parts *already inside* the package
#'   (as written by [stow] or any other producer).
#' @param content_types NULL (default: guess from extensions via
#'   [guess_content_type]), an unnamed character vector matching `entries`,
#'   or a named character vector whose names are a subset of `entries`.
#' @param out output path. Defaults to `<src minus extension>_registered.<ext>`;
#'   may equal `src`.
#' @param verify TRUE (default) requires the written file to be
#'   byte-identical to the archive built in memory.
#'
#' @return `out`, invisibly (or `src` invisibly when there was nothing to
#'   do).
#' @examples
#' \donttest{
#' src <- system.file("extdata", "fixture_minimal.pptx", package = "stowaway")
#' payload <- file.path(tempdir(), "notes.txt"); writeLines("hi", payload)
#' out <- stow(src, payload, out = file.path(tempdir(), "level_a.pptx"))
#' reg <- stow_register(out, "ppt/media/notes.txt",
#'                      out = file.path(tempdir(), "level_b.pptx"))
#' }
#' @export
stow_register <- function(src, entries, content_types = NULL, out = NULL,
                          verify = TRUE) {
  sc <- zip_scan(src)
  ct_idx <- which(sc$names == "[Content_Types].xml")
  if (!length(ct_idx)) {
    stop("no [Content_Types].xml in this container; it is not an OOXML package")
  }
  if (!is.character(entries) || !length(entries) || anyNA(entries) || !all(nzchar(entries))) {
    stop("`entries` must be a non-empty character vector of part names")
  }
  wanted <- zip_normalize_entries(entries, allow_content_types = TRUE)
  miss <- wanted[!wanted %in% sc$names]
  if (length(miss)) {
    stop("cannot register a part that is not in the package: ",
         paste(miss, collapse = ", "), " -- see stow_list(src)")
  }
  if (is.null(content_types)) {
    cts <- guess_content_type(wanted)
  } else if (is.character(content_types) && !is.null(names(content_types))) {
    m <- match(wanted, names(content_types))
    cts <- unname(content_types[m])
    fill <- is.na(cts)
    if (any(fill)) cts[fill] <- guess_content_type(wanted[fill])
  } else if (is.character(content_types) && length(content_types) == length(wanted)) {
    cts <- content_types
  } else {
    stop("`content_types` must be NULL, match the length of `entries`, ",
         "or be a named vector keyed by `entries`")
  }
  unsafe <- wanted[grepl("[ <>%{}|^`\"\\[\\]?#]", wanted)]
  if (length(unsafe)) {
    message("stowaway: note - part name(s) contain characters unsafe in OPC ",
            "part URIs; strict validators may not match them: ",
            paste(unsafe, collapse = ", "))
  }
  ct_new <- zip_ct_with_overrides(sc, ct_idx, wanted, cts)
  if (is.null(ct_new)) return(invisible(src))
  spec <- zip_add_spec("[Content_Types].xml", ct_new$bytes, method_id = 8L,
                       level = 9L, when = Sys.time())
  if (is.null(out)) out <- zip_default_out(src, "_registered")
  zip_rebuild(sc, drops = ct_idx, adds = list(spec), out = out, verify = verify)
  message(sprintf(
    "stowaway: %d Override(s) inserted into [Content_Types].xml (%d already covered) -> %s",
    ct_new$n_inserted, ct_new$n_skipped, basename(out)
  ))
  invisible(out)
}
