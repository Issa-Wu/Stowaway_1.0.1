# Removal and verification ------------------------------------------------------

#' Remove entries from a ZIP-based container
#'
#' Rebuilds the container without the named entries, copying every kept
#' entry's local record, timestamps, extra fields and central-directory
#' record verbatim; only the local-header offset field of kept records and
#' the EOCD change. If several entries share a name, *all* of them are
#' removed (ZIP allows duplicates; "remove by name" must be unambiguous in
#' effect).
#'
#' @param src path to a ZIP-based container.
#' @param entries character vector of ZIP-internal names as shown by
#'   [stow_list]. Every name must match at least one entry; removing nothing
#'   is an error, not a no-op (this is what lets `stow_register` rely on it).
#' @param out output path. Defaults to `<src minus extension>_trimmed.<ext>`;
#'   may equal `src`.
#' @param verify TRUE (default) requires the written file to be
#'   byte-identical to the archive built in memory.
#'
#' @return `out`, invisibly.
#' @examples
#' \donttest{
#' src <- system.file("extdata", "fixture_minimal.pptx", package = "stowaway")
#' payload <- file.path(tempdir(), "x.csv"); writeLines("a", payload)
#' out <- stow(src, payload, out = file.path(tempdir(), "with_x.pptx"))
#' trimmed <- stow_remove(out, "ppt/media/x.csv",
#'                        out = file.path(tempdir(), "trimmed.pptx"))
#' identical(sort(stow_list(src)$entry), sort(stow_list(trimmed)$entry))
#' }
#' @export
stow_remove <- function(src, entries, out = NULL, verify = TRUE) {
  if (!is.character(entries) || !length(entries) || anyNA(entries)) {
    stop("`entries` must be a non-empty character vector")
  }
  sc <- zip_scan(src)
  kind <- zip_ooxml_kind(sc$names)
  wanted <- zip_normalize_entries(entries, allow_content_types = TRUE)
  miss <- wanted[!wanted %in% sc$names]
  if (length(miss)) {
    stop("no such entries: ", paste(miss, collapse = ", "), " -- see stow_list(src)")
  }
  if ("[Content_Types].xml" %in% wanted && kind != "zip") {
    message("stowaway: note - removing [Content_Types].xml leaves an OOXML ",
            "container non-conformant until it is re-registered")
  }
  if ("mimetype" %in% wanted && kind == "odf") {
    warning("ODF requires 'mimetype' as the first entry; removing it breaks ODF readers",
            call. = FALSE)
  }
  drops <- which(sc$names %in% wanted)
  if (is.null(out)) out <- zip_default_out(src, "_trimmed")
  res <- zip_rebuild(sc, drops = drops, adds = list(), out = out, verify = verify)
  message(sprintf("stowaway: removed %d entr%s from %s (%d remain)",
                  length(drops), if (length(drops) == 1L) "y" else "ies",
                  basename(out), res$n_entries))
  invisible(out)
}

#' Verify the entries of a ZIP-based container
#'
#' Tests every named entry (by default all entries) by decompressing or
#' reading it in memory and checking its CRC32 against the central
#' directory. This is a real content check, not a header walk.
#'
#' @param src path to a ZIP-based container.
#' @param entries optional character vector restricting the check to these
#'   entries (default: all).
#'
#' @return A named logical vector, one element per checked entry: TRUE =
#'   CRC verified, FALSE = data corrupt. Encrypted entries are NA (cannot
#'   be checked). Also printed as a summary.
#' @examples
#' f <- system.file("extdata", "fixture_minimal.pptx", package = "stowaway")
#' all(stow_test(f))
#' @export
stow_test <- function(src, entries = NULL) {
  sc <- zip_scan(src)
  if (is.null(entries)) {
    idx <- seq_along(sc$entries)
  } else {
    if (!is.character(entries) || !length(entries)) {
      stop("`entries` must be NULL or a character vector")
    }
    wanted <- zip_normalize_entries(entries, allow_content_types = TRUE)
    miss <- wanted[!wanted %in% sc$names]
    if (length(miss)) {
      stop("no such entries: ", paste(miss, collapse = ", "), " -- see stow_list(src)")
    }
    idx <- which(sc$names %in% wanted)
  }
  res <- rep(NA, length(idx))
  names(res) <- sc$names[idx]
  for (k in seq_along(idx)) {
    i <- idx[k]
    if (sc$entries[[i]]$encrypted) next
    ok <- tryCatch({
      zip_entry_bytes(sc, i, verify = TRUE)
      TRUE
    }, error = function(e) FALSE)
    res[k] <- ok
  }
  good <- sum(res, na.rm = TRUE)
  n <- length(res)
  if (good == n) {
    message(sprintf("stowaway: %d/%d entries passed CRC32 verification", good, n))
  } else {
    bad <- names(res)[!is.na(res) & !res]
    nas <- names(res)[is.na(res)]
    message(sprintf("stowaway: %d/%d passed; FAILED: %s%s", good, n,
                    if (length(bad)) paste(bad, collapse = ", ") else "(none)",
                    if (length(nas)) paste0("; unchecked (encrypted): ",
                                            paste(nas, collapse = ", ")) else ""))
  }
  invisible(res)
}
