# Extraction: unstow (single entry) and unstow_all (guarded whole-archive) ------

# Windows reserved device names: a path component whose base name (before
# the first dot) matches one of these is unusable as a file name.
zip_windows_reserved <- c(
  "CON", "PRN", "AUX", "NUL",
  paste0("COM", 1:9), paste0("LPT", 1:9)
)

# Validate an entry name as a safe relative path for extraction (zip-slip
# guard). Returns the normalized forward-slash path or stops. This is the
# single choke point every extraction goes through.
zip_safe_rel_path <- function(entry) {
  if (!is.character(entry) || length(entry) != 1L || is.na(entry) || !nzchar(entry)) {
    stop("refusing to extract an empty or missing entry name")
  }
  e <- gsub("\\", "/", enc2utf8(entry), fixed = TRUE)
  if (any(as.integer(charToRaw(e)) < 0x20) || any(as.integer(charToRaw(e)) == 0x7F)) {
    stop("refusing to extract an entry name containing control characters: ", entry)
  }
  if (startsWith(e, "/") || grepl("^[A-Za-z]:", e)) {
    stop("refusing to extract an absolute entry name (zip-slip): ", entry)
  }
  comps <- strsplit(e, "/", fixed = TRUE)[[1L]]
  if (any(comps %in% c(".", "..")) || any(comps == "")) {
    stop("refusing to extract an entry with '.'/'..'/empty components (zip-slip): ", entry)
  }
  bad <- comps[vapply(comps, function(cp) {
    base <- strsplit(cp, ".", fixed = TRUE)[[1L]][1L]
    toupper(base) %in% zip_windows_reserved ||
      grepl("[.]$", cp) || grepl(" $", cp)
  }, logical(1))]
  if (length(bad)) {
    stop("entry name resolves to an unusable filename on Windows: ", entry)
  }
  paste(comps, collapse = "/")
}

#' Extract one entry from a ZIP-based container
#'
#' Reads a single entry out of `src` in memory (no external unzip), verifies
#' its CRC32 on the way, and writes it to `out`. The entry's bytes never
# pass through a path derived from its name, so hostile archives cannot
# write outside the destination.
#'
#' @param src path to a ZIP-based container.
#' @param entry ZIP-internal name of the entry, as shown by [stow_list].
#' @param out destination file. Defaults to the entry's basename in the
#'   current working directory (announced via message).
#' @param overwrite FALSE (default) refuses to overwrite an existing file.
#' @param verify TRUE (default) verifies the CRC32 of stored and bzip2
#'   entries (DEFLATE is always verified by the gzip reader itself). For
#'   very large stored entries set verify = FALSE to skip the extra pass.
#'
#' @return `out`, invisibly.
#' @examples
#' \donttest{
#' src <- system.file("extdata", "fixture_minimal.pptx", package = "stowaway")
#' out <- file.path(tempdir(), "app.xml")
#' unstow(src, "docProps/app.xml", out = out)
#' }
#' @export
unstow <- function(src, entry, out = NULL, overwrite = FALSE, verify = TRUE) {
  if (!is.character(entry) || length(entry) != 1L || is.na(entry) || !nzchar(entry)) {
    stop("`entry` must be a single non-empty string")
  }
  sc <- zip_scan(src)
  target <- zip_normalize_entry(entry, allow_content_types = TRUE)
  idx <- match(target, sc$names)
  if (is.na(idx)) stop("no such entry: ", entry, " -- see stow_list(src)")
  data <- zip_entry_bytes(sc, idx, verify = verify)
  if (is.null(out)) {
    bn <- basename(target)
    if (bn %in% c(".", "..") || toupper(strsplit(bn, ".", fixed = TRUE)[[1L]][1L]) %in% zip_windows_reserved) {
      stop("entry name does not yield a usable file name: ", entry)
    }
    out <- file.path(getwd(), bn)
    message("stowaway: writing to ", out)
  }
  if (file.exists(out) && !overwrite) {
    stop("`out` exists; pass overwrite = TRUE to replace: ", out)
  }
  writeBin(data, out)
  invisible(out)
}

#' Extract every entry of a ZIP-based container
#'
#' A zip-slip-guarded extraction: each entry name is validated as a safe
#' relative path (no `..`, absolute paths, drive letters, backslashes or
# Windows reserved names) *before* anything is written, then every entry is
#' decoded in memory and written to `exdir`. Encrypted entries are skipped
#' with a warning.
#'
#' @param src path to a ZIP-based container.
#' @param exdir destination directory; created if missing.
#' @param overwrite FALSE (default) stops when a target file already exists.
#' @param verify passed to the per-entry CRC check (see [unstow]).
#'
#' @return The vector of extracted file paths, invisibly.
#' @examples
#' \donttest{
#' src <- system.file("extdata", "fixture_minimal.pptx", package = "stowaway")
#' ex <- file.path(tempdir(), "unpacked")
#' p <- unstow_all(src, exdir = ex)
#' length(p) # one file per entry
#' }
#' @export
unstow_all <- function(src, exdir, overwrite = FALSE, verify = TRUE) {
  sc <- zip_scan(src)
  if (length(exdir) != 1L || is.na(exdir) || !nzchar(exdir)) {
    stop("`exdir` must be a single non-empty path")
  }

  # Validate everything first; nothing is created or written if any name is
  # hostile, so a failed run leaves the destination untouched.
  targets <- character(length(sc$entries))
  for (i in seq_along(sc$entries)) {
    targets[i] <- zip_safe_rel_path(sc$names[i])
  }
  if (!dir.exists(exdir)) dir.create(exdir, recursive = TRUE)
  paths <- file.path(exdir, targets)
  existing <- paths[file.exists(paths)]
  if (length(existing) && !overwrite) {
    stop("target file(s) already exist; pass overwrite = TRUE: ",
         paste(existing, collapse = ", "))
  }

  wrote <- character(0)
  skipped <- character(0)
  for (i in seq_along(sc$entries)) {
    e <- sc$entries[[i]]
    if (e$encrypted) {
      skipped <- c(skipped, e$name)
      next
    }
    if (e$is_dir) {
      dir.create(paths[i], recursive = TRUE, showWarnings = FALSE)
      next
    }
    if (!file.exists(dirname(paths[i]))) {
      dir.create(dirname(paths[i]), recursive = TRUE, showWarnings = FALSE)
    }
    writeBin(zip_entry_bytes(sc, i, verify = verify), paths[i])
    wrote <- c(wrote, paths[i])
  }
  if (length(skipped)) {
    warning("skipped encrypted entr", if (length(skipped) == 1L) "y" else "ies", ": ",
            paste(skipped, collapse = ", "), call. = FALSE)
  }
  dirs <- if (length(paths)) paths[which(file.info(paths)$isdir)] else character(0)
  invisible(c(wrote, dirs))
}
