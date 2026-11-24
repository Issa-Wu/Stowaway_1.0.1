# Reading: EOCD location, central-directory walk, structural scan -------------

# Locate the end-of-central-directory record. The EOCD signature can legally
# appear inside an archive comment, so every candidate is validated against
# the file geometry: a candidate is only accepted when its comment length
# makes the record end exactly at end-of-file, and the central directory it
# describes must end exactly where the EOCD begins.
zip_eocd <- function(buf) {
  n <- length(buf)
  if (n < 22L) {
    stop("not a ZIP container: smaller than an end-of-central-directory record")
  }
  lo <- max(1L, n - 65557L) # 65535 (max comment) + 22 (fixed EOCD size)
  pos <- NA_integer_
  for (i in seq.int(n - 21L, lo)) {
    if (!identical(buf[i:(i + 3L)], sig_eocd)) next
    cl <- zip_u16(buf, i + 20L)
    if (i + 21L + cl != n) next # comment must end exactly at EOF
    pos <- i
    break
  }
  if (is.na(pos)) {
    stop("not a ZIP container: no self-consistent end-of-central-directory record")
  }
  n_disk <- zip_u16(buf, pos + 4L)
  cd_disk <- zip_u16(buf, pos + 6L)
  n_disk_cd <- zip_u16(buf, pos + 8L)
  n_total <- zip_u16(buf, pos + 10L)
  cd_size <- zip_u32(buf, pos + 12L)
  cd_off <- zip_u32(buf, pos + 16L)
  comment_len <- zip_u16(buf, pos + 20L)
  if (n_total == 65535L || cd_size == ZIP_U32_MAX || cd_off == ZIP_U32_MAX) {
    stop("ZIP64 archives are not supported by stowaway")
  }
  if (n_disk != 0L || cd_disk != 0L || n_disk_cd != n_total) {
    stop("multi-disk / spanning archives are not supported by stowaway")
  }
  # Bytes physically preceding the ZIP stream (self-extracting stub etc.).
  prefix <- (pos - 1L) - cd_size - cd_off
  if (prefix < 0) {
    stop("corrupt container: central directory does not end where the EOCD begins")
  }
  comment <- if (comment_len > 0L) {
    if (pos + 21L + comment_len > n) {
      stop("corrupt container: EOCD comment runs past end of file")
    }
    buf[(pos + 22L):(pos + 21L + comment_len)]
  } else {
    raw(0)
  }
  list(pos = pos, n_total = n_total, cd_size = cd_size, cd_off = cd_off,
       comment_len = comment_len, comment = comment, prefix = prefix)
}

# Walk the central directory. Every record is validated: signature, fixed
# 46-byte header in bounds, name/extra/comment lengths in bounds, entry-level
# ZIP64 sentinels, multi-disk fields, and the walk must consume exactly
# cd_size bytes as declared by the EOCD.
zip_central_directory <- function(buf, eocd) {
  if (eocd$n_total == 0L) return(list())
  p <- eocd$prefix + eocd$cd_off + 1L # physical position, 1-based
  end <- p + eocd$cd_size - 1L
  if (p < 1L || end > length(buf)) {
    stop("corrupt container: central directory outside the file")
  }
  entries <- vector("list", eocd$n_total)
  for (k in seq_len(eocd$n_total)) {
    if (p + 45L > end || !identical(buf[p:(p + 3L)], sig_cd)) {
      stop("corrupt container: expected central-directory record no. ", k)
    }
    nlen <- zip_u16(buf, p + 28L)
    elen <- zip_u16(buf, p + 30L)
    clen <- zip_u16(buf, p + 32L)
    rec_len <- 46 + nlen + elen + clen
    if (p + rec_len - 1L > end) {
      stop("corrupt container: central-directory record no. ", k, " is truncated")
    }
    flags <- zip_u16(buf, p + 8L)
    method <- zip_u16(buf, p + 10L)
    crc <- zip_u32(buf, p + 16L)
    csize <- zip_u32(buf, p + 20L)
    usize <- zip_u32(buf, p + 24L)
    disk_start <- zip_u16(buf, p + 34L)
    loff <- zip_u32(buf, p + 42L)
    if (disk_start != 0L) {
      stop("multi-disk archives are not supported by stowaway (entry no. ", k, ")")
    }
    if (csize == ZIP_U32_MAX || usize == ZIP_U32_MAX || loff == ZIP_U32_MAX) {
      stop("entry-level ZIP64 is not supported by stowaway (entry no. ", k, ")")
    }
    nm_raw <- buf[(p + 46L):(p + 45L + nlen)]
    nm <- tryCatch(
      rawToChar(nm_raw),
      error = function(e) stop("entry name contains NUL bytes (record no. ", k, ")")
    )
    if (bitwAnd(flags, 0x800L) != 0L && validUTF8(nm)) Encoding(nm) <- "UTF-8"
    entries[[k]] <- list(
      index = k,
      name = nm,
      name_raw = nm_raw,
      flags = flags,
      encrypted = bitwAnd(flags, 1L) != 0L,
      is_dir = nlen > 0L && nm != "" && endsWith(nm, "/"),
      method = method,
      crc = crc,
      csize = csize,
      usize = usize,
      dos_time = zip_u16(buf, p + 12L),
      dos_date = zip_u16(buf, p + 14L),
      loff = loff,
      rec_start = p,
      rec_len = rec_len
    )
    p <- p + rec_len
  }
  if (p - 1L != end) {
    stop("corrupt container: central-directory records do not add up to the size declared in the EOCD")
  }
  entries
}

# Full structural scan of a container: bytes + EOCD + entries + names.
zip_scan <- function(src) {
  if (!is.character(src) || length(src) != 1L || is.na(src) || !nzchar(src)) {
    stop("`src` must be a single non-empty path")
  }
  if (!file.exists(src)) stop("`src` does not exist: ", src)
  if (dir.exists(src)) stop("`src` is a directory: ", src)
  size <- file.size(src)
  if (is.na(size)) stop("cannot stat `src`: ", src)
  if (size > ZIP_MAX_BYTES) {
    stop("container is ", size, " bytes; stowaway supports at most 2 GiB")
  }
  buf <- readBin(src, "raw", size)
  eocd <- zip_eocd(buf)
  entries <- zip_central_directory(buf, eocd)
  list(
    src = src, buf = buf, eocd = eocd, entries = entries,
    names = vapply(entries, function(e) e$name, character(1))
  )
}

# Container kind from the entry-name vector.
zip_ooxml_kind <- function(names) {
  if (!"[Content_Types].xml" %in% names) {
    if ("mimetype" %in% names) return("odf")
    return("zip")
  }
  if (any(startsWith(names, "ppt/"))) return("pptx")
  if (any(startsWith(names, "word/"))) return("docx")
  if (any(startsWith(names, "xl/"))) return("xlsx")
  "ooxml"
}

zip_media_dir <- function(kind) {
  switch(kind, pptx = "ppt/media", docx = "word/media", xlsx = "xl/media", NULL)
}

# Decode one entry in memory: local-header checks, data slicing, method
# dispatch, CRC verification. For method 8 the CRC check is free: the data is
# re-framed as gzip with the central directory's CRC/ISIZE, and both zlib and
# libdeflate verify the gzip trailer on decompression.
zip_entry_bytes <- function(sc, idx, verify = TRUE) {
  e <- sc$entries[[idx]]
  if (e$encrypted) {
    stop("entry is encrypted; stowaway does not support encrypted entries: ", e$name)
  }
  buf <- sc$buf
  lp <- sc$eocd$prefix + e$loff + 1L
  if (!identical(buf[lp:(lp + 3L)], sig_local)) {
    stop("local header missing where the central directory points; archive corrupt (entry: ", e$name, ")")
  }
  lnlen <- zip_u16(buf, lp + 26L)
  lelen <- zip_u16(buf, lp + 28L)
  dstart <- lp + 30L + lnlen + lelen
  data <- if (e$csize > 0) buf[dstart:(dstart + e$csize - 1L)] else raw(0)
  if (e$method == 0L) {
    if (verify && crc32(data) != zip_fmt_u32(e$crc)) {
      stop("CRC32 mismatch for stored entry (data corrupt): ", e$name)
    }
    return(data)
  }
  if (e$method == 8L) {
    return(tryCatch(
      memDecompress(zip_gzip_frame(data, e$crc, e$usize), "gzip"),
      error = function(err) {
        stop("entry failed DEFLATE/CRC32 verification (data corrupt): ", e$name)
      }
    ))
  }
  if (e$method == 12L) {
    out <- tryCatch(
      memDecompress(data, "bzip2"),
      error = function(err) stop("entry failed bzip2 decompression: ", e$name)
    )
    if (verify && crc32(out) != zip_fmt_u32(e$crc)) {
      stop("CRC32 mismatch for bzip2 entry (data corrupt): ", e$name)
    }
    return(out)
  }
  stop("unsupported compression method ", e$method, " for entry: ", e$name)
}

#' List the entries of a ZIP-based container
#'
#' Parses the central directory of `src` (any ZIP archive: .pptx, .docx,
#' .xlsx, .epub, plain .zip, ...) without any external tool and returns one
#' row per entry. Unlike [utils::unzip], this reports CRCs, offsets and
#' encryption flags, and works from the central directory alone.
#'
#' @param src path to a ZIP-based container.
#' @return A data.frame with columns:
#'   \item{entry}{entry name (ZIP path, forward slashes);}
#'   \item{method}{"store", "deflate", "bzip2" or "m\<n\>";}
#'   \item{encrypted,dir}{logical flags;}
#'   \item{mtime}{recorded wall-clock time as POSIXct (UTC-labelled;
#'     DOS timestamps carry no timezone) or NA;}
#'   \item{crc}{CRC32 as eight uppercase hex digits;}
#'   \item{csize,usize,offset}{compressed size, uncompressed size and
#'     local-header offset in bytes.}
#' @examples
#' f <- system.file("extdata", "fixture_minimal.pptx", package = "stowaway")
#' head(stow_list(f), 3)
#' @export
stow_list <- function(src) {
  sc <- zip_scan(src)
  n <- length(sc$entries)
  df <- data.frame(
    entry = sc$names,
    method = vapply(sc$entries, function(e) zip_method_name(e$method), character(1)),
    encrypted = vapply(sc$entries, function(e) e$encrypted, logical(1)),
    dir = vapply(sc$entries, function(e) e$is_dir, logical(1)),
    mtime = vapply(sc$entries,
                   function(e) zip_from_dos(e$dos_date, e$dos_time), numeric(1)),
    crc = vapply(sc$entries, function(e) zip_fmt_u32(e$crc), character(1)),
    csize = vapply(sc$entries, function(e) e$csize, numeric(1)),
    usize = vapply(sc$entries, function(e) e$usize, numeric(1)),
    offset = vapply(sc$entries, function(e) e$loff, numeric(1)),
    stringsAsFactors = FALSE
  )
  # vapply strips the POSIXct class; restore it (epoch seconds, UTC).
  df$mtime <- as.POSIXct(df$mtime, origin = "1970-01-01", tz = "UTC")
  df
}

#' Structural summary of a ZIP-based container
#'
#' Returns EOCD geometry, entry count, OOXML/ODF kind, default media
#' directory and archive comment in one list (printed compactly).
#'
#' @param src path to a ZIP-based container.
#' @return A list of class "stowaway_info" (see the print method for the
#'   field names).
#' @examples
#' f <- system.file("extdata", "fixture_minimal.pptx", package = "stowaway")
#' stow_info(f)
#' @export
stow_info <- function(src) {
  sc <- zip_scan(src)
  kind <- zip_ooxml_kind(sc$names)
  comment <- rawToChar(sc$eocd$comment)
  if (!validUTF8(comment)) comment <- paste0("<", length(comment), " binary bytes>")
  x <- list(
    path = normalizePath(src),
    bytes = file.size(src),
    eocd_offset = sc$eocd$pos,
    prefix_bytes = as.integer(sc$eocd$prefix),
    n_entries = sc$eocd$n_total,
    cd_offset = sc$eocd$cd_off,
    cd_size = sc$eocd$cd_size,
    comment = comment,
    kind = kind,
    media_dir = zip_media_dir(kind),
    n_encrypted = sum(vapply(sc$entries, function(e) e$encrypted, logical(1)))
  )
  class(x) <- "stowaway_info"
  x
}

#' Print a stowaway container summary
#'
#' Compactly prints the list produced by [stow_info].
#'
#' @param x an object of class "stowaway_info".
#' @param ... further arguments, ignored.
#' @return `x`, invisibly.
#' @export
print.stowaway_info <- function(x, ...) {
  cat(sprintf("<stowaway container>\n  path       : %s\n", x$path))
  cat(sprintf("  kind       : %s%s\n", x$kind,
              if (!is.null(x$media_dir)) paste0(" (media dir: ", x$media_dir, ")") else ""))
  cat(sprintf("  size       : %s bytes\n  entries    : %d\n",
              format(x$bytes, big.mark = ",", scientific = FALSE), x$n_entries))
  cat(sprintf("  EOCD @     : %s  (prefix %d bytes)\n",
              format(x$eocd_offset, big.mark = ",", scientific = FALSE), x$prefix_bytes))
  cat(sprintf("  central dir: offset %s, size %s\n",
              format(x$cd_offset, big.mark = ",", scientific = FALSE),
              format(x$cd_size, big.mark = ",", scientific = FALSE)))
  if (x$n_encrypted > 0L) cat("  warning    :", x$n_encrypted, "encrypted entries present\n")
  if (nzchar(x$comment)) cat("  comment    :", x$comment, "\n")
  invisible(x)
}

#' Default media directory of an Office document
#'
#' Returns the "dead part" directory conventionally used for unreferenced
#' media inside the document kind detected at `src`: `ppt/media` for
#' presentations, `word/media` for word processor files, `xl/media` for
#' spreadsheets, or NULL for non-OOXML containers. Passing this to
#' [stow]'s `part_dir` argument mirrors what [officer] does for real media.
#'
#' @param src path to a ZIP-based container.
#' @return A character scalar or NULL.
#' @examples
#' f <- system.file("extdata", "fixture_minimal.pptx", package = "stowaway")
#' ooxml_media_dir(f) # "ppt/media"
#' @export
ooxml_media_dir <- function(src) {
  sc <- zip_scan(src)
  zip_media_dir(zip_ooxml_kind(sc$names))
}
