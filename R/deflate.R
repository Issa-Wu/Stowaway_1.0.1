# DEFLATE streams and CRC32, harvested from base R's gzip writer ---------------
#
# Why this file exists: base R offers no CRC32 function and no DEFLATE
# compressor API, but gzfile(..., "wb") always writes a genuine RFC 1952 gzip
# stream whose 8-byte trailer carries exactly the two numbers a ZIP entry
# needs: CRC32 of the uncompressed data and ISIZE (length mod 2^32). Writing
# the payload through gzfile therefore yields both a raw DEFLATE body
# (strip the 10-byte header) and a ZIP-grade CRC32 for free.
#
# Note: memCompress(type = "gzip") looks tempting, but at least on some
# builds (verified on R 4.4.1 / Windows x64) it emits zlib framing
# (RFC 1950: 0x78 0x9c header, Adler-32 trailer) despite its name, so it
# cannot be used here. A pure R CRC32 is also a dead end because R integers
# are signed 32-bit and the CRC polynomial 0xEDB88320 overflows.

#' Re-wrap a raw DEFLATE stream into RFC 1952 gzip framing
#'
#' Pairs a stripped DEFLATE body with its CRC32 and ISIZE so that
#' [base::memDecompress] (and any other gzip reader) can inflate it. When the
#' CRC/ISIZE are the ones recorded in a ZIP central directory, successful
#' decompression *is* a successful CRC verification, because both zlib and
#' libdeflate check the gzip trailer.
#'
#' @param stream raw vector: the DEFLATE data.
#' @param crc,isize unsigned 32-bit values (doubles).
#' @keywords internal
zip_gzip_frame <- function(stream, crc, isize) {
  c(
    as.raw(c(0x1f, 0x8b, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff)),
    stream,
    zip_le32(crc),
    zip_le32(isize)
  )
}

#' Compress to a raw DEFLATE stream and harvest CRC32 + ISIZE
#'
#' Writes `x` through a [base::gzfile] connection (real gzip framing),
#' strips the header (honouring all optional FLG fields) and returns the
#' DEFLATE body plus the CRC32 and ISIZE from the gzip trailer. The
#' temporary .gz file lives in [base::tempdir] and is removed via on.exit.
#'
#' @param x raw vector to compress.
#' @param level compression level 1-9 passed to gzfile.
#' @return list(stream, crc, isize): raw DEFLATE body, unsigned 32-bit CRC32
#'   (double), and the uncompressed length mod 2^32 (double).
#' @keywords internal
zip_deflate_info <- function(x, level = 9L) {
  stopifnot(is.raw(x), is.numeric(level), length(level) == 1L)
  tf <- tempfile(fileext = ".gz")
  on.exit(unlink(tf), add = TRUE)
  con <- gzfile(tf, "wb", compression = as.integer(level))
  writeBin(x, con)
  close(con)
  g <- readBin(tf, "raw", file.size(tf))
  if (!identical(g[1:3], as.raw(c(0x1f, 0x8b, 0x08)))) {
    stop("internal error: gzfile did not produce a gzip stream")
  }
  # Fixed 10-byte header; optional fields follow, in this order.
  flg <- as.integer(g[4])
  p <- 11L
  if (bitwAnd(flg, 4L) != 0L) { # FEXTRA
    p <- p + 2L + zip_u16(g, p)
  }
  if (bitwAnd(flg, 8L) != 0L) { # FNAME: NUL-terminated
    while (as.integer(g[p]) != 0L) p <- p + 1L
    p <- p + 1L
  }
  if (bitwAnd(flg, 16L) != 0L) { # FCOMMENT: NUL-terminated
    while (as.integer(g[p]) != 0L) p <- p + 1L
    p <- p + 1L
  }
  if (bitwAnd(flg, 32L) != 0L) p <- p + 2L # FHCRC
  n <- length(g)
  if (n < p + 7L) {
    stop("internal error: truncated gzip stream from gzfile")
  }
  info <- list(stream = g[p:(n - 8L)],
               crc = zip_u32(g, n - 7L),
               isize = zip_u32(g, n - 3L))
  # Sanity: for inputs below 4 GiB the trailer ISIZE must equal the input
  # length exactly, otherwise the .gz got truncated or was misread.
  if (length(x) < ZIP_U32_MAX && info$isize != length(x)) {
    stop("internal error: gzip trailer ISIZE does not match input length")
  }
  info
}

#' CRC32 checksum of a raw vector
#'
#' Computes the CRC-32 (IEEE 802.3, the ZIP/gzip flavour) of the bytes in
#' `x` and formats it as eight uppercase hex digits. Implemented on top of
#' base R's gzip writer, so it works without any external library; the cost
#' is one compression pass over `x` (level 1), roughly 20 MB/s for
#' incompressible data.
#'
#' Test vector: `crc32(charToRaw("123456789"))` is `"CBF43926"`.
#'
#' @param x a raw vector. Character input is converted with
#'   [base::charToRaw].
#' @return A single character string: the CRC32 as eight uppercase hex
#'   digits.
#' @examples
#' crc32(charToRaw("123456789")) # "CBF43926" (standard test vector)
#' crc32(raw(0))                 # "00000000"
#' @export
crc32 <- function(x) {
  if (is.character(x)) x <- charToRaw(x)
  stopifnot(is.raw(x))
  zip_fmt_u32(zip_deflate_info(x, level = 1L)$crc)
}
