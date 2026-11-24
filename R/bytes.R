# Internal byte-level helpers ---------------------------------------------------
# All positions are 1-based indices into raw vectors; multi-byte integers are
# little-endian as required by the ZIP format. Unsigned 32-bit values are
# carried as doubles (R integers are signed 32-bit, so a CRC32 of 0xEDB88320
# or larger would overflow).

# Hard limits. ZIP32 itself caps out at 4 GiB / 65,535 entries; R raw vectors
# cap at 2^31 - 1 bytes, and stow() holds src + payload + output in memory,
# so anything approaching 2 GiB is refused with an explicit error.
ZIP_MAX_BYTES <- 2147483647
ZIP_MAX_ENTRIES <- 65535L
ZIP_U32_MAX <- 4294967295

sig_local <- as.raw(c(0x50, 0x4b, 0x03, 0x04))
sig_cd <- as.raw(c(0x50, 0x4b, 0x01, 0x02))
sig_eocd <- as.raw(c(0x50, 0x4b, 0x05, 0x06))

zip_u16 <- function(buf, pos) {
  as.integer(buf[pos]) + as.integer(buf[pos + 1L]) * 256L
}

zip_u32 <- function(buf, pos) {
  # doubles: 255 * 16777216 overflows signed integers
  as.numeric(buf[pos]) +
    as.numeric(buf[pos + 1L]) * 256 +
    as.numeric(buf[pos + 2L]) * 65536 +
    as.numeric(buf[pos + 3L]) * 16777216
}

zip_le16 <- function(x) {
  x <- as.numeric(x)
  as.raw(c(x %% 256, (x %/% 256) %% 256))
}

zip_le32 <- function(x) {
  x <- as.numeric(x)
  as.raw(c(x %% 256,
           (x %/% 256) %% 256,
           (x %/% 65536) %% 256,
           (x %/% 16777216) %% 256))
}

# Format an unsigned 32-bit value as 8 uppercase hex digits. sprintf("%08X")
# cannot take a double >= 2^31, so the value is split into two 16-bit halves.
zip_fmt_u32 <- function(v) {
  v <- as.numeric(v)
  sprintf("%04X%04X", as.integer(v %/% 65536), as.integer(v %% 65536))
}

zip_method_name <- function(m) {
  switch(as.character(m),
         "0" = "store", "8" = "deflate", "12" = "bzip2",
         paste0("m", m))
}

# DOS date/time as required by ZIP local headers. The DOS epoch is 1980-01-01
# and the format tops out at 2107-12-31 23:59:58, so timestamps outside that
# window are clamped instead of silently corrupting the header. Timestamps
# are interpreted in UTC for deterministic output across machines and DST
# boundaries (DOS timestamps carry no timezone of their own).
zip_dos_stamp <- function(when) {
  when <- as.POSIXlt(as.POSIXct(when, tz = "UTC"), tz = "UTC")
  yr <- as.integer(when$year) + 1900L
  if (is.na(yr) || yr < 1980L) {
    return(list(time = 0L, date = 33L)) # 1980-01-01 00:00:00
  }
  if (yr > 2107L) yr <- 2107L
  list(
    time = (as.integer(when$sec) %/% 2L) +
      as.integer(when$min) * 32L +
      as.integer(when$hour) * 2048L,
    date = as.integer(when$mday) +
      (as.integer(when$mon) + 1L) * 32L +
      (yr - 1980L) * 512L
  )
}

# Inverse of zip_dos_stamp() for listings. DOS timestamps carry no timezone:
# the result is the recorded wall-clock time labelled UTC.
zip_from_dos <- function(dos_date, dos_time) {
  if (dos_date == 0L && dos_time == 0L) return(NA_real_)
  as.POSIXct(
    sprintf(
      "%04d-%02d-%02d %02d:%02d:%02d",
      1980 + (dos_date %/% 512),
      (dos_date %/% 32) %% 16,
      dos_date %% 32,
      dos_time %/% 2048,
      (dos_time %/% 32) %% 64,
      (dos_time %% 32) * 2
    ),
    tz = "UTC"
  )
}
