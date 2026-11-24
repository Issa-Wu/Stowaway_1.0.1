# Test helpers: hand-built ZIP fixtures (no package internals used) ---------

le16t <- function(x) as.raw(c(x %% 256, (x %/% 256) %% 256))
le32t <- function(x) as.raw(c(x %% 256, (x %/% 256) %% 256,
                              (x %/% 65536) %% 256, (x %/% 16777216) %% 256))
sig_l <- as.raw(c(0x50, 0x4b, 0x03, 0x04))
sig_c <- as.raw(c(0x50, 0x4b, 0x01, 0x02))
sig_e <- as.raw(c(0x50, 0x4b, 0x05, 0x06))

crc_value <- function(data) {
  h <- crc32(data)
  strtoi(substring(h, 1L, 4L), 16L) * 65536 + strtoi(substring(h, 5L, 8L), 16L)
}

# Minimal empty archive: EOCD only.
empty_zip <- function(comment = raw(0)) {
  c(sig_e, le16t(0), le16t(0), le16t(0), le16t(0),
    le32t(0), le32t(0), le16t(length(comment)), comment)
}

# One stored entry, hand-assembled. flags allows crafting hostile/edge
# fixtures (encrypted bit, UTF-8 bit, weird names) that stow() itself
# refuses to write - which is exactly what we need to test against.
raw_zip_one <- function(name, data, flags = 0L) {
  nm <- charToRaw(enc2utf8(name))
  crc <- crc_value(data)
  local <- c(sig_l, le16t(20L), le16t(flags), le16t(0L),
             le16t(0), le16t(0x21),
             le32t(crc), le32t(length(data)), le32t(length(data)),
             le16t(length(nm)), le16t(0L), nm, data)
  cd <- c(sig_c, le16t(20L), le16t(20L), le16t(flags), le16t(0L),
          le16t(0), le16t(0x21),
          le32t(crc), le32t(length(data)), le32t(length(data)),
          le16t(length(nm)), le16t(0L), le16t(0L),
          le16t(0L), le16t(0L), le32t(0L), le32t(0L), nm)
  c(local, cd,
    sig_e, le16t(0), le16t(0), le16t(1), le16t(1),
    le32t(length(cd)), le32t(length(local)), le16t(0))
}

mk_payload <- function(n, ext = "bin", seed = 1) {
  path <- tempfile(fileext = paste0(".", ext))
  set.seed(seed)
  writeBin(as.raw(sample(0:255, n, replace = TRUE)), path)
  path
}

FIXTURE <- function() {
  system.file("extdata", "fixture_minimal.pptx", package = "stowaway")
}
