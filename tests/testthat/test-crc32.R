test_that("crc32 matches standard test vectors", {
  expect_identical(crc32(charToRaw("123456789")), "CBF43926")
  expect_identical(crc32(raw(0)), "00000000")
  expect_identical(crc32("123456789"), "CBF43926") # character input
  expect_identical(crc32(charToRaw("a")), "E8B7BE43")
  expect_identical(crc32(charToRaw("abc")), "352441C2")
})

test_that("crc32 formats high-bit values without integer overflow", {
  # 0x80000000-style results must format via the split-half path; the regex
  # is weak but the real regression guard is the full-file CRC round-trip
  # below, which exercises values >= 2^31 on real payloads.
  for (n in c(10L, 1000L, 12345L)) {
    set.seed(n)
    expect_match(crc32(as.raw(sample(0:255, n, TRUE))), "^[0-9A-F]{8}$")
  }
})

test_that("deflate/crc harvesting round-trips arbitrary payloads", {
  for (n in c(0L, 1L, 2L, 64L, 1000L, 100000L)) {
    set.seed(42 + n)
    x <- as.raw(sample(0:255, n, replace = TRUE))
    ez <- file.path(tempdir(), paste0("ez_", n, ".zip"))
    writeBin(empty_zip(), ez)
    f <- file.path(tempdir(), paste0("payload_", n, ".bin"))
    writeBin(x, f)
    out <- suppressMessages(
      stow(ez, f, entries = "payload.bin",
           out = file.path(tempdir(), paste0("out_", n, ".zip")))
    )
    expect_true(all(stow_test(out)))
    back <- file.path(tempdir(), paste0("back_", n, ".bin"))
    suppressMessages(unstow(out, "payload.bin", out = back, overwrite = TRUE))
    expect_identical(readBin(back, "raw", file.size(back)), x)
  }
})
