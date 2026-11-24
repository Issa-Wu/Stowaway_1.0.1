test_that("stow appends multiple files in one pass and round-trips them", {
  src <- file.path(tempdir(), "empty1.zip")
  writeBin(empty_zip(), src)

  csv <- file.path(tempdir(), "table.csv")
  writeLines(c("a,b,c", "1,2,3", "4,5,6"), csv)
  png <- mk_payload(300, ext = "png", seed = 7)
  # real PNG magic so method = "auto" picks store
  png_bytes <- readBin(png, "raw", file.size(png))
  png_bytes[1:4] <- as.raw(c(0x89, 0x50, 0x4e, 0x47))
  writeBin(png_bytes, png)
  bin <- mk_payload(5000, ext = "bin", seed = 3)

  out <- file.path(tempdir(), "carrying.zip")
  expect_message(
    out <- stow(src, c(csv, png, bin),
                entries = c("data/table.csv", "media/fig.png", "blob/payload.bin"),
                out = out, mtime = as.POSIXct("2026-01-15 12:34:56", tz = "UTC")),
    "3 entries added"
  )

  li <- stow_list(out)
  expect_equal(nrow(li), 3L)
  expect_setequal(li$entry,
                  c("data/table.csv", "media/fig.png", "blob/payload.bin"))
  m <- li$method[match(c("data/table.csv", "media/fig.png", "blob/payload.bin"),
                       li$entry)]
  expect_identical(m, c("deflate", "store", "deflate"))

  # fixed UTC mtime survives with 2-second DOS granularity
  mt <- li$mtime[li$entry == "data/table.csv"]
  expect_identical(mt, as.POSIXct("2026-01-15 12:34:56", tz = "UTC"))

  # every entry CRC-verifies and unstows byte-identically
  expect_true(all(stow_test(out)))
  originals <- list(csv, png, bin)
  for (k in seq_along(li$entry)) {
    e <- li$entry[k]
    dest <- file.path(tempdir(), basename(e))
    suppressMessages(unstow(out, e, out = dest, overwrite = TRUE))
    expect_identical(readBin(dest, "raw", file.size(dest)),
                     readBin(originals[[k]], "raw", file.size(originals[[k]])))
  }

  # base R's unzip agrees with our central-directory walk
  u <- utils::unzip(out, list = TRUE)
  expect_equal(nrow(u), 3L)
  expect_setequal(u$Name, li$entry)
})

test_that("append-only never moves existing entries (offset invariance)", {
  src <- FIXTURE()
  before <- stow_list(src)
  payload <- file.path(tempdir(), "extra.txt")
  writeLines("x", payload)
  out <- file.path(tempdir(), "fixture_appended.pptx")
  suppressMessages(stow(src, payload, out = out, mtime = as.POSIXct("2026-01-01", tz = "UTC")))
  after <- stow_list(out)
  old <- after[after$entry %in% before$entry, ]
  b <- before[match(old$entry, before$entry), ]
  expect_identical(old$offset, b$offset)
  expect_identical(old$crc, b$crc)
  expect_identical(old$csize, b$csize)
  expect_equal(nrow(after), nrow(before) + 1L)
})

test_that("duplicate guards and overwrite semantics", {
  src <- file.path(tempdir(), "empty2.zip")
  writeBin(empty_zip(), src)
  f <- mk_payload(100, seed = 11)
  out <- file.path(tempdir(), "dup.zip")
  suppressMessages(stow(src, f, entries = "payload.bin", out = out))
  expect_error(suppressMessages(stow(out, f, entries = "payload.bin", out = out)),
               "already present")
  # batch-internal duplicates are refused outright
  expect_error(suppressMessages(stow(src, c(f, f),
                                      entries = c("a.bin", "a.bin"), out = out)),
               "duplicate entry names")
  # overwrite replaces within one rebuild pass
  f2 <- mk_payload(200, seed = 12)
  suppressMessages(stow(out, f2, entries = "payload.bin", out = out, overwrite = TRUE))
  li <- stow_list(out)
  expect_equal(nrow(li), 1L)
  back <- file.path(tempdir(), "dup_back.bin")
  suppressMessages(unstow(out, "payload.bin", out = back))
  expect_identical(readBin(back, "raw", file.size(back)),
                   readBin(f2, "raw", file.size(f2)))
})

test_that("entry-name guards reject hostile or malformed names", {
  src <- FIXTURE()
  f <- mk_payload(10, seed = 5)
  out <- file.path(tempdir(), "names.pptx")
  bad <- list("../escape.txt", "a/../../x.txt", "/abs.txt", "C:/drive.txt",
              "[Content_Types].xml", "", "trailing/", "a//b.txt", "./rel.txt",
              "pct/%2e%2e/escape.txt")
  for (nm in bad) {
    expect_error(suppressMessages(stow(src, f, entries = nm, out = out)),
                 regexp = "entry|Content_Types", class = "error")
  }
  # backslashes are converted to forward slashes, not rejected
  suppressMessages(stow(src, f, entries = "sub\\dir\\nested.bin", out = out))
  expect_true("sub/dir/nested.bin" %in% stow_list(out)$entry)
})

test_that("UTF-8 entry names round-trip with the UTF-8 flag set", {
  src <- FIXTURE()
  f <- mk_payload(64, seed = 21)
  nm <- "data/\u4e2d\u6587\u6587\u4ef6.csv"
  out <- file.path(tempdir(), "utf8.pptx")
  suppressMessages(stow(src, f, entries = nm, out = out,
                        mtime = as.POSIXct("2026-02-02", tz = "UTC")))
  expect_true(nm %in% stow_list(out)$entry)
  back <- file.path(tempdir(), "utf8_back.csv")
  suppressMessages(unstow(out, nm, out = back))
  expect_identical(readBin(back, "raw", file.size(back)),
                  readBin(f, "raw", file.size(f)))
})

test_that("DOS timestamps outside 1980-2107 are clamped", {
  src <- file.path(tempdir(), "empty3.zip")
  writeBin(empty_zip(), src)
  f <- mk_payload(10, seed = 31)
  out <- file.path(tempdir(), "clamp_lo.zip")
  suppressMessages(stow(src, f, out = out,
                        mtime = as.POSIXct("1970-05-05 05:05:05", tz = "UTC")))
  mt <- stow_list(out)$mtime[1]
  expect_identical(mt, as.POSIXct("1980-01-01 00:00:00", tz = "UTC"))
  out2 <- file.path(tempdir(), "clamp_hi.zip")
  suppressMessages(stow(src, f, out = out2,
                        mtime = as.POSIXct("2150-06-01 06:06:06", tz = "UTC")))
  mt2 <- stow_list(out2)$mtime[1]
  expect_identical(mt2, as.POSIXct("2107-06-01 06:06:06", tz = "UTC"))
})

test_that("in-place stow (out == src) keeps the container valid", {
  src <- file.path(tempdir(), "inplace.zip")
  writeBin(empty_zip(), src)
  f <- mk_payload(128, seed = 41)
  suppressMessages(stow(src, f, out = src))
  li <- stow_list(src)
  expect_equal(nrow(li), 1L)
  expect_true(all(stow_test(src)))
})

test_that("large payload survives deflate round-trip", {
  src <- FIXTURE()
  big <- mk_payload(2 * 1024 * 1024, ext = "bin", seed = 51)
  out <- file.path(tempdir(), "big.pptx")
  suppressMessages(stow(src, big, entries = "ppt/media/payload.bin", out = out))
  expect_true(all(stow_test(out)))
  back <- file.path(tempdir(), "big_back.bin")
  suppressMessages(unstow(out, "ppt/media/payload.bin", out = back))
  expect_identical(file.size(back), file.size(big))
})
