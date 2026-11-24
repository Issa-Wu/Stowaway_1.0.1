test_that("unstow matches base R's own unzip byte for byte", {
  src <- FIXTURE()
  part <- "docProps/app.xml"
  ours <- file.path(tempdir(), "stowaway_app.xml")
  theirs <- file.path(tempdir(), "unzip_app.xml")
  suppressMessages(unstow(src, part, out = ours))
  ex <- tempfile("unz_"); dir.create(ex)
  utils::unzip(src, files = part, exdir = ex)
  file.copy(file.path(ex, part), theirs, overwrite = TRUE)
  expect_identical(readBin(ours, "raw", file.size(ours)),
                   readBin(theirs, "raw", file.size(theirs)))
})

test_that("unstow guards and errors", {
  src <- FIXTURE()
  expect_error(suppressMessages(unstow(src, "no/such/part.xml",
                                        out = tempfile())),
               "no such entry")
  dest <- file.path(tempdir(), "exists.xml")
  writeLines("junk", dest)
  expect_error(suppressMessages(unstow(src, "docProps/app.xml", out = dest)),
               "`out` exists")
  suppressMessages(unstow(src, "docProps/app.xml", out = dest, overwrite = TRUE))
  expect_gt(file.size(dest), 100)
})

test_that("unstow_all extracts everything and stays zip-slip-safe", {
  src <- FIXTURE()
  ex <- file.path(tempdir(), "unpacked")
  unlink(ex, recursive = TRUE)
  paths <- suppressWarnings(unstow_all(src, exdir = ex))
  li <- stow_list(src)
  expect_true(all(file.exists(paths)))
  expect_equal(length(paths), nrow(li))
  spot <- file.path(ex, "ppt/presentation.xml")
  expect_true(file.exists(spot))
  # byte-identical spot-check against the original part
  ref <- tempfile("ref_"); dir.create(ref)
  utils::unzip(src, files = "ppt/presentation.xml", exdir = ref)
  expect_identical(readBin(spot, "raw", file.size(spot)),
                   readBin(file.path(ref, "ppt/presentation.xml"), "raw",
                           file.size(file.path(ref, "ppt/presentation.xml"))))
})

test_that("hostile entry names are refused before anything is written", {
  evil_names <- c("../evil.txt", "a/../../evil.txt", "C:/evil.txt",
                 "CON.txt", "trailing.txt.", "nul.txt")
  ex <- file.path(tempdir(), "evil_out")
  unlink(ex, recursive = TRUE)
  for (nm in evil_names) {
    z <- file.path(tempdir(), "evil.zip")
    writeBin(raw_zip_one(nm, charToRaw("pwned")), z)
    expect_error(unstow_all(z, exdir = ex),
                 regexp = "zip-slip|refusing|unusable", class = "error")
    expect_false(dir.exists(ex))
  }
})

test_that("encrypted-flag entries are detected and refused", {
  z <- file.path(tempdir(), "enc.zip")
  writeBin(raw_zip_one("secret.txt", charToRaw("classified"), flags = 1L), z)
  li <- stow_list(z)
  expect_true(li$encrypted[1])
  expect_error(suppressMessages(unstow(z, "secret.txt", out = tempfile())),
               "encrypted")
  ex <- file.path(tempdir(), "enc_out")
  p <- NULL
  expect_warning({ p <- unstow_all(z, exdir = ex) }, "skipped encrypted")
  expect_equal(length(p), 0L)
})

test_that("stow_remove drops entries and leaves the rest untouched", {
  src <- file.path(tempdir(), "empty4.zip")
  writeBin(empty_zip(), src)
  f1 <- mk_payload(111, seed = 61); f2 <- mk_payload(222, seed = 62)
  out <- file.path(tempdir(), "two.zip")
  suppressMessages(stow(src, c(f1, f2), entries = c("a.bin", "b.bin"), out = out))
  trimmed <- file.path(tempdir(), "one.zip")
  suppressMessages(stow_remove(out, "a.bin", out = trimmed))
  li <- stow_list(trimmed)
  expect_equal(nrow(li), 1L)
  expect_true(all(stow_test(trimmed)))
  back <- file.path(tempdir(), "keep_back.bin")
  suppressMessages(unstow(trimmed, "b.bin", out = back))
  expect_identical(readBin(back, "raw", file.size(back)),
                   readBin(f2, "raw", file.size(f2)))

  # removing a name that matches nothing is an error, never a silent no-op
  expect_error(suppressMessages(stow_remove(trimmed, "ghost.bin",
                                            out = tempfile())),
               "no such entries")
  # removing everything yields a valid empty archive that can grow again
  allgone <- file.path(tempdir(), "allgone.zip")
  suppressMessages(stow_remove(trimmed, li$entry, out = allgone))
  expect_equal(nrow(stow_list(allgone)), 0L)
  f3 <- mk_payload(9, seed = 63)
  suppressMessages(stow(allgone, f3, out = allgone))
  expect_equal(nrow(stow_list(allgone)), 1L)
})

test_that("stow_test flags corrupted data", {
  src <- file.path(tempdir(), "empty5.zip")
  writeBin(empty_zip(), src)
  f <- mk_payload(400, seed = 71)
  out <- file.path(tempdir(), "to_corrupt.zip")
  suppressMessages(stow(src, f, entries = "blob.bin", out = out))
  buf <- readBin(out, "raw", file.size(out))
  li <- stow_list(out)
  off <- li$offset[1]
  dpos <- 30L + nchar("blob.bin") + off + 1L # local header + name, 1-based
  buf[dpos + 10L] <- as.raw(bitwXor(as.integer(buf[dpos + 10L]), 1L))
  corrupted <- file.path(tempdir(), "corrupted.zip")
  writeBin(buf, corrupted)
  res <- stow_test(corrupted)
  expect_false(res["blob.bin"])
})

test_that("EOCD geometry survives a crafted comment containing PK\\x05\\x06", {
  # A naive "last signature wins" EOCD scan would misparse this archive.
  z <- file.path(tempdir(), "commented.zip")
  writeBin(empty_zip(comment = c(sig_e, charToRaw("!"))), z)
  info <- stow_info(z)
  expect_equal(info$n_entries, 0L)
  expect_identical(info$comment, rawToChar(c(sig_e, charToRaw("!"))))
  # and stow still works, preserving the comment
  f <- mk_payload(30, seed = 81)
  out <- file.path(tempdir(), "commented_out.zip")
  suppressMessages(stow(z, f, out = out))
  expect_equal(stow_info(out)$n_entries, 1L)
  expect_identical(stow_info(out)$comment, rawToChar(c(sig_e, charToRaw("!"))))
})

test_that("self-extracting-style prefixed archives are handled", {
  plain <- file.path(tempdir(), "plain.zip")
  writeBin(empty_zip(), plain)
  f <- mk_payload(50, seed = 91)
  inner <- file.path(tempdir(), "inner.zip")
  suppressMessages(stow(plain, f, out = inner))
  sfx <- file.path(tempdir(), "sfx.zip")
  writeBin(c(charToRaw("SFX-STUB!"), readBin(inner, "raw", file.size(inner))), sfx)
  info <- stow_info(sfx)
  expect_identical(info$prefix_bytes, 9L)
  expect_equal(info$n_entries, 1L)
  expect_true(all(stow_test(sfx)))
  # stow onto a prefixed archive keeps the stub
  f2 <- mk_payload(60, seed = 92)
  out <- file.path(tempdir(), "sfx_out.zip")
  suppressMessages(stow(sfx, f2, out = out))
  expect_identical(stow_info(out)$prefix_bytes, 9L)
  expect_equal(stow_info(out)$n_entries, 2L)
})
