test_that("stow(register = TRUE) inserts Overrides and stays idempotent", {
  src <- FIXTURE()
  csv <- file.path(tempdir(), "reg_table.csv")
  writeLines(c("x,y", "1,2"), csv)
  out <- file.path(tempdir(), "reg_level_b.pptx")
  expect_message(
    stow(src, csv, out = out, register = TRUE,
         mtime = as.POSIXct("2026-03-03", tz = "UTC")),
    "registered"
  )

  # exactly one [Content_Types].xml, and it contains the new Override
  li <- stow_list(out)
  ct <- li$entry[li$entry == "[Content_Types].xml"]
  expect_equal(length(ct), 1L)
  ct_xml <- file.path(tempdir(), "ct.xml")
  suppressMessages(unstow(out, "[Content_Types].xml", out = ct_xml))
  txt <- paste(readLines(ct_xml, warn = FALSE), collapse = "")
  expect_match(txt, 'PartName="/ppt/media/reg_table.csv"', fixed = TRUE)
  expect_match(txt, 'ContentType="text/csv"', fixed = TRUE)
  # well-formedness when xml2 is available
  if (requireNamespace("xml2", quietly = TRUE)) {
    expect_silent(xml2::read_xml(txt))
  }

  # registering again is a no-op, leaving the file untouched
  before <- readBin(out, "raw", file.size(out))
  expect_message(res <- stow_register(out, "ppt/media/reg_table.csv"),
                 "already registered")
  after <- readBin(res, "raw", file.size(res))
  expect_identical(before, after)
})

test_that("stow_register standalone: guess, named types, errors", {
  src <- FIXTURE()
  txt <- file.path(tempdir(), "notes.txt")
  writeLines("hello", txt)
  out <- file.path(tempdir(), "notes_level_a.pptx")
  suppressMessages(stow(src, txt, out = out))

  # guess: .txt -> text/plain
  reg <- file.path(tempdir(), "notes_level_b.pptx")
  suppressMessages(stow_register(out, "ppt/media/notes.txt", out = reg))
  ct_xml <- file.path(tempdir(), "ct2.xml")
  suppressMessages(unstow(reg, "[Content_Types].xml", out = ct_xml))
  s <- paste(readLines(ct_xml, warn = FALSE), collapse = "")
  expect_match(s, 'PartName="/ppt/media/notes.txt"', fixed = TRUE)
  expect_match(s, 'ContentType="text/plain"', fixed = TRUE)

  # named content_types vector
  reg2 <- file.path(tempdir(), "notes_named.pptx")
  suppressMessages(
    stow_register(out, "ppt/media/notes.txt",
                  content_types = c("ppt/media/notes.txt" = "text/x-thing"),
                  out = reg2)
  )
  ct2 <- file.path(tempdir(), "ct3.xml")
  suppressMessages(unstow(reg2, "[Content_Types].xml", out = ct2))
  s2 <- paste(readLines(ct2, warn = FALSE), collapse = "")
  expect_match(s2, 'ContentType="text/x-thing"', fixed = TRUE)

  # registering a part that is not in the package is an error
  expect_error(suppressMessages(stow_register(out, "ppt/media/ghost.csv",
                                               out = tempfile())),
               "not in the package")
  # non-OOXML containers are refused
  plain <- file.path(tempdir(), "plain.zip")
  writeBin(empty_zip(), plain)
  f <- mk_payload(10, seed = 13)
  pz <- file.path(tempdir(), "plain_stowed.zip")
  suppressMessages(stow(plain, f, out = pz))
  expect_error(suppressMessages(stow_register(pz, "payload.bin",
                                              out = tempfile())),
               "not an OOXML package")
})

test_that("extensions already covered by a Default are skipped", {
  src <- FIXTURE()
  # real Office [Content_Types].xml has <Default Extension="xml" .../>
  xml_part <- file.path(tempdir(), "notes.xml")
  writeLines("<x/>", xml_part)
  out <- file.path(tempdir(), "xml_covered.pptx")
  suppressMessages(stow(src, xml_part, out = out))
  reg <- file.path(tempdir(), "xml_covered_reg.pptx")
  res <- NULL
  expect_message({ res <- stow_register(out, "ppt/media/notes.xml", out = reg) },
                "already registered|already covered")
  # nothing to do: the input is returned untouched and no output is written
  expect_identical(res, out)
  expect_false(file.exists(reg))
  expect_true(all(stow_test(out)))
})

test_that("register on an OOXML package written by officer still opens", {
  skip_on_cran()
  skip_if_not_installed("officer")
  src <- FIXTURE()
  payload <- file.path(tempdir(), "officer_payload.csv")
  writeLines("a,b\n1,2", payload)
  out <- file.path(tempdir(), "officer_reg.pptx")
  suppressMessages(stow(src, payload, out = out, register = TRUE))
  # officer reads the package through the central directory like PowerPoint
  expect_silent(officer::read_pptx(out))
})
