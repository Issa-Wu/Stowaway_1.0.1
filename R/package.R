#' stowaway: Stow Files Inside Office Documents and ZIP Containers
#'
#' Office documents (.pptx, .docx, .xlsx) and many other everyday formats
#' are ZIP-based OPC containers. stowaway appends, extracts, removes and
#' CRC-verifies arbitrary parts inside such containers using only base R:
#' no external zip utility, no compiled code, no hard dependencies.
#'
#' The core trick: every ZIP archive ends with
#' `[local records][central directory][EOCD]`, and the central directory can
#' be extended in place. stowaway inserts new entries into the gap before
#' the central directory, so existing entries never move and PowerPoint,
#' Word, Excel and LibreOffice simply see a package with extra,
#' unreferenced ("dead") parts - which they ignore, and any unzip tool can
#' still extract. DEFLATE streams and CRC32 digests are harvested from
#' base R's own gzip writer, which is why the whole package is pure R.
#'
#' Typical workflow for embedding supplementary data in a presentation:
#'
#' ```r
#' out <- stow("talk.pptx", c("data.csv", "analysis.R", "figure.png"))
#' stow_register(out, c("ppt/media/data.csv", "ppt/media/analysis.R"))
#' # later, anyone:
#' unstow(out, "ppt/media/data.csv")
#' ```
#'
#' @keywords internal
"_PACKAGE"
