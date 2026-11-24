# Writing: entry-name validation, method choice, record build, rebuild core ----

# Extensions whose files are already compressed; stored verbatim by
# method = "auto" to avoid wasting CPU on a second compression pass.
zip_store_extensions <- c(
  "7z", "avi", "bz2", "br", "docm", "docx", "ear", "epub", "flac", "gif",
  "gz", "heic", "jar", "jpeg", "jpg", "lz4", "m4v", "mkv", "mov", "mp3",
  "mp4", "mpeg", "mpg", "odg", "odp", "ods", "odt", "ogg", "opus", "parquet",
  "pdf", "png", "pptm", "pptx", "rar", "swf", "tbz2", "tgz", "ts", "ttf",
  "otf", "webm", "webp", "wmv", "woff", "woff2", "xlsm", "xlsx", "xz", "zip",
  "zst"
)

zip_auto_method <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% zip_store_extensions) return("store")
  head <- readBin(path, "raw", 8L)
  if (length(head) >= 4L) {
    if (identical(head[1:4], as.raw(c(0x50, 0x4b, 0x03, 0x04)))) return("store") # zip
    if (identical(head[1:4], as.raw(c(0x1f, 0x8b, 0x08)))) return("store")        # gzip
    if (identical(head[1:4], as.raw(c(0x89, 0x50, 0x4e, 0x47)))) return("store") # png
    if (identical(head[1:4], as.raw(c(0xff, 0xd8, 0xff)))) return("store")       # jpeg
    if (identical(head[1:4], charToRaw("%PDF"))) return("store")
    if (length(head) >= 8L && identical(head[5:8], charToRaw("ftyp"))) return("store") # mp4/mov/heic
  }
  if (length(head) >= 3L && identical(head[1:3], charToRaw("BZh"))) return("store")   # bzip2
  "deflate"
}

# Best-effort percent-decoding used to reject names like "a/%2e%2e/b" that
# would become path traversals once an OPC/URI layer decodes them.
zip_pct_decode <- function(s) {
  m_all <- gregexpr("%[0-9A-Fa-f]{2}", s)
  if (m_all[[1L]][1L] == -1L) return(s)
  hexs <- regmatches(s, m_all)[[1L]]
  vals <- strtoi(substring(hexs, 2L), 16L)
  if (any(is.na(vals)) || any(vals >= 0x80)) return(s) # non-ASCII escapes: cannot form ASCII ".."
  m <- m_all[[1L]]
  seg_start <- c(1L, m + 3L)
  seg_end <- c(m - 1L, nchar(s))
  segs <- substring(s, seg_start, seg_end)
  # interleave: segments and decoded characters (paste0 has no sep argument;
  # passing one silently merges it into the concatenation, so build by index)
  full <- character(2L * length(m) + 1L)
  full[1L] <- segs[1L]
  full[seq.int(2L, by = 2L, length.out = length(m))] <-
    vapply(vals, function(v) rawToChar(as.raw(v)), character(1))
  full[seq.int(3L, by = 2L, length.out = length(m))] <- segs[-1L]
  paste(full, collapse = "")
}

# Normalize and validate one entry name (ZIP-side rules). Must be called
# before any name reaches a header; called vectorized via vapply().
zip_normalize_entry <- function(entry, allow_content_types = FALSE) {
  if (!is.character(entry) || length(entry) != 1L || is.na(entry)) {
    stop("entry names must be single non-NA character strings")
  }
  e <- enc2utf8(entry)
  if (!nzchar(e)) stop("entry names must not be empty")
  nb <- charToRaw(e)
  if (any(as.integer(nb) < 0x20) || any(as.integer(nb) == 0x7F)) {
    stop("entry names must not contain control characters: ", entry)
  }
  e <- gsub("\\", "/", e, fixed = TRUE)
  if (grepl("/$", e)) {
    stop("entry names must not end with '/' (directory entries are not files), got: ", entry)
  }
  if (startsWith(e, "/")) {
    stop("entry names are paths *inside* the archive and must be relative, got: ", entry)
  }
  if (grepl("^[A-Za-z]:", e)) {
    stop("entry names must not be absolute/drive paths, got: ", entry)
  }
  comps <- strsplit(e, "/", fixed = TRUE)[[1L]]
  if (any(comps == "")) {
    stop("entry names must not contain empty or trailing path components (directory entries), got: ", entry)
  }
  if (any(comps %in% c(".", ".."))) {
    stop("entry names must not contain '.' or '..' components, got: ", entry)
  }
  if (grepl("%[0-9A-Fa-f]{2}", e)) {
    comps2 <- strsplit(zip_pct_decode(e), "/", fixed = TRUE)[[1L]]
    if (any(comps2 == "..")) {
      stop("entry name percent-decodes to a '..' component, got: ", entry)
    }
  }
  if (!allow_content_types && e == "[Content_Types].xml") {
    stop("[Content_Types].xml is managed by stow_register(); stow() will not write it directly")
  }
  nb <- length(charToRaw(e))
  if (nb > 65535L) {
    stop("entry name is ", nb, " bytes long; ZIP allows at most 65,535")
  }
  e
}

zip_normalize_entries <- function(entries, allow_content_types = FALSE) {
  vapply(entries, zip_normalize_entry, character(1),
         allow_content_types = allow_content_types, USE.NAMES = FALSE)
}

# Build one add-spec from a payload already in memory.
zip_add_spec <- function(entry, payload, method_id, level, when, file = NA_character_) {
  if (method_id == 8L) {
    d <- zip_deflate_info(payload, level = level)
    comp <- d$stream
    m_id <- 8L
    ver <- 20L
  } else {
    d <- zip_deflate_info(payload, level = 1L) # CRC harvest only; stream discarded
    comp <- payload
    m_id <- 0L
    ver <- 10L
  }
  if (length(payload) >= ZIP_U32_MAX || length(comp) >= ZIP_U32_MAX) {
    stop("payload exceeds the 4 GiB ZIP32 limit: ", file)
  }
  name_raw <- charToRaw(enc2utf8(entry))
  flags <- if (any(as.integer(name_raw) > 0x7F)) 2048L else 0L # bit 11: UTF-8 name
  dos <- zip_dos_stamp(when)
  local <- c(
    sig_local,
    zip_le16(ver), zip_le16(flags), zip_le16(m_id),
    zip_le16(dos$time), zip_le16(dos$date),
    zip_le32(d$crc), zip_le32(length(comp)), zip_le32(length(payload)),
    zip_le16(length(name_raw)), zip_le16(0L),
    name_raw, comp
  )
  cd <- c(
    sig_cd,
    zip_le16(20L), zip_le16(ver), zip_le16(flags), zip_le16(m_id),
    zip_le16(dos$time), zip_le16(dos$date),
    zip_le32(d$crc), zip_le32(length(comp)), zip_le32(length(payload)),
    zip_le16(length(name_raw)), zip_le16(0L), zip_le16(0L),
    zip_le16(0L), zip_le16(0L), zip_le32(0L),
    zip_le32(0L), # local-header offset: patched by zip_rebuild()
    name_raw
  )
  list(entry = entry, name_raw = name_raw, flags = flags, method_id = m_id,
       crc = d$crc, csize = length(comp), usize = length(payload),
       local = local, cd = cd, file = file)
}

# Rebuild the archive: drop some existing entries, append new ones. One
# pass, one output file. Everything the old archive contains is copied
# verbatim except (a) dropped entries' spans, (b) the local-header offset
# field of kept central-directory records, which are shifted left by the
# bytes removed before them, and (c) the EOCD. Old local data, timestamps,
# extra fields, external attributes and the archive comment are untouched.
zip_rebuild <- function(sc, drops = integer(0), adds = list(),
                        out, verify = TRUE) {
  buf <- sc$buf
  eocd <- sc$eocd
  entries <- sc$entries
  n_total <- eocd$n_total
  if (length(drops) && (any(drops < 1L) || any(drops > n_total))) {
    stop("internal error: drop index out of range")
  }
  if (anyDuplicated(sc$names[drops]) > 0L) {
    stop("internal error: duplicate names among dropped entries")
  }

  kept_n <- n_total - length(unique(drops))
  if (kept_n + length(adds) > ZIP_MAX_ENTRIES) {
    stop("the result would need more than 65,535 entries; ZIP64 is not supported")
  }

  # Span model over the local-data area (stream coordinates).
  loffs <- vapply(entries, function(e) e$loff, numeric(1))
  ord <- order(loffs)
  dropped <- rep(FALSE, n_total)
  if (length(drops)) dropped[unique(drops)] <- TRUE
  span_start <- loffs[ord]
  span_end <- c(span_start[-1L], eocd$cd_off)
  span_len <- span_end - span_start
  if (any(span_len < 0)) {
    stop("corrupt container: local records are not laid out in offset order")
  }
  # Bytes removed before each sorted position.
  drop_cum <- cumsum(ifelse(dropped[ord], span_len, 0))
  dropped_before <- c(0, if (length(drop_cum)) drop_cum[-length(drop_cum)] else 0)
  total_dropped <- if (length(drop_cum)) drop_cum[length(drop_cum)] else 0

  # Local-header signature check for every span we keep.
  kept_pos <- which(!dropped[ord])
  phys_start <- eocd$prefix + span_start + 1L
  for (j in kept_pos) {
    if (!identical(buf[phys_start[j]:(phys_start[j] + 3L)], sig_local)) {
      stop("corrupt container: no local header at stream offset ", span_start[j])
    }
  }
  # Leading gap before the first local record (rare, e.g. crafted archives):
  # preserved verbatim so no bytes silently disappear.
  lead <- if (n_total > 0L) span_start[1L] else eocd$cd_off

  # Physical byte pieces: SFX prefix, leading gap, kept spans, new locals.
  pieces <- list()
  if (eocd$prefix > 0) pieces[[length(pieces) + 1L]] <- buf[seq_len(eocd$prefix)]
  if (lead > 0) pieces[[length(pieces) + 1L]] <- buf[(eocd$prefix + 1L):(eocd$prefix + lead)]
  for (j in kept_pos) {
    a <- eocd$prefix + span_start[j] + 1L
    b <- eocd$prefix + span_end[j]
    pieces[[length(pieces) + 1L]] <- if (b >= a) buf[a:b] else raw(0)
  }
  add_local_len <- vapply(adds, function(a) length(a$local), numeric(1))
  add_base <- eocd$cd_off - total_dropped
  for (i in seq_along(adds)) {
    pieces[[length(pieces) + 1L]] <- adds[[i]]$local
  }

  # Central directory: kept records in original order (offsets patched),
  # then new records.
  new_loff_of <- numeric(n_total) # indexed by original entry index
  for (j in seq_len(n_total)) {
    idx <- ord[j]
    new_loff_of[idx] <- span_start[j] - dropped_before[j]
  }
  cd_pieces <- list()
  for (idx in seq_len(n_total)) {
    if (dropped[idx]) next
    rec <- buf[entries[[idx]]$rec_start:(entries[[idx]]$rec_start + entries[[idx]]$rec_len - 1L)]
    if (length(drops) == 0L && new_loff_of[idx] != entries[[idx]]$loff) {
      stop("internal error: kept-entry offsets must not move in append-only mode")
    }
    rec[43:46] <- zip_le32(new_loff_of[idx])
    cd_pieces[[length(cd_pieces) + 1L]] <- rec
  }
  for (i in seq_along(adds)) {
    loff <- add_base + (if (i > 1L) sum(add_local_len[seq_len(i - 1L)]) else 0)
    adds[[i]]$cd[43:46] <- zip_le32(loff)
    cd_pieces[[length(cd_pieces) + 1L]] <- adds[[i]]$cd
  }
  cd_size_new <- sum(vapply(cd_pieces, length, numeric(1)))
  cd_off_new <- add_base + sum(add_local_len)
  if (cd_size_new >= ZIP_U32_MAX || cd_off_new >= ZIP_U32_MAX) {
    stop("the result would exceed the 4 GiB ZIP32 limit; ZIP64 is not supported")
  }

  n_new <- kept_n + length(adds)
  eocd_new <- c(
    sig_eocd,
    zip_le16(0L), zip_le16(0L), zip_le16(n_new), zip_le16(n_new),
    zip_le32(cd_size_new), zip_le32(cd_off_new),
    zip_le16(eocd$comment_len), eocd$comment
  )
  pieces[[length(pieces) + 1L]] <- do.call(c, cd_pieces)
  pieces[[length(pieces) + 1L]] <- eocd_new
  body <- do.call(c, pieces)

  zip_write_atomic(out, body, verify = verify)
  list(out = out, n_entries = n_new, cd_offset = cd_off_new, cd_size = cd_size_new,
       bytes = length(body))
}

# Write `body` to `out` as temp-file + verify + rename, so a crash mid-write
# can never leave a truncated container at the destination path.
zip_write_atomic <- function(out, body, verify = TRUE) {
  dir <- dirname(out)
  if (!dir.exists(dir)) stop("output directory does not exist: ", dir)
  tmp <- tempfile(pattern = ".stowaway-", tmpdir = dir)
  on.exit(unlink(tmp), add = TRUE)
  writeBin(body, tmp)
  if (verify) {
    back <- readBin(tmp, "raw", file.size(tmp))
    if (!identical(back, body)) {
      stop("verification failed: bytes on disk differ from the archive built in memory")
    }
  }
  if (file.exists(out)) unlink(out)
  if (!file.rename(tmp, out)) {
    stop("failed to move the finished archive into place: ", out)
  }
  invisible(out)
}

zip_default_out <- function(src, suffix) {
  ext <- tools::file_ext(src)
  if (nzchar(ext)) paste0(tools::file_path_sans_ext(src), suffix, ".", ext) else paste0(src, suffix)
}

#' Stow files inside a ZIP-based container
#'
#' Appends one or more files as new entries inside an existing ZIP container
#' (`.pptx`, `.docx`, `.xlsx`, `.epub`, plain `.zip`, ...) without touching a
#' single byte of the old archive's data. The new entries are written into
#' the gap between the local records and the central directory — the same
#' trick PowerPoint itself uses when saving — so the old entries' offsets
#' never move and every consumer that walks the central directory sees
#' exactly the old archive plus the new parts. Office applications ignore
#' parts no relationship points to, which is what makes this safe.
#'
#' The implementation is pure base R: DEFLATE streams and CRC32 digests are
#' harvested from [base::gzfile] output, and the output is verified against
#' the in-memory image before being renamed into place.
#'
#' @param src path to an existing ZIP-based container.
#' @param files character vector of paths to regular files to append.
#' @param entries optional ZIP-internal names for the files (same length as
#'   `files`). By default names are derived as follows: when `part_dir` is
#'   given, `part_dir/basename(files)`; else for Office documents the
#'   conventional media directory ([ooxml_media_dir]); else
#'   `basename(files)`.
#' @param out output path. Defaults to `<src minus extension>_stowed.<ext>`.
#'   May equal `src`: the result is then written to a sibling temp file,
#'   verified, and renamed over the original.
#' @param method "auto" (default: already-compressed formats are stored
#'   verbatim, everything else is DEFLATEd), "deflate" or "store".
#' @param level DEFLATE level 1-9 when method = "deflate".
#' @param mtime modification timestamps recorded in the entry headers.
#'   NULL (default) takes each file's mtime; otherwise a single value or a
#'   vector matching `files`, interpreted in UTC. Values outside the DOS
#'   range 1980-2107 are clamped.
#' @param overwrite FALSE (default) refuses entry names that already exist
#'   in the container; TRUE replaces them in the same rebuild pass.
#' @param part_dir single string like "ppt/media" used to prefix
#'   auto-derived entry names.
#' @param register FALSE (default) appends entries only; TRUE additionally
#'   inserts `<Override/>` declarations for them into
#'   `[Content_Types].xml`, giving strict OPC validators a content type
#'   for every part. Only possible when the container is an OOXML package.
#' @param verify TRUE (default) reads the written file back and requires it
#'   to be byte-identical to the archive built in memory.
#'
#' @return `out`, invisibly.
#' @examples
#' \donttest{
#' src <- system.file("extdata", "fixture_minimal.pptx", package = "stowaway")
#' payload <- file.path(tempdir(), "table.csv")
#' writeLines(c("a,b", "1,2"), payload)
#' out <- stow(src, payload, out = file.path(tempdir(), "carrying.pptx"))
#' tail(stow_list(out), 2)
#' unstow(out, "ppt/media/table.csv",
#'        out = file.path(tempdir(), "recovered.csv"))
#' }
#' @export
stow <- function(src, files, entries = NULL, out = NULL,
                 method = c("auto", "deflate", "store"), level = 9L,
                 mtime = NULL, overwrite = FALSE, part_dir = NULL,
                 register = FALSE, verify = TRUE) {
  t0 <- proc.time()
  method <- match.arg(method)
  if (!is.character(files) || anyNA(files) || !all(nzchar(files))) {
    stop("`files` must be a character vector of non-empty paths")
  }
  bad <- files[!file.exists(files) | dir.exists(files)]
  if (length(bad)) {
    stop("not regular files: ", paste(bad, collapse = ", "))
  }
  big <- files[file.size(files) > ZIP_MAX_BYTES]
  if (length(big)) {
    stop("payload exceeds stowaway's 2 GiB limit: ", paste(big, collapse = ", "))
  }

  sc <- zip_scan(src)
  kind <- zip_ooxml_kind(sc$names)

  if (is.null(entries)) {
    base <- basename(files)
    if (!is.null(part_dir)) {
      if (length(part_dir) != 1L || is.na(part_dir) || !nzchar(part_dir)) {
        stop("`part_dir` must be a single non-empty string like \"ppt/media\"")
      }
      base <- paste0(part_dir, "/", base)
    } else if (kind %in% c("pptx", "docx", "xlsx")) {
      base <- paste0(zip_media_dir(kind), "/", base)
    }
    entries <- base
  } else {
    if (!is.character(entries) || length(entries) != length(files)) {
      stop("`entries` must be NULL or match the length of `files`")
    }
  }
  entries <- zip_normalize_entries(entries)
  if (anyDuplicated(entries) > 0L) {
    stop("duplicate entry names within this batch: ",
         paste(entries[duplicated(entries)], collapse = ", "))
  }
  dupes <- unique(entries[entries %in% sc$names])
  drops <- integer(0)
  if (length(dupes)) {
    if (!overwrite) {
      stop("entries already present (use overwrite = TRUE to replace): ",
           paste(dupes, collapse = ", "))
    }
    drops <- which(sc$names %in% dupes)
  }

  if (is.null(mtime)) {
    mtimes <- file.mtime(files)
  } else if (length(mtime) == 1L) {
    mtimes <- rep(mtime, length(files))
  } else if (length(mtime) == length(files)) {
    mtimes <- mtime
  } else {
    stop("`mtime` must be NULL, a single value, or match the length of `files`")
  }

  specs <- vector("list", length(files))
  for (i in seq_along(files)) {
    m_i <- if (method == "auto") zip_auto_method(files[i]) else method
    payload <- readBin(files[i], "raw", file.size(files[i]))
    specs[[i]] <- zip_add_spec(entries[i], payload,
                               method_id = if (m_i == "store") 0L else 8L,
                               level = level, when = mtimes[i], file = files[i])
  }
  n_stowed <- length(specs)

  registered <- 0L
  if (register) {
    ct_idx <- which(sc$names == "[Content_Types].xml")
    if (!length(ct_idx)) {
      message("stowaway: `register` requested but the container has no ",
              "[Content_Types].xml; skipping content-type registration")
    } else {
      ct_new <- zip_ct_with_overrides(sc, ct_idx, entries,
                                      guess_content_type(entries),
                                      verbose = TRUE)
      if (!is.null(ct_new)) {
        specs[[length(specs) + 1L]] <- zip_add_spec(
          "[Content_Types].xml", ct_new$bytes, method_id = 8L, level = level,
          when = Sys.time()
        )
        drops <- c(drops, ct_idx)
        registered <- ct_new$n_inserted
      }
    }
  }

  if (is.null(out)) out <- zip_default_out(src, "_stowed")
  res <- zip_rebuild(sc, drops = drops, adds = specs, out = out, verify = verify)
  el <- (proc.time() - t0)[["elapsed"]]
  msg <- sprintf("stowaway: %d entr%s added to %s (%s bytes in %.2fs)%s",
                 n_stowed, if (n_stowed == 1L) "y" else "ies", basename(out),
                 format(res$bytes, big.mark = ",", scientific = FALSE), el,
                 if (registered > 0L) sprintf("; %d part(s) registered in [Content_Types].xml", registered) else "")
  message(msg)
  invisible(out)
}
