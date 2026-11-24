# stowaway 性能基准（仓库版，输出落在 ./stowaway_bench/，已 gitignore）
# 运行：Rscript scripts/benchmark.R，然后用 python3 scripts/validate_stowaway.py stowaway_bench
suppressMessages(library(stowaway))
set.seed(20261124)
td <- "stowaway_bench"
unlink(td, recursive = TRUE); dir.create(td, showWarnings = FALSE)
md5 <- function(p) unname(tools::md5sum(p))

f_fix <- system.file("extdata", "fixture_minimal.pptx", package = "stowaway")
n_fix <- nrow(stow_list(f_fix))
cat(sprintf("fixture: %s (%d bytes, %d entries)\n", f_fix, file.size(f_fix), n_fix))

# --- A: 5,000,000 B 随机二进制，deflate ---
big <- file.path(td, "big_random.bin")
writeBin(as.raw(sample(0:255, 5e6, replace = TRUE)), big)
t1 <- system.time(
  outA <- suppressMessages(stow(f_fix, big, entries = "ppt/media/payload.bin",
                                out = file.path(td, "A_5mb.pptx"),
                                mtime = as.POSIXct("2026-11-24 12:00:00", tz = "UTC")))
)[["elapsed"]]
suppressMessages(
  unstow(outA, "ppt/media/payload.bin", out = file.path(td, "A_recovered.bin")))
cat(sprintf("A: 5,000,000 B random deflate -> %.2fs, md5 roundtrip: %s\n",
            t1, identical(md5(big), md5(file.path(td, "A_recovered.bin")))))

# --- B: pptx 嵌套进自己（store，auto 识别） ---
t2 <- system.time(
  outB <- suppressMessages(stow(f_fix, f_fix, entries = "ppt/media/nested.pptx",
                                out = file.path(td, "B_nested.pptx")))
)[["elapsed"]]
cat(sprintf("B: pptx(%d B) nested store -> %.2fs\n", file.size(f_fix), t2))

# --- C: 20 x 100 KB 单趟批量 ---
files <- vapply(seq_len(20), function(i) {
  p <- file.path(td, sprintf("c%02d.csv", i))
  writeBin(as.raw(sample(0:255, 1e5, replace = TRUE)), p); p
}, character(1))
t3 <- system.time(
  outC <- suppressMessages(stow(f_fix, files,
                                entries = paste0("ppt/media/c", sprintf("%02d", 1:20), ".csv"),
                                out = file.path(td, "C_batch.pptx")))
)[["elapsed"]]
cat(sprintf("C: 20 x 100 KB batch -> %.2fs\n", t3))

# --- D: register 路径（Content_Types 重写，无整包重压） ---
csv <- file.path(td, "reg.csv")
writeLines(c("exposure,beta", "pm25,0.32"), csv)
outD0 <- suppressMessages(stow(f_fix, csv, out = file.path(td, "D_level_a.pptx")))
t4 <- system.time(
  outD <- suppressMessages(stow_register(outD0, "ppt/media/reg.csv",
                                         out = file.path(td, "D_level_b.pptx")))
)[["elapsed"]]
cat(sprintf("D: [Content_Types].xml Override rewrite -> %.2fs\n", t4))

# --- E/F: 全量 CRC 校验 + 取回 ---
t5 <- system.time(resE <- stow_test(outA))[["elapsed"]]
t6 <- system.time(suppressMessages(
  unstow(outA, "ppt/media/payload.bin", out = file.path(td, "F_recovered.bin"),
         overwrite = TRUE)))[["elapsed"]]
cat(sprintf("E: full CRC check -> %.2fs (all pass: %s)\nF: unstow 5 MB -> %.2fs\n",
            t5, all(resE), t6))
cat(sprintf("\nBENCH OUTPUT DIR: %s%s%s\n", getwd(), .Platform$file.sep, td))
