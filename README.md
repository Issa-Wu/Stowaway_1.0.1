# stowaway — 把文件"夹带"进 Office 文档（纯 base R）

[![R-CMD-check](https://github.com/Issa-Wu/Stowaway_1.0.1/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Issa-Wu/Stowaway_1.0.1/actions/workflows/R-CMD-check.yaml)

`.pptx` / `.docx` / `.xlsx` 本质是 ZIP 容器（OPC 格式）。stowaway 在**不动原档案一个字节**的前提下，
往容器里追加任意文件、取回、删除、全量 CRC 校验——全程只用 base R，零依赖、零编译代码、零外部 zip 工具。

Office 对"没有任何关系引用的 part"（死零件）视而不见：演示文稿照常打开、照常放映，
而任何解压工具都能把夹带的文件原样取回。一次汇报、一个文件，幻灯片连同它的原始数据、
分析代码、补充材料一起走。

本包源自工作区 `pptx_zip_demo` PoC 的正式化延续，并经独立评审后重写：
EOCD 自洽校验、zip-slip 防护、条目级 ZIP64 哨兵、原子落盘等 8 项必修全部落地。

## 安装

```r
# 从本地 tar.gz
install.packages("stowaway_0.1.0.tar.gz", repos = NULL, type = "source")
# 或从源码目录
devtools::install_local("stowaway")
```

## 快速上手

```r
library(stowaway)

talk <- "mytalk.pptx"

# 1) 夹带：csv + R 脚本 + png 一趟进包（自动落位 ppt/media/，按扩展名选压缩方式）
out <- stow(talk, c("data.csv", "analysis.R", "fig1.png"))
stow_list(out)        # 查看全部条目（含 CRC、偏移、方法、加密位）
stow_test(out)        # 全量 CRC32 校验（真解压校验，非读头）

# 2) 合规（可选）：在 [Content_Types].xml 里补 Override，让严格 OPC 校验器也无话可说
out <- stow_register(out, c("ppt/media/data.csv", "ppt/media/analysis.R"))

# 3) 任何人任何时候取回（任何解压工具都行；这里用本包）
unstow(out, "ppt/media/data.csv")          # 写到当前目录（或指定 out = 路径）
unstow_all(out, exdir = "extracted/")      # 全包解出，带 zip-slip 防护

# 4) 删除 / 概览
stow_remove(out, "ppt/media/fig1.png")
stow_info(out)                              # EOCD 几何、OOXML 类型、媒体目录、注释
crc32(charToRaw("123456789"))               # "CBF43926"（独立小工具）
```

## 三种"层级"

| 层级 | 做法 | 状态 |
|---|---|---|
| A 纯夹带 | 只追加 ZIP entry，原包字节零改动 | ✅ `stow()` 默认 |
| B 合规夹带 | A + `[Content_Types].xml` 注入 `<Override/>`（幂等、字节级、不整包重压） | ✅ `stow(register=TRUE)` / `stow_register()` |
| C 真·嵌入对象 | `ppt/embeddings/*.bin` + rels + slide XML 三处联动，放映时双击可打开 | ❌ 未实现（属 officer/OLE 领域，见 PoC 文档第五节） |

> B 层级的关键问题——"[Content_Types].xml 必须是第一个 entry 吗？"——**否**。
> ISO/IEC 29500-2 只要求它叫这个名字、位于包根；PowerPoint、LibreOffice、python-pptx、
> OpenXML SDK 全部经 ZIP 中央目录按名定位。"必须在第一个"是 ODF `mimetype` 规则的误传。
> 本包的 register 实现就是"删旧 CT + 追加改写后的 CT"，单趟重建，不整包重压。

## 实测数字（R 4.4.1 x64，Windows，2026-11-24）

| 场景 | 结果 |
|---|---|
| 3 页 fixture.pptx（30,258 B，42 entry） | — |
| 5,000,000 B 随机二进制，deflate | **0.14 s**，输出 5,031,141 B；取回 md5 一致 |
| pptx 嵌套进自己（30,258 B，store 自动识别） | **0.02 s**，输出 60,634 B；嵌套包可再解析，md5 一致 |
| 20 × 100 KB 单趟批量追加 | **0.10 s**，42 → 62 entry |
| register：CT 重写（无整包重压） | **0.11 s**，产物仍恰有 1 份 CT，Override 在位 |
| 43 entry 全量 CRC 校验（stow_test） | **0.03 s** |
| 取回 5 MB deflate 条目（unstow） | **0.03 s** |
| LibreOffice 渲染注入版 vs 原版 | 两份 3 页 PDF **完全相同**（均 73.1 KB） |
| Python `zipfile.testzip()`（外部权威） | 4 个产物全部无坏条目（None） |
| PowerPoint 2016（COM，上会话同机制实测） | `Slides.Count = 3`，无修复提示 |
| officer `read_pptx()` 读注入版 | 正常解析（测试套件内） |
| **testthat 套件** | **130 通过 / 0 失败**（含 zip-slip 对抗样本、EOCD 注释陷阱、SFX 前缀档） |
| `R CMD check --as-cran --no-manual` | **0 error / 0 warning / 2 NOTE**（New submission + 沙箱无法校时） |

对照：上会话 PoC 的 5 MB 场景为 0.21 s，本包 0.14 s。

## 设计笔记：为什么 CRC32 要靠 gzfile 收割

ZIP entry 需要 DEFLATE 流 + CRC32，而 base R 两者都不直接提供。本包走了一条借道：
`gzfile(temp, "wb", compression=9)` 写出的就是**真 RFC 1952 gzip**——剥掉 10 字节头，
尾部 8 字节恰好是 CRC32 + ISIZE。于是一次压缩同时拿到两样东西，连 CRC 查表都不用写。

为什么不用看起来更顺手的替代品（全部实测踩过）：

1. `memCompress(x, "gzip")` **名不副实**：R 4.4.1（Windows x64）实际输出 zlib 格式
   （`78 9c` 头 + Adler-32 尾），不是 gzip，拿不到 CRC32。
2. `gzcon(rawConnection)` 内存路径：`close(gzcon)` 会把底层 rawConnection 一并失效，
   `rawConnectionValue()` 报 `invalid connection`，值取不出来。
3. 纯 R 位级 CRC32：R 整数是**有符号 32 位**，多项式 `0xEDB88320` 直接溢出；
   换 double 逐字节循环 5 MB 要分钟级。放弃。
4. 附带的格式化坑：CRC ≥ 2³¹ 时 `sprintf("%08x")` / `as.hexmode` 全军覆没，
   只能拆两个 16 位段格式化（包内 `zip_fmt_u32`）。

取回方向同理复用：给裸 deflate 流重新套上 gzip 框架（头 + 原 CRC + ISIZE），
`memDecompress()` 解压时 zlib/libdeflate **自动校验** CRC——所以本包的校验是
"解压即校验"，没有额外开销。

## 坑与边界（必读）

1. **这不是隐写**。ZIP 无加密，`unzip -l`、7-Zip 一眼可见全部条目；杀软/DLP 会展开扫描。
   想要保密请先自行加密载荷（如 `openssl` 包）再夹带。
2. **体积**：文件大小、邮件附件限制实打实涨；Office 打开时载入整个包结构。
3. **ZIP32 边界**：容器/载荷 > 2 GiB（R raw 向量与内存模型的实际上限）、条目 > 65,535、
   或 4 GiB 偏移 → 显式 `stop`，绝不静默回绕。ZIP64 输入一律拒绝并明说。
4. **DOS 时间戳**：区间 [1980, 2107]，按 **UTC** 记录（跨机器可复现）；区间外自动钳制。
5. **ODF 警告**：`.odt/.ods/.odp` 的 `mimetype` 必须是第一个 entry 且 store——
   本包对 ODF 只保证追加安全，删 `mimetype` 会得到明确 warning。
6. **zip-slip 已防**：`unstow_all` 先校验全部条目名（拒绝 `..`、绝对路径、盘符、
   Windows 保留名、控制字符）再写盘；校验失败时目标目录一个字节都不落。
7. **UTF-8 文件名**：非 ASCII 名自动置 bit 11（UTF-8 标志）；老解压工具可能显示乱码，
   非必要用 ASCII 名。名字长度按**字节**算（≤ 65,535）。
8. **加密档**：条目加密位（flag bit 0）会被识别并在解压路径上明确拒绝。
9. **原档案完整性**：追加/删除走的都是"span 平移"重建——被保留条目的本地数据、
   时间戳、extra 字段、外部属性一字节不动，仅 CD 偏移字段与 EOCD 重算；
   写盘走"临时文件 → 校验 → rename"，中途崩溃不会留下半成品。
10. **CRC 可选性**：`unstow(verify = FALSE)` 可跳过 store 条目的逐字节 CRC 复核
    （DEFLATE 的校验在解压时本来就是免费的）。

## API 一览

| 函数 | 作用 |
|---|---|
| `stow()` | 夹带文件（自动方法选择 / 批量 / 原地覆写 / register 一键到 B 层级） |
| `unstow()` / `unstow_all()` | 取回单条 / 全包解出（zip-slip 防护） |
| `stow_list()` / `stow_info()` | 条目清单（CRC/偏移/方法/加密位）/ 结构概览 |
| `stow_test()` | 全量或指定条目的真 CRC 校验 |
| `stow_remove()` | 删除条目（span 重建） |
| `stow_register()` | `[Content_Types].xml` Override 注入（幂等） |
| `ooxml_media_dir()` | 检测文档类型并给出惯例媒体目录 |
| `crc32()` / `guess_content_type()` | 独立小工具 |

## CRAN 发布前待办

- `DESCRIPTION` 中 `wzy-amu@example.com` 为**占位邮箱**，提交前请替换为真实联系方式。
- 可选：补一篇 `vignettes/stowaway.Rmd`（本 README 已覆盖主要内容）。
- 名字 `stowaway` 已核实 CRAN 25,833 个在库包中无冲突（2026-11-24 查证；近似名 `stow` 功能无关）。

## 复现（仓库内脚本）

```powershell
# 测试套件（130 项）
Rscript -e "testthat::test_local('.')"
# 性能基准（产物落在 ./stowaway_bench/，已 gitignore）
Rscript scripts/benchmark.R
# 外部权威校验（Python stdlib zipfile 全量 CRC）
python3 scripts/validate_stowaway.py stowaway_bench
```

## 安装（任选其一）

```r
# GitHub 在线安装（服务器/本机均可，需先 install.packages("remotes")）
remotes::install_github("<你的用户名>/stowaway")

# 或克隆后本地安装
remotes::install_local("stowaway")
```

纯 base R 包：服务器**不需要**编译器或任何系统库，R >= 4.0 即可装。
