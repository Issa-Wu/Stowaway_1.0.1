# server_install.R —— 在服务器上从 GitHub 私有仓安装 stowaway
#
# 前提：R >= 4.0.0；能访问 github.com（或走代理）
# 用法：
#   1) 生成 fine-grained PAT：https://github.com/settings/personal-access-tokens/new
#      （Resource owner: Issa-Wu；Repository access: 只勾 Stowaway_1.0.1；
#        Permissions -> Contents: Read-only）
#   2) 把 token 放进环境变量（不要写进任何脚本/聊天/提交）：
#        bash:   export GITHUB_PAT=github_pat_你的token
#        或 R:    Sys.setenv(GITHUB_PAT = "github_pat_你的token")
#        永久化:  在服务器 ~/.Renviron 里加一行  GITHUB_PAT=github_pat_你的token
#   3) Rscript server_install.R
repo <- "Issa-Wu/Stowaway_1.0.1"

if (getRversion() < "4.0.0") {
  stop("stowaway 需要 R >= 4.0.0，当前是 ", getRversion())
}
if (!nzchar(Sys.getenv("GITHUB_PAT"))) {
  stop("未检测到 GITHUB_PAT。生成后执行：\n",
       "  Sys.setenv(GITHUB_PAT = 'github_pat_你的token')  # 当前会话\n",
       "  或在 ~/.Renviron 中加入：GITHUB_PAT=github_pat_你的token")
}
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github(repo)

# 冒烟验证：装好即可用，标准 CRC32 测试向量
suppressMessages(library(stowaway))
stopifnot(crc32(charToRaw("123456789")) == "CBF43926")
cat("stowaway", as.character(packageVersion("stowaway")),
    "安装成功，CRC32 冒烟测试通过\n")
cat("试一下: stow_info(system.file('extdata/fixture_minimal.pptx', package = 'stowaway'))\n")
