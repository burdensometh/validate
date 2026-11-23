#!/usr/bin/env Rscript
## ===========================================================================
## 供方数据入仓门禁：confrontation 行级结果 -> 合格清单 + 隔离清单
##
## 用法:
##   Rscript split.R [供方CSV] [输出目录]
## 默认:
##   供方CSV = ../pkg/data/SBS2000.csv （相对本脚本）
##   输出目录 = ./out
##
## 产物（输出目录下）:
##   qualified.csv          合格记录（全部规则通过），可直接入仓
##   quarantine.csv         隔离记录：原行键 + 失败规则 + 修复理由 + 记录当前值
##   quarantine_reasons.csv 隔离理由长表（id x 失败规则，便于派单）
##   quarantine_status.csv  隔离状态台账（初始全部 open）
##   quarantine_events.csv  事件日志（quarantined）
## ===========================================================================

script_dir <- local({
  f <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(f)) dirname(normalizePath(sub("^--file=", "", f[1]))) else getwd()
})
source(file.path(script_dir, "rules.R"))
source(file.path(script_dir, "lib.R"))

args <- commandArgs(trailingOnly = TRUE)
supplier_csv <- if (length(args) >= 1L) args[1] else
  file.path(script_dir, "..", "pkg", "data", "SBS2000.csv")
out_dir <- if (length(args) >= 2L) args[2] else file.path(script_dir, "out")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

run_at <- now_str()

## 规则与规则手册一致性自检
gate <- supplier_rules()
book <- rule_book()
stopifnot(identical(names(gate), book$rule))

dat <- read_supplier_csv(supplier_csv)
check_keys(dat, "供方数据")

res <- split_by_gate(dat, gate, book)

write_csv(res$qualified,    file.path(out_dir, "qualified.csv"))
write_csv(res$quarantine,   file.path(out_dir, "quarantine.csv"))
write_csv(res$reasons_long, file.path(out_dir, "quarantine_reasons.csv"))
write_csv(init_status(res$quarantine, run_at),
          file.path(out_dir, "quarantine_status.csv"))

## split 代表新一轮入仓：事件日志重新初始化，并清除上一轮的重验产物
write_csv(data.frame(run_at = run_at, id = res$quarantine$id,
                     event = "quarantined",
                     failed_rules = res$quarantine$failed_rules,
                     stringsAsFactors = FALSE),
          file.path(out_dir, "quarantine_events.csv"))
stale <- file.path(out_dir, "qualified_from_repair.csv")
if (file.exists(stale)) invisible(file.remove(stale))

cat(sprintf("供方记录: %d | 合格入仓: %d | 隔离待修: %d\n",
            nrow(dat), nrow(res$qualified), nrow(res$quarantine)))
if (nrow(res$quarantine) > 0L) {
  cat("隔离记录（原行键 + 理由）:\n")
  print(res$quarantine[, c("id", "failed_rules", "reasons")],
        row.names = FALSE, right = FALSE)
}
