#!/usr/bin/env Rscript
## ===========================================================================
## 修复后按原键重验，并更新隔离状态
##
## 用法:
##   Rscript revalidate.R <修复CSV> [供方CSV] [输出目录]
##
## 修复CSV 要求:
##   - 与供方数据同一 schema（列名一致，分号分隔，NA 表示缺失）
##   - id 列为原行键；只需包含要重验的隔离记录（可部分提交）
##
## 行为:
##   - 修复文件中的记录按原行键重新 confront 门禁规则
##   - 全部通过 -> 状态置 resolved，记录写入 qualified_from_repair.csv（第二批次入仓）
##   - 仍有失败 -> 状态保持 open，失败规则与理由刷新为最新结果
##   - 未提交修复的隔离键 -> 状态不变（仍 open）
##   - 不在隔离台账中的键 -> 警告并跳过
##
## 产物（输出目录下）:
##   quarantine_status.csv     更新后的隔离状态台账
##   quarantine.csv            刷新后的隔离清单（仅仍 open 的记录）
##   qualified_from_repair.csv 本次解除隔离的记录（原始 schema，可入仓）
##   quarantine_events.csv     追加事件（resolved / still_open）
## ===========================================================================

script_dir <- local({
  f <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(f)) dirname(normalizePath(sub("^--file=", "", f[1]))) else getwd()
})
source(file.path(script_dir, "rules.R"))
source(file.path(script_dir, "lib.R"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L)
  stop("用法: Rscript revalidate.R <修复CSV> [供方CSV] [输出目录]")
repair_csv   <- args[1]
supplier_csv <- if (length(args) >= 2L) args[2] else
  file.path(script_dir, "..", "pkg", "data", "SBS2000.csv")
out_dir <- if (length(args) >= 3L) args[3] else file.path(script_dir, "out")

status_path <- file.path(out_dir, "quarantine_status.csv")
if (!file.exists(status_path))
  stop("未找到 ", status_path, "，请先运行 split.R 生成隔离清单")

run_at <- now_str()

original <- read_supplier_csv(supplier_csv)
check_keys(original, "供方数据")
repaired <- read_supplier_csv(repair_csv)
check_keys(repaired, "修复数据")
status <- read_status(status_path)

## 只重验台账中的隔离键；未知键警告并跳过
unknown <- setdiff(repaired$id, status$id)
if (length(unknown) > 0L)
  message("警告: 以下原行键不在隔离台账中，已跳过: ",
          paste(unknown, collapse = ", "))
rk <- repaired[repaired$id %in% status$id, , drop = FALSE]
if (nrow(rk) == 0L)
  stop("修复文件中没有可重验的隔离记录")

## 按原键重验
gate <- supplier_rules()
book <- rule_book()
rr <- row_level_results(rk, gate)
fm <- fail_matrix(rr)
n_fail <- rowSums(fm)

resolved_ids <- rk$id[n_fail == 0L]
still <- quarantine_table(rk, rr, fm, book, which(n_fail > 0L))

## 更新台账
i_res <- match(resolved_ids, status$id)
status$status[i_res]         <- STATUS_RESOLVED
status$n_failed_rules[i_res] <- 0L
status$failed_rules[i_res]   <- ""
status$reasons[i_res]        <- ""
status$updated_at[i_res]     <- run_at
if (nrow(still) > 0L) {
  i_open <- match(still$id, status$id)
  status$status[i_open]         <- STATUS_OPEN
  status$n_failed_rules[i_open] <- still$n_failed_rules
  status$failed_rules[i_open]   <- still$failed_rules
  status$reasons[i_open]        <- still$reasons
  status$updated_at[i_open]     <- run_at
}
write_csv(status, status_path)

## 第二批次合格记录（原始 schema，可直接入仓）
write_csv(rk[rk$id %in% resolved_ids, , drop = FALSE],
          file.path(out_dir, "qualified_from_repair.csv"))

## 刷新隔离清单：仅仍 open 的记录；载荷 = 原始数据叠加本次修复值
current <- original
current[match(rk$id, current$id), ] <- rk
q <- status[status$status == STATUS_OPEN,
            c("id", "n_failed_rules", "failed_rules", "reasons")]
payload <- current[match(q$id, current$id), setdiff(names(current), "id"),
                   drop = FALSE]
write_csv(cbind(q, payload, stringsAsFactors = FALSE),
          file.path(out_dir, "quarantine.csv"))

## 事件日志
append_events(file.path(out_dir, "quarantine_events.csv"),
              data.frame(run_at = run_at,
                         id = c(resolved_ids, still$id),
                         event = c(rep("resolved", length(resolved_ids)),
                                   rep("still_open", nrow(still))),
                         failed_rules = c(rep("", length(resolved_ids)),
                                          still$failed_rules),
                         stringsAsFactors = FALSE))

cat(sprintf("重验记录: %d | 解除隔离: %d | 仍隔离: %d | 未提交修复: %d\n",
            nrow(rk), length(resolved_ids), nrow(still),
            sum(status$status == STATUS_OPEN) - nrow(still)))
if (length(resolved_ids) > 0L)
  cat("解除隔离（可入仓）: ", paste(resolved_ids, collapse = ", "), "\n")
if (nrow(still) > 0L) {
  cat("仍隔离记录（原行键 + 最新理由）:\n")
  print(still[, c("id", "failed_rules", "reasons")],
        row.names = FALSE, right = FALSE)
}
