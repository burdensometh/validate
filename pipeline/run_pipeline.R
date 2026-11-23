#!/usr/bin/env Rscript
# ============================================================================
# 证书数据质量流水线
#
# 背景：上游偶发在证书编号尾部填空格。值班员已确认"去除尾部空白"这一类
# 安全修正可以批处理（见 corrections.yml 登记表）。
#
# 行为准则：
#   1. 只有"命中某条规则 且 该规则已在登记表中登记修正方式"的行才会被转换；
#   2. 转换后只重跑受影响的校验（引用了被修改列的规则），并记录前后结果；
#   3. 修正后仍失败（或无法判定）的行保留在 pending 清单中待人工处理；
#   4. 审批外（未登记）规则只报告，绝不修改数据。
#
# 用法：
#   Rscript run_pipeline.R [--data=CSV] [--rules=YML] [--registry=YML] [--outdir=DIR]
# 默认读取本脚本目录下的 data/certificates_upstream.csv、rules.yml、
# corrections.yml，结果写入 out/。
# ============================================================================

suppressMessages({
  library(validate)
  library(yaml)
})

## ---- 安全修正函数白名单 ---------------------------------------------------
## 登记表中引用的 fix 必须在此有实现，否则视为未批准，不执行任何转换。
FIXES <- list(
  trim_trailing_ws = function(x) sub("[[:space:]]+$", "", x)
)

## ---- 上游 feed 的列类型 ----------------------------------------------------
DATE_COLS <- c("valid_from", "valid_to")
NUM_COLS  <- c("qty")

## ---- 参数解析 --------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
opt  <- list()
for (a in grep("^--", args, value = TRUE)) {
  parts <- strsplit(sub("^--", "", a), "=", fixed = TRUE)[[1]]
  opt[[parts[1]]] <- if (length(parts) > 1) paste(parts[-1], collapse = "=") else TRUE
}
script_dir <- local({
  f <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(f)) dirname(normalizePath(sub("^--file=", "", f[1]))) else getwd()
})
data_path     <- if (!is.null(opt$data))     opt$data     else file.path(script_dir, "data", "certificates_upstream.csv")
rules_path    <- if (!is.null(opt$rules))    opt$rules    else file.path(script_dir, "rules.yml")
registry_path <- if (!is.null(opt$registry)) opt$registry else file.path(script_dir, "corrections.yml")
outdir        <- if (!is.null(opt$outdir))   opt$outdir   else file.path(script_dir, "out")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

## ---- 日志（同时写控制台与 pipeline.log） -----------------------------------
log_con <- file(file.path(outdir, "pipeline.log"), open = "wt")
logf <- function(fmt, ...) {
  line <- sprintf(fmt, ...)
  cat(line, "\n", sep = "")
  cat(line, "\n", sep = "", file = log_con)
}

## ---- 工具函数 ---------------------------------------------------------------
# 逐行校验结果矩阵（行 x 规则），单列时保持维度
get_res <- function(cf, rule_names) {
  m <- values(cf)
  if (is.null(dim(m))) m <- matrix(m, ncol = length(rule_names),
                                   dimnames = list(NULL, rule_names))
  m[, rule_names, drop = FALSE]
}
count_rule <- function(m, rn) {
  x <- m[, rn]
  c(passes = sum(x %in% TRUE), fails = sum(x %in% FALSE), nNA = sum(is.na(x)))
}

## ---- 读取输入 ---------------------------------------------------------------
logf("[%s] 质量流水线启动", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
logf("数据:   %s", data_path)
logf("规则:   %s", rules_path)
logf("登记表: %s", registry_path)
logf("输出:   %s", outdir)

dat <- read.csv(data_path, stringsAsFactors = FALSE)
for (cc in intersect(DATE_COLS, names(dat))) dat[[cc]] <- as.Date(dat[[cc]])
for (cc in intersect(NUM_COLS,  names(dat))) dat[[cc]] <- suppressWarnings(as.numeric(dat[[cc]]))
logf("读取数据 %d 行 x %d 列", nrow(dat), ncol(dat))

rules <- validator(.file = rules_path)
lbl <- setNames(as.character(label(rules)), names(rules))
logf("加载规则 %d 条: %s", length(rules), paste(names(rules), collapse = ", "))

registry <- read_yaml(registry_path)$corrections
if (is.null(registry)) registry <- list()
logf("登记表已批准修正 %d 条: %s", length(registry),
     if (length(registry)) paste(names(registry), collapse = ", ") else "(无，全部规则只报告)")
report_only <- setdiff(names(rules), names(registry))
if (length(report_only))
  logf("审批外规则（只报告，不修改数据）: %s", paste(report_only, collapse = ", "))

## ---- 第一步：修正前校验（记录"前"结果） -------------------------------------
cf_before  <- confront(dat, rules)
res_before <- get_res(cf_before, names(rules))
logf("---- 修正前校验结果 ----")
for (rn in names(rules)) {
  b <- count_rule(res_before, rn)
  logf("  [%s] %s: 通过 %d / 失败 %d / 无法判定 %d", rn, lbl[rn], b["passes"], b["fails"], b["nNA"])
}

## ---- 第二步：按登记表做安全修正（仅命中行） ---------------------------------
corrected_cols <- character(0)
rows_corrected <- setNames(integer(length(rules)), names(rules))
reg_fix        <- setNames(rep(NA_character_, length(rules)), names(rules))

for (rn in names(registry)) {
  reg      <- registry[[rn]]
  fix_name <- if (is.null(reg$fix)) "" else as.character(reg$fix)
  col      <- if (is.null(reg$column)) "" else as.character(reg$column)
  reg_fix[rn] <- fix_name

  if (!rn %in% names(rules)) {
    logf("!! 登记表中的规则 '%s' 不在规则文件中，跳过（不做任何转换）", rn)
    next
  }
  fx <- FIXES[[fix_name]]
  if (is.null(fx)) {
    logf("!! 规则 '%s' 登记的修正函数 '%s' 未在白名单实现，跳过（不做任何转换）", rn, fix_name)
    next
  }
  if (!col %in% names(dat)) {
    logf("!! 规则 '%s' 登记的列 '%s' 不在数据中，跳过（不做任何转换）", rn, col)
    next
  }
  fail_idx <- which(!is.na(res_before[, rn]) & !res_before[, rn])
  if (!length(fail_idx)) {
    logf("-- 规则 '%s' 无命中行，无需修正", rn)
    next
  }
  old <- dat[[col]][fail_idx]
  new <- fx(old)
  dat[[col]][fail_idx] <- new
  changed <- fail_idx[new != old]
  rows_corrected[rn] <- length(changed)
  corrected_cols <- union(corrected_cols, col)
  logf("** 规则 '%s' 命中 %d 行，按登记方式 '%s' 修正列 '%s'，实际改写 %d 行",
       rn, length(fail_idx), fix_name, col, length(changed))
  for (i in changed)
    logf("     行 %d: \"%s\" -> \"%s\"", i, old[fail_idx == i], dat[[col]][i])
}

## ---- 第三步：重跑受影响的校验（记录"后"结果） -------------------------------
vars_by_rule <- variables(rules, as = "list")
affected <- names(rules)[vapply(vars_by_rule, function(v) any(v %in% corrected_cols), logical(1))]
logf("---- 受影响校验（引用被修正列%s）----",
     if (length(corrected_cols)) paste0(" ", paste(corrected_cols, collapse = ", ")) else "：无")

res_after <- res_before
rerun     <- character(0)
if (length(affected)) {
  cf_after <- confront(dat, rules[affected])
  res_after[, affected] <- get_res(cf_after, affected)
  rerun <- affected
  logf("重跑规则: %s", paste(affected, collapse = ", "))
  for (rn in affected) {
    b <- count_rule(res_before, rn); a <- count_rule(res_after, rn)
    logf("  [%s] %s: 失败 %d -> %d（通过 %d -> %d，无法判定 %d -> %d）",
         rn, lbl[rn], b["fails"], a["fails"], b["passes"], a["passes"], b["nNA"], a["nNA"])
  }
} else {
  logf("无受影响规则，无需重跑")
}
unaffected <- setdiff(names(rules), rerun)
if (length(unaffected))
  logf("未受影响规则（输入列未变，沿用修正前结果，不重跑）: %s", paste(unaffected, collapse = ", "))

## ---- 第四步：仍失败的行保留待处理 -------------------------------------------
fail_mat    <- is.na(res_after) | !res_after
pending_idx <- which(rowSums(fail_mat) > 0)
failed_rules <- vapply(pending_idx, function(i) {
  tags <- vapply(names(rules), function(rn) {
    if (is.na(res_after[i, rn])) paste0(rn, "(NA)")
    else if (!res_after[i, rn]) rn
    else NA_character_
  }, character(1))
  paste(tags[!is.na(tags)], collapse = ";")
}, character(1))

## ---- 输出 -------------------------------------------------------------------
# 1) 修正后的完整数据
f_corrected <- file.path(outdir, "certificates_corrected.csv")
write.csv(dat, f_corrected, row.names = FALSE)

# 2) 待处理行清单
pending_df <- data.frame(row_id = pending_idx,
                         dat[pending_idx, , drop = FALSE],
                         failed_rules = failed_rules,
                         stringsAsFactors = FALSE)
f_pending <- file.path(outdir, "pending_rows.csv")
write.csv(pending_df, f_pending, row.names = FALSE)

# 3) 前后结果对照报告
report <- do.call(rbind, lapply(names(rules), function(rn) {
  b <- count_rule(res_before, rn)
  was_rerun <- rn %in% rerun
  a <- if (was_rerun) count_rule(res_after, rn) else c(passes = NA_integer_, fails = NA_integer_, nNA = NA_integer_)
  data.frame(rule           = rn,
             label          = lbl[rn],
             scope          = if (rn %in% names(registry)) "registered-auto-fix" else "report-only",
             fix            = reg_fix[rn],
             rows_corrected = rows_corrected[rn],
             before_passes  = b["passes"], before_fails = b["fails"], before_nNA = b["nNA"],
             rerun          = if (was_rerun) "yes" else "no",
             after_passes   = a["passes"], after_fails = a["fails"], after_nNA = a["nNA"],
             stringsAsFactors = FALSE)
}))
f_report <- file.path(outdir, "validation_report.csv")
write.csv(report, f_report, row.names = FALSE)

## ---- 汇总 -------------------------------------------------------------------
before_bad <- sum(rowSums(is.na(res_before) | !res_before) > 0)
logf("---- 汇总 ----")
logf("修正前待处理行: %d -> 修正后待处理行: %d（%d 行经安全修正后全部通过）",
     before_bad, length(pending_idx), before_bad - length(pending_idx))
if (length(pending_idx))
  for (k in seq_along(pending_idx))
    logf("  待处理 行 %d: 仍失败规则 [%s]", pending_idx[k], failed_rules[k])
logf("审批外规则未做任何数据修改，仅报告。")
logf("输出文件: %s", paste(c(f_corrected, f_pending, f_report, file.path(outdir, "pipeline.log")), collapse = ", "))
close(log_con)
