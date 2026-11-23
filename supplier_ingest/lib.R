## ===========================================================================
## 供方数据入仓门禁：公共函数
##   - 读取供方/修复数据（统一 schema 与列类型）
##   - confrontation 行级结果 -> 合格/隔离拆分
##   - 隔离状态与事件日志的读写
## 依赖：base R + validate
## ===========================================================================

## 供方数据列类型（供方原始文件与修复文件共用同一 schema）
COL_CLASSES <- c(
  id          = "character",
  size        = "character",
  incl.prob   = "numeric",
  staff       = "numeric",
  turnover    = "numeric",
  other.rev   = "numeric",
  total.rev   = "numeric",
  staff.costs = "numeric",
  total.costs = "numeric",
  profit      = "numeric",
  vat         = "numeric"
)

STATUS_OPEN     <- "open"       # 隔离中，待修复
STATUS_RESOLVED <- "resolved"   # 修复后重验通过，已解除隔离

now_str <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

## 读取供方/修复 CSV（分号分隔，NA 表示缺失）；列顺序固定为 COL_CLASSES
read_supplier_csv <- function(path) {
  d <- read.csv(path, sep = ";", na.strings = c("NA", ""),
                stringsAsFactors = FALSE, check.names = FALSE)
  missing_cols <- setdiff(names(COL_CLASSES), names(d))
  if (length(missing_cols) > 0L)
    stop("数据缺少必需列: ", paste(missing_cols, collapse = ", "),
         "（文件: ", path, "）")
  extra_cols <- setdiff(names(d), names(COL_CLASSES))
  if (length(extra_cols) > 0L)
    message("提示: 忽略多余列: ", paste(extra_cols, collapse = ", "))
  d <- d[, names(COL_CLASSES), drop = FALSE]
  for (nm in names(COL_CLASSES)) {
    d[[nm]] <- switch(COL_CLASSES[[nm]],
                      character = as.character(d[[nm]]),
                      numeric   = as.numeric(d[[nm]]),
                      integer   = as.integer(d[[nm]]))
  }
  d
}

## 原行键是隔离/重验的定位基础：缺失或重复时直接中止
check_keys <- function(d, what = "供方数据") {
  if (any(is.na(d$id)))
    stop(what, "存在缺失的原行键 id，无法按键隔离/重验，请先修复键列")
  if (anyDuplicated(d$id))
    stop(what, "原行键 id 重复: ",
         paste(unique(d$id[duplicated(d$id)]), collapse = ", "))
}

## confrontation 的行级结果：data.frame(id, <每条规则一列 TRUE/FALSE/NA>)
row_level_results <- function(dat, rules) {
  m <- values(confront(dat, rules))
  data.frame(id = dat$id, as.data.frame(m), stringsAsFactors = FALSE)
}

## 行级结果 -> 失败标记矩阵（TRUE=未通过；规则无法评估的 NA 按未通过处理）
fail_matrix <- function(rr) {
  rcols <- setdiff(names(rr), "id")
  fm <- as.matrix(as.data.frame(lapply(rr[rcols], function(x) is.na(x) | !x)))
  colnames(fm) <- rcols
  fm
}

## 隔离记录表：id + 失败规则 + 修复理由 + 记录当前值（payload）
quarantine_table <- function(dat, rr, fm, book, idx) {
  rcols <- colnames(fm)
  if (length(idx) == 0L)
    return(data.frame(id = character(), n_failed_rules = integer(),
                      failed_rules = character(), reasons = character(),
                      stringsAsFactors = FALSE))
  rows <- lapply(idx, function(i) {
    failed <- rcols[fm[i, ]]
    b <- book[match(failed, book$rule), ]
    data.frame(id             = rr$id[i],
               n_failed_rules = length(failed),
               failed_rules   = paste(failed, collapse = ";"),
               reasons        = paste(b$reason, collapse = " | "),
               stringsAsFactors = FALSE)
  })
  q <- do.call(rbind, rows)
  payload <- dat[match(q$id, dat$id), setdiff(names(dat), "id"), drop = FALSE]
  cbind(q, payload, stringsAsFactors = FALSE)
}

## 长表：id x 失败规则（每条失败规则一行，便于派单/跟踪）
reasons_long_table <- function(rr, fm, book, idx) {
  rcols <- colnames(fm)
  if (length(idx) == 0L)
    return(data.frame(id = character(), rule = character(),
                      fields = character(), reason = character(),
                      stringsAsFactors = FALSE))
  rows <- lapply(idx, function(i) {
    failed <- rcols[fm[i, ]]
    b <- book[match(failed, book$rule), ]
    data.frame(id = rr$id[i], rule = b$rule, fields = b$fields,
               reason = b$reason, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

## 合格/隔离拆分：全部规则通过 -> 合格；否则隔离（含原行键与理由）
split_by_gate <- function(dat, rules, book) {
  rr <- row_level_results(dat, rules)
  fm <- fail_matrix(rr)
  n_fail <- rowSums(fm)
  bad <- which(n_fail > 0L)
  list(
    qualified    = dat[n_fail == 0L, , drop = FALSE],
    quarantine   = quarantine_table(dat, rr, fm, book, bad),
    reasons_long = reasons_long_table(rr, fm, book, bad)
  )
}

## 初始隔离状态：全部 open
init_status <- function(quar, run_at) {
  data.frame(id             = quar$id,
             status         = STATUS_OPEN,
             n_failed_rules = quar$n_failed_rules,
             failed_rules   = quar$failed_rules,
             reasons        = quar$reasons,
             updated_at     = run_at,
             stringsAsFactors = FALSE)
}

write_csv <- function(x, path) {
  write.csv(x, path, row.names = FALSE, quote = TRUE, na = "",
            fileEncoding = "UTF-8")
  invisible(path)
}

read_status <- function(path) {
  s <- read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                fileEncoding = "UTF-8")
  s$n_failed_rules <- as.integer(s$n_failed_rules)
  s
}

## 追加事件日志（quarantined / resolved / still_open）
append_events <- function(path, events) {
  if (nrow(events) == 0L) return(invisible(path))
  old <- if (file.exists(path))
    read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
             fileEncoding = "UTF-8")
  write_csv(rbind(old, events), path)
}
