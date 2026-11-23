## ===========================================================================
## 供方数据入仓门禁规则（数据工程维护）
##
## 已知供方问题：少数记录的财务连续字段整块缺失。
## 门禁口径：
##   1. key_present : 原行键 id 必须存在（隔离与重验都靠它定位记录）；
##   2. fin_w1..fin_wN : 财务连续块 FIN_BLOCK 内，任一连续 WIN 个字段不得全缺。
##
## 规则与规则手册 rule_book() 由同一份窗口定义生成：
## 只改 FIN_BLOCK / WIN，规则表达式和修复理由会同步变化。
## ===========================================================================

library(validate)

## 供方报表中财务字段的相邻顺序（连续缺失检查针对该块）
FIN_BLOCK <- c("turnover", "other.rev", "total.rev",
               "staff.costs", "total.costs", "profit")

## 连续缺失容忍上限：任一连续 WIN 个字段不得全部为 NA
WIN <- 3L

## 生成 !(is.na(f1) & is.na(f2) & ...) 形式的表达式
.na_conj <- function(fields) {
  calls <- lapply(fields, function(f) as.call(list(as.name("is.na"), as.name(f))))
  Reduce(function(a, b) as.call(list(as.name("&"), a, b)), calls)
}

## 滑动窗口定义：名称 / 涉及字段 / 规则表达式（单一事实来源）
.window_specs <- function(fields = FIN_BLOCK, win = WIN) {
  stopifnot(length(fields) >= win, win >= 2L)
  lapply(seq_len(length(fields) - win + 1L), function(i) {
    w <- fields[i:(i + win - 1L)]
    list(
      name   = sprintf("fin_w%d", i),
      fields = w,
      expr   = as.call(list(as.name("!"), .na_conj(w)))
    )
  })
}

## 门禁规则集（validator 对象，可直接用于 confront）
supplier_rules <- function() {
  specs <- .window_specs()
  exprs <- c(list(quote(!is.na(id))), lapply(specs, function(s) s$expr))
  v <- do.call(validate::validator, exprs)
  names(v) <- c("key_present", vapply(specs, function(s) s$name, character(1)))
  v
}

## 规则手册：rule -> 涉及字段 / 修复理由（写入隔离清单，供修复参考）
rule_book <- function() {
  specs <- .window_specs()
  rbind(
    data.frame(
      rule   = "key_present",
      fields = "id",
      reason = "原行键 id 缺失，无法按键定位记录",
      stringsAsFactors = FALSE
    ),
    data.frame(
      rule   = vapply(specs, function(s) s$name, character(1)),
      fields = vapply(specs, function(s) paste(s$fields, collapse = ","), character(1)),
      reason = vapply(specs, function(s)
        sprintf("连续字段缺失：%s 全部为 NA", paste(s$fields, collapse = "/")),
        character(1)),
      stringsAsFactors = FALSE
    )
  )
}
