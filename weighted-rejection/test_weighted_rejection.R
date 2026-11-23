## ---------------------------------------------------------------------------
## weighted_rejection.R 的断言式测试
## 运行：Rscript weighted-rejection/test_weighted_rejection.R   （从仓库根目录）
## ---------------------------------------------------------------------------

src <- file.path("weighted-rejection", "weighted_rejection.R")
if (!file.exists(src)) src <- "weighted_rejection.R"  # 允许在脚本所在目录运行
source(src)

ok <- function(cond, msg) {
  if (!isTRUE(cond)) stop("FAILED: ", msg, call. = FALSE)
  cat("ok -", msg, "\n")
}

## --- 1. 手工核算的玩具例子 -------------------------------------------------
fail   <- c(TRUE, FALSE, NA,   TRUE, TRUE, FALSE, TRUE,   FALSE)
weight <- c(2,    4,     6,    NA,   -1,   0,     Inf,    3)
batch  <- c("A",  "A",   "A",  "B",  "B",  "B",   "B",    "C")
tab <- weighted_rejection_rate(fail, weight, batch)

ok(identical(tab$batch, c("A", "B", "C")), "toy: batches sorted and complete")
ok(identical(tab$n_records, c(3L, 4L, 1L)), "toy: n_records")
ok(identical(tab$n_included, c(2L, 0L, 1L)), "toy: n_included (纳入记录)")
ok(isTRUE(all.equal(tab$effective_weight, c(6, 0, 3))), "toy: effective_weight (有效权重)")
ok(isTRUE(all.equal(tab$missing_weight, c(6, 0, 0))), "toy: missing_weight (缺失权重)")
ok(identical(tab$n_weight_missing, c(0L, 4L, 0L)), "toy: unusable weights counted")
ok(isTRUE(all.equal(tab$weighted_reject_rate[1], 2/6)), "toy: A rate = 2/6")
ok(is.na(tab$weighted_reject_rate[2]), "toy: B has no finite positive weight -> rate NA")
ok(tab$weighted_reject_rate[3] == 0, "toy: C rate = 0")

## --- 2. 没有有限正权重的小组保持可识别 --------------------------------------
ok("B" %in% tab$batch && tab$n_included[tab$batch == "B"] == 0,
   "group without finite positive weights stays identifiable")

bf <- factor(c("x", "x"), levels = c("x", "empty"))
t3 <- weighted_rejection_rate(c(TRUE, FALSE), c(1, 2), bf)
ok(nrow(t3) == 2 && t3$n_records[2] == 0 && is.na(t3$weighted_reject_rate[2]),
   "unobserved factor level kept as identifiable empty batch")

## --- 3. 重排源行不改变统计（identical 级别） --------------------------------
n <- length(fail)
set.seed(42)
perms <- list(rev(seq_len(n)), sample(n), sample(n), c(4, 1, 7, 2, 8, 3, 6, 5))
for (p in perms) {
  tp <- weighted_rejection_rate(fail[p], weight[p], batch[p])
  ok(identical(tab, tp), paste0("row reordering leaves statistics identical (perm ",
                                paste(p, collapse = ","), ")"))
}

## --- 4. 真实数据：retailers + validate -------------------------------------
dat <- load_retailers()
real <- compute_weighted_rejection(dat)
print(real)

ok(all(c("batch", "n_records", "n_included", "effective_weight",
         "missing_weight", "n_weight_missing", "weighted_reject_rate") %in% names(real)),
   "real: required columns present")
ok(nrow(real) == nlevels(dat$size), "real: one row per batch")
ok(all(real$n_included <= real$n_records), "real: n_included <= n_records")
ok(all(is.na(real$weighted_reject_rate) |
       (real$weighted_reject_rate >= 0 & real$weighted_reject_rate <= 1)),
   "real: rates within [0,1] or NA")
ok(all((real$n_included == 0) == is.na(real$weighted_reject_rate)),
   "real: rate is NA exactly when no record is included")

## 独立重算（朴素循环、不同实现路径）交叉验证
V    <- validate::values(validate::confront(dat, validate::validator(
  nonneg_staff      = staff >= 0,
  nonneg_turnover   = turnover >= 0,
  nonneg_otherrev   = other.rev >= 0,
  balance_totalrev  = turnover + other.rev == total.rev,
  balance_profit    = profit == total.rev - total.costs,
  staff_costs_bound = staff.costs <= 100 * staff,
  profit_turnover   = profit <= 0.6 * turnover
)))
fail0 <- record_rejected(V)
w0    <- dat$turnover / dat$incl.prob
for (i in seq_len(nrow(real))) {
  b   <- as.character(real$batch[i])
  sel <- which(as.character(dat$size) == b)
  w   <- w0[sel]; f <- fail0[sel]
  inc <- is.finite(w) & w > 0 & !is.na(f)
  ok(real$n_included[i] == sum(inc), paste0("real ", b, ": n_included cross-check"))
  ok(isTRUE(all.equal(real$effective_weight[i], sum(w[inc]))),
     paste0("real ", b, ": effective_weight cross-check"))
  ok(isTRUE(all.equal(real$missing_weight[i],
                      sum(w[is.finite(w) & w > 0 & is.na(f)]))),
     paste0("real ", b, ": missing_weight cross-check"))
  ok(isTRUE(all.equal(real$weighted_reject_rate[i],
                      sum(w[inc] * as.numeric(f[inc])) / sum(w[inc]))),
     paste0("real ", b, ": weighted_reject_rate cross-check"))
  ok(isTRUE(all.equal(real$effective_weight[i] + real$missing_weight[i],
                      sum(w[is.finite(w) & w > 0]))),
     paste0("real ", b, ": effective + missing = total usable weight"))
}

## 真实数据上的行序不变性：重排源数据后整表 identical
set.seed(7)
for (k in 1:3) {
  p <- sample(nrow(dat))
  ok(identical(real, compute_weighted_rejection(dat[p, ])),
     paste0("real: shuffling source rows keeps table identical (round ", k, ")"))
}

## --- 5. 输入校验 ------------------------------------------------------------
err1 <- tryCatch(weighted_rejection_rate(c(TRUE, FALSE), 1, c("a", "a")),
                 error = function(e) "error")
ok(identical(err1, "error"), "length mismatch is an error")
err2 <- tryCatch(weighted_rejection_rate(TRUE, 1, NA_character_),
                 error = function(e) "error")
ok(identical(err2, "error"), "NA batch key is an error")
err3 <- tryCatch(weighted_rejection_rate(c(0.5, 1), c(1, 1), c("a", "a")),
                 error = function(e) "error")
ok(identical(err3, "error"), "non-binary fail is an error")

cat("\nAll tests passed.\n")
