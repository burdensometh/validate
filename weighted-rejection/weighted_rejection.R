## ---------------------------------------------------------------------------
## 按批加权拒收率 / Weighted rejection rate per batch
##
## 动机：抽样检测的货物按批发量配权。简单平均行失败率把每条记录等权对待，
## 会低估大批次（大批发量）的异常。本文件从真实校验结果
## （validate::confront 的输出）与抽样权重计算每个批次的加权拒收率。
##
## 输出列（每个批次一行）：
##   batch                批次键
##   n_records            批次内源记录总数
##   n_included           纳入记录：实际进入拒收率计算的记录数
##                        （权重为有限正数 且 拒收状态已知）
##   effective_weight     有效权重：纳入记录的权重之和（加权拒收率的分母）
##   missing_weight       缺失权重：权重有限为正、但因校验结果缺失（NA）而
##                        未能进入计算的权重质量
##   n_weight_missing     权重缺失/不可用（NA、NaN、Inf 或 <= 0）的记录数
##   weighted_reject_rate 加权拒收率 = sum(w * fail) / sum(w)（对纳入记录）
##
## 口径约定：
##   * 没有有限正权重的批次不会被丢弃：它们以 n_included = 0、
##     effective_weight = 0、weighted_reject_rate = NA 的行保持可识别。
##   * 重排源行不改变统计结果：分组键排序后输出，组内先按 (weight, fail)
##     排序再做浮点累加，因此结果与输入行序无关（identical 级别一致）。
## ---------------------------------------------------------------------------

## 由 validate::values() 的真值矩阵推出每条记录的拒收状态：
##   任一规则为 FALSE      -> TRUE  （拒收）
##   全部规则无法评估 (NA) -> NA    （状态未知）
##   其余                  -> FALSE （通过）
record_rejected <- function(V) {
  V <- as.matrix(V)
  apply(V, 1L, function(r) {
    if (any(!r, na.rm = TRUE)) return(TRUE)
    if (all(is.na(r))) return(NA)
    FALSE
  })
}

## 核心聚合器：按批加权拒收率。
##   fail   逻辑或 0/1 向量，NA 表示拒收状态未知
##   weight 数值抽样权重；仅有限且严格为正的权重可用
##   batch  批次键（factor / character / numeric），不允许 NA
weighted_rejection_rate <- function(fail, weight, batch) {
  n <- length(fail)
  if (length(weight) != n || length(batch) != n)
    stop("fail, weight and batch must have equal lengths")
  if (anyNA(batch))
    stop("`batch` contains NA: every record needs an identifiable batch")

  if (is.logical(fail)) fail <- as.numeric(fail)
  if (!is.numeric(fail) || any(!is.na(fail) & !fail %in% c(0, 1)))
    stop("`fail` must be logical or 0/1, with NA for unknown outcomes")
  weight <- as.numeric(weight)

  ## 批次键：factor 保持水平顺序（含未观测水平），其余按自然顺序排序。
  ## 输出行的顺序因此与源行顺序无关。
  if (is.factor(batch)) {
    f     <- batch
    keys  <- factor(levels(batch), levels = levels(batch),
                    ordered = is.ordered(batch))
  } else {
    u     <- sort(unique(batch))
    f     <- factor(batch, levels = u)
    keys  <- u
  }
  idx_by_group <- split(seq_len(n), f)  # 按水平顺序；空组保留

  cols <- c("n_records", "n_included", "effective_weight",
            "missing_weight", "n_weight_missing", "weighted_reject_rate")

  summarize_group <- function(f, w) {
    w_ok  <- is.finite(w) & w > 0
    known <- !is.na(f)
    incl  <- w_ok & known          # 纳入记录
    miss  <- w_ok & !known         # 权重有效但结果缺失 -> 缺失权重
    ## 确定性累加：先按 (weight, fail) 排序，浮点求和与源行顺序无关
    o     <- order(w[incl], f[incl])
    wi    <- (w[incl])[o]
    fi    <- (f[incl])[o]
    eff   <- sum(wi)
    stats <- c(
      n_records        = length(f),
      n_included       = length(wi),
      effective_weight = eff,
      missing_weight   = sum(sort(w[miss])),
      n_weight_missing = sum(!w_ok),
      weighted_reject_rate = if (length(wi) > 0L) sum(wi * fi) / eff
                             else NA_real_
    )
    stats[cols]
  }

  S <- vapply(idx_by_group,
              function(ix) summarize_group(fail[ix], weight[ix]),
              setNames(numeric(length(cols)), cols))
  M <- t(S)
  colnames(M) <- cols

  data.frame(
    batch                = keys,
    n_records            = as.integer(M[, "n_records"]),
    n_included           = as.integer(M[, "n_included"]),
    effective_weight     = M[, "effective_weight"],
    missing_weight       = M[, "missing_weight"],
    n_weight_missing     = as.integer(M[, "n_weight_missing"]),
    weighted_reject_rate = M[, "weighted_reject_rate"],
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

## ---------------------------------------------------------------------------
## 真实数据管线：validate 包内置 retailers 数据（60 家抽样企业）
##   批（batch）  = size        企业规模类别 sc0..sc3，作为批次键
##   校验结果     = validate::confront() 对 JSS 论文规则的真实评估
##   抽样权重     = turnover / incl.prob
##                  批发量（turnover）乘以设计权重（1/包含概率），
##                  即该抽样记录所代表的总体批发量
## ---------------------------------------------------------------------------

load_retailers <- function() {
  if (!requireNamespace("validate", quietly = TRUE))
    stop("package 'validate' is required for the real-data pipeline")
  env <- new.env()
  data("retailers", package = "validate", envir = env)
  env$retailers
}

compute_weighted_rejection <- function(dat) {
  ## 真实校验规则（源自本仓库 jsspaper/rules.R 的零售商规则子集）
  rules <- validate::validator(
    nonneg_staff      = staff >= 0,
    nonneg_turnover   = turnover >= 0,
    nonneg_otherrev   = other.rev >= 0,
    balance_totalrev  = turnover + other.rev == total.rev,
    balance_profit    = profit == total.rev - total.costs,
    staff_costs_bound = staff.costs <= 100 * staff,
    profit_turnover   = profit <= 0.6 * turnover
  )
  cf   <- validate::confront(dat, rules)
  fail <- record_rejected(validate::values(cf))

  weight <- dat$turnover / dat$incl.prob   # 按批发量配权的抽样权重

  tab <- weighted_rejection_rate(fail, weight, dat$size)
  ## 对照列：等权（weight = 1）即简单平均行失败率
  simple <- weighted_rejection_rate(fail, rep(1, length(fail)), dat$size)
  tab$simple_reject_rate <- simple$weighted_reject_rate
  tab
}

retailers_weighted_rejection <- function() {
  compute_weighted_rejection(load_retailers())
}

## 作为脚本直接运行时：执行真实数据管线并写出 CSV
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  out_csv <- if (length(args) >= 1L) args[[1L]] else
    file.path("weighted-rejection", "output", "weighted_rejection_by_batch.csv")
  dir.create(dirname(out_csv), showWarnings = FALSE, recursive = TRUE)
  tab <- retailers_weighted_rejection()
  write.csv(tab, out_csv, row.names = FALSE, quote = FALSE)
  print(tab)
  message("written: ", out_csv)
}
