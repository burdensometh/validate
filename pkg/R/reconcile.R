# Reconcile indicator values reported by two data sources that use
# different units (for example yuan versus thousand-yuan). Unit ratios
# and absolute/relative tolerances are declared per indicator, records
# are matched on a business key, and differences are computed after
# conversion to a common unit. Results keep the values before and after
# conversion so they can be reviewed, and distinguish missing unit
# declarations from genuine out-of-tolerance differences.


# statuses, in the order they are reported.
.REC_STATUSES <- c("ok","out_of_tolerance","missing_unit","missing_value","unmatched")


#' Declare unit ratios and tolerances per indicator
#'
#' Declare, for each indicator, how values from two data sources are
#' converted to a common unit and which differences are tolerated when
#' the sources are \code{\link{reconcile}}d.
#'
#' @param indicator [\code{character}] Name of an indicator (column) that
#'   occurs in both data sources.
#' @param ratio [\code{numeric}] Unit ratio: one unit of the indicator in
#'   the second data source (\code{y}) equals \code{ratio} units in the
#'   first data source (\code{x}). For example, if \code{x} reports in
#'   yuan and \code{y} reports in thousand-yuan, then \code{ratio} is
#'   1000. Use \code{NA} when the unit of an indicator is not declared.
#' @param abs_tol [\code{numeric}] Absolute tolerance, expressed in units
#'   of \code{x}. Use \code{NA} to take the default set in
#'   \code{\link{reconcile}}.
#' @param rel_tol [\code{numeric}] Relative tolerance (a fraction). Use
#'   \code{NA} to take the default set in \code{\link{reconcile}}.
#'
#' @return A \code{data.frame} of class \code{unit_spec} with one row per
#'   indicator and columns \code{indicator}, \code{ratio}, \code{abs_tol}
#'   and \code{rel_tol}.
#'
#' @seealso \code{\link{reconcile}}
#' @family reconciliation
#' @export
unit_spec <- function(indicator, ratio=1, abs_tol=0, rel_tol=0){
  indicator <- as.character(indicator)
  n <- length(indicator)
  for (nm in c("ratio","abs_tol","rel_tol")){
    v <- get(nm)
    if ( !(is.numeric(v) || all(is.na(v))) || !(length(v) %in% c(1L, n)) )
      stop(sprintf("'%s' must be a numeric vector of length 1 or %d", nm, n)
         , call.=FALSE)
  }
  spec <- data.frame(
      indicator = indicator
    , ratio     = rep_len(as.numeric(ratio), n)
    , abs_tol   = rep_len(as.numeric(abs_tol), n)
    , rel_tol   = rep_len(as.numeric(rel_tol), n)
    , stringsAsFactors = FALSE
  )
  validate_unit_spec(spec)
  class(spec) <- c("unit_spec","data.frame")
  spec
}

# check a data frame with columns indicator, ratio, abs_tol, rel_tol.
validate_unit_spec <- function(spec){
  if ( anyDuplicated(spec$indicator) )
    stop(sprintf("duplicate indicator in unit specification: %s"
      , paste(unique(spec$indicator[duplicated(spec$indicator)]),collapse=", "))
      , call.=FALSE)
  if ( any(!is.na(spec$ratio) & (!is.finite(spec$ratio) | spec$ratio <= 0)) )
    stop("'ratio' must be a positive, finite number (or NA when undeclared)"
      , call.=FALSE)
  if ( any(!is.na(spec$abs_tol) & spec$abs_tol < 0) )
    stop("'abs_tol' must be non-negative", call.=FALSE)
  if ( any(!is.na(spec$rel_tol) & spec$rel_tol < 0) )
    stop("'rel_tol' must be non-negative", call.=FALSE)
  invisible(spec)
}


#' Reconcile indicators from two data sources with different units
#'
#' Compare the same financial indicators reported by two data sources
#' that express them in different units (for example yuan versus
#' thousand-yuan). Records are matched on a business key, values of the
#' second source are converted to the unit of the first source using the
#' unit ratio declared per indicator, and differences are tested against
#' absolute and relative tolerances. This prevents legitimate rounding
#' (for example rounding to whole thousands) from being reported as an
#' anomaly.
#'
#' @section Comparison rule:
#' For indicator \eqn{i}, values \code{y} of the second source are
#' converted to the unit of the first source \code{x} with
#' \eqn{y_{common} = ratio_i \times y}, while \eqn{x_{common} = x}. The
#' difference is \eqn{diff = x_{common} - y_{common}}. A record is
#' \code{"ok"} when
#' \deqn{|diff| \le \max(abs\_tol_i,\ rel\_tol_i \times \max(|x_{common}|,|y_{common}|))}{|diff| <= max(abs_tol_i, rel_tol_i * max(|x_common|, |y_common|))}
#' and \code{"out_of_tolerance"} otherwise. Tolerances declared in
#' \code{spec} take precedence; where the specification holds \code{NA},
#' the defaults passed as \code{abs_tol} and \code{rel_tol} are used.
#'
#' @section Status values:
#' Each combination of business key and indicator gets one of the
#' following statuses.
#' \itemize{
#'   \item{\code{ok}: values agree within tolerance.}
#'   \item{\code{out_of_tolerance}: the difference exceeds the declared
#'     tolerances (a genuine discrepancy).}
#'   \item{\code{missing_unit}: no unit ratio was declared for the
#'     indicator, so no conversion and comparison could be made.}
#'   \item{\code{missing_value}: the indicator is \code{NA} in at least
#'     one of the sources.}
#'   \item{\code{unmatched}: the business key occurs in only one of the
#'     sources.}
#' }
#'
#' @param x [\code{data.frame}] First (reference) data source. Its unit
#'   is the common unit in which differences are reported.
#' @param y [\code{data.frame}] Second data source.
#' @param by [\code{character}] Business key column(s), present in both
#'   \code{x} and \code{y}. The key must uniquely identify records in
#'   both sources and may not contain missing values.
#' @param spec [\code{unit_spec} | \code{data.frame} | \code{NULL}] Unit
#'   ratios and tolerances per indicator, created with
#'   \code{\link{unit_spec}} or a \code{data.frame} with columns
#'   \code{indicator} and \code{ratio} and optionally \code{abs_tol} and
#'   \code{rel_tol}. Indicators without a (non-\code{NA}) \code{ratio}
#'   are reported as \code{"missing_unit"}.
#' @param indicators [\code{character} | \code{NULL}] Columns to compare.
#'   By default all numeric columns that occur in both sources (and are
#'   not key columns) are compared.
#' @param abs_tol [\code{numeric}] Default absolute tolerance, in units
#'   of \code{x}, for indicators where \code{spec} declares \code{NA}.
#' @param rel_tol [\code{numeric}] Default relative tolerance for
#'   indicators where \code{spec} declares \code{NA}.
#'
#' @return An object of class \code{reconciliation}: a
#'   \code{data.frame} with one row per business key and indicator, with
#'   columns
#'   \itemize{
#'     \item{the business key columns in \code{by}}
#'     \item{\code{indicator}: the indicator compared}
#'     \item{\code{x_value}, \code{y_value}: the raw values as recorded
#'       in each source (before conversion)}
#'     \item{\code{ratio}, \code{abs_tol}, \code{rel_tol}: the rules
#'       applied}
#'     \item{\code{x_common}, \code{y_common}: the values after
#'       conversion to the common unit}
#'     \item{\code{diff}: \code{x_common - y_common}}
#'     \item{\code{status}: factor with the status of the comparison}
#'   }
#'   Use \code{as.data.frame} to extract a plain \code{data.frame} for
#'   further review, and \code{summary} for counts per indicator and
#'   status.
#'
#' @example ../examples/reconcile.R
#' @family reconciliation
#' @export
reconcile <- function(x, y, by, spec=NULL, indicators=NULL, abs_tol=0, rel_tol=0){
  if ( inherits(x,"tbl") ) x <- as.data.frame(x)
  if ( inherits(y,"tbl") ) y <- as.data.frame(y)
  if ( !is.data.frame(x) || !is.data.frame(y) )
    stop("'x' and 'y' must be data frames", call.=FALSE)

  if ( missing(by) || length(by) == 0 )
    stop("provide one or more business key columns in 'by'", call.=FALSE)
  if ( is.numeric(by) ) by <- names(x)[by]
  by <- as.character(by)
  check_rec_keys(x, y, by)

  if ( !is.numeric(abs_tol) || length(abs_tol) != 1 || is.na(abs_tol) || abs_tol < 0 )
    stop("'abs_tol' must be a single non-negative number", call.=FALSE)
  if ( !is.numeric(rel_tol) || length(rel_tol) != 1 || is.na(rel_tol) || rel_tol < 0 )
    stop("'rel_tol' must be a single non-negative number", call.=FALSE)

  indicators <- rec_indicators(x, y, by, indicators)
  rules      <- rec_rules(indicators, spec, abs_tol, rel_tol)

  # full outer join on the business key, tracking record presence.
  xs <- x[c(by, indicators)]; xs$.in_x <- TRUE
  ys <- y[c(by, indicators)]; ys$.in_y <- TRUE
  m  <- merge(xs, ys, by=by, all=TRUE, sort=TRUE, suffixes=c(".x",".y"))
  in_x <- !is.na(m$.in_x)
  in_y <- !is.na(m$.in_y)

  blocks <- vector("list", length(indicators))
  for ( i in seq_along(indicators) ){
    ind <- indicators[i]
    xv  <- m[[paste0(ind,".x")]]
    yv  <- m[[paste0(ind,".y")]]
    ratio_i   <- rules$ratio[i]
    abs_tol_i <- rules$abs_tol[i]
    rel_tol_i <- rules$rel_tol[i]
    declared  <- !is.na(ratio_i)

    x_common <- y_common <- diff <- rep(NA_real_, nrow(m))
    if ( declared ){
      x_common <- as.numeric(xv)
      y_common <- as.numeric(yv) * ratio_i
      diff     <- x_common - y_common
    }

    blocks[[i]] <- data.frame(
        m[by]
      , indicator = ind
      , x_value   = xv
      , y_value   = yv
      , ratio     = ratio_i
      , abs_tol   = abs_tol_i
      , rel_tol   = rel_tol_i
      , x_common  = x_common
      , y_common  = y_common
      , diff      = diff
      , status    = rec_status(in_x, in_y, declared, xv, yv
                    , x_common, y_common, diff, abs_tol_i, rel_tol_i)
      , stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, blocks)
  rownames(out) <- NULL
  out$status <- factor(out$status, levels=.REC_STATUSES)

  attr(out,"by")   <- by
  attr(out,"spec") <- rules
  attr(out,"call") <- match.call()
  class(out) <- c("reconciliation","data.frame")
  out
}


# business key must exist, be complete, and be unique in both sources.
check_rec_keys <- function(x, y, by){
  for ( d in c("x","y") ){
    dat <- get(d)
    no  <- setdiff(by, names(dat))
    if ( length(no) > 0 )
      stop(sprintf("key column(s) not found in '%s': %s", d
         , paste(no, collapse=", ")), call.=FALSE)
    for ( k in by )
      if ( anyNA(dat[[k]]) )
        stop(sprintf("missing values in key column '%s' of '%s'", k, d)
           , call.=FALSE)
    if ( anyDuplicated(dat[by]) )
      stop(sprintf("business key does not uniquely identify records in '%s'", d)
         , call.=FALSE)
  }
  invisible(TRUE)
}


# columns to compare: explicit, or all common numeric non-key columns.
rec_indicators <- function(x, y, by, indicators){
  common <- intersect(setdiff(names(x),by), setdiff(names(y),by))
  isn    <- setNames(
    vapply(common, function(nm) is.numeric(x[[nm]]) && is.numeric(y[[nm]])
         , logical(1))
    , common)
  if ( is.null(indicators) ){
    indicators <- common[isn]
    if ( length(indicators) == 0 )
      stop("no common numeric columns to compare", call.=FALSE)
  } else {
    indicators <- as.character(indicators)
    no <- setdiff(indicators, common)
    if ( length(no) > 0 )
      stop(sprintf("indicator column(s) not present in both data sources: %s"
         , paste(no, collapse=", ")), call.=FALSE)
    no <- indicators[ !isn[indicators] ]
    if ( length(no) > 0 )
      stop(sprintf("indicator column(s) must be numeric in both data sources: %s"
         , paste(no, collapse=", ")), call.=FALSE)
  }
  indicators
}


# resolve, for every compared indicator, the unit ratio and tolerances.
rec_rules <- function(indicators, spec, abs_tol, rel_tol){
  rules <- data.frame(
      indicator = indicators
    , ratio     = NA_real_
    , abs_tol   = abs_tol
    , rel_tol   = rel_tol
    , stringsAsFactors = FALSE
  )
  if ( is.null(spec) ) return(rules)

  spec <- as.data.frame(spec)
  if ( !("indicator" %in% names(spec)) )
    stop("'spec' must have an 'indicator' column", call.=FALSE)
  if ( !("ratio" %in% names(spec)) )
    stop(paste("'spec' must have a 'ratio' column, declaring how many units"
             , "of 'x' make one unit of 'y' (NA when undeclared)"), call.=FALSE)
  if ( !("abs_tol" %in% names(spec)) ) spec$abs_tol <- NA_real_
  if ( !("rel_tol" %in% names(spec)) ) spec$rel_tol <- NA_real_
  spec$indicator <- as.character(spec$indicator)
  spec <- spec[c("indicator","ratio","abs_tol","rel_tol")]
  validate_unit_spec(spec)

  extra <- setdiff(spec$indicator, indicators)
  if ( length(extra) > 0 )
    warnf("ignoring declared indicator(s) not compared: %s"
        , paste(extra, collapse=", "))

  i <- match(indicators, spec$indicator)
  has <- !is.na(i)
  rules$ratio[has] <- spec$ratio[i[has]]
  a <- spec$abs_tol[i[has]]
  rules$abs_tol[has] <- ifelse(is.na(a), abs_tol, a)
  r <- spec$rel_tol[i[has]]
  rules$rel_tol[has] <- ifelse(is.na(r), rel_tol, r)
  rules
}


# classify every key x indicator combination.
rec_status <- function(in_x, in_y, declared, xv, yv
             , x_common, y_common, diff, abs_tol, rel_tol){
  status   <- character(length(in_x))
  matched  <- in_x & in_y
  status[!matched] <- "unmatched"
  if ( !declared ){
    status[matched] <- "missing_unit"
    return(status)
  }
  missing_value <- matched & (is.na(xv) | is.na(yv))
  status[missing_value] <- "missing_value"
  comparable <- matched & !missing_value
  tol  <- pmax(abs_tol, rel_tol * pmax(abs(x_common), abs(y_common)))
  pass <- comparable & !is.na(diff) & !is.na(tol) & (abs(diff) <= tol)
  status[pass] <- "ok"
  status[comparable & !pass] <- "out_of_tolerance"
  status
}


#' @param x Object of class \code{reconciliation}
#' @param n Maximum number of rows printed.
#' @param ... Passed to \code{print.data.frame}.
#' @rdname reconcile
#' @export
print.reconciliation <- function(x, n=10, ...){
  cat("Object of class 'reconciliation'\n")
  cl <- attr(x,"call")
  if ( !is.null(cl) )
    cat(sprintf("  call: %s\n", paste(deparse(cl),collapse=" ")))
  if ( "status" %in% names(x) ){
    cat("\nStatus counts:\n")
    print(table(x$status))
    cat("\n")
  }
  d <- as.data.frame(x)
  if ( nrow(d) > n ){
    print(utils::head(d, n), ...)
    cat(sprintf("... %d more rows; use as.data.frame() to extract all results.\n"
      , nrow(d) - n))
  } else {
    print(d, ...)
  }
  invisible(x)
}


#' @param object Object of class \code{reconciliation}
#' @return For \code{summary}: a \code{table} of counts per indicator and
#'   status.
#' @rdname reconcile
#' @export
summary.reconciliation <- function(object, ...){
  d <- as.data.frame(object)
  table(indicator = d$indicator, status = d$status)
}


#' @return For \code{as.data.frame}: a plain \code{data.frame} with the
#'   comparison results (see the return value of \code{reconcile}).
#' @rdname reconcile
#' @export
as.data.frame.reconciliation <- function(x, ...){
  attr(x,"by")   <- NULL
  attr(x,"spec") <- NULL
  attr(x,"call") <- NULL
  class(x) <- "data.frame"
  x
}
