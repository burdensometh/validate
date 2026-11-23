#' Derive per-partition release decisions from a validation
#'
#' Aggregate the results of a \code{\link{confront}}ation over a partitioning
#' variable (for example a region or stratum) and derive, for each partition,
#' a release decision that can be consumed by a downstream (loading) task.
#'
#' Sampling sizes may differ strongly between partitions. For that reason the
#' decision is based on two numbers: the \emph{failure ratio} (the fraction of
#' effectively evaluated items that failed) and the \emph{effective volume}
#' (the number of items that could actually be evaluated, i.e. passes plus
#' fails). A partition is only released when enough items were effectively
#' evaluated and the failure ratio does not exceed the allowed maximum.
#'
#' @section Counting:
#' Counts are in items (rule \eqn{\times}{x} record). For each partition the
#' following independent counts are reported.
#' \itemize{
#'   \item{\code{npass}} items that passed
#'   \item{\code{nfail}} items that failed (normal failures)
#'   \item{\code{nNA}} items that could not be evaluated because the result
#'        was \code{NA}
#'   \item{\code{nerror}} items that could not be evaluated because the rule
#'        threw an error. Erroring rules are treated as record-level rules,
#'        so each erroring rule contributes one error item per record in the
#'        partition.
#' }
#' The effective volume is \code{neffective = npass + nfail}: \code{NA} and
#' error items are \emph{not} effectively evaluated and are therefore kept out
#' of the failure ratio \code{rel.fail = nfail / neffective}.
#'
#' Rules whose result is not a logical vector with one value per record
#' (for example data-set level rules such as \code{nrow(.) > 10}) cannot be
#' attributed to individual partitions. They are excluded from the counts with
#' a warning and reported in the \code{dropped_rules} attribute.
#'
#' @section Decision:
#' The \code{decision} column takes one of three values, evaluated in this
#' order:
#' \enumerate{
#'   \item{\code{"pending"}}: \code{neffective < min_effective}. The sample is
#'        too small for a definitive decision and the partition is flagged for
#'        investigation.
#'   \item{\code{"reject"}}: \code{rel.fail > max_fail_ratio}.
#'   \item{\code{"release"}}: otherwise.
#' }
#'
#' @param x An object of class \code{\link{validation}}, typically created with
#'   \code{\link{confront}} or \code{\link{check_that}}.
#' @param by Partitioning variable: a vector (usually \code{character} or
#'   \code{factor}) with one value per record of the confronted data.
#'   Alternatively, a single string naming a column of the keyset stored with
#'   \code{x} (see the \code{key} argument of \code{\link{confront}}; when no
#'   keyset is stored, a single string is treated as one partition value).
#'   Records
#'   where \code{by} is \code{NA} are ignored (with a warning). When \code{by}
#'   is a factor, all levels are reported, including levels that do not occur
#'   in the data.
#' @param max_fail_ratio \code{[numeric]} in \eqn{[0,1]}. Maximum failure ratio
#'   tolerated for release.
#' @param min_effective \code{[numeric]} Minimum effective volume
#'   (\code{npass + nfail}) required for a definitive decision. Partitions
#'   below this volume are \code{"pending"}.
#'
#' @return A \code{data.frame} with one row per partition and the columns
#' \tabular{ll}{
#'   \code{partition}   \tab partition key\cr
#'   \code{nrecords}    \tab number of sampled records in the partition\cr
#'   \code{npass}       \tab number of passed items\cr
#'   \code{nfail}       \tab number of failed items\cr
#'   \code{nNA}         \tab number of items with \code{NA} result\cr
#'   \code{nerror}      \tab number of items lost to rule errors\cr
#'   \code{neffective}  \tab effective volume: \code{npass + nfail}\cr
#'   \code{rel.fail}    \tab failure ratio \code{nfail / neffective}
#'                            (\code{NA} when \code{neffective == 0})\cr
#'   \code{decision}    \tab \code{"release"}, \code{"reject"} or \code{"pending"}
#' }
#' The attributes \code{max_fail_ratio}, \code{min_effective},
#' \code{dropped_records} and \code{dropped_rules} document the settings and
#' any records or rules that were left out of the counts. The data frame can
#' be written directly, e.g. with \code{\link[utils]{write.csv}} or
#' \code{jsonlite::write_json}, for consumption by a loading task.
#'
#' @example ../examples/partition_decision.R
#' @family validation-methods
#' @export
partition_decision <- function(x, by, max_fail_ratio = 0, min_effective = 1){
  stopifnot(inherits(x, "validation"))
  stopifnot( is.numeric(max_fail_ratio), length(max_fail_ratio) == 1
           , !is.na(max_fail_ratio), max_fail_ratio >= 0, max_fail_ratio <= 1 )
  stopifnot( is.numeric(min_effective), length(min_effective) == 1
           , !is.na(min_effective), min_effective >= 0 )

  # resolve the partitioning variable: vector, or name of a keyset column
  # (note: confront stores dat[key], which is a 0-column data frame when
  # no key was given, not NULL)
  ks <- x$._keys$keyset
  if ( is.character(by) && length(by) == 1 && !is.null(ks) && length(ks) > 0 ){
    if ( !by %in% names(ks) ){
      stop(sprintf( paste0( "No keyset column '%s' stored with this confrontation. "
                          , "Use confront(..., key=) or pass a vector to 'by'." ), by)
         , call.=FALSE)
    }
    by <- ks[[by]]
  }
  if ( !is.atomic(by) || length(by) == 0 ){
    stop("'by' must be an atomic vector with one value per record.", call.=FALSE)
  }
  levels_by <- if ( is.factor(by) ) levels(by) else NULL
  by <- as.character(by)
  if ( is.null(levels_by) ) levels_by <- sort(unique(by[!is.na(by)]))
  n <- length(by)

  # records without partition key cannot be assigned to a partition
  dropped_records <- sum(is.na(by))
  if ( dropped_records > 0 ){
    warnf("%d record(s) with missing partition key are ignored.", dropped_records)
  }
  keep <- which(!is.na(by))
  idx <- split(keep, factor(by[keep], levels=levels_by))
  nrecords <- lengths(idx)

  vals  <- values(x, simplify=FALSE, drop=FALSE)
  rname <- names(vals)
  if ( is.null(rname) ) rname <- sprintf("V%d", seq_along(vals))
  expr  <- vapply(x$._calls, call2text, character(1))
  err   <- has_error(x)

  npass <- nfail <- nNA <- nerror <-
    setNames(integer(length(idx)), names(idx))
  dropped_rules <- list()

  # guard against a 'by' that does not match the confronted data at all
  len <- vapply(vals[!err], function(v) if (is.logical(v)) length(v) else NA_integer_, integer(1))
  len <- len[!is.na(len)]
  if ( length(len) > 0 && !any(len == n) && any(len != 1) ){
    stop(sprintf( "Length of 'by' (%d) does not match the validation results (%d): wrong partition variable?"
                , n, len[1] ), call.=FALSE)
  }

  for ( i in seq_along(vals) ){
    if ( err[i] ){
      # erroring rules cannot be evaluated: one error item per record
      nerror <- nerror + nrecords
      next
    }
    v <- vals[[i]]
    if ( !is.logical(v) || length(v) != n ){
      reason <- if ( !is.logical(v) ) "result is not logical"
                else sprintf("result length %d does not match number of records", length(v))
      dropped_rules[[length(dropped_rules)+1]] <-
        data.frame( name=rname[i], expression=expr[i], reason=reason
                  , row.names=NULL, stringsAsFactors=FALSE )
      next
    }
    for ( p in names(idx) ){
      vp <- v[idx[[p]]]
      npass[p] <- npass[p] + sum(vp, na.rm=TRUE)
      nfail[p] <- nfail[p] + sum(!vp, na.rm=TRUE)
      nNA[p]   <- nNA[p]   + sum(is.na(vp))
    }
  }
  if ( length(dropped_rules) > 0 ){
    warnf( "Ignored %d rule(s) with non record-level results. See attribute 'dropped_rules'."
         , length(dropped_rules) )
  }

  neffective <- npass + nfail
  rel_fail   <- ifelse(neffective > 0, nfail / neffective, NA_real_)
  decision <- rep("release", length(idx))
  decision[ !is.na(rel_fail) & rel_fail > max_fail_ratio ] <- "reject"
  decision[ neffective < min_effective ] <- "pending"

  out <- data.frame( partition  = names(idx)
             , nrecords   = as.integer(nrecords)
             , npass      = as.integer(npass)
             , nfail      = as.integer(nfail)
             , nNA        = as.integer(nNA)
             , nerror     = as.integer(nerror)
             , neffective = as.integer(neffective)
             , rel.fail   = as.numeric(rel_fail)
             , decision   = decision
             , row.names  = NULL
             , stringsAsFactors = FALSE )
  attr(out, "max_fail_ratio")  <- max_fail_ratio
  attr(out, "min_effective")   <- min_effective
  attr(out, "dropped_records") <- dropped_records
  attr(out, "dropped_rules")   <-
    if ( length(dropped_rules) > 0 ) do.call(rbind, dropped_rules)
    else data.frame( name=character(0), expression=character(0)
                   , reason=character(0), stringsAsFactors=FALSE )
  out
}
