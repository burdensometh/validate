
#' Check whether a variable represents a linear sequence
#'
#' A variable \eqn{X = (x_1, x_2,\ldots, x_n)} (\eqn{n\geq 0}) represents a
#' \emph{linear sequence} when \eqn{x_{j+1} - x_j} is constant for all
#' \eqn{j\geq 1}. That is,  elements in the series are equidistant and without
#' gaps. 
#'
#' @details
#' 
#' Presence of a missing value (\code{NA}) in \code{x} will result in \code{NA},
#' except when \code{length(x) <= 2} and \code{start} and \code{end} are
#' \code{NULL}. Any sequence of length \eqn{\leq 2} is a linear sequence. 
#' 
#'
#'
#' @param x An R vector.
#' @param sort \code{[logical]}. When set to \code{TRUE}, \code{x}
#'        is sorted within each group before testing.
#' @param begin Optionally, a value that should equal \code{min(x)}
#' @param end   Optionally, a value that should equal \code{max(x)}
#' @param by bare (unquoted) variable name or a list of unquoted variable names,
#'        used to split \code{x} into groups. The check is executed for each group.
#' @param calendar Optionally, a reporting calendar. When given, the check is no
#'        longer "every step is the same size" but "every working day within the
#'        validity window is reported exactly once, and nothing else is reported".
#'        \code{calendar} is either a vector of working days (\code{Date},
#'        \code{POSIXct} or ISO-8601 \code{character}), or a \code{data.frame}
#'        with a \code{date} column and a logical \code{working} column. With a
#'        calendar, \code{x} is interpreted as dates even when it is
#'        \code{character}, and \code{begin}/\code{end} (\code{valid_from}/\code{valid_until})
#'        are day-based.
#' @param valid_from,valid_until Optionally, the start and end of the period in
#'        which reports are expected. Either a single value, or one value per
#'        record; they must be constant within a group. When \code{NULL}, the
#'        corresponding side is unbounded. Used only together with
#'        \code{calendar}; \code{begin}/\code{end} are accepted as aliases.
#' @param ... Arguments passed to other methods.
#'
#' @return For \code{is_linear_sequence}: a single \code{TRUE} or \code{FALSE},
#' equal to \code{all(in_linear_sequence)}.
#'
#' @examples
#'
#' is_linear_sequence(1:5) # TRUE
#' is_linear_sequence(c(1,3,5,4,2)) # FALSE
#' is_linear_sequence(c(1,3,5,4,2), sort=TRUE) # TRUE 
#' is_linear_sequence(NA_integer_) # TRUE
#' is_linear_sequence(NA_integer_, begin=4) # FALSE
#' is_linear_sequence(c(1, NA, 3)) # FALSE
#'
#'
#' d <- data.frame(
#'     number = c(pi, exp(1), 7)
#'   , date = as.Date(c("2015-12-17","2015-12-19","2015-12-21"))
#'   , time = as.POSIXct(c("2015-12-17","2015-12-19","2015-12-20"))
#' )
#'
#' rules <- validator(
#'     is_linear_sequence(number)  # fails
#'   , is_linear_sequence(date)    # passes
#'   , is_linear_sequence(time)    # fails
#' )
#' summary(confront(d,rules))
#'
#' ## check groupwise data
#' dat <- data.frame(
#'    time = c(2012, 2013, 2012, 2013, 2015)
#'  , type = c("hi", "hi", "ha", "ha", "ha")
#' )
#' rule <- validator(in_linear_sequence(time, by=type))
#' values(confront(dat, rule)) ## 2xT, 3xF
#'
#'
#' rule <- validator(in_linear_sequence(time, type))
#' values( confront(dat, rule) )
#'
#' ## reporting calendar: reports on every working day, per store validity
#' cal <- as.Date(c("2024-01-01","2024-01-02","2024-01-03","2024-01-04","2024-01-05",
#'                  "2024-01-08","2024-01-09","2024-01-10"))
#' stores <- rep(c("A","B"), c(5,3))
#' dates  <- cal
#' from   <- as.Date(rep(c("2024-01-01","2024-01-08"), c(5,3)))
#' until  <- as.Date(rep(c("2024-01-05","2024-01-10"), c(5,3)))
#' is_linear_sequence(dates, by=stores, calendar=cal
#'                    , valid_from=from, valid_until=until) # TRUE
#'
#' ## inside a validator, the calendar is passed as reference data
#' dat  <- data.frame(store=stores, date=dates)
#' rule <- validator(in_linear_sequence(date, by=store, calendar=cal
#'                    , valid_from=valid_from, valid_until=valid_until))
#' confront(dat, rule, ref=list(cal=cal, valid_from=from, valid_until=until))
#'
#' @family cross-record-helpers
#'
#' @export
is_linear_sequence <- function(x, by=NULL,...) UseMethod("is_linear_sequence")

# workhorse function
is_lin_num_seq <- function(x, begin=NULL, end=NULL, sort=TRUE, tol=1e-8,...){

  # Edge cases: empty sequence, or length 1 sequence with missing value.  In
  # those cases, return FALSE when any of begin or end is checked, otherwise
  # return TRUE
  if ( length(x) <= 2 && all(is.na(x)) ) 
    return(is.null(begin) && is.null(end))

  if (anyNA(x)) return(NA)

  # the regular case
  !anyNA(x) &&
    (is.null(begin) || abs(begin - min(x)) <= tol) &&
    (is.null(end)   || abs(end - max(x))   <= tol) &&
    ( length(x) <= 1 || { if(sort) x <- sort(x)
                          d <- diff(x)
                          all(abs(d - d[1]) <= tol)
                        })
}

as_int <- function(x){
  if( is.null(x)) NULL else as.integer(x)
}
as_num <- function(x){
  if (is.null(x)) NULL else as.numeric(x)
}


all_lin_num_seq <- function(x, by=NULL, begin=NULL, end=NULL, sort=TRUE, tol=1e-8){
  
  if (length(by) == 0){
    is_lin_num_seq(x, begin=begin, end=end, sort=sort, tol=tol)
  } else {
    all(tapply(x, INDEX=by, FUN=is_lin_num_seq, begin=begin, end=end, sort=sort, tol=tol))
  }
} 



#' @rdname is_linear_sequence
#' @param tol numerical tolerance for gaps.
#' @export
is_linear_sequence.numeric <- function(x, by=NULL, begin=NULL, end=NULL, sort=TRUE, tol = 1e-8
                                       , calendar=NULL,...){
  if (!is.null(calendar))
    stop("A reporting 'calendar' applies to date-like vectors, not to numeric 'x'.", call. = FALSE)
  all_lin_num_seq(x, by=by, begin=begin, end=end, sort=sort, tol=1e-8)
}

#' @rdname is_linear_sequence
#' @export
is_linear_sequence.Date <- function(x, by=NULL, begin=NULL, end=NULL, sort=TRUE
                                    , calendar=NULL, valid_from=NULL, valid_until=NULL,...){
  if (is.null(calendar))
    return(all_lin_num_seq(as.integer(x), by=by, begin=as_int(begin), end=as_int(end), sort=sort, tol=0))
  all_cal_seq(x, by=by, calendar=calendar, begin=begin, end=end
              , valid_from=valid_from, valid_until=valid_until, sort=sort)
}

#' @rdname is_linear_sequence
#' @export
is_linear_sequence.POSIXct <- function(x, by=NULL , begin=NULL, end=NULL, sort = TRUE, tol=1e-6
                                       , calendar=NULL, valid_from=NULL, valid_until=NULL,...){
  # Note. POSIXct can express fractions of a second. Conversion from and to POSIXlt
  # is better than microseconds, so that is what we use as default tolerance/
  if (is.null(calendar))
    return(all_lin_num_seq(as.numeric(x), by=by, begin=as_num(begin), end=as_num(end), sort=sort, tol=tol))
  all_cal_seq(x, by=by, calendar=calendar, begin=begin, end=end
              , valid_from=valid_from, valid_until=valid_until, sort=sort)
}

#' @rdname is_linear_sequence
#'
#' @param format \code{[character]}. How to interpret \code{x} as a time period.
#' Either \code{"auto"} for automatic detection or a specification passed to
#' \code{\link{strptime}}. Automatically detected periods are of the form year:
#' \code{"2020"}, yearMmonth: \code{"2020M01"},  yearQquarter: \code{"2020Q3"},
#' or year-Qquarter: \code{"2020-Q3"}. 
#'
#' @export
is_linear_sequence.character <- function(x, by=NULL, begin=NULL, end=NULL, sort=TRUE, format="auto"
                                         , calendar=NULL, valid_from=NULL, valid_until=NULL,...){
  if ( !is.null(calendar) ){
    # A reporting calendar is day-based: interpret x (and bounds) as dates.
    y     <- as_date(x)
    begin <- as_date(begin)
    end   <- as_date(end)
    return(is_linear_sequence.Date(y, by=by, begin=begin, end=end, sort=sort
      , calendar=calendar, valid_from=valid_from, valid_until=valid_until,...))
  }
  if ( format == "auto" ){
    y     <- period_to_int(x, by=by)
    begin <- period_to_int(begin)
    end   <- period_to_int(end)
    is_linear_sequence.numeric(y, by=by, begin=begin, end=end, sort=sort, tol=0,...)
  } else {
    y     <- strptime(x, format=format)
    begin <- strptime(begin, format=format)
    end   <- strptime(end, format=format)
    is_linear_sequence.POSIXct(y, by=by, begin=begin, end=end, sort=sort, tol=1e-6
      , calendar=calendar, valid_from=valid_from, valid_until=valid_until, ...)
  }

}





#' @rdname is_linear_sequence
#'
#' @return For \code{in_linear_sequence}: a \code{logical} vector with the same length as \code{x}.
#' @export
in_linear_sequence <- function(x, ...) UseMethod("in_linear_sequence")

in_lin_num_seq <- function(x, by=NULL, begin=NULL, end=NULL, sort=TRUE, tol=1e8,...){
  rep(is_lin_num_seq(x, begin=begin, end=end, sort=sort, tol=tol), length(x))
}


## TODO: postpone conversion to integer to inside the split-apply-combine loop.

#' @rdname is_linear_sequence
#' @export
in_linear_sequence.character <- function(x, by=NULL, begin=NULL, end=NULL, sort=TRUE, format="auto"
                                         , calendar=NULL, valid_from=NULL, valid_until=NULL,...){
  if ( !is.null(calendar) ){
    # A reporting calendar is day-based: interpret x (and bounds) as dates.
    y     <- as_date(x)
    begin <- as_date(begin)
    end   <- as_date(end)
    return(in_linear_sequence.Date(y, by=by, begin=begin, end=end, sort=sort
      , calendar=calendar, valid_from=valid_from, valid_until=valid_until,...))
  }
  if ( format == "auto" ){
    y     <- period_to_int(x,by=by)
    begin <- period_to_int(begin)
    end   <- period_to_int(end)
    in_linear_sequence.numeric(y, by=by, begin=begin, end=end, sort=sort, tol=0,...)
  } else {
    y     <- strptime(x, format=format)
    begin <- strptime(begin, format=format)
    end   <- strptime(end, format=format)
    in_linear_sequence.POSIXct(y, by=by, begin=begin, end=end, sort=sort, tol=1e-6
      , calendar=calendar, valid_from=valid_from, valid_until=valid_until, ...)
  }

}


#' @rdname is_linear_sequence
#' @export
in_linear_sequence.numeric <- function(x, by=NULL, begin=NULL, end=NULL, sort=TRUE, tol=1e-8
                                       , calendar=NULL,...){
  if (!is.null(calendar))
    stop("A reporting 'calendar' applies to date-like vectors, not to numeric 'x'.", call. = FALSE)
  if (is.null(by)){
    in_lin_num_seq(as.integer(x), begin=as_int(begin), end=as_int(end), sort=sort, tol=tol)
  } else {
    result <- tapply(as.integer(x), by, in_lin_num_seq, begin=as_int(begin), end=as_int(end), sort=sort, tol=tol)
    unsplit(result, by)
  }
}

#' @rdname is_linear_sequence
#' @export
in_linear_sequence.Date <- function(x, by=NULL, begin=NULL, end=NULL, sort=TRUE
                                    , calendar=NULL, valid_from=NULL, valid_until=NULL,...){
  if (is.null(calendar))
    return(in_linear_sequence.numeric(as.integer(x), by=by, begin=as_int(begin), end=as_int(end), sort=TRUE, tol=0))
  in_cal_seq(x, by=by, calendar=calendar, begin=begin, end=end
             , valid_from=valid_from, valid_until=valid_until)
}


#' @rdname is_linear_sequence
#' @export
in_linear_sequence.POSIXct <- function(x, by=NULL, begin=NULL, end=NULL, sort=TRUE, tol=1e-6
                                       , calendar=NULL, valid_from=NULL, valid_until=NULL,...){
  if (is.null(calendar))
    return(in_linear_sequence.numeric(as.numeric(x), by=by, begin=as_num(begin), end=as_num(end), sort=sort, tol=0))
  in_cal_seq(x, by=by, calendar=calendar, begin=begin, end=end
             , valid_from=valid_from, valid_until=valid_until)
}



period_type <- function(x, undefined=NA_character_){
  if ( all( grepl("^[12][0-9]{3}$",x) ) )           return("annual")
  if ( all( grepl("^[12][0-9]{3}-?Q[1-4]$",x) ) )   return("quarterly")
  if ( all( grepl("^[12][0-9]{3}M[01][0-9]$",x) ) ) return("monthly")

  warning("Cannot detect period notation: undefined period type or different period types in single column.", call.=FALSE)
  undefined
}


# Turn a period into an integer
# 
# Annual periods are turned in to the integer year. Quarterly 
# and Monthly periods are turned in to the month number, counted
# from the year zero, so quarters and months have consecutive numbers
# accross years.
#
# @param x a \code{character} vector.
# @param by \code{character} split x into groups before coercion
# 
#
#
#
period_to_int <- function(x, by=NULL){

  if (is.null(x)) return(NULL)

  f <- function(xx){
    from <- period_type(xx)
    if (is.na(from)) return(rep(NA, length(xx)))

    if (from == "annual"){
      res <- as.numeric(xx)
    }


    if (from ==  "quarterly" ){
      L       <- strsplit(xx,"-?Q")
      year    <- as.numeric(sapply(L, `[[`,1))
      quarter <- as.numeric(sapply(L, `[[`, 2))
      res     <- 4*year + quarter-1
    }

    if ( from == "monthly" ){
      L     <- strsplit(xx, "M")
      year  <- as.numeric( sapply(L,`[[`,1) )
      month <- as.numeric( sapply(L, `[[`, 2) )
      res   <- 12*year + month-1 == 1
    }
    res
  }
  if (is.null(by)) by <- character(length(x))
  unsplit(lapply(split(x, f=by), f), f=by)
}


# --- reporting calendar ------------------------------------------------------
#
# Support for time rules that must follow a given reporting calendar: reports
# are expected on every working day within a validity window (per group, e.g.
# per store) and must not be delivered on non-working days.

# Coerce to Date, keeping NULL and rejecting uninterpretable input.
as_date <- function(x){
  if (is.null(x)) return(NULL)
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))
  if (is.character(x)) return(as.Date(x))
  stop("Cannot interpret a '", class(x)[1], "' object as a date.", call. = FALSE)
}

# Normalize a calendar specification to the sorted set of reportable (working)
# dates.
#
# calendar is either a vector (Date, POSIXct, or ISO character) listing the
# working days, or a data.frame with a 'date' column and a logical column named
# 'working' (or 'is_working_day', 'report', 'working_day').
calendar_dates <- function(calendar){
  if (is.null(calendar))
    stop("A reporting calendar is required.", call. = FALSE)
  if (is.data.frame(calendar)){
    nm <- names(calendar)
    if (!"date" %in% nm)
      stop("A calendar data.frame must contain a 'date' column.", call. = FALSE)
    wcol <- intersect(nm, c("working", "is_working_day", "working_day", "report"))
    if (length(wcol) == 0L)
      stop("A calendar data.frame must contain a logical 'working' column.", call. = FALSE)
    iswork <- calendar[[wcol[1]]]
    if (!is.logical(iswork))
      stop("The calendar working-day indicator must be logical.", call. = FALSE)
    d <- as_date(calendar[["date"]])
    d <- d[!is.na(iswork) & iswork & !is.na(d)]
    return(sort(unique(d)))
  }
  d <- as_date(calendar)
  sort(unique(d[!is.na(d)]))
}

# Split key that mirrors how 'split()' groups the input. Accepts NULL, a single
# vector, or a (possibly named) list of vectors.
group_key <- function(by, n){
  if (is.null(by) || length(by) == 0L) return(rep(".", n))
  if (inherits(by, "data.frame")) by <- as.list(by)
  if (is.list(by)){
    v <- lapply(by, as.character)
    return(do.call(paste, c(v, sep = "\r")))
  }
  as.character(by)
}

# Resolve validity bounds to a Date vector of length n. Each bound may be NULL,
# a single value, or one value per record; it must be constant within a group.
resolve_bounds <- function(bounds, n, keys, what){
  if (is.null(bounds)) return(rep(as.Date(NA), length(keys)))
  b <- as_date(bounds)
  if (length(b) == 1L && length(keys) != 1L) b <- rep(b, length(keys))
  if (length(b) != length(keys))
    stop(sprintf("'%s' must be a single value or one value per record.", what), call. = FALSE)
  within_varies <- tapply(b, keys, function(d) length(unique(d[!is.na(d)])) > 1L)
  if (any(within_varies))
    stop(sprintf("'%s' varies within group '%s'; validity must be constant per group."
                , what, names(within_varies)[which(within_varies)[1]]), call. = FALSE)
  b
}

# Expected reportable dates for one group: working days within the validity window.
expected_dates <- function(working, from, until){
  out <- working
  if (!is.na(from))  out <- out[out >= from]
  if (!is.na(until)) out <- out[out <= until]
  out
}

# Overall status of one group: TRUE when every expected working day is reported
# exactly once and no report falls on a non-working day or outside the window.
# Returns NA when the group contains a missing date.
cal_group_ok <- function(d, working, from, until){
  if (anyNA(d)) return(NA)
  expected <- expected_dates(working, from, until)
  # report outside validity window
  if ((!is.na(from) && any(d < from)) || (!is.na(until) && any(d > until))) return(FALSE)
  # report on a non-working day
  if (any(!(d %in% working))) return(FALSE)
  # duplicated report date
  if (anyDuplicated(d)) return(FALSE)
  # expected working day not reported
  if (!all(expected %in% d)) return(FALSE)
  TRUE
}

in_cal_seq <- function(x, by=NULL, calendar, begin=NULL, end=NULL
                      , valid_from=NULL, valid_until=NULL, sort=TRUE){
  w <- calendar_dates(calendar)
  d <- as_date(x)
  n <- length(d)
  if (n == 0L) return(logical(0))
  if (is.null(valid_from))  valid_from  <- begin
  if (is.null(valid_until)) valid_until <- end
  keys  <- group_key(by, n)
  from  <- resolve_bounds(valid_from,  n, keys, "valid_from")
  until <- resolve_bounds(valid_until, n, keys, "valid_until")
  unsplit(lapply(split(seq_len(n), keys), function(i){
    rep(cal_group_ok(d[i], w, from[i][1], until[i][1]), length(i))
  }), keys)
}

all_cal_seq <- function(x, by=NULL, calendar, begin=NULL, end=NULL
                       , valid_from=NULL, valid_until=NULL, sort=TRUE){
  all(in_cal_seq(x, by=by, calendar=calendar, begin=begin, end=end
                , valid_from=valid_from, valid_until=valid_until, sort=sort))
}

# Group columns as a data.frame with n rows (or NULL when there is no grouping).
group_df <- function(by, n){
  if (is.null(by) || length(by) == 0L) return(NULL)
  if (inherits(by, "data.frame")){
    d <- as.data.frame(by, stringsAsFactors = FALSE)
  } else if (is.list(by)){
    nm <- names(by)
    if (is.null(nm) || any(!nzchar(nm))) nm <- paste0("by", seq_along(by))
    d <- as.data.frame(setNames(lapply(by, as.character), nm), stringsAsFactors = FALSE)
  } else {
    d <- data.frame(by = as.character(by), stringsAsFactors = FALSE)
  }
  d
}


#' Diagnose reporting gaps against a reporting calendar
#'
#' Given a set of reporting dates, a reporting \code{calendar} and per-group
#' validity windows, this function lists the dates that are expected but absent
#' from the data, and classifies records that should not have been delivered.
#'
#' @param x \code{[Date]}, \code{[POSIXct]} or ISO-8601 \code{[character]}
#'   vector of reporting dates, one per record.
#' @param calendar Reporting calendar: a vector of working days (Date, POSIXct or
#'   ISO-8601 character), or a \code{data.frame} with a \code{date} column and a
#'   logical \code{working} column. Only working days are expected to be
#'   reported.
#' @param by Grouping variable(s), such as the store identifier. A vector or a
#'   (named) list of vectors. Each group gets its own validity window and its
#'   own set of expected dates.
#' @param valid_from,valid_until Start and end of the validity window. Either a
#'   single value applied to all records, or one value per record. They must be
#'   constant within a group. When \code{NULL}, the window is unbounded on that
#'   side.
#' @param date Name of the output column holding the date.
#' @param issue Name of the output column holding the issue classification.
#'
#' @return A \code{data.frame} with the grouping columns, the reporting
#'   \code{date}, and an \code{issue} column. Every record falls in exactly one
#'   class, with the following precedence:
#'   \tabular{ll}{
#'     \code{"non-working-day"} \tab A report delivered on a day that is not a
#'       working day according to the calendar (checked first, so it also catches
#'       such reports outside the validity window). \cr
#'     \code{"outside-validity"} \tab A report on a working day, delivered outside
#'       the group's validity window. \cr
#'     \code{"duplicate"} \tab A working day inside the validity window that is
#'       reported more than once. Every record of the duplicate is listed. \cr
#'     \code{"missing"} \tab An expected working day that does not occur at all in
#'       the data. The row is synthesized, so its date is present in the output
#'       but absent from the input. \cr
#'   }
#'   Records that are reported exactly once, on a working day inside their
#'   validity window, are not listed. The result is group-major, with the
#'   reported problems ahead of synthesized \code{"missing"} rows within each
#'   group; it is empty (zero rows) when there is nothing to report.
#'
#' @examples
#' # two stores, each with its own start date
#' stores <- c("A", "A", "A", "B", "B", "B", "B")
#' dates <- as.Date(c(
#'     "2024-01-01", "2024-01-02", "2024-01-03"   # store A: complete
#'   , "2024-01-08", "2024-01-08", "2024-01-09", "2024-01-13" # B: duplicate + Sat.
#' ))
#' calendar <- as.Date(c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-04",
#'                       "2024-01-05", "2024-01-08", "2024-01-09", "2024-01-10",
#'                       "2024-01-11", "2024-01-12"))
#' reporting_gaps(dates, calendar = calendar, by = stores
#'   , valid_from = as.Date(c("2024-01-01", "2024-01-08"))[
#'       match(stores, c("A", "B"))]
#'   , valid_until = as.Date("2024-01-12"))
#'
#' @family cross-record-helpers
#' @export
reporting_gaps <- function(x, calendar, by = NULL, valid_from = NULL
                           , valid_until = NULL
                           , date = "date", issue = "issue"){
  w  <- calendar_dates(calendar)
  d  <- as_date(x)
  n  <- length(d)
  keys  <- group_key(by, n)
  from  <- resolve_bounds(valid_from,  n, keys, "valid_from")
  until <- resolve_bounds(valid_until, n, keys, "valid_until")
  gdf   <- group_df(by, n)

  # Accumulate atomic pieces per group, then assemble once. This avoids
  # rbind() silently dropping the Date class.
  acc <- lapply(split(seq_len(n), keys), function(i){
    f <- from[i][1]; u <- until[i][1]
    gpos <- i[1]
    did  <- d[i]; did <- did[!is.na(did)]
    # Classification, each record lands in exactly one bucket:
    #  - a report on a day absent from the calendar is a non-working-day report,
    #    whether or not it also lies outside the validity window;
    #  - a working-day report outside the window is 'outside-validity';
    #  - a working-day report inside the window, reported twice, is a duplicate.
    nonwork <- did[!(did %in% w)]
    work    <- did[did %in% w]
    outside <- work[!((is.na(f) | work >= f) & (is.na(u) | work <= u))]
    dup     <- work[(is.na(f) | work >= f) & (is.na(u) | work <= u)]
    dup     <- dup[duplicated(dup) | duplicated(dup, fromLast = TRUE)]
    expected <- expected_dates(w, f, u)
    missing  <- expected[!(expected %in% unique(did))]
    list(gpos = gpos
        , date = c(nonwork, outside, dup, missing)
        , issue = c(rep("non-working-day",  length(nonwork))
                   , rep("outside-validity", length(outside))
                   , rep("duplicate",        length(dup))
                   , rep("missing",          length(missing))))
  })

  gpos  <- unlist(lapply(acc, `[[`, "gpos"))
  dates <- unlist(lapply(acc, `[[`, "date"))
  issues <- unlist(lapply(acc, `[[`, "issue"), use.names = FALSE)
  if (length(dates) == 0L){
    out <- if (is.null(gdf)) data.frame(row.names = integer(0))
           else gdf[integer(0), , drop = FALSE]
    out[[date]]  <- as.Date(character(0))
    out[[issue]] <- character(0)
    return(out)
  }
  dates <- as.Date(dates, origin = "1970-01-01")

  # group identifier per emitted row: repeat the group's first-row values,
  # and number the groups so the result keeps group-major order.
  gn   <- sapply(acc, function(a) length(a$date))
  gsrc <- rep(gpos, gn)
  gord <- rep(seq_along(acc), gn)
  out <- if (is.null(gdf)) data.frame(row.names = seq_along(dates))
         else gdf[gsrc, , drop = FALSE]
  rownames(out) <- NULL
  out[[date]]  <- dates
  out[[issue]] <- issues

  # group-major; within a group reported problems before synthesized 'missing'
  # rows; ties broken by date.
  o <- order(gord, out[[issue]] == "missing", out[[date]])
  out <- out[o, , drop = FALSE]
  rownames(out) <- NULL
  out
}


#' Check variable range
#'
#' Test wether a variable falls within a range.
#'
#' @param x A bare (unquoted) variable name.
#' @param min lower bound
#' @param max upper bound
#' @param ... arguments passed to other methods
#'
#'
#' @examples
#'
#' d <- data.frame(
#'    number = c(3,-2,6)
#'  , time   = as.Date(c("2018-02-01", "2018-03-01", "2018-04-01"))
#'  , period = c("2020Q1", "2021Q2", "2020Q3") 
#' )
#'
#' rules <- validator(
#'    in_range(number, min=-2, max=7, strict=TRUE)
#'  , in_range(time,   min=as.Date("2017-01-01"), max=as.Date("2018-12-31"))
#'  , in_range(period, min="2020Q1", max="2020Q4")
#' )
#'
#' result <- confront(d, rules)
#' values(result)
#'
#'
#' @export
in_range <- function(x, min, max,...) UseMethod("in_range")

#' @rdname in_range
#' @param strict \code{[logical]} Toggle between including the range boundaries
#'               (default) or not including them (when strict=TRUE).
#' 
#' @export             
in_range.default <- function(x, min, max, strict=FALSE, ...){
  if (strict) x > min & x < max
  else x >= min & x <= max
}

#' @rdname in_range
#'
#' @param format \code{[character]} of \code{NULL}. If \code{format=NULL} the
#' character vector is interpreted as is. And the whether a character lies
#' within a character range is determined by the collation order set by the
#' current locale. See the details of "\code{\link{<}}".  If \code{format} is
#' not \code{NULL}, it specifies how to interpret the character vector as a
#' time period.  It can take the value \code{"auto"} for automatic detection or
#' a specification passed to \code{\link{strptime}}. Automatically detected
#' periods are of the form year: \code{"2020"}, yearMmonth: \code{"2020M01"},
#' yearQquarter: \code{"2020Q3"}, or year-Qquarter: \code{"2020-Q3"}. 
#'
#'
#' @export
in_range.character <- function(x, min, max, strict=FALSE, format = "auto",...){
  if (is.null(format)) 
    in_range.default(x=x, min=min, max=max, strict=strict, ...)
  else if ( format == "auto" ){
    y     <- period_to_int(x, by=NULL)
    ymin  <- period_to_int(min)
    ymax  <- period_to_int(max)
    in_range.default(y, min=ymin, max=ymax, strict=strict, ...)
  } else {
    y     <- strptime(x, format=format)
    ymin  <- strptime(min, format=format)
    ymax  <- strptime(max, format=format)
    in_range(y, min=ymin, max=ymax, strict=strict, ...)
  }
}

#' Test whether details combine to a chosen aggregate
#' 
#' Data in 'long' format often contain records representing totals
#' (or other aggregates) as well as records that contain details
#' that add up to the total. This function facilitates checking the
#' part-whole relation in such cases.
#'
#'
#' @param values A bare (unquoted) variable name holding the values to aggregate
#' @param labels A bare (unquoted) variable name holding the labels indicating
#'        whether a value is an aggregate or a detail.
#' @param whole \code{[character]} literal label or pattern recognizing a whole
#'        in \code{labels}. Use \code{\link{glob}} or \code{\link{rx}} to label
#'        as a globbing or regular expression pattern (see examples).
#' @param part \code{[character]} vector of label values or pattern recognizing
#'        a part in \code{labels}. Use \code{\link{glob}} or \code{\link{rx}}
#'        to label as a globbing or regular expression pattern. When labeled
#'        with \code{glob} or \code{rx}, it must be a single string. If `part` is
#'        left unspecified, all values not recognized as an aggregate are
#'        interpreted as details that must be aggregated to the whole.
#' @param aggregator \code{[function]} used to aggregate subsets of \code{x}. It should
#'        accept a \code{numeric} vector and return a single number.
#' @param tol \code{[numeric]} tolerance for equality checking
#' @param by Name of variable, or \code{list} of bare variable names, used to
#'        split the values and labels before computing the aggregates. 
#' @param ... Extra arguments passed to aggregator (for example \code{na.rm=TRUE}).
#'  
#'
#' @return A \code{logical} vector of size \code{length(value)}.
#'
#' @examples
#' df <- data.frame(
#'    id = 10011:10020
#'  , period   = rep(c("2018Q1", "2018Q2", "2018Q3", "2018Q4","2018"),2)
#'  , direction = c(rep("import",5), rep("export", 5))
#'  , value     = c(1,2,3,4,10, 3,3,3,3,13)
#' )
#' ## use 'rx' to interpret 'whole' as a regular expression.
#' rules <- validator(
#'   part_whole_relation(value, period, whole=rx("^\\d{4}$")
#'   , by=direction)
#' )
#'
#' out <- confront(df, rules, key="id")
#' as.data.frame(out)
#' @export
part_whole_relation <- function(values, labels, whole, part = NULL
    , aggregator = sum, tol=1e-8, by = NULL, ...){
 
 
  df <- data.frame(values=values, labels=labels)
  f <- function(d, ...){
    i_aggregate <- igrepl(whole, d$labels)
    aggregate   <- d$values[i_aggregate]
    if (length(aggregate)>1){ 
      stop(
          sprintf("Multiple labels matching aggregate: %s. Expecting one"
               , paste(aggregate,collapse=", "))
        , call.=FALSE
      )
    }
    i_details   <- if (is.null(part)) !i_aggregate
                   else igrepl(part, d$labels)

    details     <- d$values[i_details]
    out <- if (length(aggregate)==0){
      FALSE 
    } else {
      abs(aggregator(details, ...) - aggregate) < tol
    }
    values <- !logical(length(d$labels))
    values[i_details | i_aggregate] <- out
    values
  }

  if (is.null(by)){
    return( f(df, ...) )
  } else {
    unsplit(lapply(split(df, by), f, ...),by)
  }

}


#' Label objects for interpretation as pattern
#'
#' Label objects  (typically strings or data frames containing keys combinations) 
#' to be interpreted as regular expression or globbing pattern.
#'
#'
#' @param x Object to label as regular expression (\code{rx(x)}) or globbing
#' (\code{glob(x)}) pattern.
#'
#' 
#'
#' @export
rx <- function(x){ 
    structure(x, class=c("regex",class(x)))
}

#' @rdname rx
#' @export
glob <- function(x){ 
  structure(x, class=c("glob",class(x)))
}

igrepl <- function(pattern, x,...){
  if (inherits(pattern, "glob")){
    Reduce(`|`, lapply(utils::glob2rx(pattern), grepl,x,...))
  } else if (inherits(pattern, "regex",...)){
    Reduce(`|`, lapply(pattern, grepl, x, ...))
  } else {
    x %in% pattern
  }
}

#' split-apply-combine for vectors, with equal-length outptu
#'
#' Group \code{x} by one or more categorical variables, compute
#' an aggregate, repeat that aggregate to match the size of the
#' group, and combine results. The functions \code{sum_by} and 
#' so on are convenience wrappers that call \code{do_by} internally.
#'
#' @param x A bare variable name
#' @param by a bare variable name, or a list of bare variable names, used to
#'        split \code{x} into groups.
#' @param fun \code{[function]} A function that aggregates \code{x} to a single value.
#' @param ... passed as extra arguments to \code{fun} (e.g. \code{na.rm=TRUE}
#' @param na.rm Toggle ignoring \code{NA}
#'
#' @examples
#' x <- 1:10
#' y <- rep(letters[1:2], 5)
#' do_by(x, by=y, fun=max)
#' do_by(x, by=y, fun=sum)
#'
#' @family cross-record-helpers
#' @export
do_by <- function(x, by, fun, ...){
  unsplit( lapply(split(x,by), function(d) rep(fun(d,...), length(d))),by)
}

#' @rdname do_by
#' @export
sum_by <- function(x, by, na.rm=FALSE) do_by(x,by,sum, na.rm=na.rm)

#' @rdname do_by
#' @export
mean_by <- function(x, by, na.rm=FALSE) do_by(x,by,mean, na.rm=na.rm)

#' @rdname do_by
#' @export
min_by <- function(x, by, na.rm=FALSE) do_by(x,by,min, na.rm=na.rm)

#' @rdname do_by
#' @export
max_by <- function(x, by, na.rm=FALSE) do_by(x,by,max, na.rm=na.rm)

#' Hiridoglu-Berthelot function
#'
#' A function to measure `outlierness' for skew distributed data with long
#' right tails. The method works by measuring deviation from a reference
#' value, by default the median. Deviation from above is measured as the
#' ratio between observed and refence values. Deviation from below is
#' measured as the inverse: the ratio between reference value and 
#' observed values.
#'
#' @param x \code{[numeric]}
#' @param ref \code{[function]} or \code{[numeric]}
#' @param ... arguments passed to \code{ref} after \code{x}
#'
#' @return \eqn{\max\{x/ref(x), ref(x)/x\}-1} if \code{ref} is a function,
#'         otherwise \eqn{\max\{x/ref, ref/x\}-1}
#'
#' @references
#' Hidiroglou, M. A., & Berthelot, J. M. (1986). Statistical editing and
#' imputation for periodic business surveys. Survey methodology, 12(1), 73-83.
#'
#' @export
#'
#' @family cross-record-helpers
#' @examples
#' x <- seq(1,20,by=0.1)
#' plot(x,hb(x), 'l')
#'
hb <- function(x, ref=stats::median,...){
  refval <- if(is.numeric(ref)) ref else ref(x,...)
  pmax(x/refval, refval/x) -1
}


#' Check number of code points
#'
#' A convenience function testing for field length.
#'
#'
#' @param x Bare (unquoted) name of a variable. 
#'     Otherwise a vector of class \code{character}. Coerced to character as 
#'     necessary.
#' @param n Number of code points required.
#' @param min Mimimum number of code points
#' @param max Maximum number of code points
#' @param ... passed to \code{nchar} (for example \code{type="width"})
#' @section Details:
#'
#' The number of code points (string length) may depend on current locale
#' settings or encoding issues, including those caused by inconsistent choices
#' of \code{UTF} normalization.
#'
#' @return A \code{[logical]} of size \code{length(x)}.
#' 
#' @examples
#'
#' df <- data.frame(id = 11001:11003, year = c("2018","2019","2020"), value = 1:3)
#' rule <- validator(field_length(year, 4), field_length(id, 5))
#' out <- confront(df, rule) 
#' as.data.frame(out)
#' 
#' @family format-checkers
#'
#' @export
field_length <- function(x, n=NULL, min=NULL, max=NULL,...){
  len <- nchar(as.character(x),...)

  if (!is.null(n) & is.null(min) & is.null(max)){
    len == n 
  } else if (!is.null(min) & !is.null(max) & is.null(n) ){
    len >= min & len <= max
  } else {
    stop("Ill-specified check: either n, or min and max must be not-NULL")
  }
}


#' Check whether a field conforms to a regular expression
#' 
#' A convenience wrapper around \code{grepl} to make rule sets more readable.
#'
#' @param x Bare (unquoted) name of a variable. 
#'     Otherwise a vector of class \code{character}. Coerced to character as 
#'     necessary.
#' @param pattern \code{[character]} a regular expression
#' @param type \code{[character]} How to interpret \code{pattern}. In globbing,
#' the asterisk (`*`) is used as a wildcard that stands for 'zero or more
#' characters'.
#' @param ... passed to grepl
#'
#' @family format-checkers
#' @export
field_format <- function(x, pattern, type=c("glob","regex"), ...){
  type <- match.arg(type)
  if (type == "glob") pattern <- utils::glob2rx(pattern)
  grepl(pattern, x=as.character(x),...)
}



#' Check the layouts of numbers.
#'
#' Convenience function to check layout of numbers stored as
#' a character vector.
#'
#'
#' @param x \code{[character]} vector. If \code{x} is not of type
#'          \code{character} it will be converted.
#' @param format \code{[character]} denoting the number format (see below). 
#' @param min_dig \code{[numeric]} minimal number of digits after decimal separator.
#' @param max_dig \code{[numeric]} maximum number of digits after decimal separator.
#' @param dec \code{[character]} decimal seperator.
#' 
#' 
#' @details
#' If \code{format} is specified, then \code{min_dig}, \code{max_dig} and \code{dec}
#' are ignored.
#'
#' Numerical formats can be specified as a sequence of characters. There are a few 
#' special characters:
#' \itemize{
#'  \item{\code{d}} Stands for digit.
#'  \item{\code{*}} (digit globbing) zero or more digits
#' }
#' 
#' Here are some examples.
#' \tabular{ll}{
#' \code{"d.dd"}   \tab One digit, a decimal point followed by two digits.\cr
#' \code{"d.ddddddddEdd"}\tab Scientific notation with eight digits behind the decimal point.\cr
#' \code{"0.ddddddddEdd"}\tab Same, but starting with a zero.\cr
#' \code{"d,dd*"} \tab one digit before the comma and at least two behind it.\cr
#' }
#'
#'
#' @examples 
#' df <- data.frame(number = c("12.34","0.23E55","0.98765E12"))
#' rules <- validator(
#'    number_format(number, format="dd.dd")
#'    , number_format(number, "0.ddEdd")
#'    , number_format(number, "0.*Edd")
#' )
#'
#' out <- confront(df, rules)
#' values(out)
#'
#' # a few examples, without 'validator'
#' number_format("12.345", min_dig=2) # TRUE
#' number_format("12.345", min_dig=4) # FALSE
#' number_format("12.345", max_dig=2) # FALSE
#' number_format("12.345", max_dig=5) # TRUE
#' number_format("12,345", min_dig=2, max_dig=3, dec=",") # TRUE
#'
#' @family format-checkers
#' @export
number_format <- function(x, format=NULL, min_dig=NULL, max_dig=NULL, dec="."){
  if ( !is.null(format) ){
    rx <- utils::glob2rx(format, trim.tail=FALSE)
    rx <- gsub("d",  "\\d", rx, fixed=TRUE)
    rx <- gsub(".*", "\\d*",   rx, fixed=TRUE)
    return( grepl(rx, as.character(x)) )
  }
  rx <- if (dec == ".") "^.*\\." else sprintf("^.*\\%s",dec)
  decimal_digits <- sub(rx, "", x)
  min_dig <- if (is.null(min_dig)) "0" else as.character(min_dig)
  max_dig <- if (is.null(max_dig)) ""  else as.character(max_dig)
  rx <- sprintf("^\\d{%s,%s}$",min_dig,max_dig)
  grepl(rx,decimal_digits)
  
}



#' Check records using a predifined table of (im)possible values
#'
#' Given a set of keys or key combinations, check whether all thos combinations
#' occur, or check that they do not occur.  Supports globbing and regular
#' expressions.
#'
#'
#' @param keys A data frame or bare (unquoted) name of a data
#'        frame passed as a reference to \code{confront} (see examples).
#'        The column names of \code{keys} must also occurr in the columns
#'        of the data under scrutiny.
#' @param by A bare (unquoted) variable or list of variable names that occur in
#' the data under scrutiny. The data will be split into groups according to
#' these variables and the check is performed on each group.
#' @param allow_duplicates \code{[logical]} toggle whether key combinations can occur 
#'        more than once.
#'
#' @details
#'
#' \tabular{ll}{
#'   \code{contains_exactly} \tab dataset contains exactly the key set, no more, no less. \cr
#'   \code{contains_at_least}\tab dataset contains at least the given keys. \cr
#'   \code{contains_at_most} \tab all keys in the data set are contained the given keys. \cr
#'   \code{does_not_contain} \tab The keys are interpreted as forbidden key combinations. \cr
#' }
#'
#'
#' @section Globbing:
#' Globbing is a simple method of defining string patterns where the asterisks
#' (\code{*}) is used a wildcard. For example, the globbing pattern
#' \code{"abc*"} stands for any string starting with \code{"abc"}.
#'
#'
#' @return 
#' For \code{contains_exactly}, \code{contains_at_least}, and
#' \code{contains_at_most} a \code{logical} vector with one entry for each
#' record in the dataset. Any group not conforming to the test keys will have
#' \code{FALSE} assigned to each record in the group (see examples).
#'
#' @family cross-record-helpers
#' @family key-checkers
#'
#' @examples
#'
#' ## Check that data is present for all quarters in 2018-2019
#' dat <- data.frame(
#'     year    = rep(c("2018","2019"),each=4)
#'   , quarter = rep(sprintf("Q%d",1:4), 2)
#'   , value   = sample(20:50,8)
#' )
#' 
#' # Method 1: creating a data frame in-place (only for simple cases)
#' rule <- validator(contains_exactly(
#'            expand.grid(year=c("2018","2019"), quarter=c("Q1","Q2","Q3","Q4"))
#'           )
#'         )
#' out <- confront(dat, rule)
#' values(out)
#' 
#' # Method 2: pass the keyset to 'confront', and reference it in the rule.
#' # this scales to larger key sets but it needs a 'contract' between the
#' # rule definition and how 'confront' is called.
#' 
#' keyset <- expand.grid(year=c("2018","2019"), quarter=c("Q1","Q2","Q3","Q4"))
#' rule <- validator(contains_exactly(all_keys))
#' out <- confront(dat, rule, ref=list(all_keys = keyset))
#' values(out)
#' 
#' ## Globbing (use * as a wildcard)
#' 
#' # transaction data 
#' transactions <- data.frame(
#'     sender   = c("S21", "X34", "S45","Z22")
#'   , receiver = c("FG0", "FG2", "DF1","KK2")
#'   , value    = sample(70:100,4)
#' )
#' 
#' # forbidden combinations: if the sender starts with "S", 
#' # the receiver can not start "FG"
#' forbidden <- data.frame(sender="S*",receiver = "FG*")
#'
#' rule <- validator(does_not_contain(glob(forbidden_keys)))
#' out <- confront(transactions, rule, ref=list(forbidden_keys=forbidden))
#' values(out)
#'
#'
#' ## Quick interactive testing
#' # use 'with':
#' with(transactions, does_not_contain(forbidden)) 
#'
#'
#'
#' ## Grouping 
#' 
#' # data in 'long' format
#' dat <- expand.grid(
#'   year = c("2018","2019")
#'   , quarter = c("Q1","Q2","Q3","Q4")
#'   , variable = c("import","export")
#' )
#' dat$value <- sample(50:100,nrow(dat))
#' 
#' 
#' periods <- expand.grid(
#'   year = c("2018","2019")
#'   , quarter = c("Q1","Q2","Q3","Q4")
#' )
#' 
#' rule <- validator(contains_exactly(all_periods, by=variable))
#' 
#' out <- confront(dat, rule, ref=list(all_periods=periods))
#' values(out)
#' 
#' # remove one  export record
#' 
#' dat1 <- dat[-15,]
#' out1 <- confront(dat1, rule, ref=list(all_periods=periods))
#' values(out1)
#' values(out1)
#' 
#' @export
contains_exactly <- function(keys, by=NULL, allow_duplicates=FALSE){
  given_keys   <- do.call(paste, keys)
  L <- list()
  for ( keyname in names(keys) ) L[[keyname]] <- dynGet(keyname)

  found_keys   <- do.call(paste, L)
  
  if (is.null(by)) by <- character(length(found_keys))

  unsplit(lapply(split(found_keys, f=by), function(fk){
    out <- all(fk %in% given_keys) && all(given_keys %in% fk)
    if (!allow_duplicates) out <- out && !any(duplicated(fk))
    rep(out, length(fk))
  }), by)

}


#' @rdname contains_exactly
#' @export
contains_at_least <- function(keys, by=NULL){
  L <- list()
  for ( keyname in names(keys) ) L[[keyname]] <- dynGet(keyname)

  given_keys   <- do.call(paste, keys)
  found_keys   <- do.call(paste, L)

  if (is.null(by)) by <- character(length(found_keys))

  unsplit(lapply(split(found_keys, f=by), function(fk){
    rep(all(given_keys %in% fk), length(fk))
  }), by)

}

#' @rdname contains_exactly
#' @return
#' For \code{contains_at_least}: a \code{logical} vector equal to the number of
#' records under scrutiny. It is \code{FALSE} where key combinations do not match
#' any value in \code{keys}.
#' @export
contains_at_most <- function(keys, by=NULL){

  L <- list()
  for ( keyname in names(keys) ) L[[keyname]] <- dynGet(keyname)
  
  contains(L, keys, by=by)

}



#' @rdname contains_exactly
#'
#'
#' @return 
#' For \code{does_not_contain}:  a \code{logical} vector with size equal to the
#' number of records under scrutiny. It is \code{FALSE} where key combinations
#' do not match any value in \code{keys}.
#' @export
does_not_contain <- function(keys){

  L <- list()
  for ( keyname in names(keys) ) L[[keyname]] <- dynGet(keyname)

  !contains(L, keys, by=NULL)
}

# for each 'x' see if it matches any regular expression in 'pattern'
rxin <- function(x, pattern){
  A <- sapply(pattern, grepl, x=x)
  if (!is.array(A)) A <- matrix(A,ncol=length(pattern))
  apply(A, 1, any)
}

# for each 'x' see if it matches any globbing pattern in 'pattern' 
glin <- function(x, pattern){
  pattern <- utils::glob2rx(pattern)
  rxin(x, pattern)
}

get_keytype <- function(keys){ 
  out <- grep("^(regex)|(glob)$", class(keys), value=TRUE)
  if (length(out) < 1) out <- "fixed"
  out
}

contains <- function(dat, keys, by){

  keytype <- get_keytype(keys)
  

  if (isTRUE(keytype=="regex") && length(keys) > 1){
    # some preparations before pasting
    for (keyname in names(keys)[-1]){ 
      key <- keys[[keyname]]
      keys[[keyname]] <- ifelse( substr(key,1,1) == "^"
                          , sub("^\\^", "", keys[[keyname]])
                          , paste0(".*", key) )
    }
    for (keyname in names(keys)[-length(keys)]){
      key <- keys[[keyname]]
      keys[[keyname]] <- ifelse( substr(key, nchar(key), nchar(key)) == "$"
                           ,  sub("\\$$", "", key)
                           ,  paste0(key, ".*"))
    }

  } 

  # note: globbing patterns may be pasted before transformation
  # to regex.
  given_keys   <- do.call(paste, keys)
  found_keys   <- do.call(paste, dat)
  if (is.null(by)) by <- character(length(found_keys)) 

  unsplit(lapply(split(found_keys, f=by), function(fk){
    switch(keytype
      , "fixed" = fk %in% given_keys
      , "glob"    = glin(fk, given_keys)
      , "regex"   = rxin(fk, given_keys)
    )
  }), by)

}

#' Check aggregates defined by a hierarchical code list
#'
#' Check all aggregates defined by a code hierarchy.
#'
#'
#' @param values bare (unquoted) name of a variable that holds values that
#'        must aggregate according to the \code{hierarchy}.
#' @param labels bare (unquoted) name of variable holding a grouping variable (a code
#'        from a hierarchical code list)
#' @param hierarchy \code{[data.frame]} defining a hierarchical code list. The
#'        first column must contain (child) codes, and the second column contains their
#'        corresponding parents.
#' @param by A bare (unquoted) variable or list of variable names that occur in
#'        the data under scrutiny. The data will be split into groups according 
#'        to these variables and the check is performed on each group.
#' @param na_value \code{[logical]} or \code{NA}. Value assigned to values that
#'        do not occurr in checks.
#' @param aggregator \code{[function]} that aggregates children to their parents.
#' @param tol \code{[numeric]} tolerance for equality checking
#' @param ... arguments passed to \code{aggregator} (e.g. \code{na.rm=TRUE}).
#' 
#'
#' @return A \code{logical} vector with the size of \code{length(values)}. Every
#'         element involved in an aggregation error is labeled \code{FALSE} (aggregate
#'         plus aggregated elements). Elements that are involved in correct
#'         aggregations are set to \code{TRUE}, elements that are not involved in 
#'         any check get the value \code{na_value} (by default: \code{TRUE}).
#'
#'
#'
#' @family cross-record-helpers
#' @export
#' @examples
#' # We check some data against the built-in NACE revision 2 classification.
#' data(nace_rev2)
#' head(nace_rev2[1:4]) # columns 3 and 4 contain the child-parent relations.
#'
#' d <- data.frame(
#'      nace   = c("01","01.1","01.11","01.12", "01.2")
#'    , volume = c(100 ,70    , 30    ,40     , 25    )
#' )
#' # It is possible to perform checks interactively
#' d$nacecheck <- hierarchy(d$volume, labels = d$nace, hierarchy=nace_rev2[3:4])
#' # we have that "01.1" == "01.11" + "01.12", but not "01" == "01.1" +  "01.2"
#' print(d)
#'
#' # Usage as a valiation rule is as follows
#' rules <- validator(hierarchy(volume, labels = nace, hierarchy=validate::nace_rev_2[3:4]))
#' confront(d, rules)
#'
#' # you can also pass a hierarchy as a reference, for example.
#' 
#' rules <- validator(hierarchy(volume, labels = nace, hierarchy=ref$nacecodes))
#' out <- confront(d, rules, ref=list(nacecodes=nace_rev2[3:4]))
#' summary(out)
#' 
#' # set a output to NA when a code does not occur in the code list.
#' d <- data.frame(
#'      nace   = c("01","01.1","01.11","01.12", "01.2", "foo")
#'    , volume = c(100 ,70    , 30    ,40     , 25     , 60)
#' )
#' 
#' d$nacecheck <- hierarchy(d$volume, labels = d$nace, hierarchy=nace_rev2[3:4]
#'                          , na_value = NA)
#' # we have that "01.1" == "01.11" + "01.12", but not "01" == "01.1" +  "01.2"
#' print(d)
#'
hierarchy <- function(values, labels, hierarchy, by=NULL, tol=1e-8, na_value=TRUE, aggregator = sum, ...){

  if (is.null(by)) by <- character(length(values))
 
  dat <- cbind(data.frame(values=values, labels=labels), by)
  unsplit(lapply(split(dat, f=by)
          , check_hagg,  h=hierarchy, na_value = na_value, tol=tol, fun=aggregator,...)
    , f=by)

}


check_hagg <- function(dat, h, na_value, tol, fun,...){

  parents <- unique(h[,2])
  keytype <- get_keytype(h)
  out <- rep(na_value, nrow(dat))

  for (parent in parents){
    J <- dat$labels %in% parent
    children <- h[,1][h[,2] == parent]
    I <- switch(keytype
          , "glob"  = glin(dat$labels, children)
          , "regex" = rxin(dat$labels, children)
          ,  dat$labels %in% children)
    # found 'parent' too often, so we can't check aggregate
    if (sum(J) > 1){
      grp <- paste0("(",paste(t(dat[1,-(1:2)]), collapse=", "),")")
      msg <- "Parent '%s' occurs more than once (%d times) in group %s"
      warning(sprintf(msg, parent, sum(J), grp), call.=FALSE)
      out[I|J] <- FALSE
      next
    }

    if (!any(J) && !any(I)) next
    if (!any(J) &&  any(I)) out[I] <- FALSE # no parent but children present
    if ( any(J) && !any(I)) out[J] <- FALSE # no children but parent present
    ii   <- I|J
    test <- abs(dat$values[J] - fun(dat$value[I],...)) <= tol
    if (any(J) && any(I)){ 
      # equivalent, but slower statement:
      # if ( any(J) &&  any(I)) out[ii] <- ifelse(is.na(out[ii]), test, out[ii] & test)
      out[ii] <- (is.na(out[ii]) & test) | (!is.na(out[ii]) & out[ii] & test)
    }
  }
  out
}



