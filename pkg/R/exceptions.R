# TEMPORARY RULE EXCEPTIONS (WAIVERS) ----------------------------------------
#
# A supplier may be granted a temporary exemption from a single validation
# rule during e.g. a system upgrade.  Such an exemption is registered in an
# 'exceptions' registry with a validity window.  Applying the registry to a
# validation result leaves the original verdicts untouched, records the
# effective verdict and the basis of every exemption, and stops waiving as
# soon as the approval expires (so blocking resumes automatically).
#
# Conflicting approvals (same supplier, same rule, overlapping window, but a
# different decision) are detected *before* any result is computed.

## CONSTRUCTOR ----------------------------------------------------------------

#' Store the result of applying temporary exceptions
#'
#' An object of class \code{excepted_validation} is a \code{\link{validation}}
#' whose \code{\link{values}} are the effective verdicts (after exceptions were
#' applied).  The original verdicts and the applied exceptions are retained;
#' see \code{\link{original_values}} and \code{\link{exception_report}}.
#'
#' @aliases excepted_validation
#' @family exceptions
#' @export
setRefClass("excepted_validation", contains = "validation")

#' Register temporary exceptions (waivers) for validation rules
#'
#' An \code{exceptions} object is a small registry describing which validation
#' rule is temporarily waived for which supplier, and during which period.  It
#' is the counterpart of a \code{\link{validator}}: the validator describes what
#' must hold, the registry describes when a rule is allowed to fail.
#'
#' @section Required columns:
#' \describe{
#'   \item{\code{supplier}}{Supplier (or any other unit) the exception applies to.}
#'   \item{\code{rule}}{Name of the rule to waive, as used in the validator.}
#'   \item{\code{start}}{Start of the validity window.}
#'   \item{\code{end}}{End of the validity window (inclusive).}
#'   \item{\code{basis}}{Free-text justification of the exception; stored with
#'     every applied exception so reports remain auditable.}
#' }
#' Two optional columns are recognized:
#' \describe{
#'   \item{\code{decision}}{\code{"exempt"} (default) to waive a failing rule,
#'     or \code{"enforce"} to (re)impose it.}
#'   \item{\code{id}}{Identifier of the approval. Defaults to the row number.}
#' }
#'
#' @param ... Columns of the registry as named vectors, or a single
#'   \code{data.frame}.
#' @param .data A \code{data.frame} holding the registry.
#' @param .file Path to a \code{.csv} or \code{.yaml} file holding the registry.
#'
#' @return An object of class \code{exceptions} (inheriting from
#'   \code{data.frame}).
#'
#' @seealso \code{\link{apply_exceptions}}, \code{\link{check_exceptions}}
#'
#' @example ../examples/temporary_exceptions.R
#'
#' @examples
#' dat <- data.frame(supplier = c("A", "A", "B", "B")
#'                 , turnover = c(10, -1, 10, -1))
#' rules <- validator(turnover >= 0)
#' cf <- confront(dat, rules, key = "supplier")
#'
#' exc <- exceptions(supplier = "A", rule = "V1"
#'                 , start = "2026-01-01", end = "2026-03-31"
#'                 , basis = "upgrade of supplier A")
#' out <- apply_exceptions(cf, exc, at = as.POSIXct("2026-02-01", tz = "UTC"))
#' original_values(out)  # original failure of A is retained
#' values(out)           # effective verdict: A waived, B still blocked
#'
#' @family exceptions
#' @export
exceptions <- function(..., .data, .file){
  if (!missing(.file)){
    if (!missing(.data)) stop("Specify either '.file' or '.data', not both")
    dat <- read_exceptions_file(.file)
  } else if (!missing(.data)){
    dat <- .data
  } else {
    args <- list(...)
    if (length(args) == 1L && is.data.frame(args[[1]])) dat <- args[[1]]
    else dat <- data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
  }
  as_exceptions(dat)
}

read_exceptions_file <- function(file){
  if (!file.exists(file)) stop(sprintf("File '%s' does not exist", file))
  if (grepl("\\.csv$", file, ignore.case = TRUE)){
    utils::read.csv(file, stringsAsFactors = FALSE)
  } else if (grepl("\\.ya?ml$", file, ignore.case = TRUE)){
    L <- yaml::yaml.load_file(file)
    if (is.null(L)) return(data.frame())
    if (!is.null(names(L)) && !all(sapply(L, is.list))) L <- list(L)
    rows <- lapply(L, function(r){
      r <- lapply(r, function(v) if (length(v) == 1L) v else paste(v, collapse = ";"))
      as.data.frame(r, stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
  } else {
    stop(sprintf("Unsupported exceptions file type '%s' (use .csv or .yaml)", file))
  }
}

#' Coerce to an exceptions registry
#'
#' Validates the structure of a registry and returns it as an object of class
#' \code{exceptions}.  This function is mostly internal but exported so that
#' other packages can rely on the same coercion rules.
#'
#' @param x A \code{data.frame} (or an existing \code{exceptions} object).
#' @param ... Ignored.
#'
#' @return An object of class \code{exceptions}.
#' @family exceptions
#' @export
as_exceptions <- function(x, ...){
  if (inherits(x, "exceptions")) return(x)
  if (missing(x) || is.null(x)) x <- data.frame()
  if (!is.data.frame(x)) stop("'x' must be a data.frame")
  if (nrow(x) == 0L){
    out <- empty_exceptions()
    return(out)
  }
  needed <- c("supplier", "rule", "start", "end", "basis")
  missing_cols <- setdiff(needed, names(x))
  if (length(missing_cols)){
    stop(sprintf("Missing required column(s) in exceptions registry: %s"
       , paste(missing_cols, collapse = ", ")))
  }
  if (is.null(x$decision)) x$decision <- "exempt"
  if (is.null(x$id)) x$id <- seq_len(nrow(x))
  x$supplier <- as.character(x$supplier)
  x$rule     <- as.character(x$rule)
  x$basis    <- as.character(x$basis)
  x$decision <- as.character(x$decision)
  x$start    <- as_time(x$start)
  x$end      <- as_time(x$end)

  bad <- !nzchar(x$supplier) | is.na(x$supplier) |
         !nzchar(x$rule)     | is.na(x$rule) |
         !nzchar(x$basis)    | is.na(x$basis)
  if (any(bad)){
    stop(sprintf("Missing supplier, rule or basis on row(s) %s"
       , paste(which(bad), collapse = ", ")))
  }
  badtime <- is.na(x$start) | is.na(x$end)
  if (any(badtime)){
    stop(sprintf("Unparseable start or end time on row(s) %s"
       , paste(which(badtime), collapse = ", ")))
  }
  if (any(x$start > x$end)){
    stop(sprintf("Start lies after end on row(s) %s"
       , paste(which(x$start > x$end), collapse = ", ")))
  }
  ok <- x$decision %in% c("exempt", "enforce")
  if (any(!ok)){
    stop(sprintf("Invalid decision on row(s) %s; use 'exempt' or 'enforce'"
       , paste(which(!ok), collapse = ", ")))
  }
  attr(x, "class") <- c("exceptions", "data.frame")
  x
}

empty_exceptions <- function(){
  x <- data.frame( supplier = character(0)
                 , rule     = character(0)
                 , start    = as.POSIXct(character(0), tz = "UTC")
                 , end      = as.POSIXct(character(0), tz = "UTC")
                 , basis    = character(0)
                 , decision = character(0)
                 , id       = character(0)
                 , stringsAsFactors = FALSE)
  class(x) <- c("exceptions", "data.frame")
  x
}

# robust conversion of user-supplied times to POSIXct (UTC)
as_time <- function(x){
  if (inherits(x, "POSIXct")) return(as.POSIXct(x, tz = "UTC"))
  if (inherits(x, "Date"))    return(as.POSIXct(x, tz = "UTC"))
  if (is.numeric(x))          return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
  x <- as.character(x)
  out <- as.POSIXct(x, tz = "UTC")
  for (fmt in c("%Y-%m-%dT%H:%M:%SZ", "%Y-%m-%dT%H:%M:%S", "%Y%m%dT%H%M%S")){
    i <- is.na(out) & !is.na(x)
    if (!any(i)) break
    out[i] <- as.POSIXct(strptime(x[i], format = fmt, tz = "UTC"))
  }
  out
}

# Convert a 'period' key column to a [start, end] span.  Periods are often
# stored as integers (a year) while an exception window is expressed on a finer
# grid, so a bare year is mapped to its full calendar span before matching.
as_period <- function(x){
  if (inherits(x, "POSIXct") || inherits(x, "Date") || !is.numeric(x)){
    p <- as_time(x)
    return(list(start = p, end = p))
  }
  year <- as.integer(x)
  start <- as.POSIXct(sprintf("%04d-01-01 00:00:00", year), tz = "UTC")
  end <- as.POSIXct(sprintf("%04d-12-31 23:59:59", year), tz = "UTC")
  list(start = start, end = end)
}

# Determine whether each record's period span overlaps an exception window.
period_in_window <- function(per, start, end){
  per$start <= end & per$end >= start
}


## CONFLICT DETECTION ---------------------------------------------------------

#' Detect conflicting exceptions before computing results
#'
#' A conflict occurs when two approvals for the same supplier and the same rule
#' have overlapping validity windows but a different \code{decision}.  Because
#' such a pair makes the effective verdict ambiguous, it is reported as an
#' error \emph{before} any validation result is computed or modified.
#'
#' @param x An \code{exceptions} registry (or a \code{data.frame} that can be
#'   coerced to one).
#' @param ... Ignored.
#'
#' @return Invisibly \code{TRUE} when no conflict is found; otherwise an error
#'   listing all conflicting pairs is thrown.
#'
#' @family exceptions
#' @export
check_exceptions <- function(x, ...){
  x <- as_exceptions(x)
  if (nrow(x) == 0L) return(invisible(TRUE))
  conflict <- list()
  grp <- paste(x$supplier, x$rule, sep = "\r")
  for (g in unique(grp)){
    i <- which(grp == g)
    if (length(i) < 2L) next
    for (a in seq_len(length(i) - 1L)){
      for (b in seq.int(a + 1L, length(i))){
        ia <- i[a]; ib <- i[b]
        if (x$decision[ia] == x$decision[ib]) next
        overlap <- x$start[ia] <= x$end[ib] && x$start[ib] <= x$end[ia]
        if (overlap) conflict[[length(conflict) + 1L]] <- c(ia, ib)
      }
    }
  }
  if (length(conflict)){
    txt <- vapply(conflict, function(ij){
      sprintf("  supplier '%s', rule '%s': approval '%s' [%s .. %s, %s] conflicts with approval '%s' [%s .. %s, %s]"
        , x$supplier[ij[1]], x$rule[ij[1]]
        , x$id[ij[1]], fmt_time(x$start[ij[1]]), fmt_time(x$end[ij[1]]), x$decision[ij[1]]
        , x$id[ij[2]], fmt_time(x$start[ij[2]]), fmt_time(x$end[ij[2]]), x$decision[ij[2]])
    }, character(1))
    stop(paste0("Conflicting exceptions detected; refusing to apply them:\n"
              , paste(txt, collapse = "\n")), call. = FALSE)
  }
  invisible(TRUE)
}

fmt_time <- function(x) format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%d %H:%M:%S UTC")

#' Test which registered exceptions are in force
#'
#' An exception is in force when the reference time \code{at} falls inside its
#' closed validity window \code{[start, end]}.
#'
#' @param x An \code{exceptions} registry.
#' @param at A \code{POSIXct} (or something coercible to it) reference time.
#'   Defaults to the current time.
#' @param ... Ignored.
#'
#' @return A logical vector with one element per registered exception.
#' @family exceptions
#' @export
in_force <- function(x, at = Sys.time(), ...){
  x  <- as_exceptions(x)
  at <- as_time(at)
  if (length(at) != 1L || is.na(at)) stop("'at' must be a single, valid time")
  if (nrow(x) == 0L) return(logical(0))
  x$start <= at & at <= x$end
}


## APPLYING EXCEPTIONS --------------------------------------------------------

#' Apply registered exceptions to a validation result
#'
#' The original verdicts of \code{x} are never altered.  For every record of an
#' excepted supplier, for the excepted rule, and while the approval is in force
#' at time \code{at}, an original failure is reported as an effective pass.
#' The original verdict, the effective verdict and the basis of the exception
#' are all retained in the returned object (see \code{\link{exception_report}}
#' and \code{\link{original_values}}).
#'
#' Records are attributed to suppliers through the key set stored by
#' \code{\link{confront}} (call it with \code{key=}).  The key must contain a
#' column named by the \code{supplier} argument.
#'
#' Because exceptions are evaluated relative to \code{at}, an approval stops
#' being applied as soon as \code{at} lies after its \code{end}: blocking then
#' resumes automatically.  Historical reports can be reproduced by passing the
#' historical reference time as \code{at}.
#'
#' @param x A \code{\link{validation}} object, as returned by
#'   \code{\link{confront}}.
#' @param exceptions An \code{exceptions} registry.
#' @param at Reference time at which validity of the approvals is evaluated.
#' @param supplier Name of the key column that identifies the supplier.
#' @param period Optional name of a key column holding the period of each
#'   record.  When given, an exception only applies to records whose period
#'   lies inside the exception window as well.
#' @param ... Ignored.
#'
#' @return An object of class \code{excepted_validation} (a
#'   \code{\link{validation}}), whose \code{\link{values}} are the effective
#'   verdicts.
#'
#' @family exceptions
#' @export
apply_exceptions <- function(x, exceptions, at = Sys.time()
                           , supplier = "supplier", period = NULL, ...){
  if (!inherits(x, "validation")) stop("'x' must be a validation object")
  exc <- as_exceptions(exceptions)
  # conflicts are detected before any computation takes place
  check_exceptions(exc)

  at <- as_time(at)
  if (length(at) != 1L || is.na(at)) stop("'at' must be a single, valid time")

  ks <- keyset(x)
  if (is.null(ks) || ncol(ks) == 0L || !supplier %in% names(ks)){
    stop(sprintf(paste0("Cannot attribute records to suppliers: no key column '%s'. "
      , "Call confront() with key= including '%s'."), supplier, supplier), call. = FALSE)
  }
  sups <- as.character(ks[[supplier]])
  nrec <- nrow(ks)

  per <- NULL
  if (!is.null(period)){
    if (!period %in% names(ks)) stop(sprintf("Key column '%s' not found", period))
    per <- as_period(ks[[period]])
    if (all(is.na(per$start)) && any(!is.na(ks[[period]]))){
      stop(sprintf(paste0("Key column '%s' could not be interpreted as time; "
        , "use Date, POSIXct, a year, or an ISO 8601 string"), period), call. = FALSE)
    }
  }

  original  <- if (length(x$._original)) x$._original else x$._value
  effective <- original
  report <- list()

  force <- in_force(exc, at = at)
  # process waivers ('exempt') before re-impositions ('enforce') so the outcome
  # does not depend on the order rows happen to be stored in
  order_k <- which(force)[order(exc$decision[force] != "exempt")]
  if (length(order_k)) for (k in order_k){
    rule <- exc$rule[k]
    j <- match(rule, names(effective))
    if (is.na(j)){
      warning(sprintf("Exception refers to unknown rule '%s'; it was ignored", rule)
            , call. = FALSE)
      next
    }
    v <- effective[[j]]
    if (is.null(v)) next
    if (length(v) != nrec){
      if (any(!is.na(sups) & sups == exc$supplier[k])){
        warning(sprintf(paste0("Rule '%s' is not record-level; supplier-specific "
          , "exception was not applied"), rule), call. = FALSE)
      }
      next
    }
    match_sup <- !is.na(sups) & sups == exc$supplier[k]
    if (!is.null(per)){
      in_win <- period_in_window(per, exc$start[k], exc$end[k])
      match_sup <- match_sup & !is.na(in_win) & in_win
    }
    rec <- which(match_sup)
    if (!length(rec)) next

    old <- v[rec]
    orig <- original[[j]][rec]
    if (exc$decision[k] == "exempt"){
      # waive failures: an original fail becomes an effective pass
      changed <- rec[!is.na(old) & !old]
      newval  <- rep(TRUE, length(changed))
      origval <- rep(FALSE, length(changed))
    } else {
      # 'enforce' re-imposes the original rule, undoing any waiver recorded for
      # these records
      changed <- rec[!is.na(old) & old != orig]
      newval  <- orig[!is.na(old) & old != orig]
      origval <- newval
    }
    if (!length(changed)) next
    v[changed] <- newval
    effective[[j]] <- v
    for (ii in seq_along(changed)){
      r <- changed[ii]
      report[[length(report) + 1L]] <- data.frame(
          supplier  = sups[r]
        , rule      = rule
        , record    = r
        , original  = origval[ii]
        , effective = newval[ii]
        , decision  = exc$decision[k]
        , basis     = exc$basis[k]
        , start     = exc$start[k]
        , end       = exc$end[k]
        , id        = as.character(exc$id[k])
        , at        = at
        , stringsAsFactors = FALSE
      )
    }
  }

  rep <- if (length(report)) do.call(rbind, report) else empty_exception_report()

  new("excepted_validation"
    , ._call       = x$._call
    , ._calls      = x$._calls
    , ._value      = effective
    , ._warn       = x$._warn
    , ._error      = x$._error
    , ._keys       = x$._keys
    , ._event      = x$._event
    , ._original   = original
    , ._exceptions = rep
  )
}

empty_exception_report <- function(){
  data.frame(
      supplier  = character(0)
    , rule      = character(0)
    , record    = integer(0)
    , original  = logical(0)
    , effective = logical(0)
    , decision  = character(0)
    , basis     = character(0)
    , start     = as.POSIXct(character(0), tz = "UTC")
    , end       = as.POSIXct(character(0), tz = "UTC")
    , id        = character(0)
    , at        = as.POSIXct(character(0), tz = "UTC")
    , stringsAsFactors = FALSE
  )
}

# subset the exception log to the rules that remain after `[`-selection
slice_exception_report <- function(d, rules){
  if (is.null(d) || !is.data.frame(d) || ncol(d) == 0L) return(empty_exception_report())
  d[d$rule %in% rules, , drop = FALSE]
}


## ACCESSORS ------------------------------------------------------------------

#' Report the exceptions that were applied
#'
#' Returns one row per record whose verdict was changed by an exception, with
#' the supplier, rule, the original and effective verdict, the decision, the
#' basis of the approval and the approval window.
#'
#' @param x A confrontation object.
#' @param ... Ignored.
#'
#' @return A \code{data.frame}.  For objects without applied exceptions the
#'   data frame has zero rows and the documented columns.
#'
#' @family exceptions
#' @export
setGeneric("exception_report", function(x, ...) standardGeneric("exception_report"))

#' @rdname exception_report
setMethod("exception_report", "confrontation", function(x, ...){
  d <- x$._exceptions
  if (is.null(d) || !is.data.frame(d) || ncol(d) == 0L) return(empty_exception_report())
  d
})

#' Extract the original (pre-exception) verdicts
#'
#' The verdicts exactly as produced by \code{\link{confront}}, before any
#' exception was applied.  They are retained even after an approval expires, so
#' historical failures remain visible.
#'
#' @param x A confrontation object.
#' @param simplify Combine results with similar dimension structure into arrays?
#' @param drop If a single vector or array results, drop the list attribute?
#' @param ... Ignored.
#'
#' @return The original verdicts, shaped like \code{\link{values}}.
#'
#' @family exceptions
#' @export
setGeneric("original_values", function(x, ...) standardGeneric("original_values"))

#' @rdname original_values
setMethod("original_values", "confrontation", function(x, simplify = TRUE, drop = TRUE, ...){
  v <- x$._original
  if (!length(v)) v <- x$._value
  out <- if (simplify){
    simplify_list(v[!sapply(v, is.null)])
  } else {
    v
  }
  if (drop && length(out) == 1L) out[[1]] else out
})


## METHODS FOR excepted_validation --------------------------------------------

#' @rdname values
#' @param which Return the \code{"effective"} (default) or the
#'   \code{"original"} verdicts?
#' @family exceptions
setMethod("values", "excepted_validation"
  , function(x, simplify = TRUE, drop = TRUE, which = c("effective", "original"), ...){
  which <- match.arg(which)
  if (which == "original") return(original_values(x, simplify = simplify, drop = drop))
  int_values(x, simplify = simplify, drop = drop, ...)
})

#' @rdname validate-summary
#' @family exceptions
setMethod("summary", "excepted_validation", function(object, ...){
  s <- callNextMethod()
  orig <- object$._original
  if (!length(orig)) orig <- object$._value
  of <- vapply(orig, function(v) if (is.null(v)) NA_integer_ else sum(!v, na.rm = TRUE)
             , FUN.VALUE = integer(1))
  names(of) <- names(object)
  rep <- exception_report(object)
  waived <- vapply(names(object), function(nm) sum(rep$rule == nm)
                 , FUN.VALUE = integer(1))
  s$original_fails <- unname(of[s$name])
  s$waived <- unname(waived[s$name])
  s
})

#' @rdname as.data.frame
#' @family exceptions
setMethod("as.data.frame", "excepted_validation", function(x, ...){
  df <- callNextMethod()
  if (nrow(df) == 0L){
    df$original_value <- logical(0)
    df$waived <- logical(0)
    df$basis <- character(0)
    return(df)
  }
  df$original_value <- NA
  df$waived <- FALSE
  df$basis <- NA_character_
  rep <- exception_report(x)
  orig <- x$._original
  if (!length(orig)) orig <- x$._value
  for (nm in unique(df$name)){
    idx <- which(df$name == nm)
    j <- match(nm, names(x))
    o <- if (is.na(j)) NULL else orig[[j]]
    if (is.null(o)) o <- x$._value[[j]]
    if (!is.null(o) && length(o) == length(idx)) df$original_value[idx] <- o
    rr <- rep[rep$rule == nm, , drop = FALSE]
    if (nrow(rr)){
      for (k in seq_len(nrow(rr))){
        rec <- rr$record[k]
        if (!is.na(rec) && rec >= 1L && rec <= length(idx)){
          df$waived[idx[rec]] <- TRUE
          df$basis[idx[rec]] <- rr$basis[k]
        }
      }
    }
  }
  df
})

#' @rdname exceptions
#' @export
print.exceptions <- function(x, ...){
  cat(sprintf("Object of class 'exceptions': %d registered exception(s)\n", nrow(x)))
  print.data.frame(as.data.frame(unclass(x)), ...)
  invisible(x)
}

#' @rdname apply_exceptions
setMethod("show", "excepted_validation", function(object){
  callNextMethod()
  n <- nrow(exception_report(object))
  cat(sprintf('   Exceptions applied: %d\n', n))
})
