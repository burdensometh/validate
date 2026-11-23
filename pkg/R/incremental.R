#' Incremental revalidation of cleaned data
#'
#' Re-run only those validation rules that are actually affected by a change in
#' the data, and merge the results that are still valid from a previous
#' confrontation, matching records by row key.
#'
#' @section Motivation:
#' A cleaning operation often changes a single column of a data set while a
#' complete rule set is expensive to evaluate. Many rules are unaffected by such
#' a change and their earlier results can be reused. \code{revalidate} computes
#' the dependency of every expanded rule expression on the data columns and on
#' external reference objects, re-evaluates only the affected expressions and
#' copies the remaining results from \code{previous}. Results are matched
#' record-by-record using the row key that was used when \code{previous} was
#' created with \code{\link{confront}}.
#'
#' @section Derived variables:
#' Transient variables defined with the \code{:=} operator are substituted
#' before dependencies are computed. A rule that uses a changed column only
#' through one or more derived variables is therefore still recognized as
#' affected, also transitively. The same holds for variable groups defined with
#' \code{\link{var_group}}, whose expansions are analyzed individually.
#'
#' @section Reference data:
#' Variables occurring in a rule that are not columns of \code{dat} are treated
#' as external (reference) variables. A \code{\link{confront}}ation records the
#' reference data it used. Passing the (possibly changed) \code{ref} again lets
#' \code{revalidate} detect external changes automatically: rules that use a
#' changed reference object are re-evaluated, other rules are reused. If
#' \code{ref} is not passed while \code{previous} used reference data, all rules
#' using reference data are re-evaluated conservatively. Rules that use
#' reference data are never merged across a reordering of the records, because
#' reference data is typically aligned with the record order.
#'
#' @section Detecting changed columns:
#' When \code{changed} is not specified, the columns of \code{dat} are compared
#' value-by-value with those of the data used to create \code{previous},
#' considering only the records present in both versions (matched by row key).
#' Columns that were added, removed or modified are considered changed, while a
#' reordering or extension of the records does not make a column "changed".
#' When \code{previous} does not carry the data it was computed from (e.g.
#' because it was created by an older version of the package), all columns are
#' considered changed unless \code{changed} is given. Rules that address the
#' data set as a whole (using \code{.}) or that aggregate over records (e.g.
#' \code{mean(x)} or \code{is_unique(id)}) are re-evaluated whenever the column
#' set, the number of records, or one of their input columns changes.
#'
#' @section Memory use:
#' To enable change detection, \code{\link{confront}} keeps a \emph{reference}
#' to the data and reference objects it was given; it does not copy them (R
#' copies on modification, and it will not modify these objects). A
#' \code{confrontation} therefore keeps its input data alive for as long as it
#' exists. In the typical cleaning workflow, where the data set is retained
#' anyway, this costs no extra memory; pass \code{changed} to \code{revalidate}
#' if the snapshot is undesirable.
#'
#' @param dat A \code{data.frame} holding the current data.
#' @param x A \code{\link{validator}} or \code{\link{indicator}} object.
#' @param previous A \code{\link{confrontation}} object (e.g. the
#'   \code{validation} returned by a previous call to \code{\link{confront}}).
#'   If missing or \code{NULL}, \code{revalidate} simply calls
#'   \code{\link{confront}}.
#' @param changed \code{[character]} names of the columns (or external reference
#'   objects) that changed. When \code{NULL} the change is detected by comparing
#'   \code{dat} and \code{ref} with the data and reference objects recorded in
#'   \code{previous}.
#' @param ref Optionally, the current reference data (an environment, a
#'   \code{data.frame} or a named \code{list}), exactly as passed to
#'   \code{\link{confront}}.
#' @param key \code{[character]} name(s) of the identifying variable(s). When
#'   \code{NULL} the key stored with \code{previous} is used.
#' @param ref_changed Optionally, \code{[character]} names of external reference
#'   objects that changed. When \code{NULL} this is detected from \code{ref}.
#' @param ... Options used at execution time, passed to \code{\link{voptions}}.
#'
#' @return An object of the same class as \code{previous} (usually
#'   \code{validation}) holding results for all rules in \code{x}, evaluated on
#'   \code{dat}. The attributes \code{"recomputed"}, \code{"reused"} and
#'   \code{"partially_reused"} list the affected rule names. The same
#'   information, plus the detected changes, is stored with the object and can
#'   be retrieved with \code{\link{incremental_info}}.
#'
#' @examples
#' dat <- data.frame(id = 1:5, a = c(1,2,3,4,5), b = c(5,4,3,2,1))
#' rules <- validator(
#'     pos_a = a > 0
#'   , twice = d := 2*a, d < 100
#'   , pos_b = b > 0
#' )
#' previous <- confront(dat, rules, key = "id")
#'
#' # only 'a' changed: the rule using the derived variable 'd' is affected too
#' dat$a[2] <- -1
#' out <- revalidate(dat, rules, previous = previous, changed = "a")
#' incremental_info(out)
#' summary(out)
#'
#' @family confrontation-methods
#' @family validation-methods
#' @export
revalidate <- function(dat, x, previous, changed = NULL, ref, key = NULL
                     , ref_changed = NULL, ...){

  if (missing(previous) || is.null(previous)){
    if (missing(ref))
      return(confront(dat, x, key = key, ...))
    else
      return(confront(dat, x, ref, key = key, ...))
  }

  stopifnot(is.data.frame(dat))
  stopifnot(inherits(x, "expressionset"))
  stopifnot(inherits(previous, "confrontation"))

  opts <- x$clone_options(...)

  if (missing(ref)){
    data_env <- list2env(dat)
    data_env$. <- dat
    refv <- NULL
  } else if (is.environment(ref)){
    data_env <- namecheck(list2env(dat, parent = ref))
    data_env$. <- dat
    refv <- ref
  } else if (is.data.frame(ref)){
    env <- new.env()
    env$ref <- ref
    data_env <- namecheck(list2env(dat, parent = env))
    data_env$. <- dat
    refv <- ref
  } else if (is.list(ref)){
    env <- list2env(ref)
    data_env <- namecheck(list2env(dat, parent = env))
    data_env$. <- dat
    refv <- ref
  } else {
    stop("'ref' must be an environment, data.frame or list")
  }

  revalidate_work(dat = dat, x = x, previous = previous, data_env = data_env
    , key = key, ref = refv, changed = changed, ref_changed = ref_changed
    , opts = opts, call = match.call())
}


#' @rdname revalidate
#' @export
confront_incremental <- function(...) revalidate(...)


#' Inspect an incremental revalidation
#'
#' Report which rules were recomputed, which were reused, and which changes were
#' detected or assumed during the last \code{\link{revalidate}} call.
#'
#' @param x An object returned by \code{\link{revalidate}}.
#'
#' @return A \code{list} with elements \code{changed}, \code{ref_changed},
#'   \code{recomputed}, \code{reused}, \code{reordered} and
#'   \code{partially_reused}. For objects not created by \code{revalidate}
#'   all elements are \code{NULL}.
#'
#' @family validation-methods
#' @export
incremental_info <- function(x){
  if (inherits(x, "confrontation")) info <- x$._keys$incremental else info <- NULL
  list(
      changed          = if (is.null(info)) NULL else info$changed
    , ref_changed      = if (is.null(info)) NULL else info$ref_changed
    , recomputed       = if (is.null(info)) NULL else info$recomputed
    , reused           = if (is.null(info)) NULL else info$reused
    , reordered        = if (is.null(info)) NULL else info$reordered
    , partially_reused = if (is.null(info)) NULL else info$partially_reused
  )
}


## -----------------------------------------------------------------------------
## DEPENDENCY ANALYSIS
## -----------------------------------------------------------------------------

# Functions whose outcome for a record depends only on that record's own values.
# A rule is treated as record-*independent* only if every function it calls is
# on this list; any unknown function makes the rule record-dependent (never
# spliced across row subsets or reordered).
.elementwise_funs <- c(
    "+", "-", "*", "/", "^", "%%", "%/%", ":", "(", "!"
  , "==", "!=", "<", "<=", ">", ">=", "&", "|", "&&", "||"
  , "ifelse", "pmin", "pmax"
  , "abs", "sqrt", "exp", "expm1", "log", "log1p", "log2", "log10"
  , "round", "signif", "floor", "ceiling", "trunc", "sign"
  , "is.na", "is.nan", "is.finite", "is.infinite"
  , "is.numeric", "is.double", "is.integer", "is.character", "is.logical"
  , "is.factor", "is.null", "is.ordered"
  , "as.numeric", "as.double", "as.integer", "as.character", "as.logical"
  , "as.factor", "as.Date", "as.POSIXct", "as.POSIXlt"
  , "nchar", "nzchar", "grepl", "startsWith", "endsWith", "toupper", "tolower"
  , "trimws", "substr", "substring", "sub", "gsub", "sprintf", "format"
  , "formatC", "strtrim", "chartr", "enc2utf8"
  , "paste", "paste0"
  , "$"
  , "field_length", "number_format", "field_format", "in_range"
  , "contains_exactly", "contains_at_least", "contains_at_most"
  , "does_not_contain", "part_whole_relation"
)
# Note: '%vin%' (the expanded '%in%') is intentionally absent: its table
# argument may be a data column, which makes the outcome depend on other
# records. Such rules are therefore never spliced across a row subset.

# Symbols used as values (not as function names, field names behind '$' or '@',
# or package names behind '::'/':::').
dep_vars <- function(x){
  out <- character(0)
  walk <- function(e){
    if (is.symbol(e)){
      nm <- as.character(e)
      if (!nm %in% out) out <<- c(out, nm)
      return(invisible(NULL))
    }
    if (!is.call(e)) return(invisible(NULL))
    op <- e[[1]]
    if (is.symbol(op)){
      nm <- as.character(op)
      if (nm %in% c("::", ":::")) return(invisible(NULL))
      if (nm %in% c("$", "@")){ walk(e[[2]]); return(invisible(NULL)) }
    } else {
      walk(op)
    }
    if (length(e) > 1) for (k in 2:length(e)) walk(e[[k]])
    invisible(NULL)
  }
  walk(x)
  out
}

# Names of all functions called in an expression
called_funs <- function(x){
  out <- character(0)
  walk <- function(e){
    if (!is.call(e)) return(invisible(NULL))
    op <- e[[1]]
    if (is.symbol(op)) out <<- c(out, as.character(op)) else walk(op)
    if (length(e) > 1) for (k in 2:length(e)) walk(e[[k]])
    invisible(NULL)
  }
  walk(x)
  unique(out)
}

# A rule is record independent when the outcome for a record depends only on
# that record (and on external references), so that outcomes can be matched to
# records by row key.
is_record_independent <- function(call, data_vars = NULL){
  vars <- dep_vars(call)
  if ("." %in% vars) return(FALSE)
  funs <- called_funs(call)
  other <- setdiff(funs, .elementwise_funs)
  if (length(other) > 0){
    # '%vin%' (expanded '%in%') compares each value against the whole table.
    # It is element-wise only when that table is constant (a literal or a
    # reference object), not when it is (derived from) a data column.
    if (!all(other %in% "%vin%")) return(FALSE)
    if (!all(vin_tables_constant(call, data_vars))) return(FALSE)
  }
  TRUE
}

# For each '%vin%' call in an expression, determine whether its table argument
# is constant with respect to the data: a literal, a call of literals (such as
# c(1, 2, 3)), or a symbol that is not a column of the data.
vin_tables_constant <- function(call, data_vars){
  out <- logical(0)
  is_const <- function(e){
    if (is.null(e)) return(TRUE)
    if (is.numeric(e) || is.character(e) || is.logical(e)) return(TRUE)
    if (is.symbol(e)){
      return(is.null(data_vars) || !as.character(e) %in% data_vars)
    }
    if (is.call(e)){
      op <- e[[1]]
      # c(1, 2), -1, etc.: constant when all arguments are constant literals
      if (is.symbol(op) && as.character(op) %in% c("c", "-", "+", ":")){
        return(all(vapply(as.list(e)[-1], is_const, logical(1))))
      }
    }
    FALSE
  }
  walk <- function(e){
    if (!is.call(e)) return(invisible(NULL))
    if (is.symbol(e[[1]]) && identical(as.character(e[[1]]), "%vin%") &&
        length(e) == 3){
      out <<- c(out, is_const(e[[3]]))
    }
    if (length(e) > 1) for (k in 2:length(e)) walk(e[[k]])
    invisible(NULL)
  }
  walk(call)
  out
}

# Does an expanded rule use the data set as a whole?
uses_dot <- function(call) "." %in% dep_vars(call)

# Functions whose value depends only on the shape of the data set, not on the
# values in its cells.
.shape_funs <- c("nrow", "ncol", "dim", "length", "names", "colnames", "rownames")

# Classify how '.' is used: "none", "shape" (only via a shape function such as
# nrow(.)), or "data" (any other use).
dot_usage <- function(call){
  walk <- function(e){
    if (is.symbol(e)) return(if (as.character(e) == ".") "data" else "none")
    if (!is.call(e)) return("none")
    op <- e[[1]]
    if (is.symbol(op) && as.character(op) %in% .shape_funs) return("shape")
    res <- "none"
    if (!is.symbol(op)){
      r <- walk(op); if (r == "data") return("data") else if (r == "shape") res <- "shape"
    }
    if (length(e) > 1) for (k in 2:length(e)){
      r <- walk(e[[k]])
      if (r == "data") return("data") else if (r == "shape") res <- "shape"
    }
    res
  }
  walk(call)
}

# Determine whether an expanded rule expression must be re-evaluated.
#
# call              an expanded (assignment-free) call
# changed           names of changed data columns and reference objects
# ref_changed       names of changed reference objects
# data_vars         names of the columns of the current data (NULL: unknown)
# ref_vars          names of known reference objects
# structure_changed did the set of columns change?
# rows_changed      did the number of rows change?
rule_is_affected <- function(call, changed, ref_changed
                           , data_vars, ref_vars
                           , structure_changed = FALSE
                           , rows_changed = FALSE){
  vars <- dep_vars(call)

  if ("." %in% vars){
    du <- dot_usage(call)
    if (is.null(data_vars)){
      # without knowing the data shape we can not rule out a change
      return(TRUE)
    }
    # '.' used as a data set: any change in values or shape matters.
    # '.' used only through nrow()/dim()/...: only the shape matters.
    if (du == "data" && (structure_changed || rows_changed || length(changed) > 0))
      return(TRUE)
    if (du == "shape" && (structure_changed || rows_changed))
      return(TRUE)
  }

  # cross-record outcomes can change when the set of records changes
  if (rows_changed && !is_record_independent(call, data_vars))
    return(TRUE)

  for (v in setdiff(vars, ".")){
    known_data <- !is.null(data_vars) && v %in% data_vars
    known_ref  <- v %in% ref_vars
    if (known_data){
      if (v %in% changed) return(TRUE)
      # a data column that shadows a reference object is ambiguous
      if (known_ref && v %in% ref_changed) return(TRUE)
    } else if (known_ref){
      if (v %in% ref_changed) return(TRUE)
    } else if (!is.null(data_vars)){
      # neither a data column nor a known reference object: the symbol may
      # resolve to anything in the calling environment, so be conservative
      return(TRUE)
    } else {
      # unknown schema: rely on the supplied change information
      if (v %in% changed || v %in% ref_changed) return(TRUE)
    }
  }
  FALSE
}


#' Select the rules affected by changed fields
#'
#' Determine which rules of a rule set depend on one or more changed data
#' columns or external reference objects. Dependencies are computed on the
#' expanded expressions, so transient variables (\code{:=}), variable groups
#' (\code{\link{var_group}}), and rules that reference reference data are taken
#' into account.
#'
#' @param x A \code{\link{validator}} or \code{\link{indicator}}.
#' @param changed \code{[character]} names of changed fields.
#' @param ref_changed \code{[character]} names of changed reference objects.
#' @param data Optionally, the current \code{data.frame}. When given, symbols
#'   that are not columns of \code{data} and not in \code{ref} are treated as
#'   unknown and rules using them are conservatively marked as affected.
#' @param ref Optionally, the reference data (environment, \code{data.frame} or
#'   named \code{list}), used to learn the names of reference objects.
#' @param structure_changed \code{[logical]} Did the set of columns change?
#' @param rows_changed \code{[logical]} Did the set of records change?
#' @param expand \code{[logical]} When \code{TRUE}, return the names of the
#'   expanded rule expressions (the names used in a \code{\link{confrontation}}).
#'   When \code{FALSE}, return the names of the rules in \code{x}.
#' @param ... Passed to the expression expansion (see \code{\link{validate_extend}}).
#'
#' @return A \code{character} vector of rule names.
#'
#' @examples
#' rules <- validator(a > 0, d := 2*a, d < 10, b > 0)
#' affected_rules(rules, changed = "a")
#' affected_rules(rules, changed = "b")
#'
#' @family validation-methods
#' @export
affected_rules <- function(x, changed = character(0)
                         , ref_changed = character(0)
                         , data = NULL, ref = NULL
                         , structure_changed = FALSE, rows_changed = FALSE
                         , expand = TRUE, ...){
  stopifnot(inherits(x, "expressionset"))
  stopifnot(is.character(changed), is.character(ref_changed))

  # reference objects that are also columns of the data: the data column wins
  # (see confront methods), so such names only count as data changes
  ref_vars  <- setdiff(ref_names(ref), if (is.null(data)) character(0) else names(data))
  data_vars <- if (is.null(data)) NULL else names(data)

  calls <- x$exprs(expand_assignments = TRUE, ...)
  if (length(calls) == 0) return(character(0))

  affected <- vapply(calls, rule_is_affected, logical(1)
    , changed = changed, ref_changed = ref_changed
    , data_vars = data_vars, ref_vars = ref_vars
    , structure_changed = structure_changed, rows_changed = rows_changed)

  if (isTRUE(expand)) return(names(calls)[affected])

  rule_idx <- vapply(calls, function(cl){
    r <- attr(cl, "reference")
    if (is.null(r)) NA_integer_ else as.integer(r[1])
  }, integer(1))
  unique(names(x)[unique(rule_idx[affected])])
}


## -----------------------------------------------------------------------------
## CHANGE DETECTION
## -----------------------------------------------------------------------------

# Reference objects that are visible to the rules. Returns a named list mapping
# each name to the object, or NULL.
#
# For an environment, only the objects bound directly in it are considered
# reference data: its parents are part of the caller's scope (they hold e.g.
# the data, other helper variables) and are not intended as reference data.
# Any symbol that is not a data column and not captured here is treated
# conservatively by rule_is_affected() (always re-evaluated), so reference
# objects reached only through a parent environment still give correct results,
# just without reuse.
snapshot_ref <- function(ref){
  if (is.null(ref)) return(NULL)
  if (is.environment(ref)){
    nm <- ls(ref, all.names = FALSE)
    out <- lapply(nm, function(n)
      tryCatch(get(n, envir = ref, inherits = FALSE), error = function(e) NULL))
    return(setNames(out, nm))
  }
  if (is.data.frame(ref)){
    # a data.frame is passed to confront as 'ref', i.e. a single object named
    # 'ref' in the parent environment
    return(list(ref = ref))
  }
  if (is.list(ref)){
    # a named list is turned into an environment by confront, so each element
    # is visible under its name
    nm <- names(ref)
    if (is.null(nm)) nm <- rep("", length(ref))
    nm[nm == ""] <- sprintf("..%d", which(nm == ""))
    return(setNames(unname(ref), nm))
  }
  list(ref = ref)
}

# Names of all objects visible in an environment, used to discover reference
# object names without copying the objects.
ref_names <- function(ref){
  if (is.null(ref)) return(character(0))
  names(snapshot_ref(ref))
}

# Names of the reference objects that (potentially) changed between an old and
# a new snapshot. Objects that are identical are skipped cheaply by R.
changed_refs <- function(old, new){
  if (is.null(old) && is.null(new)) return(character(0))
  if (is.null(old) || is.null(new)) return(union(names(old), names(new)))
  all_nm <- union(names(old), names(new))
  changed <- vapply(all_nm, function(nm){
    !identical(old[[nm]], new[[nm]])
  }, logical(1))
  all_nm[changed]
}

# Names of the data columns whose values differ between two versions, compared
# only on the records present in both (matched by row key).
changed_columns <- function(dat, prev_dat, matched_new, matched_old, cols){
  common <- intersect(cols, names(prev_dat))
  common[ vapply(common, function(nm){
    a <- dat[[nm]][matched_new]
    b <- prev_dat[[nm]][matched_old]
    !identical(a, b)
  }, logical(1)) ]
}

# Metadata stored with every confrontation, so that a later revalidate can
# detect changed columns, rows and reference objects. See confront_work().


## -----------------------------------------------------------------------------
## ROW KEY MATCHING
## -----------------------------------------------------------------------------

# Encode key columns into one character string per row. The encoding includes
# the column type, and distinguishes NA from the string "NA".
key_strings <- function(keys){
  if (is.null(keys) || ncol(keys) == 0L) return(rep("", nrow(keys)))
  cols <- lapply(keys, function(col){
    if (is.factor(col)) col <- as.character(col)
    if (is.logical(col)){
      typ <- "lgl"; s <- ifelse(is.na(col), "", ifelse(col, "TRUE", "FALSE"))
      miss <- is.na(col)
    } else if (is.numeric(col)){
      typ <- "num"; miss <- is.na(col)
      s <- ifelse(miss, "", format(col, trim = TRUE, scientific = FALSE))
    } else {
      typ <- "chr"; miss <- is.na(col); s <- as.character(col)
      s[miss] <- ""
    }
    paste0(typ, "\002", ifelse(miss, "\001NA\001", paste0("\001", s, "\001")))
  })
  do.call(paste, c(cols, list(sep = "\r")))
}

# Match rows of the new data to rows of the previous confrontation.
# Returns 'm' (the index into the old rows, NA for new records) and 'usable'.
match_rows <- function(keys_new, keys_old, key_new, key_old){
  n_new <- nrow(keys_new); n_old <- nrow(keys_old)
  if (length(key_new) > 0 && !identical(key_new, key_old)){
    # the new key is not available in the previous keyset: no alignment
    return(list(m = rep(NA_integer_, n_new), usable = FALSE))
  }
  if (ncol(keys_new) == 0L){
    # no key: rows can only be aligned by position
    m <- if (n_new == n_old) seq_len(n_new) else rep(NA_integer_, n_new)
    return(list(m = m, usable = n_new == n_old))
  }

  # Fast path: an unchanged single key column in the same order needs no
  # encoding. identical() short-circuits on pointer equality, so this is O(1)
  # when the caller did not touch the key.
  if (ncol(keys_new) == 1L && n_new == n_old &&
      identical(keys_new[[1]], keys_old[[1]])){
    if (anyDuplicated(keys_old[[1]]))
      return(list(m = rep(NA_integer_, n_new), usable = FALSE))
    return(list(m = seq_len(n_new), usable = TRUE))
  }

  # For a single column, match on the column directly (match() handles the
  # common types natively); only multi-column keys need string encoding.
  if (ncol(keys_new) == 1L){
    x <- keys_new[[1]]; y <- keys_old[[1]]
    if (is.factor(y)) y <- as.character(y)
    if (is.factor(x)) x <- as.character(x)
    if (anyDuplicated(y) || anyDuplicated(x))
      return(list(m = rep(NA_integer_, n_new), usable = FALSE))
    return(list(m = match(x, y), usable = TRUE))
  }

  s_new <- key_strings(keys_new); s_old <- key_strings(keys_old)
  if (anyDuplicated(s_old) || anyDuplicated(s_new))
    return(list(m = rep(NA_integer_, n_new), usable = FALSE))
  list(m = match(s_new, s_old), usable = TRUE)
}

# Evaluate calls on a subset of rows, in an environment that mirrors
# 'data_env' but holds only the selected records.
execute_on_rows <- function(calls, data_env, opts, rows){
  df <- get(".", envir = data_env, inherits = FALSE)
  sub <- new.env(parent = parent.env(data_env))
  for (nm in setdiff(ls(data_env, all.names = TRUE), ".")){
    assign(nm, get(nm, envir = data_env, inherits = FALSE)[rows], envir = sub)
  }
  sub$. <- df[rows, , drop = FALSE]
  execute(calls, sub, opts)
}


## -----------------------------------------------------------------------------
## WORKER
## -----------------------------------------------------------------------------

revalidate_work <- function(dat, x, previous, data_env, key = NULL, ref = NULL
                          , changed = NULL, ref_changed = NULL, opts, call = NULL){

  cls        <- class(previous)[1]
  prev_calls <- previous$._calls
  prev_value <- previous$._value
  prev_warn  <- previous$._warn
  prev_error <- previous$._error
  prev_keys  <- previous$._keys
  prev_kset  <- prev_keys$keyset

  if (is.null(prev_kset)) prev_kset <- dat[NULL]
  key_old <- names(prev_kset)
  if (is.null(key)) key <- key_old
  if (is.null(changed)) changed <- character(0)

  n_old    <- nrow(prev_kset)
  cols_new <- names(dat)
  ref_new  <- snapshot_ref(ref)

  # ---- align records by key -------------------------------------------------
  # Done first: change detection compares values on matched records, so that
  # reordering or adding records does not make every column look changed.
  if (!all(key %in% cols_new) && length(key) > 0){
    keys_new <- dat[NULL]
    align <- list(m = rep(NA_integer_, nrow(dat)), usable = FALSE)
  } else {
    keys_new <- dat[key]
    align <- match_rows(keys_new, prev_kset, key, key_old)
  }
  m <- align$m
  new_rows <- which(is.na(m))
  missing_old <- if (align$usable) setdiff(seq_len(n_old), m) else integer(0)
  rows_changed <- length(new_rows) > 0 || length(missing_old) > 0

  # ---- what changed? --------------------------------------------------------
  cols_old <- prev_keys$columns
  ref_old  <- prev_keys$ref
  prev_dat <- prev_keys$data

  structure_changed <- is.null(cols_old) ||
    !identical(sort(cols_new), sort(cols_old))

  if (length(changed) == 0){
    if (structure_changed){
      # columns added or removed: they are all "changed" by definition
      changed <- union(cols_new, if (is.null(cols_old)) character(0) else cols_old)
    } else if (!is.null(prev_dat) && align$usable && any(!is.na(m))){
      # compare values only on records present in both versions
      matched_new <- which(!is.na(m))
      matched_old <- m[matched_new]
      changed <- changed_columns(dat, prev_dat, matched_new, matched_old, cols_new)
    } else {
      # no previous data snapshot or no alignment: assume all columns changed
      changed <- cols_new
    }
  }

  # ---- reference data changes ----------------------------------------------
  ref_vars <- union(names(ref_new), names(ref_old))
  if (is.null(ref_changed)) ref_changed <- changed_refs(ref_old, ref_new)
  # a changed name that is not a current data column but is a reference object
  ref_changed <- union(ref_changed, intersect(setdiff(changed, cols_new), ref_vars))

  if (is.null(ref) && !is.null(ref_old) && length(ref_old) > 0){
    warning("Reference data was used to create 'previous' but is not provided; "
          , "rules that depend on it are re-evaluated.", call. = FALSE)
  }

  # options that influence evaluation invalidate all previous results
  opts_changed <- is.null(prev_keys$options) || !identical(prev_keys$options, opts())

  # ---- expand the current rule set -----------------------------------------
  calls <- x$exprs(expand_assignments = TRUE
             , lin_eq_eps = opts('lin.eq.eps')
             , lin_ineq_eps = opts('lin.ineq.eps')
             , dat = data_env)
  nm_calls <- names(calls)
  n_call <- length(calls)

  # ---- dependency analysis -------------------------------------------------
  idx_prev <- match(nm_calls, names(prev_calls))
  same_expr <- vapply(seq_len(n_call), function(i){
    j <- idx_prev[i]
    !is.na(j) && identical(call2text(calls[[i]]), call2text(prev_calls[[j]]))
  }, logical(1))

  affected <- vapply(seq_len(n_call), function(i){
    if (opts_changed || !same_expr[i]) return(TRUE)
    rule_is_affected(calls[[i]]
      , changed = changed, ref_changed = ref_changed
      , data_vars = cols_new, ref_vars = ref_vars
      , structure_changed = structure_changed
      , rows_changed = rows_changed)
  }, logical(1))

  row_indep <- vapply(calls, is_record_independent, logical(1), data_vars = cols_new)
  # reference data is aligned with record order, so a reference-using rule can
  # only be reused when no record moved
  uses_ref <- vapply(calls, function(cl) any(dep_vars(cl) %in% ref_vars), logical(1))

  # ---- decide per rule how the previous result is reused -------------------
  # full_reuse[i] : previous vector is valid as-is
  # permute[i]    : previous vector is valid record-wise; reorder it with m
  # row_reuse[i]  : record-aligned; splice fresh values in for new records
  full_reuse <- logical(n_call)
  permute    <- logical(n_call)
  row_reuse  <- logical(n_call)
  identity_rows <- align$usable && identical(m, seq_len(nrow(dat))) &&
    nrow(dat) == n_old
  for (i in seq_len(n_call)){
    j <- idx_prev[i]
    if (is.na(j) || affected[i] || !same_expr[i]) next
    pv <- prev_value[[j]]
    if (is.null(pv)) next
    if (length(pv) != n_old){
      # data-set level result; safe because the dependency analysis flags
      # structure and row-set changes for such rules
      full_reuse[i] <- TRUE
    } else if (!align$usable){
      next                                  # can not map old rows to new rows
    } else if (uses_ref[i]){
      # reference data is aligned with the record order of the previous data,
      # so a reference-using rule can only be reused when nothing moved.
      if (identity_rows) full_reuse[i] <- TRUE
    } else if (row_indep[i]){
      if (length(new_rows) == 0){
        permute[i] <- TRUE
      } else {
        row_reuse[i] <- TRUE
      }
    }
    # cross-record rules without reference data: only reusable when nothing
    # moved (handled by the 'affected' logic above); otherwise recompute.
    else if (identity_rows){
      full_reuse[i] <- TRUE
    }
  }

  # ---- evaluate what must be recomputed ------------------------------------
  full_idx <- which(!full_reuse & !permute & !row_reuse)
  part_idx <- which(row_reuse)

  L_full <- if (length(full_idx) > 0)
              execute(calls[full_idx], data_env, opts)
            else list()
  L_part <- if (length(part_idx) > 0 && length(new_rows) > 0)
              execute_on_rows(calls[part_idx], data_env, opts, new_rows)
            else list()

  # ---- assemble results ----------------------------------------------------
  value <- vector("list", n_call)
  wrn   <- vector("list", n_call)
  err   <- vector("list", n_call)
  names(value) <- names(wrn) <- names(err) <- nm_calls
  recomputed <- character(0)
  partially  <- character(0)

  for (i in seq_len(n_call)){
    nm <- nm_calls[i]
    j  <- idx_prev[i]

    if (full_reuse[i]){
      value[i] <- list(prev_value[[j]])
      wrn[i]   <- list(prev_warn[[j]])
      err[i]   <- list(prev_error[[j]])
      next
    }

    if (permute[i]){
      # same records, possibly in a different order: reorder the outcome
      value[i] <- list(prev_value[[j]][m])
      wrn[i]   <- list(prev_warn[[j]])
      err[i]   <- list(prev_error[[j]])
      next
    }

    if (row_reuse[i]){
      part <- L_part[[ match(nm, names(L_part)) ]]
      ok <- !is.null(part[[1]]) && length(part[[1]]) == length(new_rows)
      if (ok){
        val <- prev_value[[j]][m]
        val[new_rows] <- part[[1]]
        value[i] <- list(val)
        # keep warnings raised while evaluating the reused rows and add those
        # raised for the newly evaluated rows
        wrn[i]   <- list(c(prev_warn[[j]], part[[2]]))
        err[i]   <- list(part[[3]])
        partially <- c(partially, nm)
        next
      }
      # fall back: evaluate this rule on all records
      one <- execute(calls[i], data_env, opts)[[1]]
      value[i] <- list(one[[1]]); wrn[i] <- list(one[[2]]); err[i] <- list(one[[3]])
      recomputed <- c(recomputed, nm)
      next
    }

    k <- match(nm, names(L_full))
    if (is.na(k)){
      one <- execute(calls[i], data_env, opts)[[1]]
      value[i] <- list(one[[1]]); wrn[i] <- list(one[[2]]); err[i] <- list(one[[3]])
    } else {
      value[i] <- list(L_full[[k]][[1]])
      wrn[i]   <- list(L_full[[k]][[2]])
      err[i]   <- list(L_full[[k]][[3]])
    }
    recomputed <- c(recomputed, nm)
  }

  reused <- nm_calls[full_reuse | permute]
  reordered <- nm_calls[permute]

  info <- list(
      changed          = changed
    , ref_changed      = ref_changed
    , recomputed       = recomputed
    , reused           = reused
    , reordered        = reordered
    , partially_reused = partially
  )

  keys_meta <- list(
      keyset   = keys_new
    , columns  = cols_new
    , data     = dat
    , ref      = ref_new
    , options  = opts()
    , nrow     = nrow(dat)
    , incremental = info
  )

  out <- new(cls
    , ._call = if (!is.null(call)) call else previous$._call
    , ._calls = calls
    , ._value = value
    , ._warn  = wrn
    , ._error = err
    , ._keys  = keys_meta
    , ._event = c(
          agent = sprintf("%s > %s %s.%s > validate %s"
                     , R.version[["platform"]]
                     , R.version[["language"]], R.version[["major"]], R.version[["minor"]]
                     , utils::packageVersion("validate"))
        , time    = format(Sys.time(), "%Y%m%dT%H%M%S%z")
        , actor   = Sys.info()[["user"]]
        , trigger = sprintf("incremental revalidation: %d recomputed, %d reused, %d partially reused"
                          , length(recomputed), length(reused), length(partially)))
    )

  attr(out, "recomputed")       <- recomputed
  attr(out, "reused")           <- reused
  attr(out, "reordered")        <- reordered
  attr(out, "partially_reused") <- partially
  attr(out, "changed")          <- changed
  attr(out, "ref_changed")      <- ref_changed
  out
}