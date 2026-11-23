#' @include confrontation.R
NULL


### COMPARING TWO VALIDATION RESULTS -----------------------------------------

#' Compare two validation results record by record
#'
#' Compare the results of two \code{\link{confront}}ations of (versions of)
#' a data set with validation rules. Results are aligned by **business key**
#' (one or more columns identifying a record) and by **rule identity** (the
#' rule name). The order of the rows in the underlying data is irrelevant:
#' records are matched on key values, not on row position.
#'
#' The status of a record under a rule is one of
#' \describe{
#'  \item{\code{pass}}{the rule evaluated to \code{TRUE}}
#'  \item{\code{block}}{the rule evaluated to \code{FALSE}}
#'  \item{\code{unverifiable}}{the rule evaluated to \code{NA}}
#'  \item{\code{error}}{the rule could not be evaluated}
#'  \item{\code{absent}}{the record or rule did not occur in that confrontation}
#' }
#'
#' Every combination of a business key occurring in at least one of the two
#' confrontations and a rule name occurring in at least one of them yields one
#' row in the output, with its \code{transition}:
#'
#' \itemize{
#'  \item{For a rule occurring in both confrontations with the **same
#'  expression**, the transition is \code{<status_old>_to_<status_new>}, for
#'  example \code{pass_to_block} (a record that used to pass but is now
#'  blocked) or \code{block_to_pass}.
#'  \item{A rule that only occurs in the new confrontation gets transition
#'  \code{rule_added} for every record; a rule that only occurs in the old
#'  one gets \code{rule_removed}.
#'  \item{A rule with the same name but a **changed expression** gets
#'  transition \code{rule_modified}: the rule name no longer identifies the
#'  same check, so its per-record results are not compared as a status
#'  transition. The individual statuses are still reported in
#'  \code{status_old} and \code{status_new}.
#'  \item{A record occurring in only one confrontation gets \code{absent} as
#'  the status on the other side (for example \code{absent_to_pass}).
#' }
#'
#' @param old A \code{\link{validation}} object, the baseline result of
#'   \code{\link{confront}}.
#' @param new A \code{\link{validation}} object, the new result of
#'   \code{\link{confront}}.
#' @param key Optional \code{character} vector with the name(s) of the
#'   business key column(s). The columns must be present in the keyset of
#'   both confrontations, i.e. the confrontations must have been created
#'   with \code{confront(dat, rules, key=...)}. If omitted, the key columns
#'   stored with the confrontations are used.
#'
#' @return A \code{data.frame} (class \code{validationComparison}) with one
#' row per business-key/rule combination. Columns:
#' \itemize{
#'  \item{the business key column(s), as character, in the order of \code{key}}
#'  \item{\code{rule}: name of the validation rule}
#'  \item{\code{status_old}: status under the old confrontation}
#'  \item{\code{status_new}: status under the new confrontation}
#'  \item{\code{transition}: the status transition (see Details)}
#' }
#' The rows are ordered by key (keys from \code{old} first, in their original
#' order, followed by keys only occurring in \code{new}) and then by rule
#' (rules from \code{old} first), so the output does not depend on the row
#' order of the input data.
#'
#' @family comparing
#' @family validation-methods
#'
#' @example ../examples/compare_validations.R
#'
#' @export
compare_validations <- function(old, new, key = NULL){

  if (!inherits(old, "validation") || !inherits(new, "validation")){
    stop("both 'old' and 'new' must be 'validation' objects (see ?confront)")
  }

  ks_old <- keyset(old)
  ks_new <- keyset(new)

  # resolve the business key ----
  if (is.null(key) || length(key) == 0){
    key <- intersect(names(ks_old), names(ks_new))
    if (length(key) == 0){
      stop(paste0("No business key available. Re-run confront() with a non-empty"
        , " key= argument, or pass key= explicitly."))
    }
  }
  missing_cols <- setdiff(key, union(names(ks_old), names(ks_new)))
  if (length(missing_cols) > 0){
    stop(sprintf("key column(s) not found in both confrontations: %s"
      , paste(missing_cols, collapse = ", ")))
  }

  # composite keys, independent of row order ----
  k_old <- compose_business_key(ks_old, key)
  k_new <- compose_business_key(ks_new, key)
  if (anyDuplicated(k_old) > 0){
    stop("duplicate business key values found in 'old'; a business key must be unique")
  }
  if (anyDuplicated(k_new) > 0){
    stop("duplicate business key values found in 'new'; a business key must be unique")
  }

  n_old <- length(k_old)
  n_new <- length(k_new)

  # full outer join on the composite key ----
  all_keys <- c(k_old, setdiff(k_new, k_old))
  i_old <- match(all_keys, k_old)  # NA where a key is absent in 'old'
  i_new <- match(all_keys, k_new)  # NA where a key is absent in 'new'

  # rule inventory and rule identity (name + expression) ----
  rules_old <- names(old$._calls)
  rules_new <- names(new$._calls)
  all_rules <- c(rules_old, setdiff(rules_new, rules_old))

  expr_old <- vapply(old$._calls, call2text, FUN.VALUE = character(1))
  expr_new <- vapply(new$._calls, call2text, FUN.VALUE = character(1))

  val_old <- values(old, simplify = FALSE, drop = FALSE)
  val_new <- values(new, simplify = FALSE, drop = FALSE)

  # key columns in the output (character, coalesced over both confrontations)
  out_keys <- coalesce_key_columns(ks_old, ks_new, key, i_old, i_new)

  rows <- vector("list", length(all_rules))
  skipped <- character(0)

  for ( r in seq_along(all_rules) ){
    rule <- all_rules[r]
    in_old <- rule %in% rules_old
    in_new <- rule %in% rules_new

    v_old <- if (in_old) val_old[[rule]] else NULL
    v_new <- if (in_new) val_new[[rule]] else NULL

    # rules whose result is not one value per record cannot be aligned
    if ((in_old && !is.null(v_old) && length(v_old) != n_old) ||
        (in_new && !is.null(v_new) && length(v_new) != n_new)){
      skipped <- c(skipped, rule)
      next
    }

    s_old <- if (in_old) confrontation_status(v_old, n_old) else rep("absent", n_old)
    s_new <- if (in_new) confrontation_status(v_new, n_new) else rep("absent", n_new)

    status_old <- pick_status(s_old, i_old)
    status_new <- pick_status(s_new, i_new)

    if (in_old && !in_new){
      transition <- rep("rule_removed", length(all_keys))
    } else if (!in_old && in_new){
      transition <- rep("rule_added", length(all_keys))
    } else if (!identical(unname(expr_old[rule]), unname(expr_new[rule]))){
      transition <- rep("rule_modified", length(all_keys))
    } else {
      transition <- paste0(status_old, "_to_", status_new)
    }

    rows[[r]] <- data.frame(
        out_keys
      , rule = rule
      , status_old = status_old
      , status_new = status_new
      , transition = transition
      , stringsAsFactors = FALSE
    )
  }

  if (length(skipped) > 0){
    warning(sprintf(paste0("Ignored rule(s) whose result is not one value per"
      , " record and therefore cannot be aligned by business key: %s")
      , paste(unique(skipped), collapse = ", ")))
  }

  out <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  if (is.null(out)){
    out <- data.frame(
        out_keys[0, , drop = FALSE]
      , rule = character(0)
      , status_old = character(0)
      , status_new = character(0)
      , transition = character(0)
      , stringsAsFactors = FALSE
    )
  }
  rownames(out) <- NULL
  class(out) <- c("validationComparison", "data.frame")
  out
}


#' @rdname compare
#' @section Comparing two validation objects:
#' The \code{compare} method for two \code{\link{validation}} objects is a
#' shortcut for \code{\link{compare_validations}}: it aligns the outcomes of
#' two confrontations by business key and rule. See
#' \code{\link{compare_validations}} for details.
#'
#' @param y A \code{\link{validation}} object (for the
#' \code{validation} method only).
#' @param key See \code{\link{compare_validations}}.
#' @export
setMethod("compare", "validation", function(x, y, key = NULL, ...){
  compare_validations(old = x, new = y, key = key)
})


# Build a single collision-resistant key string per record.
compose_business_key <- function(keyset, key){
  if (length(key) == 1){
    return(as.character(keyset[[key]]))
  }
  do.call(paste, c(
      lapply(key, function(k) as.character(keyset[[k]]))
    , list(sep = "\r")
  ))
}


# Translate a rule result vector into per-record status labels.
# A NULL value means the rule threw an error during confrontation.
confrontation_status <- function(v, n){
  if (is.null(v)) return(rep("error", n))
  s <- rep("unverifiable", n)
  s[!is.na(v) & v]  <- "pass"
  s[!is.na(v) & !v] <- "block"
  s
}


# Look up per-record statuses, labelling records absent from this side.
pick_status <- function(status, index){
  out <- status[index]
  out[is.na(index)] <- "absent"
  out
}


# Output key columns: values from 'old' where available, otherwise 'new'.
coalesce_key_columns <- function(ks_old, ks_new, key, i_old, i_new){
  n <- length(i_old)
  out <- as.data.frame(
      matrix(NA_character_, nrow = n, ncol = length(key))
    , stringsAsFactors = FALSE
  )
  names(out) <- key
  has_old <- !is.na(i_old)
  has_new <- !is.na(i_new)
  for ( j in seq_along(key) ){
    if (any(has_old)){
      out[has_old, j] <- as.character(ks_old[[key[j]]][i_old[has_old]])
    }
    if (any(has_new)){
      out[has_new, j] <- as.character(ks_new[[key[j]]][i_new[has_new]])
    }
  }
  out
}


#' Summarize a validation comparison
#'
#' Counts the number of record/rule combinations in each transition type.
#'
#' @param object A \code{validationComparison} object, as returned by
#'   \code{\link{compare_validations}}.
#' @param ... Ignored.
#'
#' @return A \code{data.frame} with columns \code{transition} and
#'   \code{count}, ordered by decreasing count.
#'
#' @family comparing
#' @export
summary.validationComparison <- function(object, ...){
  tab <- table(object$transition)
  out <- data.frame(
      transition = names(tab)
    , count = as.integer(tab)
    , stringsAsFactors = FALSE
  )
  out[order(-out$count, out$transition), , drop = FALSE]
}


#' @export
print.validationComparison <- function(x, ...){
  cat(sprintf("Validation comparison: %d record/rule transition(s)\n", nrow(x)))
  print.data.frame(x, ...)
  invisible(x)
}
