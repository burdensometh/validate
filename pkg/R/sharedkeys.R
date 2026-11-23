#' @include confrontation.R
{}

# SHARED KEY UNIQUENESS ACROSS DATA SHARDS -----------------------------------
#
# When a single report is delivered in separate files (shards) that are each
# validated independently, within-file checks such as is_unique() cannot
# detect key combinations that occur in more than one file. The functions
# below implement a shared, composite-key uniqueness check for such
# shard-wise processed data:
#
#   store <- key_store()                     # one store per load batch
#   register_keys(store, "part1.csv", keys)  # collect a shard's keys
#   shared_duplicates(store)                 # aggregate over all shards
#   cf <- flag_shared_keys(cf, store)        # flag duplicates in the results
#                                            # of each shard involved
#
# Each key store is fully independent, so different load batches never
# interfere with each other. Re-registering a shard replaces its keys, so
# after fixing a file the correct results can be regenerated.


#' Create a key store for shared key checks across data shards
#'
#' A key store collects the (composite) keys of data shards that together
#' make up a single report (one \emph{load batch}). After all shards are
#' registered with \code{\link{register_keys}}, key combinations that occur
#' in more than one shard can be found with \code{\link{shared_duplicates}}
#' and flagged in each shard's validation results with
#' \code{\link{flag_shared_keys}}.
#'
#' Every call to \code{key_store()} creates a new, fully independent store.
#' Use one store per load batch: shards of different batches never interfere
#' with each other. There is no global or package-level state, so re-running
#' a complete batch from scratch always starts with a clean slate.
#'
#' @return For \code{key_store}: an object of class \code{validate_key_store}.
#'
#' @seealso \code{\link{register_keys}}, \code{\link{shared_duplicates}},
#'   \code{\link{flag_shared_keys}}
#'
#' @examples
#' # A report of orders, delivered in two separate files.
#' part1 <- data.frame(order_id = c("A1","A2","A3"), amount = c(10,20,30))
#' part2 <- data.frame(order_id = c("A3","A4","A5"), amount = c(30,40,50))
#'
#' rules <- validator(amount >= 0, is_unique(order_id))
#'
#' # Within each file, order numbers are unique: both files pass.
#' cf1 <- confront(part1, rules, key="order_id")
#' cf2 <- confront(part2, rules, key="order_id")
#' all(cf1) # TRUE
#' all(cf2) # TRUE
#'
#' # Collect the keys of both shards in a single store (one load batch).
#' store <- key_store()
#' register_keys(store, "part1.csv", keyset(cf1))
#' register_keys(store, "part2.csv", keyset(cf2))
#'
#' # After aggregation, the cross-file duplicate is found...
#' shared_duplicates(store)
#'
#' # ...and flagged in the results of each file involved.
#' cf1 <- flag_shared_keys(cf1, store)
#' cf2 <- flag_shared_keys(cf2, store)
#' summary(cf1)
#' summary(cf2)
#'
#' @export
key_store <- function(){
  e <- new.env(parent=emptyenv())
  e$keys     <- list() # named list: shard id -> data.frame of key columns
  e$keynames <- NULL   # character vector of key column names
  class(e) <- "validate_key_store"
  e
}

#' @rdname key_store
#' @param x An object of class \code{validate_key_store}.
#' @param ... Ignored.
#' @export
print.validate_key_store <- function(x, ...){
  cat("Object of class 'validate_key_store'\n")
  keynames <- if (is.null(x$keynames)) "<none registered yet>" else paste(x$keynames, collapse=", ")
  cat(sprintf("Key columns        : %s\n", keynames))
  cat(sprintf("Shards registered  : %d\n", length(x$keys)))
  if (length(x$keys) > 0L){
    nrec <- vapply(x$keys, nrow, integer(1))
    cat(sprintf("Records per shard  : %s\n"
      , paste(sprintf("%s=%d", names(x$keys), nrec), collapse=", ")))
  }
  invisible(x)
}


# Coerce user input to a data frame of key columns.
# Accepts a data.frame, a named list of vectors, or a confrontation object
# (from which the key set is extracted).
as_keyset <- function(keys){
  if (inherits(keys, "confrontation")){
    keys <- keyset(keys)
    if (is.null(keys)){
      stop("Confrontation object has no key set. Use confront(..., key=...) first."
         , call.=FALSE)
    }
  }
  if (is.list(keys) && !is.data.frame(keys)){
    keys <- as.data.frame(keys, stringsAsFactors=FALSE)
  }
  if (!is.data.frame(keys)){
    stop("`keys` must be a data frame, a named list of vectors, or a confrontation object"
       , call.=FALSE)
  }
  if (ncol(keys) < 1L){
    stop("`keys` must contain at least one key column", call.=FALSE)
  }
  if (is.null(names(keys)) || any(!nzchar(names(keys)))){
    stop("All key columns must be named", call.=FALSE)
  }
  as.data.frame(keys, stringsAsFactors=FALSE)
}


# check that 'store' is a key store
check_key_store <- function(store){
  if (!inherits(store, "validate_key_store")){
    stop("`store` must be a key store, created with key_store()", call.=FALSE)
  }
}


#' Register, find and flag shared key combinations across data shards
#'
#' These functions operate on a \code{\link{key_store}}. They support the
#' situation where a single report is split into shards (files) that are
#' each sent through validation separately. Within each shard, key
#' combinations may be unique (as checked e.g. with \code{\link{is_unique}}),
#' while the combination of all shards contains duplicates across files.
#'
#' \itemize{
#'  \item{\code{register_keys}} registers the (composite) keys of one shard
#'    in the store. Registering a shard id that already exists
#'    \emph{replaces} its keys, so after fixing a file it can be
#'    re-registered and correct results can be regenerated. All shards in a
#'    store must use the same key columns (the composite key definition of
#'    the report).}
#'  \item{\code{shared_duplicates}} aggregates over all registered shards and
#'    returns the key combinations that occur in more than one shard, in long
#'    format: one row per duplicated key combination per shard involved.
#'    Combinations that occur multiple times within a single shard only are
#'    not reported; those are caught by within-file checks such as
#'    \code{\link{is_unique}}.}
#'  \item{\code{in_shared_duplicates}} returns, for each record in \code{keys},
#'    \code{TRUE} if its key combination occurs in more than one registered
#'    shard.}
#'  \item{\code{flag_shared_keys}} appends a rule to a
#'    \code{\link[=confront]{validation}} object \code{cf} (created with
#'    \code{confront(..., key=...)}), that is \code{FALSE} for each record
#'    whose key combination occurs in more than one shard. This way the
#'    cross-file duplicates show up in the validation results of each file
#'    involved. If a rule with the same \code{name} was added before, it is
#'    replaced, so re-flagging after a shard was fixed and re-registered
#'    regenerates correct results.}
#' }
#'
#' @param store An object of class \code{validate_key_store}, created with
#'    \code{\link{key_store}}.
#' @param shard \code{[character]} length 1. Identifier of the shard,
#'    typically a file name.
#' @param keys A \code{data.frame} or named \code{list} of key column(s), or a
#'    \code{confrontation} object (from which the key set is extracted).
#'    Multiple columns are interpreted as a composite key.
#' @param cf A \code{validation} object, created with
#'    \code{\link{confront}} or \code{\link{check_that}} using the \code{key=}
#'    argument.
#' @param name \code{[character]} length 1. Name of the rule that is added to
#'    \code{cf} by \code{flag_shared_keys}.
#'
#' @return
#'   For \code{register_keys}: the store, invisibly. Note that the store is
#'   updated by reference, so the return value does not need to be assigned.
#'
#'   For \code{shared_duplicates}: a \code{data.frame} with the key columns
#'   and a column \code{shard}, holding one row per duplicated key
#'   combination per shard in which it occurs.
#'
#'   For \code{in_shared_duplicates}: a \code{logical} vector with one entry
#'   per record in \code{keys}.
#'
#'   For \code{flag_shared_keys}: the updated \code{validation} object
#'   \code{cf}, with the shared-key rule appended (or replaced).
#'
#' @seealso \code{\link{key_store}}, \code{\link{is_unique}}
#'
#' @examples
#' # Composite key: order lines are identified by (order_id, line_nr).
#' part1 <- data.frame( order_id = c("A1","A1","A2")
#'                    , line_nr  = c(1, 2, 1)
#'                    , amount   = c(10, 20, 30) )
#' part2 <- data.frame( order_id = c("A2","A3")
#'                    , line_nr  = c(1, 1)
#'                    , amount   = c(30, 40) )
#'
#' rules <- validator(amount >= 0, is_unique(order_id, line_nr))
#' cf1 <- confront(part1, rules, key=c("order_id","line_nr"))
#' cf2 <- confront(part2, rules, key=c("order_id","line_nr"))
#'
#' store <- key_store()
#' register_keys(store, "part1.csv", keyset(cf1))
#' register_keys(store, "part2.csv", keyset(cf2))
#'
#' # (A2, 1) occurs in both files.
#' shared_duplicates(store)
#'
#' # Flag the duplicates in each file's own validation results.
#' cf1 <- flag_shared_keys(cf1, store)
#' cf2 <- flag_shared_keys(cf2, store)
#' summary(cf1)
#'
#' # After fixing part2.csv, re-register and re-flag to regenerate results.
#' part2_fixed <- data.frame(order_id = "A3", line_nr = 1, amount = 40)
#' cf2 <- confront(part2_fixed, rules, key=c("order_id","line_nr"))
#' register_keys(store, "part2.csv", keyset(cf2))
#' shared_duplicates(store) # empty: no more cross-file duplicates
#' cf1 <- flag_shared_keys(cf1, store)
#' summary(cf1)
#'
#' @export
register_keys <- function(store, shard, keys){
  check_key_store(store)
  if (!is.character(shard) || length(shard) != 1L || is.na(shard) || !nzchar(shard)){
    stop("`shard` must be a single, non-empty character value", call.=FALSE)
  }
  keys <- as_keyset(keys)

  if (is.null(store$keynames)){
    store$keynames <- names(keys)
  } else if (!setequal(names(keys), store$keynames)){
    stop(sprintf("All shards in a key store must use the same key columns. Expected: %s"
       , paste(store$keynames, collapse=", ")), call.=FALSE)
  }
  # replace the shard's keys (idempotent re-registration after a fix)
  store$keys[[shard]] <- keys[, store$keynames, drop=FALSE]
  invisible(store)
}


# name of the column identifying the shard in aggregated output
shard_column_name <- function(keynames){
  if ("shard" %in% keynames) ".shard" else "shard"
}


# Long data frame with one row per (key combination, shard) pair, for key
# combinations that occur in more than one registered shard.
shared_dup_rows <- function(store){
  keynames  <- store$keynames
  shard_col <- shard_column_name(keynames)
  L <- lapply(names(store$keys), function(s){
    d <- store$keys[[s]]
    d[[shard_col]] <- rep(s, nrow(d))
    d
  })
  all_keys <- do.call(rbind, unname(L))
  # one row per unique (key combination, shard) pair, so duplicates that
  # occur within a single shard only are not counted as shared.
  u <- unique(all_keys)
  combo <- u[, keynames, drop=FALSE]
  dup <- duplicated(combo) | duplicated(combo, fromLast=TRUE)
  out <- u[dup, , drop=FALSE]
  rownames(out) <- NULL
  out
}


# Unique key combinations occurring in more than one registered shard.
shared_dup_combos <- function(store){
  dups <- shared_dup_rows(store)
  unique(dups[, store$keynames, drop=FALSE])
}


#' @rdname register_keys
#' @export
shared_duplicates <- function(store){
  check_key_store(store)
  if (is.null(store$keynames)){
    return(data.frame(shard=character(0), stringsAsFactors=FALSE))
  }
  out <- shared_dup_rows(store)
  keynames <- store$keynames
  out <- out[, c(keynames, shard_column_name(keynames)), drop=FALSE]
  if (nrow(out) > 0L){
    out <- out[do.call(order, out), , drop=FALSE]
  }
  rownames(out) <- NULL
  out
}


#' @rdname register_keys
#' @export
in_shared_duplicates <- function(store, keys){
  check_key_store(store)
  keys <- as_keyset(keys)
  if (nrow(keys) == 0L) return(logical(0))
  if (is.null(store$keynames)) return(rep(FALSE, nrow(keys)))

  missing_cols <- setdiff(store$keynames, names(keys))
  if (length(missing_cols) > 0L){
    stop(sprintf("`keys` is missing key column(s): %s"
       , paste(missing_cols, collapse=", ")), call.=FALSE)
  }
  combos <- shared_dup_combos(store)
  if (nrow(combos) == 0L) return(rep(FALSE, nrow(keys)))

  keys <- keys[, store$keynames, drop=FALSE]
  # a record is a shared duplicate if its key combination occurs among the
  # duplicated combinations (which are unique, and precede the records).
  both <- rbind(combos, keys)
  duplicated(both)[-seq_len(nrow(combos))]
}


#' @rdname register_keys
#' @export
flag_shared_keys <- function(cf, store, name="shared_key_unique"){
  if (!inherits(cf, "validation")){
    stop("`cf` must be a validation object, created with confront() or check_that()"
       , call.=FALSE)
  }
  check_key_store(store)
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)){
    stop("`name` must be a single, non-empty character value", call.=FALSE)
  }
  keys <- keyset(cf)
  if (is.null(keys)){
    stop(paste("`cf` has no key set. Use confront(dat, rules, key=...) so records"
             , "can be matched against the key store."), call.=FALSE)
  }
  keys <- as_keyset(keys)
  keynames <- if (is.null(store$keynames)) names(keys) else store$keynames
  missing_cols <- setdiff(keynames, names(keys))
  if (length(missing_cols) > 0L){
    stop(sprintf("Key set of `cf` is missing key column(s): %s"
       , paste(missing_cols, collapse=", ")), call.=FALSE)
  }

  # FALSE for each record whose key combination occurs in more than one shard
  value <- !in_shared_duplicates(store, keys[, keynames, drop=FALSE])

  # Append (or replace, if 'name' is already present) a synthetic rule so the
  # cross-file duplicates show up in this file's own validation results.
  expr <- as.call(c(quote(shared_key_unique), lapply(keynames, as.name)))
  cf$._value[[name]] <- value
  cf$._calls[[name]] <- expr
  cf$._warn[name]    <- list(NULL)
  cf$._error[name]   <- list(NULL)
  cf
}
