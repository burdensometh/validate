# PROVENANCE ------------------------------------------------------------------
#
# Validation deliveries (objects of class 'validations' produced by
# run_validation_file() / run_validation_dir()) are records of a batch of
# checks.  To be able to reproduce such a batch in a different shift, and to
# review *what* was checked, a delivery must carry the identity of
#
#   - the rules that were loaded (name, expression, origin),
#   - the data under scrutiny (content fingerprint, dimensions, columns),
#   - the reference data and the version it came from,
#   - a fingerprint of the result summary that can be recomputed.
#
# The functions below compute these identities. They use a content fingerprint
# (an MD5 over the serialized object) with a fixed serialization version so the
# same object yields the same identity across sessions and machines running the
# same R version. Timestamps are deliberately excluded from rule identities,
# since a rule's *content* is what matters for reproduction, not when it was
# constructed.

# Content fingerprint of an arbitrary R object.
#
# Uses serialization version 2 so the result does not depend on the default
# serialization format of the running R version. Returns NA_character_ when an
# object cannot be serialized (in which case it cannot be identified).
fingerprint <- function(x){
  f <- tempfile()
  on.exit(unlink(f), add=TRUE)
  out <- tryCatch({
    writeBin(serialize(x, NULL, version = 2L), f)
    unname(tools::md5sum(f))
  }, error = function(e) NA_character_)
  out
}

# A canonical, timestamp-free table describing a set of rules.
rule_table <- function(x){
  n <- length(x)
  if (n == 0){
    return(data.frame(
      name = character(0), expression = character(0), label = character(0)
      , description = character(0), origin = character(0)
      , stringsAsFactors = FALSE))
  }
  data.frame(
    name        = vapply(x$rules, function(r) if (length(r@name)) r@name[1] else "", "")
    , expression = vapply(x$rules, function(r) call2text(r@expr), "")
    , label       = vapply(x$rules, function(r) paste(r@label, collapse=""), "")
    , description = vapply(x$rules, function(r) paste(r@description, collapse=""), "")
    , origin      = vapply(x$rules, function(r) paste(r@origin, collapse=""), "")
    , stringsAsFactors = FALSE
    , row.names = NULL
  )
}

# Identity of a rule set. `created` and other volatile metadata are excluded so
# that the same rules loaded twice share an identity.
rules_identity <- function(x){
  tab <- rule_table(x)
  list(
    fingerprint = fingerprint(tab)
    , n         = nrow(tab)
    , table     = tab
  )
}

# Identity of the data under scrutiny. Only metadata is stored; the
# fingerprint captures the content.
data_identity <- function(dat, source=NULL){
  if (is.null(dat)) return(NULL)
  list(
    fingerprint = fingerprint(dat)
    , class     = class(dat)[1]
    , nrow      = NROW(dat)
    , ncol      = NCOL(dat)
    , columns   = if (!is.null(names(dat))) names(dat) else character(0)
    , source    = source
  )
}

# Metadata attributes that may identify the *version* of a reference object.
# We look for the commonly used spellings, plus provenance attributes set by
# the SDMX helpers of this package.
VERSION_FIELDS    <- c("version", "ver", "release", "asof", "as_of", "snapshot")
PROVENANCE_FIELDS <- c("origin", "source", "url", "comment")

reference_meta <- function(x){
  a <- attributes(x)
  if (is.null(a)) return(list())
  out <- list()
  for (k in c(VERSION_FIELDS, PROVENANCE_FIELDS)){
    if (!is.null(a[[k]])) out[[k]] <- paste(as.character(a[[k]]), collapse=",")
  }
  out
}

# Extract a version string from a reference object, or NA when absent. Returns
# the field that carried the version so the caller can report what was checked.
reference_version <- function(x){
  m <- reference_meta(x)
  for (k in VERSION_FIELDS){
    if (!is.null(m[[k]]) && nzchar(m[[k]])){
      return(list(value = m[[k]], field = k))
    }
  }
  list(value = NA_character_, field = NA_character_)
}

# Where a reference object came from, when the object itself records it.
reference_origin <- function(x){
  m <- reference_meta(x)
  for (k in PROVENANCE_FIELDS){
    if (!is.null(m[[k]]) && nzchar(m[[k]])) return(m[[k]])
  }
  NA_character_
}

# Content identity of an object, ignoring the metadata attributes that record
# where it came from and which version it is. Those are checked separately
# (see reference_version()), so a reference with identical numbers but a
# different source annotation still has the same *content* identity.
content_fingerprint <- function(x){
  a <- attributes(x)
  strip <- intersect(names(a), c(VERSION_FIELDS, PROVENANCE_FIELDS))
  if (length(strip) > 0){
    attributes(x)[strip] <- NULL
  }
  fingerprint(x)
}

# Identity of reference data. Reference data may be a data.frame, a list of
# named reference objects, or an environment. Each element is identified
# separately so a missing version can be attributed to a specific source.
reference_identity <- function(ref, source=NULL){
  if (is.null(ref)) return(NULL)

  if (is.environment(ref)){
    nm  <- sort(ls(ref))
    els <- mget(nm, envir=ref, inherits=FALSE)
    kind <- "environment"
  } else if (is.data.frame(ref)){
    nm   <- "ref"
    els  <- list(ref)
    kind <- "data.frame"
  } else if (is.list(ref)){
    nm <- names(ref)
    if (is.null(nm)) nm <- sprintf("[[%d]]", seq_along(ref))
    els  <- ref
    kind <- "list"
  } else {
    nm   <- "ref"
    els  <- list(ref)
    kind <- class(ref)[1]
  }

  elements <- lapply(seq_along(els), function(i){
    x <- els[[i]]
    v <- reference_version(x)
    list(
      name        = nm[i]
      , class     = class(x)[1]
      , fingerprint = content_fingerprint(x)
      , version   = v$value
      , version_field = v$field
      , origin    = reference_origin(x)
      , source    = source
    )
  })

  list(
    kind        = kind
    , fingerprint = fingerprint(lapply(els, content_fingerprint))
    , elements  = elements
    , source    = source
  )
}

# Reference data may also be pulled in *inside* the rule expressions rather
# than through `ref=`. The SDMX helpers generate rules of the form
#   x %in% sdmx_codelist(endpoint=..., agency_id=..., resource_id=..., version=...)
# so the reference source and version live in the rule text. Detect such calls
# and report their source and version, so a delivery does not silently list no
# reference data when in fact a code list was consulted.
CODELIST_FUNS <- c("sdmx_codelist","estat_codelist","global_codelist")

# Formal argument order of each code-list helper, so positional arguments are
# attributed to the right field. These mirror the signatures in sdmx.R.
CODELIST_FORMALS <- list(
    sdmx_codelist   = c("endpoint","agency_id","resource_id","version")
  , estat_codelist  = c("resource_id","agency_id","version")
  , global_codelist = c("resource_id","agency_id","version")
)

find_codelist_call <- function(x){
  if (is.symbol(x) || !is.call(x)) return(list())
  out <- list()
  if (is.name(x[[1]]) && as.character(x[[1]]) %in% CODELIST_FUNS){
    fname <- as.character(x[[1]])
    args <- as.list(x)[-1]
    nm   <- names(args)
    if (is.null(nm)) nm <- rep("", length(args))
    # Match positional then named arguments.
    formal <- CODELIST_FORMALS[[fname]]
    vals <- list()
    pos <- 0L
    for (i in seq_along(args)){
      if (nzchar(nm[i])){
        vals[[nm[i]]] <- args[[i]]
      } else {
        pos <- pos + 1L
        if (pos <= length(formal)) vals[[formal[pos]]] <- args[[i]]
      }
    }
    getv <- function(k){
      v <- vals[[k]]
      if (is.null(v)) return(NA_character_)
      # Only literal constants are read back: the arguments generated by
      # validator_from_dsd() are string literals, and evaluating arbitrary
      # expressions while recording provenance could have side effects.
      if (is.character(v) || is.numeric(v) || is.logical(v)){
        return(paste(as.character(v), collapse=","))
      }
      if (is.call(v)) return(paste(deparse(v), collapse=" "))
      NA_character_
    }
    out[[length(out)+1L]] <- list(
      function_name = fname
      , resource_id = getv("resource_id")
      , agency_id   = getv("agency_id")
      , endpoint    = getv("endpoint")
      , version     = getv("version")
      , call        = call2text(x)
    )
  }
  for (i in seq_along(x)) out <- c(out, find_codelist_call(x[[i]]))
  out
}

# Collect code-list references from all rules of an expressionset.
codelist_references <- function(x){
  calls <- lapply(x$rules, function(r) find_codelist_call(r@expr))
  calls <- Filter(Negate(is.null), calls)
  if (length(calls) == 0) return(list())
  out <- unlist(Filter(function(z) length(z) > 0, calls), recursive=FALSE)
  # de-duplicate identical sources.
  keys <- vapply(out, function(z) paste(z$resource_id, z$version, z$endpoint, sep="|"), "")
  out[!duplicated(keys)]
}

# Build the provenance record stored on a single confrontation.
new_provenance <- function(x, input, ref, call_text){
  list(
    rules      = rules_identity(x)
    , input     = data_identity(input)
    , reference = reference_identity(ref, source=call_text)
    , embedded_references = codelist_references(x)
    , call      = call_text
    , call_site = NULL
    , package   = c(
        validate = as.character(utils::packageVersion("validate"))
        , R      = paste(R.version[["major"]], R.version[["minor"]], sep=".")
      )
    , event_time = format(Sys.time(), "%Y%m%dT%H%M%S%z")
  )
}


# ACCESSORS -------------------------------------------------------------------

#' Provenance of a validation delivery
#'
#' Retrieve the identity of the rules, input data and reference data that were
#' used when a \code{\link{validation}} or \code{\link{validations}} object was
#' created, together with a fingerprint of the result summary. The record makes
#' a completed validation self-describing, so that it can be reviewed and
#' reproduced.
#'
#' @param x An object of class \code{confrontation} (such as a
#'   \code{validation}) or \code{validations}.
#' @param ... Currently unused.
#'
#' @return For a \code{confrontation} object, the provenance list recorded at
#'   \code{\link{confront}} time. For a \code{validations} object, a
#'   \code{provenance} object with elements:
#'   \itemize{
#'     \item{\code{rules}: fingerprint, per-confrontation rule sets and a table
#'       of name, expression, origin and source line for every rule loaded.}
#'     \item{\code{inputs}: one row per confrontation with the input's class,
#'       dimensions, columns and content fingerprint.}
#'     \item{\code{references}: one row per reference element with its content
#'       fingerprint, detected version, version source and origin.}
#'     \item{\code{embedded_references}: code lists pulled in by rules (e.g.
#'       \code{sdmx_codelist}), with the version they were pinned to.}
#'     \item{\code{results}: a fingerprint over the rule-wise outcome counts,
#'       recomputable from a re-run.}
#'     \item{\code{scripts}, \code{files}: the validation scripts (re-run by
#'       replay) and every source file, each with a content fingerprint.}
#'     \item{\code{missing_reference_versions}: reference data (including code
#'       lists) whose version could not be determined, with its source.}
#'   }
#'
#' @family provenance
#' @export
provenance <- function(x, ...) UseMethod("provenance")

#' @rdname provenance
#' @export
provenance.confrontation <- function(x, ...){
  p <- x$._provenance
  if (length(p) == 0) NULL else p
}

#' @rdname provenance
#' @export
provenance.indication <- function(x, ...){
  p <- x$._provenance
  if (length(p) == 0) NULL else p
}

#' @rdname provenance
#' @export
provenance.validations <- function(x, ...){
  attr(x, "provenance")
}

#' @rdname provenance
#' @export
provenance.default <- function(x, ...){
  NULL
}


# Verifiable summary of a completed delivery. Uses the S4 summary methods of
# the confrontation objects (a plain list would dispatch to summary.default).
# The fingerprint covers only the rule-wise outcome counts, so it is stable
# across runs and sessions for an unchanged rule set and input.
result_summary <- function(vals){
  parts <- lapply(seq_along(vals), function(i){
    s <- summary(vals[[i]])
    cols <- intersect(c("name","items","passes","fails","nNA","error","warning"), names(s))
    s <- s[, cols, drop=FALSE]
    s$confrontation <- i
    s
  })
  counts <- if (length(parts)) do.call(rbind, parts) else
    data.frame(name=character(0), items=integer(0), passes=integer(0)
               , fails=integer(0), nNA=integer(0), error=logical(0)
               , warning=logical(0), confrontation=integer(0))
  list(
    fingerprint = fingerprint(counts)
    , counts    = counts
  )
}

# DELIVERY MANIFEST -----------------------------------------------------------

# Build the manifest of a completed validation delivery from its parts.
# `vals` is a list of confrontation objects, `call` the recorded call.
build_validations_provenance <- function(vals, call=NULL){
  provs <- lapply(vals, function(v) v$._provenance)
  # The recorded call is the one captured by run_validation_file (it replaces
  # the call captured inside confront), so take it from the final object.
  call_texts <- lapply(vals, function(v){
    if (is.null(v$._call)) "" else call2text(v$._call)
  })
  for (i in seq_along(provs)) if (!is.null(provs[[i]])) provs[[i]]$call <- call_texts[[i]]

  site_of <- function(p){
    if (is.null(p$call_site) || is.null(p$call_site$file)) return(NA_character_)
    sprintf("%s:%s", basename(p$call_site$file), p$call_site$line)
  }
  sites <- vapply(provs, site_of, "")

  # --- rules: which rules were loaded, and from where -------------------------
  rtabs <- lapply(seq_along(provs), function(i){
    t <- provs[[i]]$rules$table
    if (is.null(t) || nrow(t) == 0) return(NULL)
    t$confrontation <- i
    t$call          <- call_texts[[i]]
    t$source        <- sites[i]
    t
  })
  rtabs <- Filter(Negate(is.null), rtabs)
  rules_table <- if (length(rtabs)) do.call(rbind, rtabs) else NULL
  rules_sets  <- lapply(provs, function(p) p$rules$fingerprint)
  rules_fp    <- fingerprint(rules_sets)

  # --- inputs ----------------------------------------------------------------
  itabs <- lapply(seq_along(provs), function(i){
    d <- provs[[i]]$input
    if (is.null(d)) return(NULL)
    data.frame(
      confrontation = i
      , call        = call_texts[[i]]
      , source      = sites[i]
      , fingerprint = d$fingerprint
      , class       = d$class
      , nrow        = d$nrow
      , ncol        = d$ncol
      , columns     = paste(d$columns, collapse=",")
      , stringsAsFactors = FALSE
    )
  })
  itabs <- Filter(Negate(is.null), itabs)
  inputs <- if (length(itabs)) do.call(rbind, itabs) else NULL

  # --- references: identity + version, one row per element --------------------
  rtabs2 <- lapply(seq_along(provs), function(i){
    r <- provs[[i]]$reference
    if (is.null(r)) return(NULL)
    do.call(rbind, lapply(r$elements, function(e){
      data.frame(
        confrontation  = i
        , call         = call_texts[[i]]
        , source       = sites[i]
        , element      = e$name
        , class        = e$class
        , fingerprint  = e$fingerprint
        , version      = if (is.na(e$version)) "<missing>" else e$version
        , version_missing = is.na(e$version)
        , origin       = if (is.na(e$origin)) "" else e$origin
        , stringsAsFactors = FALSE
      )
    }))
  })
  rtabs2 <- Filter(Negate(is.null), rtabs2)
  references <- if (length(rtabs2)) do.call(rbind, rtabs2) else NULL
  missing_refs <- if (!is.null(references)) references[references$version_missing, , drop=FALSE] else NULL

  # --- code lists embedded in rule expressions (SDMX) -------------------------
  etabs <- lapply(seq_along(provs), function(i){
    er <- provs[[i]]$embedded_references
    if (is.null(er) || length(er) == 0) return(NULL)
    do.call(rbind, lapply(er, function(z){
      data.frame(
        confrontation = i
        , call        = call_texts[[i]]
        , source      = sites[i]
        , function_name = z$function_name
        , resource_id = z$resource_id
        , agency_id   = z$agency_id
        , endpoint    = z$endpoint
        , version     = if (is.na(z$version)) "<missing>" else z$version
        , version_missing = is.na(z$version)
        , stringsAsFactors = FALSE
      )
    }))
  })
  etabs <- Filter(Negate(is.null), etabs)
  embedded <- if (length(etabs)) do.call(rbind, etabs) else NULL
  # Code lists without a version are also reported, attributed to their rule.
  if (!is.null(embedded)){
    miss_emb <- embedded[embedded$version_missing, , drop=FALSE]
    if (nrow(miss_emb) > 0){
      miss_emb <- data.frame(
        confrontation = miss_emb$confrontation
        , call        = miss_emb$call
        , source      = miss_emb$source
        , element     = sprintf("%s(%s)", miss_emb$function_name, miss_emb$resource_id)
        , class       = "codelist"
        , fingerprint = NA_character_
        , version     = "<missing>"
        , version_missing = TRUE
        , origin      = ifelse(is.na(miss_emb$endpoint), "", miss_emb$endpoint)
        , stringsAsFactors = FALSE
      )
      missing_refs <- if (is.null(missing_refs)) miss_emb
                      else rbind(missing_refs, miss_emb)
    }
  }

  # --- verifiable result summary ---------------------------------------------
  results <- result_summary(vals)

  # --- source files: fingerprint so the rule source can be checked -----------
  # `scripts` are the validation scripts that replay re-runs, in order.
  # `files` additionally contains the rule files the rules were loaded from
  # (a rule's `origin`, e.g. a .yaml file), which are checked for changes but
  # are not executed themselves. Relative rule-file origins are resolved
  # against the directory of the validation script.
  script_dirs <- vapply(provs, function(p){
    f <- p$call_site$file
    if (is.null(f)) NA_character_ else dirname(f)
  }, "")
  scripts <- unique(unlist(lapply(provs, function(p){
    if (is.null(p$call_site$file)) NULL else p$call_site$file
  })))
  scripts <- scripts[!is.na(scripts)]
  origin_paths <- unlist(lapply(seq_along(provs), function(i){
    t <- provs[[i]]$rules$table
    if (is.null(t) || nrow(t) == 0) return(NULL)
    o <- unique(t$origin)
    o <- o[nzchar(o) & !is.na(o)]
    base <- script_dirs[i]
    cand <- if (!is.na(base)) file.path(base, o) else o
    cand[file.exists(cand)]
  }))
  file_paths <- unique(c(scripts, origin_paths))
  file_paths <- file_paths[!is.na(file_paths)]
  files <- lapply(file_paths, function(fp){
    list(
      path        = fp
      , role        = if (fp %in% scripts) "script" else "rules"
      , fingerprint = if (file.exists(fp)) fingerprint(readLines(fp, warn=FALSE)) else NA_character_
    )
  })

  structure(list(
    package                   = if (length(provs)) provs[[1]]$package else NULL
    , rules                   = list(fingerprint=rules_fp, sets=rules_sets, n=length(provs), table=rules_table)
    , inputs                  = inputs
    , references              = references
    , embedded_references     = embedded
    , results                 = results
    , scripts                 = scripts
    , files                   = files
    , missing_reference_versions = missing_refs
    , call                    = call
  ), class = "provenance")
}


# Assemble a 'validations' object from parts, recording its manifest.
as_validations <- function(vals, call=NULL){
  prov <- build_validations_provenance(vals, call=call)
  structure(vals, class=c("validations","list"), call=call, provenance=prov)
}


# PRINTING --------------------------------------------------------------------

# Shorten a fingerprint for display.
short_fp <- function(x){
  if (is.null(x) || is.na(x) || !nzchar(x)) return("<none>")
  substr(x, 1, 12)
}

#' @export
print.provenance <- function(x, ...){
  cat("Validation delivery provenance\n")
  if (!is.null(x$package)){
    cat(sprintf("  package : validate %s (R %s)\n", x$package[["validate"]], x$package[["R"]]))
  }
  cat(sprintf("  rules   : %d rule set(s), fingerprint %s\n",
              if (is.null(x$rules$table)) 0L else length(unique(x$rules$table$confrontation)),
              short_fp(x$rules$fingerprint)))
  if (!is.null(x$rules$table) && nrow(x$rules$table) > 0){
    tab <- x$rules$table
    for (i in seq_len(nrow(tab))){
      # `origin` is where the rule itself was defined (e.g. a .yaml rule file);
      # `source` is the script line that confronted the data.
      where <- tab$origin[i]
      if (!is.na(tab$source[i])) where <- sprintf("%s [%s]", where, tab$source[i])
      cat(sprintf("      [%d] %-8s %-30s from: %s\n"
        , tab$confrontation[i], tab$name[i]
        , substr(tab$expression[i], 1, 30)
        , where))
    }
  }

  cat("  input   :\n")
  if (is.null(x$inputs) || nrow(x$inputs) == 0){
    cat("      <none recorded>\n")
  } else {
    for (i in seq_len(nrow(x$inputs))){
      d <- x$inputs[i,]
      cat(sprintf("      [%d] %s  %d x %d  fp %s  (%s)\n"
        , d$confrontation, ifelse(is.na(d$source), d$call, d$source)
        , d$nrow, d$ncol, short_fp(d$fingerprint)
        , substr(d$columns, 1, 40)))
    }
  }

  cat("  reference:\n")
  if (is.null(x$references) || nrow(x$references) == 0){
    cat("      <none>\n")
  } else {
    for (i in seq_len(nrow(x$references))){
      d <- x$references[i,]
      cat(sprintf("      [%d] element '%s'  version: %s  fp %s  (%s)\n"
        , d$confrontation, d$element
        , ifelse(d$version_missing, "MISSING", d$version)
        , short_fp(d$fingerprint)
        , ifelse(is.na(d$source), d$call, d$source)))
    }
  }

  if (!is.null(x$embedded_references) && nrow(x$embedded_references) > 0){
    cat("  reference (in rules):\n")
    for (i in seq_len(nrow(x$embedded_references))){
      d <- x$embedded_references[i,]
      cat(sprintf("      [%d] %s(\"%s\")  version: %s  (%s)\n"
        , d$confrontation, d$function_name, d$resource_id
        , ifelse(d$version_missing, "MISSING", d$version)
        , ifelse(is.na(d$source), d$call, d$source)))
    }
  }

  cat(sprintf("  results : fingerprint %s (%d rule result(s))\n"
    , short_fp(x$results$fingerprint)
    , if (is.null(x$results$counts)) 0L else nrow(x$results$counts)))

  miss <- x$missing_reference_versions
  if (!is.null(miss) && nrow(miss) > 0){
    cat("  Reference data without a version:\n")
    for (i in seq_len(nrow(miss))){
      cat(sprintf("      - element '%s' from %s (%s): no %s attribute\n"
        , miss$element[i]
        , ifelse(is.na(miss$source[i]), miss$call[i], miss$source[i])
        , miss$call[i]
        , paste(c(VERSION_FIELDS, PROVENANCE_FIELDS), collapse="/")))
    }
  }
  invisible(x)
}


# VERIFICATION AND REPLAY -----------------------------------------------------

# check_mismatch is the shared policy handler for the verify/replay functions.
check_mismatch <- function(ok, msg, policy){
  if (isTRUE(ok)) return(invisible(TRUE))
  switch(policy
    , stop   = stop(msg, call.=FALSE)
    , warn   = warning(msg, call.=FALSE)
    , ignore = invisible(FALSE)
  )
  invisible(FALSE)
}

#' Verify a delivered validation against supplied versions
#'
#' Compare the identity of rules, input data, and/or reference data against the
#' identity recorded in a completed validation. This is the "check the version
#' first" step used by \code{\link{replay_validation}}: a batch may only be
#' replayed with the same rules and the same reference version.
#'
#' @param prov A \code{provenance} object as returned by \code{\link{provenance}}.
#' @param rules Optional: a \code{validator} or path to a \code{.yaml} rule file.
#' @param data Optional: the data under scrutiny.
#' @param reference Optional: the reference data (data.frame, list, or environment).
#' @param what Which identities to verify: any of \code{"rules"}, \code{"input"},
#'   \code{"reference"}.
#' @param on_mismatch What to do on a mismatch: \code{"stop"} (default),
#'   \code{"warn"}, or \code{"ignore"}.
#'
#' @return A \code{data.frame} with one row per checked identity, with columns
#'   \code{item}, \code{expected}, \code{actual} and \code{match}.
#'
#' @family provenance
#' @export
verify_provenance <- function(prov, rules=NULL, data=NULL, reference=NULL
  , what=c("rules","input","reference")
  , on_mismatch=c("stop","warn","ignore"))
{
  if (!inherits(prov, "provenance"))
    stop("'prov' must be a 'provenance' object (see ?provenance)", call.=FALSE)
  what <- match.arg(what, several.ok=TRUE)
  on_mismatch <- match.arg(on_mismatch)

  rows <- list()
  if ("rules" %in% what && !is.null(rules)){
    id <- if (inherits(rules, "expressionset")){
      rules_identity(rules)
    } else if (is.character(rules) && length(rules) == 1 && file.exists(rules)){
      rules_identity(validator(.file=rules))
    } else {
      stop("'rules' must be a validator or a path to a .yaml rule file", call.=FALSE)
    }
    exp <- if (is.null(prov$rules$sets)) prov$rules$fingerprint else prov$rules$sets
    ok  <- id$fingerprint %in% exp
    rows$rules <- data.frame(
      item = "rules", expected = paste(unique(exp), collapse="|")
      , actual = id$fingerprint
      , match = ok
      , detail = if (ok) "ok" else "content differs"
      , stringsAsFactors = FALSE)
  }
  if ("input" %in% what && !is.null(data)){
    id  <- data_identity(data)
    exp <- if (is.null(prov$inputs)) character(0) else unique(prov$inputs$fingerprint)
    ok  <- id$fingerprint %in% exp
    rows$input <- data.frame(
      item = "input", expected = paste(exp, collapse="|")
      , actual = id$fingerprint
      , match = ok
      , detail = if (ok) "ok" else "content differs"
      , stringsAsFactors = FALSE)
  }
  if ("reference" %in% what && !is.null(reference)){
    id <- reference_identity(reference)
    deliv <- prov$references
    # Compare element-by-element: the manifest stores one row per reference
    # element (e.g. each item of a named list), each with its own fingerprint.
    supplied <- id$elements
    supplied_fp   <- vapply(supplied, function(e) e$fingerprint, "")
    supplied_name <- vapply(supplied, function(e) e$name, "")
    supplied_ver  <- vapply(supplied, function(e) if (is.na(e$version)) "" else e$version, "")
    if (is.null(deliv) || nrow(deliv) == 0){
      fp_ok  <- FALSE
      ver_ok <- FALSE
      exp    <- character(0)
      note   <- "delivery recorded no reference"
    } else {
      exp   <- unique(paste(deliv$element, deliv$fingerprint, sep="="))
      fp_ok <- all(vapply(seq_len(nrow(deliv)), function(i){
        j <- which(supplied_name == deliv$element[i])
        length(j) > 0 && any(supplied_fp[j] == deliv$fingerprint[i])
      }, logical(1)))
      # A delivered reference *with* a version must be supplied with the same
      # version: matching content alone is not sufficient.
      with_version <- !deliv$version_missing
      ver_ok <- if (any(with_version)){
        all(vapply(which(with_version), function(i){
          j <- which(supplied_name == deliv$element[i])
          length(j) > 0 && any(supplied_ver[j] == deliv$version[i])
        }, logical(1)))
      } else TRUE
      note <- if (!fp_ok) "content differs"
              else if (!ver_ok) "version differs"
              else "ok"
    }
    rows$reference <- data.frame(
      item = "reference", expected = paste(exp, collapse="|")
      , actual = paste(supplied_name, supplied_fp, sep="=", collapse="|")
      , match = fp_ok && ver_ok
      , detail = note
      , stringsAsFactors = FALSE)
  }

  out <- if (length(rows)) do.call(rbind, rows)
         else data.frame(item=character(0), expected=character(0)
                         , actual=character(0), match=logical(0)
                         , detail=character(0)
                         , stringsAsFactors=FALSE)
  bad <- out[!out$match, , drop=FALSE]
  if (nrow(bad) > 0){
    lines <- vapply(seq_len(nrow(bad)), function(i){
      if (identical(bad$detail[i], "version differs")){
        sprintf("%s: version differs (content fingerprint matches)", bad$item[i])
      } else if (identical(bad$detail[i], "content differs")){
        sprintf("%s: content differs (expected %s, got %s)"
                , bad$item[i], bad$expected[i], bad$actual[i])
      } else {
        sprintf("%s: %s", bad$item[i], bad$detail[i])
      }
    }, "")
    msg <- paste0("Version check failed. ", paste(lines, collapse="; "))
    check_mismatch(FALSE, msg, on_mismatch)
  }
  out
}

# Compare the manifests of two runs.
compare_provenance <- function(old, new){
  if (!inherits(old, "provenance") || !inherits(new, "provenance"))
    stop("both arguments must be 'provenance' objects", call.=FALSE)
  # Use the identities recorded in each manifest (not a fingerprint of the
  # manifest data frames, which also carry volatile columns such as timing).
  input_fp <- function(p){
    if (is.null(p$inputs) || nrow(p$inputs) == 0) return("<none>")
    paste(unique(p$inputs$fingerprint), collapse="|")
  }
  ref_fp <- function(p){
    parts <- character(0)
    if (!is.null(p$references) && nrow(p$references) > 0){
      parts <- paste(p$references$element, p$references$fingerprint
                     , p$references$version, sep="=")
    }
    if (!is.null(p$embedded_references) && nrow(p$embedded_references) > 0){
      e <- p$embedded_references
      parts <- c(parts, paste("codelist", e$resource_id, e$version, sep="="))
    }
    if (length(parts) == 0) return("<none>")
    paste(unique(parts), collapse="|")
  }
  out <- data.frame(
    item     = c("rules", "input", "reference", "results")
    , expected = c(old$rules$fingerprint, input_fp(old), ref_fp(old), old$results$fingerprint)
    , actual   = c(new$rules$fingerprint, input_fp(new), ref_fp(new), new$results$fingerprint)
    , stringsAsFactors = FALSE
  )
  out$match <- mapply(identical, out$expected, out$actual)
  rownames(out) <- NULL
  structure(out, class=c("provenance_comparison", "data.frame"))
}

#' @export
print.provenance_comparison <- function(x, ...){
  cat("Replay comparison (delivered vs. re-run)\n")
  for (i in seq_len(nrow(x))){
    cat(sprintf("  %-10s %s\n", x$item[i], ifelse(x$match[i], "identical", "DIFFERENT")))
  }
  invisible(x)
}

# Verify that the recorded rule source files are unchanged.
verify_files <- function(prov, on_mismatch){
  files <- prov$files
  if (is.null(files) || length(files) == 0)
    return(data.frame(path=character(0), expected=character(0)
                      , actual=character(0), match=logical(0)
                      , stringsAsFactors=FALSE))
  rows <- lapply(files, function(f){
    present <- file.exists(f$path)
    actual  <- if (present) fingerprint(readLines(f$path, warn=FALSE)) else NA_character_
    data.frame(
      path = f$path, expected = f$fingerprint, actual = actual
      , match = present && identical(f$fingerprint, actual)
      , stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  bad <- out[!out$match, , drop=FALSE]
  if (nrow(bad) > 0){
    msg <- sprintf("Rule source changed since delivery: %s"
      , paste(bad$path, collapse=", "))
    check_mismatch(FALSE, msg, on_mismatch)
  }
  out
}

#' Replay a completed validation
#'
#' Re-run the source files of a completed validation *after* checking that the
#' rules (and any supplied input or reference data) are the same version that
#' was used when the delivery was made. Only when the version check passes are
#' the same rules executed again. The result carries both the original checks
#' and a comparison of the new run against the delivered result summary.
#'
#' When the delivered manifest contains reference data without a version, the
#' specific source of each such reference is reported before replaying.
#'
#' @param x A \code{validations} object produced by
#'   \code{\link{run_validation_file}} or \code{\link{run_validation_dir}}.
#' @param rules Optional validator or path to a rule file to check against the
#'   delivered rule identity.
#' @param data Optional data under scrutiny to check against the delivered
#'   input identity.
#' @param reference Optional reference data to check against the delivered
#'   reference identity and version.
#' @param what Which identities to verify before replaying.
#' @param on_mismatch What to do when a version check fails: \code{"stop"}
#'   (default), \code{"warn"}, or \code{"ignore"}.
#' @param verbose Print progress and the comparison.
#' @param ... Currently unused.
#'
#' @return A new \code{validations} object with a \code{"replay"} attribute
#'   holding the file check, the object check and the comparison.
#'
#' @family provenance
#' @export
replay_validation <- function(x, rules=NULL, data=NULL, reference=NULL
  , what=c("rules","input","reference")
  , on_mismatch=c("stop","warn","ignore")
  , verbose=TRUE, ...)
{
  if (!inherits(x, "validations"))
    stop("'x' must be a 'validations' object created by run_validation_file() or run_validation_dir()", call.=FALSE)
  prov <- provenance(x)
  if (is.null(prov))
    stop("This delivery has no recorded provenance and cannot be replayed safely. Re-create it with this version of 'validate'.", call.=FALSE)
  what <- match.arg(what, several.ok=TRUE)
  on_mismatch <- match.arg(on_mismatch)

  # Point out reference data whose version could not be determined.
  miss <- prov$missing_reference_versions
  if (!is.null(miss) && nrow(miss) > 0){
    msgf("Reference data without a recorded version in this delivery:")
    for (i in seq_len(nrow(miss))){
      if (identical(miss$class[i], "codelist")){
        msgf("  - code list %s pulled in by a rule from %s (call: %s): version is missing"
          , miss$element[i]
          , ifelse(is.na(miss$source[i]), "<unknown>", miss$source[i])
          , miss$call[i])
      } else {
        msgf("  - element '%s' from %s (call: %s): no %s attribute"
          , miss$element[i]
          , ifelse(is.na(miss$source[i]), "<unknown>", miss$source[i])
          , miss$call[i]
          , paste(VERSION_FIELDS, collapse="/"))
      }
    }
  }
  # A code list pinned to a moving target cannot be reproduced.
  emb <- prov$embedded_references
  if (!is.null(emb) && nrow(emb) > 0){
    latest <- emb[!emb$version_missing & emb$version == "latest", , drop=FALSE]
    if (nrow(latest) > 0){
      for (i in seq_len(nrow(latest))){
        msgf("  - code list %s(%s) uses version 'latest' from %s; it may change between runs"
          , latest$function_name[i], latest$resource_id[i]
          , ifelse(is.na(latest$source[i]), "<unknown>", latest$source[i]))
      }
    }
  }

  # STEP 1: verify versions *before* running.
  file_report <- verify_files(prov, on_mismatch=on_mismatch)
  obj_report  <- verify_provenance(prov, rules=rules, data=data, reference=reference
                                   , what=what, on_mismatch=on_mismatch)

  # STEP 2: run the same rules.
  scripts <- prov$scripts
  if (is.null(scripts) || length(scripts) == 0)
    scripts <- vapply(prov$files, function(f) f$path, "")
  if (length(scripts) == 0)
    stop("No source file recorded in the delivery; cannot replay.", call.=FALSE)
  new <- if (length(scripts) == 1){
    run_validation_file(scripts, verbose=verbose)
  } else {
    parts <- lapply(scripts, run_validation_file, verbose=verbose)
    as_validations(do.call(c, lapply(parts, as.list)), call=attr(x, "call"))
  }

  # STEP 3: compare the re-run with the delivery.
  cmp <- compare_provenance(prov, provenance(new))
  # rules/input/reference must be identical for the result to be comparable.
  core <- cmp[cmp$item %in% c("rules","input","reference"), , drop=FALSE]
  if (any(!core$match)){
    msg <- sprintf("Replay produced different %s than the delivery; results are not comparable."
                   , paste(core$item[!core$match], collapse=", "))
    check_mismatch(FALSE, msg, on_mismatch)
  }
  attr(new, "replay") <- list(files=file_report, objects=obj_report, comparison=cmp)
  if (verbose) print(cmp)
  new
}
