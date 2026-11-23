#' @importFrom utils capture.output
{}

# create reference object to store or ignore output
# of validation functions
output <- function(){
  e <- new.env(parent=globalenv())
  e$n     <- 0 # validating calls
  e$nrule <- 0 # rules executed
  e$nwarn <- 0 # warnings thrown
  e$nerrs <- 0 # errors thrown
  e$nfail <- 0 # items with fails
  e$nNA   <- 0 # items with NA


  re <- "^V[0-9]+"  # regex to retrieve validations
  e$add <- function(x){
    e$n <<- e$n + 1
    e[[sprintf("V%04d",e$n)]] <- x

    # Administration of some of the content
    e$nrule <- e$nrule + length(x)
    e$nwarn <- e$nwarn + confrontation_nwarn(x)
    e$nerrs <- e$nerrs + confrontation_nerrs(x)
    s <- summary(x)
    e$nfail <- e$nfail + sum(s[,"fails"])
    e$nNA   <- e$nNA   + sum(s[,"nNA"])
  }
  e$gimme <- function(){
    vr <- ls(e, pattern = re)
    lapply(vr, function(i) e[[i]])
  }
  e
}



capture <- function(fun, env){
  function(...){
    out <- fun(...)
    # Reconstruct the call that was captured
    out$._call <- match.call(fun)
    attr(out, "file")   <- env$file
    attr(out, "lines")  <- c(fst = env$fst, lst=env$lst)
    # Record where this confrontation occurred so the delivery can point back
    # to the exact source of its rules.
    prov <- out$._provenance
    if (is.null(prov)) prov <- list()
    prov$call_site <- list(file=env$file, line=env$fst, last_line=env$lst)
    out$._provenance <- prov

    env$add(out)
    out
  }
}


#' Run a file with confrontations. Capture results
#'
#' A validation script is a regular R script, intersperced with \code{confront}
#' or \code{check_that} statements. This function will run the script file
#' and capture all output from calls to these functions.
#'
#' The returned object records the identity of everything that determined the
#' outcome, so a completed delivery can be reviewed and reproduced: the rules
#' that were loaded (name, expression, and the file/line they came from), a
#' content fingerprint of the input data, and the reference data together with
#' the version it carries. See \code{\link{provenance}} for the record and
#' \code{\link{replay_validation}} to re-run a delivery after checking it
#' against a supplied version.
#'
#'
#' @param file \code{[character]} location of an R file.
#' @param verbose \code{[logical]} toggle verbose output.
#'
#' @return \code{run_validation_file}: An object of class \code{validations}. This is
#'   a \code{list} of objects of class \code{\link{validation}}. The recorded
#'   provenance is available via \code{\link{provenance}}.
#'
#' @family validations validation-files
#' @export
run_validation_file <- function(file, verbose=TRUE){

  catf <-  function(fmt, ...) if (verbose) cat(sprintf(fmt,...))

  # Absolute path, resolved before changing directory, so the delivery can be
  # located and replayed from any working directory.
  absfile <- normalizePath(file, mustWork=FALSE)
  dir <- dirname(file)
  oldwd <- getwd()
  on.exit(setwd(oldwd))

  setwd(dir)

  # environment to capture confrontations 
  o <- output()
  e <- new.env()
  e$confront   <- capture(confront, o)
  e$check_that <- capture(check_that, o)

  fname  <- basename(file)
  parsed <- parse(fname, keep.source=TRUE)
  src    <- attr(parsed, "srcref")

  prfile <- substr(fname, 1,24)
  prfile <- gsub(" ",".", x=sprintf("%-26s", prfile), fixed=TRUE)


  # Absolute path so the delivery can be located and replayed from any working
  # directory (the process temporarily chdirs into the script's directory).
  o$file <- absfile
  for ( i in seq_along(parsed) ){
    expr  <- parsed[[i]]
    o$fst <- src[[i]][1]
    o$lst <- src[[i]][3]
    o$expr <- expr
    out <- eval(expr, envir=e)
    # print status for quick overview.
    prefix <- sprintf("\rRunning %s %3d calls %3d rules", prfile,o$n, o$nrule)
    postfix <- ""
    if (o$nfail == 0 & o$nNA == 0){ 
      postfix <- "OK"
    } else { 
      if (o$nfail > 0) postfix <- sprintf("%d FAILS", o$nfail)
      if (o$nNA   > 0) postfix <- sprintf("%s %d NA", postfix, o$nNA)
      if (o$nerrs > 0) postfix <- sprintf("%s %d ERRORS",postfix, o$nerrs)
      if (o$nwarn > 0) postfix <- sprintf("%s %d WARNINGS",postfix, o$nwarn)
    }
    catf("%s %s",prefix,postfix)
  }
  catf("\n")

  as_validations(o$gimme(), call=match.call())
}

#' @param dir     \code{[character]} path to directory.
#' @param pattern \code{[characer]} regular expression that selects validation files to run.
#' 
#' 
#' 
#' @rdname run_validation_file
#' @return \code{run_validation_dir}: An object of class \code{validations}. This is 
#'   a \code{list} of objects of class \code{\link{validation}}.
#' @export
#' @family validation validation-files
run_validation_dir <- function(dir="./", pattern="^validate.+[rR]", verbose=TRUE){
  if (!dir.exists(dir)) stop(sprintf("%s\nnot found or is not a directory"))

  files <- dir(path=dir, pattern=pattern, full.names=TRUE)

  L <- list()
  for ( f in files ){
    L <- c(L, as.list(run_validation_file(f, verbose=verbose)))
  }

  as_validations(L, call=match.call())
}

#' @param x An R object
#' @param ... Unused
#' @export
#' @return \code{print}: \code{NULL}, invisibly.
#' @rdname run_validation_file
print.validations <- function(x,...){
  cat(sprintf("Object of class 'validations'\nCall:\n    "))
  print(attr(x,"call"))
  cat("\n")
  cat(sprintf("Confrontations: %d\n",sum(sapply(x,length))))
  cat(sprintf("With fails    : %d\n",sum(sapply(x, failed_confrontations)) ))
  cat(sprintf("Warnings      : %d\n", sum(sapply(x, function(y) length(warnings(y))  ))  ))
  cat(sprintf("Errors        : %d\n", sum(sapply(x, function(y) length(errors(y))  ))  ))
  prov <- attr(x, "provenance")
  if (!is.null(prov) && !is.null(prov$results)){
    cat(sprintf("Result fingerprint: %s\n", short_fp(prov$results$fingerprint)))
    miss <- prov$missing_reference_versions
    if (!is.null(miss) && nrow(miss) > 0){
      cat(sprintf("WARNING: %d reference element(s) without a recorded version\n", nrow(miss)))
    }
  }
  invisible(NULL)
}

#' @rdname run_validation_file
#' @param object An R object
#' @return \code{summary}: A data frame similar to the data frame returned
#'   when summarizing a \code{\link{validation}} object. There are extra columns listing
#'   each call, file and first and last line where the code occurred.
#'
#' @method summary validations
#' @export
summary.validations <- function(object, ...){
  L <- lapply(object, function(x){
    s <- summary(x)
    s$call <- capture.output(print(x$._call))
    s$file <- attr(x,"file")
    s$fst  <- attr(x,"lines")[1]
    s$lst <- attr(x, "lines")[2]
    s
  }) 
  do.call(rbind,L)
}








