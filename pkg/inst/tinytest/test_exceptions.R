### Temporary exceptions (waivers) ------------------------------------------

dat <- data.frame(
    supplier = c("A", "A", "B", "B")
  , turnover = c(10, -5, 10, -5)
  , stringsAsFactors = FALSE
)
rules <- validator(turnover >= 0)
cf <- confront(dat, rules, key = "supplier")

exc <- exceptions(supplier = "A", rule = "V1"
                , start = "2026-02-01", end = "2026-02-28"
                , basis = "supplier A system upgrade")

## registry structure ----
expect_true(inherits(exc, "exceptions"))
expect_true(is.data.frame(exc))
expect_equal(names(exc)[1:5], c("supplier", "rule", "start", "end", "basis"))
expect_equal(exc$decision, "exempt")
expect_true(inherits(exc$start, "POSIXct"))

## a registry can be built from a data.frame ----
exc2 <- exceptions(.data = data.frame(
    supplier = "A", rule = "V1"
  , start = "2026-02-01", end = "2026-02-28", basis = "upgrade"))
expect_equal(exc2$basis, "upgrade")

## incomplete registries are rejected ----
expect_error(exceptions(supplier = "A", rule = "V1"))
expect_error(exceptions(supplier = "A", rule = "V1"
  , start = "2026-02-01", end = "2026-01-01", basis = "x")) # start after end
expect_error(exceptions(supplier = "A", rule = "V1"
  , start = "2026-02-01", end = "2026-03-01", basis = "x", decision = "maybe"))

## validity window ----
expect_equal(in_force(exc, at = as.POSIXct("2026-02-15", tz = "UTC")), TRUE)
expect_equal(in_force(exc, at = as.POSIXct("2026-03-15", tz = "UTC")), FALSE)
expect_equal(in_force(exc, at = as.POSIXct("2026-01-31", tz = "UTC")), FALSE)

## during the window: only supplier A is waived ----
out <- apply_exceptions(cf, exc, at = as.POSIXct("2026-02-15", tz = "UTC"))
expect_true(inherits(out, "excepted_validation"))
expect_true(inherits(out, "validation"))
expect_equivalent(values(out), matrix(c(TRUE, TRUE, TRUE, FALSE), ncol = 1))
expect_equivalent(original_values(out)
  , matrix(c(TRUE, FALSE, TRUE, FALSE), ncol = 1))

## the original failure is retained and reported ----
rep <- exception_report(out)
expect_equal(nrow(rep), 1L)
expect_equal(rep$supplier, "A")
expect_equal(rep$rule, "V1")
expect_equal(rep$record, 2L)
expect_equal(rep$original, FALSE)
expect_equal(rep$effective, TRUE)
expect_equal(rep$basis, "supplier A system upgrade")

## after expiry the rule blocks again, while the history stays visible ----
out2 <- apply_exceptions(cf, exc, at = as.POSIXct("2026-03-15", tz = "UTC"))
expect_equivalent(values(out2), matrix(c(TRUE, FALSE, TRUE, FALSE), ncol = 1))
expect_equal(nrow(exception_report(out2)), 0L)
expect_equivalent(original_values(out2)
  , matrix(c(TRUE, FALSE, TRUE, FALSE), ncol = 1))

## historical report can be reproduced with the historical reference time ----
hist <- apply_exceptions(cf, exc, at = as.POSIXct("2026-02-15", tz = "UTC"))
expect_equal(values(hist), values(out))

## summary and data.frame expose effective and original counts ----
s <- summary(out)
expect_true(all(c("original_fails", "waived") %in% names(s)))
expect_equal(s$fails, 1L)
expect_equal(s$original_fails, 2L)
expect_equal(s$waived, 1L)

d <- as.data.frame(out)
expect_true(all(c("original_value", "waived", "basis") %in% names(d)))
expect_equal(sum(d$waived), 1L)
expect_equal(d$basis[d$waived], "supplier A system upgrade")
expect_true(all(d$original_value[!d$waived] == d$value[!d$waived]))

## conflicting approvals are detected before any computation ----
conflict <- exceptions(
    supplier = c("A", "A")
  , rule     = c("V1", "V1")
  , start    = c("2026-02-01", "2026-02-10")
  , end      = c("2026-02-28", "2026-03-05")
  , basis    = c("upgrade", "rollback")
  , decision = c("exempt", "enforce")
)
expect_error(check_exceptions(conflict), "Conflicting exceptions")
expect_error(apply_exceptions(cf, conflict, at = as.POSIXct("2026-02-15", tz = "UTC"))
           , "Conflicting exceptions")

## non-overlapping or agreeing approvals do not conflict ----
ok <- exceptions(
    supplier = c("A", "A")
  , rule     = c("V1", "V1")
  , start    = c("2026-02-01", "2026-03-01")
  , end      = c("2026-02-28", "2026-03-31")
  , basis    = c("first", "second")
  , decision = c("exempt", "enforce")
)
expect_silent(check_exceptions(ok))

## an 'enforce' approval re-imposes a rule that is otherwise exempted ----
dat2 <- data.frame(supplier = c("A", "A"), turnover = c(10, -5))
cf2 <- confront(dat2, rules, key = "supplier")
enforce <- exceptions(supplier = "A", rule = "V1"
  , start = "2026-02-01", end = "2026-02-28", basis = "rollback"
  , decision = "enforce")
# 'enforce' restores the original verdict, so a genuine failure stays a failure
expect_equivalent(values(apply_exceptions(cf2, enforce, at = as.POSIXct("2026-02-15", tz = "UTC")))
                , matrix(c(TRUE, FALSE), ncol = 1))

## exceptions can be restricted to a period column ----
dat3 <- data.frame(supplier = c("A", "A")
                 , period = c(2025L, 2026L)
                 , turnover = c(-1, -1))
cf3 <- confront(dat3, rules, key = c("supplier", "period"))
pout <- apply_exceptions(cf3, exceptions(supplier = "A", rule = "V1"
          , start = "2026-01-01", end = "2026-12-31", basis = "upgrade")
       , at = as.POSIXct("2026-06-01", tz = "UTC"), period = "period")
prep <- exception_report(pout)
expect_equal(nrow(prep), 1L)
expect_equal(prep$record, 2L)

## supplier attribution requires the key column ----
expect_error(apply_exceptions(confront(dat2, rules), exc)
           , "no key column 'supplier'")

## a registry loaded from file ----
tmp <- tempfile(fileext = ".yaml")
writeLines(c(
    "- supplier: A"
  , "  rule: V1"
  , "  start: 2026-02-01"
  , "  end: 2026-02-28"
  , "  basis: upgrade"), tmp)
fx <- exceptions(.file = tmp)
expect_equal(nrow(fx), 1L)
expect_equal(fx$supplier, "A")
expect_equal(in_force(fx, at = as.POSIXct("2026-02-15", tz = "UTC")), TRUE)

## an exception log survives subsetting ----
sub <- out[1]
expect_equal(nrow(exception_report(sub)), 1L)
expect_equivalent(values(sub, which = "original"), original_values(out))

## an empty registry is a no-op ----
out0 <- apply_exceptions(cf, empty_registry <- as_exceptions(data.frame())
       , at = as.POSIXct("2026-02-15", tz = "UTC"))
expect_equal(nrow(exception_report(out0)), 0L)
expect_equivalent(values(out0), values(cf))
