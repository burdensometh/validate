
## partition_decision: basic counting and decisions ----
# These tests rely on confront() catching rule errors. Other test files may
# leave a different global 'raise' option behind, so set it and restore it.
old_raise <- voptions()$raise
voptions(raise = "none")

dat <- data.frame(
  region = c(rep("A", 4), rep("B", 2), "C")
  , x = c(1, 2, -1, NA, 5, -2, 7)
  , y = 1
)
cf <- check_that(dat, x > 0, y > 0)
d <- partition_decision(cf, dat$region, max_fail_ratio = 0.2, min_effective = 3)

expect_equal(d$partition,  c("A", "B", "C"))
expect_equal(d$nrecords,   c(4L, 2L, 1L))
expect_equal(d$npass,      c(6L, 3L, 2L))
expect_equal(d$nfail,      c(1L, 1L, 0L))
expect_equal(d$nNA,        c(1L, 0L, 0L))
expect_equal(d$nerror,     c(0L, 0L, 0L))
expect_equal(d$neffective, c(7L, 4L, 2L))
expect_equal(d$rel.fail,   c(1/7, 1/4, 0))
expect_equal(d$decision,   c("release", "reject", "pending"))
expect_equal(attr(d, "max_fail_ratio"), 0.2)
expect_equal(attr(d, "min_effective"), 3)
expect_equal(attr(d, "dropped_records"), 0L)
expect_equal(nrow(attr(d, "dropped_rules")), 0L)


## NA, rule errors and normal fails are counted independently ----
cf <- check_that(dat, x > 0, ape > 0)  # 'ape' does not exist: rule error
d <- partition_decision(cf, dat$region, min_effective = 1)
expect_equal(d$nerror, d$nrecords)             # one error item per record
expect_equal(d$npass, c(2L, 1L, 1L))           # only from x > 0
expect_equal(d$nfail, c(1L, 1L, 0L))
expect_equal(d$nNA,   c(1L, 0L, 0L))
expect_equal(d$neffective, c(3L, 2L, 1L))      # errors stay out of effective volume
# invariant: all items are accounted for exactly once
expect_equal(d$npass + d$nfail + d$nNA + d$nerror, 2 * d$nrecords)


## small sample regions are pending, also when nothing failed ----
d <- partition_decision(cf, dat$region, max_fail_ratio = 0.5, min_effective = 2)
expect_equal(d$decision, c("release", "release", "pending"))
# boundary: failure ratio equal to the maximum still releases
expect_equal(d$rel.fail[2], 0.5)
expect_equal(d$decision[2], "release")
# pending takes precedence over reject
d <- partition_decision(cf, dat$region, max_fail_ratio = 0, min_effective = 2)
expect_equal(d$decision[2], "reject")   # rel.fail 0.5 > 0, enough volume
expect_equal(d$decision[3], "pending")  # too little volume to decide


## defaults: any fail rejects, zero effective volume is pending ----
d <- partition_decision(check_that(dat, y > 0), dat$region)
expect_equal(d$decision, c("release", "release", "release"))
expect_equal(attr(d, "max_fail_ratio"), 0)
expect_equal(attr(d, "min_effective"), 1)


## factor 'by': empty levels are reported and pending ----
by <- factor(dat$region, levels = c("A", "B", "C", "D"))
d <- partition_decision(cf, by, min_effective = 1)
expect_equal(d$partition, c("A", "B", "C", "D"))
expect_equal(d$nrecords[4], 0L)
expect_equal(d$neffective[4], 0L)
expect_true(is.na(d$rel.fail[4]))
expect_equal(d$decision[4], "pending")


## records with missing partition key are ignored, with a warning ----
by <- dat$region
by[1] <- NA
expect_warning(d <- partition_decision(cf, by, min_effective = 1))
expect_equal(attr(d, "dropped_records"), 1L)
expect_equal(d$nrecords, c(3L, 2L, 1L))


## non record-level rules are dropped, with a warning ----
cf2 <- check_that(dat, x > 0, nrow(.) > 0)
expect_warning(d <- partition_decision(cf2, dat$region, min_effective = 1))
dr <- attr(d, "dropped_rules")
expect_equal(nrow(dr), 1L)
expect_equal(dr$name, "V2")
expect_equal(d$npass + d$nfail + d$nNA + d$nerror, d$nrecords)  # only x > 0 counted


## partitioning via a keyset stored with the confrontation ----
cf3 <- confront(dat, validator(x > 0), key = "region")
d <- partition_decision(cf3, "region", min_effective = 1)
expect_equal(d$partition, c("A", "B", "C"))
expect_equal(d$npass, c(2L, 1L, 1L))
expect_error(partition_decision(cf3, "no_such_key"))


## numeric partition keys are supported ----
d <- partition_decision(cf, c(1, 1, 1, 1, 2, 2, 3), min_effective = 1)
expect_equal(d$partition, c("1", "2", "3"))
expect_equal(d$nrecords, c(4L, 2L, 1L))


## input validation ----
expect_error(partition_decision(cf, dat$region[1:3]))       # 'by' too short
expect_error(partition_decision(cf, dat$region, max_fail_ratio = 1.5))
expect_error(partition_decision(cf, dat$region, max_fail_ratio = -0.1))
expect_error(partition_decision(cf, dat$region, min_effective = -1))
expect_error(partition_decision(dat, dat$region))           # not a validation

# restore global option state
voptions(raise = old_raise)
