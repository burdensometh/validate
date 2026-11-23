# Two versions of a data set, confronted with (possibly different) rule sets.
# The confrontations must be told which column identifies a business record.
dat_old <- data.frame(
    id = c("a", "b", "c")
  , turnover = c(100, -5, NA)
  , staff = c(5, 1, 10)
  , stringsAsFactors = FALSE
)
# new version: rows in a different order, one negative turnover repaired
dat_new <- data.frame(
    id = c("c", "a", "b")
  , turnover = c(NA, 100, 8)
  , staff = c(10, 5, 1)
  , stringsAsFactors = FALSE
)

# old and new rule set: the staff rule is tightened, a rule is added
rules_old <- validator(turnover_nonneg = turnover >= 0, staff_nonneg = staff >= 0)
rules_new <- validator(turnover_nonneg = turnover >= 0, staff_nonneg = staff >= 1
                       , staff_upper = staff <= 8)

old <- confront(dat_old, rules_old, key = "id")
new <- confront(dat_new, rules_new, key = "id")

# every record/rule combination with its status transition
(cmp <- compare_validations(old, new))

# record "b" moved from blocked to passing after its turnover was repaired
subset(cmp, transition == "block_to_pass")

# rule-level changes: staff_nonneg was modified (same name, stricter expression),
# staff_upper was added
unique(cmp[, c("rule", "transition")])

# aggregated counts per transition type
summary(cmp)

# equivalent, via the compare() generic
stopifnot(identical(compare(old, new), cmp))
