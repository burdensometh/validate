# Tests for compare_validations(): comparing two confrontation results
# by business key and rule identity.

## helper ------------------------------------------------------------------
ns <- asNamespace("validate")
cv <- function(old, new, key = NULL) validate::compare_validations(old, new, key)

d_old <- data.frame(
    id = c("a", "b", "c")
  , x  = c(1, -1, NA)
  , y  = c(5, 5, 5)
  , stringsAsFactors = FALSE
)
# same records in a different order, with one value changed
d_new <- data.frame(
    id = c("c", "a", "b")
  , x  = c(NA, 1, 2)
  , y  = c(5, 5, 5)
  , stringsAsFactors = FALSE
)

v_old <- validator(r1 = x > 0, r2 = y > 0)
v_new <- validator(r1 = x > 0, r2 = y > 0, r3 = x < 10)

cf_old <- confront(d_old, v_old, key = "id")
cf_new <- confront(d_new, v_new, key = "id")


## basic structure ----------------------------------------------------------
cmp <- cv(cf_old, cf_new)

expect_inherits(cmp, "validationComparison")
expect_equal(nrow(cmp), 9)           # 3 keys x 3 rules
expect_equal(names(cmp), c("id", "rule", "status_old", "status_new", "transition"))
expect_equal(sort(unique(cmp$rule)), c("r1", "r2", "r3"))
expect_equal(sort(unique(cmp$id)), c("a", "b", "c"))


## row order of the input must not create differences -----------------------
# confronting a shuffled copy of the *same* data must yield all *_to_*same*
d_shuffled <- d_old[c(3, 1, 2), ]
cmp_same <- cv(cf_old, confront(d_shuffled, v_old, key = "id"))
expect_true(all(cmp_same$status_old == cmp_same$status_new))
expect_equal(sort(cmp_same$transition),
             sort(c("pass_to_pass", "block_to_block", "unverifiable_to_unverifiable",
                    "pass_to_pass", "pass_to_pass", "pass_to_pass")))


## per-record status transitions ---------------------------------------------
r1 <- cmp[cmp$rule == "r1", ]
expect_equal(r1$transition[r1$id == "a"], "pass_to_pass")
expect_equal(r1$transition[r1$id == "b"], "block_to_pass")   # -1 repaired to 2
expect_equal(r1$transition[r1$id == "c"],
             "unverifiable_to_unverifiable")
r2 <- cmp[cmp$rule == "r2", ]
expect_true(all(r2$transition == "pass_to_pass"))


## rule added ---------------------------------------------------------------
r3 <- cmp[cmp$rule == "r3", ]
expect_true(all(r3$transition == "rule_added"))
expect_true(all(r3$status_old == "absent"))
expect_equal(r3$status_new[r3$id == "c"], "unverifiable")
expect_equal(r3$status_new[r3$id == "a"], "pass")


## rule removed --------------------------------------------------------------
v_less <- validator(r1 = x > 0)
cmp_rem <- cv(cf_old, confront(d_new, v_less, key = "id"))
r2_rem <- cmp_rem[cmp_rem$rule == "r2", ]
expect_true(all(r2_rem$transition == "rule_removed"))
expect_true(all(r2_rem$status_new == "absent"))
expect_false(any(r2_rem$status_old == "absent"))


## rule with the same name but a modified expression -------------------------
v_mod <- validator(r1 = x > 1, r2 = y > 0)
cmp_mod <- cv(cf_old, confront(d_new, v_mod, key = "id"))
expect_true(all(cmp_mod$transition[cmp_mod$rule == "r1"] == "rule_modified"))
# the statuses themselves remain available
r1_mod <- cmp_mod[cmp_mod$rule == "r1", ]
expect_equal(r1_mod$status_old[r1_mod$id == "b"], "block")
expect_equal(r1_mod$status_new[r1_mod$id == "b"], "pass")
# an untouched rule is still compared normally
expect_true(all(cmp_mod$transition[cmp_mod$rule == "r2"] == "pass_to_pass"))


## records added to / removed from the data ----------------------------------
d_a <- data.frame(id = c("a", "b"), x = c(1, -1), stringsAsFactors = FALSE)
d_b <- data.frame(id = c("b", "c"), x = c(2, 1), stringsAsFactors = FALSE)
v_one <- validator(r1 = x > 0)
cmp_rec <- cv(confront(d_a, v_one, key = "id"),
              confront(d_b, v_one, key = "id"))
expect_equal(cmp_rec$transition[cmp_rec$id == "a"], "pass_to_absent")
expect_equal(cmp_rec$transition[cmp_rec$id == "b"], "block_to_pass")
expect_equal(cmp_rec$transition[cmp_rec$id == "c"], "absent_to_pass")


## erroring rules get an explicit status -------------------------------------
v_err <- validator(r1 = x > 0, r_bad = absent_column > 0)
cf_err_old <- confront(d_old, v_err, key = "id")
cf_err_new <- confront(d_new, v_err, key = "id")
cmp_err <- cv(cf_err_old, cf_err_new)
bad <- cmp_err[cmp_err$rule == "r_bad", ]
expect_true(all(bad$status_old == "error"))
expect_true(all(bad$status_new == "error"))
expect_true(all(grepl("^error_to_", bad$transition)))
# an error rule that disappears is reported as rule removal
cmp_err2 <- cv(cf_err_old, cf_new)
bad2 <- cmp_err2[cmp_err2$rule == "r_bad", ]
expect_true(all(bad2$transition == "rule_removed"))
expect_equal(bad2$status_old[bad2$id == "a"], "error")
expect_equal(bad2$status_new[bad2$id == "a"], "absent")


## the summary counts every transition ---------------------------------------
s <- summary(cmp)
expect_inherits(s, "data.frame")
expect_equal(names(s), c("transition", "count"))
expect_equal(sum(s$count), nrow(cmp))
expect_equal(s$count[s$transition == "rule_added"], 3)
expect_equal(s$count[s$transition == "block_to_pass"], 1)


## the compare() S4 method gives the same answer -----------------------------
expect_identical(compare(cf_old, cf_new), cmp)


## multi-column business keys -------------------------------------------------
d_m1 <- data.frame(id = c("a", "a", "b"), k2 = c("x", "y", "x"),
                   x = c(1, -1, 3), stringsAsFactors = FALSE)
d_m2 <- d_m1[c(3, 1, 2), ]
cf_m1 <- confront(d_m1, v_one, key = c("id", "k2"))
cf_m2 <- confront(d_m2, v_one, key = c("id", "k2"))
cmp_m <- cv(cf_m1, cf_m2)
expect_equal(names(cmp_m)[1:2], c("id", "k2"))
expect_equal(nrow(cmp_m), 3)
expect_true(all(cmp_m$status_old == cmp_m$status_new))
# explicit key argument is accepted as well
expect_identical(cv(cf_m1, cf_m2, key = c("id", "k2")), cmp_m)


## validation and error conditions -------------------------------------------
# no key stored and none supplied
cf_nokey <- confront(data.frame(x = 1), v_one)
expect_error(cv(cf_nokey, cf_nokey))

# duplicate business key
d_dup <- data.frame(id = c("a", "a"), x = 1:2)
cf_dup <- confront(d_dup, v_one, key = "id")
expect_error(cv(cf_dup, cf_dup))

# unknown key column
expect_error(cv(cf_old, cf_new, key = "nope"))

# non-validation inputs
expect_error(cv(v_old, cf_new))
expect_error(cv(d_old, cf_new))
