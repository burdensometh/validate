## Incremental revalidation ----

## affected rules via derived variables (transitive) ----
v <- validator(
    pos_a = a > 0
  , d1    := a * 2
  , d2    := d1 + 1
  , deep  = d2 > 0
  , pos_b = b > 0
)
expect_equal(affected_rules(v, changed = "a"), c("pos_a", "deep"))
expect_equal(affected_rules(v, changed = "b"), c("pos_b"))
# names of the rules in the validator (not the expanded expressions)
expect_equal(affected_rules(v, changed = "a", expand = FALSE), c("pos_a", "deep"))

# the same in the reverse order: a rule defined before the derived variable is
# not affected by it
v <- validator(d1 := a * 2, d1 > 0, b > 0)
expect_equal(affected_rules(v, changed = "a"), c("V2"))

# a derived variable that is not used by a rule does not spread the effect
v <- validator(u := a * 2, b > 0)
expect_equal(affected_rules(v, changed = "a"), character(0))

## reference data ----
v <- validator(a > 0, a %in% codes, b > 0)
expect_equal(affected_rules(v, ref_changed = "codes"),
             c("V2"))
expect_equal(affected_rules(v, changed = "a"), c("V1", "V2"))

## data set level rules ----
v <- validator(nrow(.) == 3, ncol(.) == 2, a > 0)
# a change in values does not affect nrow()/ncol(); a rule on 'a' is affected
expect_equal(affected_rules(v, changed = "a", data = data.frame(a = 1:3)),
             c("V3"))
# '.' rules are conservatively assumed affected when the schema is unknown
expect_equal(affected_rules(v, changed = "a"), c("V1", "V2", "V3"))
# changing the number of records affects nrow(.) but not ncol(.)
expect_equal(affected_rules(v, changed = "a", data = data.frame(a = 1:3),
                            structure_changed = TRUE),
             c("V1", "V2", "V3"))


## basic revalidation: one column changed ----
dat <- data.frame(
    id = 1:6
  , a  = c(1, 2, 3, 4, 5, 6)
  , b  = c(6, 5, 4, 3, 2, 1)
  , g  = c("x", "x", "y", "y", "x", "y")
)
rules <- validator(
    a_pos = a > 0
  , ab    = a + b > 3
  , twice = d := 2 * a, d < 100
  , b_pos = b >= 0
  , recs  = nrow(.) == 6
  , uq    = is_unique(id)
)
previous <- confront(dat, rules, key = "id")

dat$a[2] <- -10
out <- revalidate(dat, rules, previous = previous, changed = "a")

info <- incremental_info(out)
expect_equal(info$changed, "a")
# the transient variable 'd' expands to '2 * a < 100', named 'V4'
expect_true(all(c("a_pos", "ab", "V4") %in% info$recomputed))
expect_true(all(c("b_pos", "recs", "uq") %in% info$reused))
expect_equal(sort(c(info$recomputed, info$reused)), sort(names(previous)))

# incremental results must equal a full confrontation
full <- confront(dat, rules, key = "id")
expect_equivalent(values(out), values(full))
expect_equivalent(summary(out), summary(full))

## automatic change detection ----
out2 <- revalidate(dat, rules, previous = previous)
expect_equal(incremental_info(out2)$changed, "a")
expect_equivalent(values(out2), values(full))

# nothing changed: nothing recomputed
out3 <- revalidate(dat, rules, previous = confront(dat, rules, key = "id"))
expect_equal(incremental_info(out3)$changed, character(0))
expect_equal(incremental_info(out3)$recomputed, character(0))

## reference data changes ----
ref  <- list(codes = 1:3, scale = 10)
rules <- validator(
    in_codes = a %in% codes
  , scaled   = a * scale > 0
  , b_pos    = b > 0
)
prev <- confront(dat, rules, ref = ref, key = "id")
ref2 <- ref; ref2$codes <- 1:2
out <- revalidate(dat, rules, previous = prev, ref = ref2)
info <- incremental_info(out)
expect_true("codes" %in% info$ref_changed)
expect_true("in_codes" %in% info$recomputed)
expect_true("scaled" %in% info$reused)
expect_true("b_pos" %in% info$reused)
expect_equivalent(values(out), values(confront(dat, rules, ref = ref2, key = "id")))

# changed reference object passed explicitly
out <- revalidate(dat, rules, previous = prev, ref = ref, ref_changed = "scale")
expect_true("scaled" %in% incremental_info(out)$recomputed)
expect_true("in_codes" %in% incremental_info(out)$reused)

## reordering records ----
dat2 <- dat[c(6, 1, 2, 3, 4, 5), ]
prev <- confront(dat, validator(a_pos = a > 0, pos_b = b > 0, uq = is_unique(id)),
                 key = "id")
out <- revalidate(dat2, validator(a_pos = a > 0, pos_b = b > 0, uq = is_unique(id)),
                  previous = prev)
expect_equal(incremental_info(out)$changed, character(0))
expect_equivalent(values(out), values(confront(dat2, validator(a_pos = a > 0, pos_b = b > 0,
                                                                uq = is_unique(id)), key = "id")))

## adding and removing records ----
sub_rules <- validator(a_pos = a > 0, b_pos = b > 0, uq = is_unique(id))
prev <- confront(dat, sub_rules, key = "id")
dat_sub <- dat[1:4, ]
out <- revalidate(dat_sub, sub_rules, previous = prev)
expect_equivalent(values(out), values(confront(dat_sub, sub_rules, key = "id")))
# is_unique() is a cross-record rule: it must be re-evaluated when the row set
# changes, even if no column changed
expect_true("uq" %in% incremental_info(out)$recomputed)

# adding a record
dat_add <- rbind(dat, data.frame(id = 7, a = 7, b = 0, g = "x"))
out <- revalidate(dat_add, sub_rules, previous = prev)
expect_equivalent(values(out), values(confront(dat_add, sub_rules, key = "id")))
# record-wise rules for existing records are reused and only the new record is
# evaluated
expect_true("a_pos" %in% incremental_info(out)$partially_reused)

## partial reuse: old rows kept, new rows evaluated ----
prev <- confront(dat, sub_rules, key = "id")
out <- revalidate(dat_add, sub_rules, previous = prev)
val <- values(out, simplify = FALSE)[["a_pos"]]
expect_equal(length(val), nrow(dat_add))
expect_equivalent(val, values(confront(dat_add, sub_rules, key = "id"),
                              simplify = FALSE)[["a_pos"]])

## multiple keys ----
mk <- data.frame(k1 = c(1, 1, 2, 2), k2 = c("a", "b", "a", "b"), x = 1:4)
v  <- validator(x > 0, x < 3)
prev <- confront(mk, v, key = c("k1", "k2"))
mk2 <- mk[c(4, 3, 2, 1), ]
out <- revalidate(mk2, v, previous = prev)
expect_equivalent(values(out), values(confront(mk2, v, key = c("k1", "k2"))))

## no key: align by position when dimensions match ----
v <- validator(x > 0)
d <- data.frame(x = c(1, -1, 2, -2))
prev <- confront(d, v)
d$x[2] <- 1
out <- revalidate(d, v, previous = prev, changed = "x")
expect_equivalent(values(out), values(confront(d, v)))

# without a key and with a different number of rows, nothing record-wise can
# be merged
d2 <- d[1:2, , drop = FALSE]
out <- revalidate(d2, v, previous = prev, changed = character(0))
expect_equivalent(values(out), values(confront(d2, v)))

## changed rule set: new, removed and edited rules ----
v1 <- validator(a > 0, b > 0)
prev <- confront(dat, v1, key = "id")
v2 <- validator(a > 0, b >= 0, c = a + b > 5)
out <- revalidate(dat, v2, previous = prev)
expect_equal(incremental_info(out)$reused, "V1")
expect_true(all(c("V2", "c") %in% incremental_info(out)$recomputed))
expect_equivalent(values(out), values(confront(dat, v2, key = "id")))

## the result can be used as the input of a next revalidation ----
v <- validator(a_pos = a > 0, b_pos = b > 0)
d1 <- data.frame(id = 1:3, a = c(1, -1, 2), b = 1:3)
d2 <- d1; d2$a[2] <- 5
d3 <- d2; d3$b[1] <- -5
p1 <- confront(d1, v, key = "id")
p2 <- revalidate(d2, v, previous = p1)
p3 <- revalidate(d3, v, previous = p2)
expect_equal(incremental_info(p3)$changed, "b")
expect_equal(incremental_info(p3)$recomputed, "b_pos")
expect_equivalent(values(p3), values(confront(d3, v, key = "id")))

## indicators are supported ----
ind <- indicator(mean(a), max(b))
prev <- confront(dat, ind)
dat$a[1] <- 100
out <- revalidate(dat, ind, previous = prev)
expect_true(inherits(out, "indication"))
expect_equivalent(values(out), values(confront(dat, ind)))

## variable groups are expanded before dependencies are computed ----
v <- validator(G := var_group(a, b), G > 0)
expect_equal(affected_rules(v, changed = "a"), "V2.1")
expect_equal(affected_rules(v, changed = "b"), "V2.2")
dat <- data.frame(id = 1:3, a = 1:3, b = 3:1)
prev <- confront(dat, v, key = "id")
dat$a[2] <- -3
out <- revalidate(dat, v, previous = prev, changed = "a")
expect_equal(incremental_info(out)$recomputed, "V2.1")
expect_equal(incremental_info(out)$reused, "V2.2")
expect_equivalent(values(out), values(confront(dat, v, key = "id")))

## previously errored rules are recomputed ----
v <- validator(present = a > 0, absent = nope > 0)
dat <- data.frame(id = 1:3, a = 1:3)
prev <- confront(dat, v, key = "id")
out <- revalidate(dat, v, previous = prev)
expect_true("absent" %in% incremental_info(out)$recomputed)
expect_equal(length(errors(out)), 1)

## NULL previous falls back to a normal confrontation ----
out <- revalidate(dat, v, previous = NULL)
expect_true(inherits(out, "validation"))
expect_equal(length(out), 2)

## options that influence evaluation invalidate all results ----
v <- validator(a > 0, b > 0)
d <- data.frame(id = 1:3, a = c(NA, -1, 2), b = 1:3)
prev <- confront(d, v, key = "id", na.value = FALSE)
out <- revalidate(d, v, previous = prev, na.value = FALSE)
expect_equal(incremental_info(out)$recomputed, character(0))
expect_true(is.na(values(prev)[1, 1]) == FALSE)  # na.value replaced the NA
out2 <- revalidate(d, v, previous = prev, na.value = NA)
expect_equal(sort(incremental_info(out2)$recomputed), c("V1", "V2"))
expect_equivalent(values(out2), values(confront(d, v, key = "id")))

## '%in%' against a data column is not spliced across a row subset ----
v <- validator(x %in% y)
d <- data.frame(id = 1:3, x = c(1, 2, 3), y = c(3, 2, 1))
prev <- confront(d, v, key = "id")
d2 <- rbind(d, data.frame(id = 4, x = 9, y = 9))
out <- revalidate(d2, v, previous = prev)
expect_equivalent(values(out), values(confront(d2, v, key = "id")))

## cross-record rules are re-evaluated when the row set changes ----
v <- validator(uq = is_unique(id), pos = a > 0)
d <- data.frame(id = 1:4, a = c(1, -1, 2, 3))
prev <- confront(d, v, key = "id")
d2 <- rbind(d, data.frame(id = 5, a = -5))
out <- revalidate(d2, v, previous = prev)
expect_true("uq" %in% incremental_info(out)$recomputed)
expect_equivalent(values(out), values(confront(d2, v, key = "id")))

## reference-using rules are not merged when records are reordered ----
ref <- data.frame(lim = 1:4)
v <- validator(a > lim)
d <- data.frame(id = 1:4, a = c(2, 3, 4, 5))
prev <- confront(d, v, ref = ref, key = "id")
d2 <- d[c(4, 3, 2, 1), , drop = FALSE]
out <- revalidate(d2, v, previous = prev, ref = ref)
expect_equivalent(values(out), values(confront(d2, v, ref = ref, key = "id")))

## character and multi-column keys ----
v <- validator(a > 0)
d <- data.frame(k = c("x", "y", "z"), a = c(-1, 2, 3))
prev <- confront(d, v, key = "k")
d2 <- d[c(3, 1, 2), , drop = FALSE]
expect_equivalent(values(revalidate(d2, v, previous = prev)),
                  values(confront(d2, v, key = "k")))

d <- data.frame(k1 = c(1, 1, 2, 2), k2 = c("a", "b", "a", "b"), a = c(-1, 1, 2, -2))
prev <- confront(d, v, key = c("k1", "k2"))
d2 <- d[c(4, 3, 2, 1), , drop = FALSE]
expect_equivalent(values(revalidate(d2, v, previous = prev)),
                  values(confront(d2, v, key = c("k1", "k2"))))

## duplicate keys disable record-wise merging ----
v <- validator(a > 0)
d <- data.frame(id = c(1, 1, 2), a = c(-1, 2, 3))
prev <- confront(d, v, key = "id")
d$a[1] <- 5
out <- revalidate(d, v, previous = prev)
expect_equal(incremental_info(out)$reused, character(0))
expect_equivalent(values(out), values(confront(d, v, key = "id")))

## changed / ref_changed that would otherwise be detected can be passed ----
v <- validator(a > 0, a %in% codes)
d <- data.frame(id = 1:3, a = 1:3)
prev <- confront(d, v, ref = list(codes = 1:3), key = "id")
out <- revalidate(d, v, previous = prev, ref = list(codes = 1:2))
expect_true("codes" %in% incremental_info(out)$ref_changed)
expect_true("V2" %in% incremental_info(out)$recomputed)
expect_true("V1" %in% incremental_info(out)$reused)

## unchanged reference data is not spuriously reported as changed, for each
## form of 'ref' (this guards against capturing internal environments) ----
d <- data.frame(id = 1:3, a = 1:3)

prev <- confront(d, validator(a %in% codes, a > t), ref = list(codes = 1:2, t = 0),
                 key = "id")
out <- revalidate(d, validator(a %in% codes, a > t), previous = prev,
                  ref = list(codes = 1:2, t = 0))
expect_equal(incremental_info(out)$ref_changed, character(0))
expect_equal(incremental_info(out)$recomputed, character(0))

v2 <- validator(a > ref$lim)
prev <- confront(d, v2, ref = data.frame(lim = 0), key = "id")
out <- revalidate(d, v2, previous = prev, ref = data.frame(lim = 0))
expect_equal(incremental_info(out)$ref_changed, character(0))
expect_equal(incremental_info(out)$recomputed, character(0))

e <- new.env(); e$lim <- 0
v3 <- validator(a > lim)
prev <- confront(d, v3, ref = e, key = "id")
out <- revalidate(d, v3, previous = prev, ref = e)
expect_equal(incremental_info(out)$ref_changed, character(0))
expect_equal(incremental_info(out)$recomputed, character(0))
e$lim <- 2
out <- revalidate(d, v3, previous = prev, ref = e)
expect_equal(incremental_info(out)$ref_changed, "lim")
expect_equivalent(values(out), values(confront(d, v3, ref = e, key = "id")))

## randomized differential test: revalidate must equal a full confrontation ----
set.seed(1)
for (trial in 1:40){
  n <- sample(3:12, 1)
  d <- data.frame(
      id = seq_len(n)
    , a  = round(rnorm(n), 2)
    , b  = round(rnorm(n), 2)
    , c  = sample(1:5, n, TRUE)
    , g  = sample(c("x", "y", "z"), n, TRUE)
  )
  v <- validator(
      r_a = a > 0
    , r_ab = a + b > 1
    , r_c = c %in% c(1, 2, 3)
    , r_uq = is_unique(id)
    , r_mean = mean(a) > -5
    , d1 := a * 2, r_derived = d1 + b > 0
    , r_n = nrow(.) >= 3
  )
  p <- suppressWarnings(confront(d, v, key = "id"))

  d2 <- d
  op <- sample(c("edit", "add", "remove", "reorder"), 1)
  if (op == "edit"){
    rows <- sample(n, max(1, n %/% 3))
    d2$a[rows] <- round(rnorm(length(rows)), 2)
  } else if (op == "add"){
    d2 <- rbind(d, data.frame(id = n + 1, a = 1, b = 1, c = 2, g = "x"))
  } else if (op == "remove" && n > 3){
    d2 <- d[-1, , drop = FALSE]
  } else {
    d2 <- d[sample(n), , drop = FALSE]
  }

  out  <- suppressWarnings(revalidate(d2, v, previous = p))
  full <- suppressWarnings(confront(d2, v, key = "id"))
  expect_equivalent(summary(out)[, -1], summary(full)[, -1])
}
