## unit_spec construction and validation
spec <- unit_spec(c("revenue","staff"), ratio=1000, abs_tol=500, rel_tol=0.001)
expect_true(is.data.frame(spec))
expect_equal(nrow(spec), 2)
expect_equal(names(spec), c("indicator","ratio","abs_tol","rel_tol"))
expect_equal(spec$ratio, c(1000,1000))

# scalar defaults are recycled
spec1 <- unit_spec("revenue")
expect_equal(unlist(spec1[1,-1]), c(ratio=1, abs_tol=0, rel_tol=0))

expect_error(unit_spec(c("a","a")))                 # duplicate indicator
expect_error(unit_spec("a", ratio=0))               # ratio must be positive
expect_error(unit_spec("a", ratio=-1000))
expect_error(unit_spec("a", abs_tol=-1))            # tolerances non-negative
expect_error(unit_spec("a", rel_tol=-1))
expect_error(unit_spec("a", ratio=c(1,2)))          # length must be 1 or n
expect_silent(unit_spec("a", ratio=NA))             # undeclared unit is allowed


## basic reconciliation: yuan versus thousand-yuan
fin <- data.frame(
    id      = c("A","B","C","D")
  , revenue = c(100000, 250000, 75000, 123456)
  , stringsAsFactors = FALSE
)
stat <- data.frame(
    id      = c("A","B","C","D")
  , revenue = c(100, 250.4, 75, 123.5)   # thousand-yuan, rounded
  , stringsAsFactors = FALSE
)
spec <- unit_spec("revenue", ratio=1000, abs_tol=500)
out  <- reconcile(fin, stat, by="id", spec=spec)

expect_true(inherits(out, "reconciliation"))
expect_true(is.data.frame(out))
expect_equal(nrow(out), 4)
# raw difference check would flag B (400) and D (44); all are legitimate
# rounding within 500 yuan.
expect_equal(as.character(out$status), rep("ok", 4))

# values before and after conversion can be reviewed
expect_equal(out$x_value, c(100000, 250000, 75000, 123456))
expect_equal(out$y_value, c(100, 250.4, 75, 123.5))
expect_equal(out$x_common, out$x_value)
expect_equal(out$y_common, out$y_value * 1000)
expect_equal(out$diff, out$x_common - out$y_common)
expect_equal(out$diff, c(0, -400, 0, -44))


## genuine exceedance is flagged out_of_tolerance
stat2 <- stat
stat2$revenue[2] <- 251   # 1000 yuan off
out2 <- reconcile(fin, stat2, by="id", spec=spec)
expect_equal(as.character(out2$status), c("ok","out_of_tolerance","ok","ok"))


## relative tolerance: pass if within rel_tol * magnitude
big <- data.frame(id="E", revenue=1e8)
big2 <- data.frame(id="E", revenue=100000.4)  # 400 yuan off
spec_rel <- unit_spec("revenue", ratio=1000, abs_tol=0, rel_tol=1e-5)
expect_equal(as.character(reconcile(big, big2, by="id", spec=spec_rel)$status), "ok")
spec_rel2 <- unit_spec("revenue", ratio=1000, abs_tol=0, rel_tol=1e-7)
expect_equal(as.character(reconcile(big, big2, by="id", spec=spec_rel2)$status)
  , "out_of_tolerance")


## missing unit declaration is marked separately from real exceedance
fin$staff  <- c(30000, 80000, 20000, 5000)
stat$staff <- c(30, 80, 20, 5)
out3 <- reconcile(fin, stat, by="id", spec=spec)  # spec declares revenue only
expect_equal(as.character(out3$status[out3$indicator=="staff"])
  , rep("missing_unit", 4))
expect_true(all(is.na(out3$diff[out3$indicator=="staff"])))
expect_true(all(is.na(out3$ratio[out3$indicator=="staff"])))
expect_equal(as.character(out3$status[out3$indicator=="revenue"]), rep("ok",4))


## missing values and unmatched keys
fin4  <- data.frame(id=c("A","B","C"), revenue=c(100000, NA, 75000))
stat4 <- data.frame(id=c("A","B","D"), revenue=c(100, 80, 75))
out4  <- reconcile(fin4, stat4, by="id", spec=spec)
expect_equal(as.character(out4$status)
  , c("ok","missing_value","unmatched","unmatched"))
expect_equal(as.character(out4$id), c("A","B","C","D"))


## key requirements
expect_error(reconcile(fin, stat, by="nosuchcol", spec=spec))
expect_error(reconcile(fin, stat, spec=spec))  # no key given
fin_dup <- rbind(fin, fin[1,])
expect_error(reconcile(fin_dup, stat, by="id", spec=spec))  # key not unique
fin_na <- fin; fin_na$id[1] <- NA
expect_error(reconcile(fin_na, stat, by="id", spec=spec))   # NA in key


## spec as a plain data frame; function-level tolerance defaults fill NA
spec_df <- data.frame(indicator="revenue", ratio=1000, abs_tol=NA, rel_tol=NA)
out5 <- reconcile(fin, stat, by="id", spec=spec_df, abs_tol=500)
expect_equal(as.character(out5$status[out5$indicator=="revenue"]), rep("ok", 4))
out6 <- reconcile(fin, stat, by="id", spec=spec_df)  # default tolerance 0
expect_equal(as.character(out6$status[out6$indicator=="revenue"])
  , c("ok","out_of_tolerance","ok","out_of_tolerance"))
expect_error(reconcile(fin, stat, by="id", spec=data.frame(indicator="revenue")))
expect_warning(reconcile(fin, stat, by="id"
  , spec=unit_spec(c("revenue","nosuch"), ratio=1000)))  # declared, not in data


## indicator selection
# common numeric non-key columns are compared by default
expect_equal(unique(out3$indicator), c("revenue","staff"))
# explicit selection
out7 <- reconcile(fin, stat, by="id", indicators="revenue", spec=spec)
expect_equal(unique(out7$indicator), "revenue")
expect_error(reconcile(fin, stat, by="id", indicators="nosuch", spec=spec))
fin_chr <- fin; fin_chr$comment <- "ok"
stat_chr <- stat; stat_chr$comment <- "ok"
expect_error(reconcile(fin_chr, stat_chr, by="id", indicators="comment", spec=spec))


## multiple key columns
fin8  <- data.frame(reg=c("r1","r1","r2"), ent=c("a","b","a")
       , revenue=c(1000, 2000, 3000))
stat8 <- data.frame(reg=c("r1","r1","r2"), ent=c("a","b","a")
       , revenue=c(1, 2, 3.001))
out8  <- reconcile(fin8, stat8, by=c("reg","ent")
       , spec=unit_spec("revenue", ratio=1000, abs_tol=2))
expect_equal(nrow(out8), 3)
expect_equal(as.character(out8$status), c("ok","ok","ok"))


## methods
expect_equal(class(as.data.frame(out)), "data.frame")
s <- summary(out3)
expect_true(is.table(s))
expect_equal(sum(s), nrow(out3))
expect_equal(unname(colSums(s)[c("missing_unit","ok")]), c(4,4))
expect_silent(print(out))
