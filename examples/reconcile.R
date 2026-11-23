# Two sources report the same financial indicators.
# 'accounts' reports in yuan, 'survey' reports in thousand-yuan.
accounts <- data.frame(
    id      = c("ent01","ent02","ent03","ent04")
  , revenue = c(1020000, 540000, 75000, 88300)
  , staff   = c(120000, 61000, 9000, 15200)
)
survey <- data.frame(
    id      = c("ent01","ent02","ent03","ent05")
  , revenue = c(1020.0, 541.0, 74.9, 91.2)   # thousand-yuan, rounded
  , staff   = c(120.0, 61.0, 9.0, 15.2)
)

# Declare, per indicator, the unit ratio and the tolerances.
# One thousand-yuan equals 1000 yuan; differences up to 500 yuan or
# 0.1 percent (whichever is larger) are accepted as legitimate rounding.
spec <- unit_spec(
    indicator = c("revenue","staff")
  , ratio     = 1000
  , abs_tol   = 500
  , rel_tol   = 0.001
)

# Reconcile the sources on the business key 'id'.
out <- reconcile(accounts, survey, by="id", spec=spec)
out

# Counts per indicator and status.
summary(out)

# Extract a plain data frame for review: raw values (x_value, y_value),
# converted values (x_common, y_common), the difference, and the status.
as.data.frame(out)

# Records occurring in one source only are 'unmatched'; ent02/revenue is
# 'out_of_tolerance' (a difference of 1000 yuan).
subset(as.data.frame(out), status != "ok")

# Indicators without a declared unit are flagged 'missing_unit', not
# mistaken for anomalies.
reconcile(accounts, survey, by="id"
  , spec=unit_spec("revenue", ratio=1000, abs_tol=500))
