# Temporary exceptions (waivers) ---------------------------------------------

# Data of two suppliers; supplier A fails a turnover rule.
dat <- data.frame(
    supplier = c("A", "A", "B", "B")
  , turnover = c(10, -5, 10, -5)
  , stringsAsFactors = FALSE
)
rules <- validator(turnover >= 0)
cf <- confront(dat, rules, key = "supplier")

# Register a temporary exemption for supplier A during February 2026.
exc <- exceptions(
    supplier = "A"
  , rule     = "V1"
  , start    = "2026-02-01"
  , end      = "2026-02-28"
  , basis    = "supplier A system upgrade"
)

# Apply the exception as of a reference time inside the window.
out <- apply_exceptions(cf, exc, at = as.POSIXct("2026-02-15", tz = "UTC"))

# Effective verdicts: the fail of supplier A is waived, B is still blocked.
values(out)

# The original failure is retained for the historical report.
original_values(out)

# Which records were waived, on what basis, and until when.
exception_report(out)

# Automated blocking resumes once the approval has expired.
out_expired <- apply_exceptions(cf, exc, at = as.POSIXct("2026-03-15", tz = "UTC"))
values(out_expired)
