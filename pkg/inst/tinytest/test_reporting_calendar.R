
# Reporting on working days, with per-store validity windows.

## calendar as a plain vector of working days -------------------------------

cal <- as.Date(c(
    "2024-01-01", "2024-01-02", "2024-01-03", "2024-01-04", "2024-01-05"
  , "2024-01-08", "2024-01-09", "2024-01-10"
))

# a complete store is a valid calendar sequence
expect_true( is_linear_sequence(cal, calendar = cal) )

# a gap (missing third working day) is not
gappy <- cal[-3]
expect_false( is_linear_sequence(gappy, calendar = cal) )

# character dates are interpreted as dates when a calendar is supplied
expect_true( is_linear_sequence(as.character(cal), calendar = as.character(cal)) )
expect_false( is_linear_sequence(as.character(gappy), calendar = as.character(cal)) )

# order does not matter
expect_true( is_linear_sequence(rev(cal), calendar = cal) )

# a report on a non-working day fails, even if every working day is present
weekend <- as.Date("2024-01-06") # Saturday, absent from the calendar
expect_false( is_linear_sequence(c(cal, weekend), calendar = cal) )

# duplicates fail
expect_false( is_linear_sequence(c(cal, cal[1]), calendar = cal) )

# missing values propagate as NA
expect_true( is.na(is_linear_sequence(as.Date(c("2024-01-01", NA, "2024-01-03"))
                                     , calendar = cal)) )

# POSIXct dates
expect_true( is_linear_sequence(as.POSIXct(cal), calendar = cal) )


## calendar given as a data.frame with a logical column ---------------------

cdf <- data.frame(
    date    = seq(as.Date("2024-01-01"), by = "day", length.out = 10)
  , working = seq(as.Date("2024-01-01"), by = "day", length.out = 10) %in% cal
)
expect_true( is_linear_sequence(cal, calendar = cdf) )
expect_false( is_linear_sequence(gappy, calendar = cdf) )
# alternative column names are accepted
cdf2 <- data.frame(date = cdf$date, is_working_day = cdf$working)
expect_true( is_linear_sequence(cal, calendar = cdf2) )


## per-group validity windows ----------------------------------------------

# Store A reports 2024-01-01..05, store B reports 2024-01-08..10. Both are
# complete within their own validity window.
stores <- c(rep("A", 5), rep("B", 3))
dates  <- cal
from   <- as.Date(c(rep("2024-01-01", 5), rep("2024-01-08", 3)))
until  <- as.Date(c(rep("2024-01-05", 5), rep("2024-01-10", 3)))

expect_true(
  is_linear_sequence(dates, by = stores, calendar = cal
                     , valid_from = from, valid_until = until)
)

# one record per groupwise call
expect_equivalent(
    in_linear_sequence(dates, by = stores, calendar = cal
                       , valid_from = from, valid_until = until)
  , rep(TRUE, 8)
)

# Store B drops its last working day
dates2  <- dates[-8]
stores2 <- stores[-8]
from2   <- from[-8]
until2  <- until[-8]
expect_equivalent(
    in_linear_sequence(dates2, by = stores2, calendar = cal
                       , valid_from = from2, valid_until = until2)
  , c(rep(TRUE, 5), FALSE, FALSE)
)

# A scalar window applies to every group; here A lacks 08-10, so it fails
expect_false(
  is_linear_sequence(dates, by = stores, calendar = cal
                     , valid_from = as.Date("2024-01-01")
                     , valid_until = as.Date("2024-01-10"))
)

# A report before the validity window fails
dates3 <- dates
dates3[1] <- as.Date("2023-12-29") # a working day, but before valid_from
expect_false(
  is_linear_sequence(dates3, by = stores, calendar = cal
                     , valid_from = from, valid_until = until)
)

# validity that varies within a single group is an error
bad_from <- from
bad_from[2] <- as.Date("2024-01-02")
expect_error(
  in_linear_sequence(dates, by = stores, calendar = cal
                     , valid_from = bad_from, valid_until = until)
  , pattern = "varies within group"
)

# empty input yields an empty result, not an error
expect_equal( in_linear_sequence(as.Date(character(0)), calendar = cal), logical(0) )

# begin/end are accepted as aliases for valid_from/valid_until
expect_true(
  is_linear_sequence(cal, calendar = cal, begin = as.Date("2024-01-01")
                     , end = as.Date("2024-01-10"))
)

# a calendar on a numeric vector is rejected rather than silently ignored
expect_error( is_linear_sequence(1:5, calendar = cal), pattern = "date-like" )
expect_error( in_linear_sequence(1:5, calendar = cal), pattern = "date-like" )


## reporting_gaps materializes absent dates and classifies problems ----------

dates <- as.Date(c(
    "2024-01-01", "2024-01-02", "2024-01-03"                # store A: complete
  , "2024-01-08", "2024-01-08", "2024-01-09", "2024-01-13"  # B: dup + Saturday
))
stores <- c(rep("A", 3), rep("B", 4))
vf     <- as.Date(c("2024-01-01", "2024-01-08"))[match(stores, c("A", "B"))]

g <- reporting_gaps(dates, calendar = cal, by = stores
                    , valid_from = vf, valid_until = as.Date("2024-01-12"))

# grouping columns + date + issue
expect_true( all(c("by", "date", "issue") %in% names(g)) )
expect_true( inherits(g$date, "Date") )

# store A reports 01-01..01-03; its window 01-01..01-12 holds the working days
# 01-01..05 and 01-08..10, so 01-04, 01-05, 01-08, 01-09, 01-10 are synthesized
expect_equal( sum(g$by == "A" & g$issue == "missing"), 5 )

# store B: the duplicated working day is listed twice
expect_equal( sum(g$issue == "duplicate"), 2 )
expect_equal( unique(g$date[g$issue == "duplicate"]), as.Date("2024-01-08") )

# store B's Saturday report is a non-working-day report
expect_equal( sum(g$issue == "non-working-day"), 1 )

# a date may be missing for one group and duplicated for another (01-08 here)
dup_day <- g$date[g$issue == "duplicate"]
expect_false( any(g$by == "B" & g$issue == "missing" & g$date %in% dup_day) )
expect_true(  any(g$by == "A" & g$issue == "missing" & g$date %in% dup_day) )

# the same working day reported once, on a working day, inside the window,
# does not appear
cal2 <- as.Date(c("2024-01-01","2024-01-02","2024-01-03"))
expect_equal( nrow(reporting_gaps(cal2, calendar = cal2)), 0L )
expect_true( inherits(reporting_gaps(cal2, calendar = cal2)$date, "Date") )

# a non-working-day report inside the validity window is classified as such
cal3 <- as.Date(c("2024-01-01","2024-01-02","2024-01-04","2024-01-05"))
expect_equal(
    reporting_gaps(as.Date(c("2024-01-01","2024-01-02","2024-01-03","2024-01-04"))
                  , calendar = cal3
                  , valid_from = as.Date("2024-01-01")
                  , valid_until = as.Date("2024-01-04"))$issue
  , "non-working-day"
)

