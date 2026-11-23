
# sample data: regions with strongly differing sample sizes
dat <- data.frame(
    region   = c(rep("north", 8), rep("south", 3), "east")
  , turnover = c(100, 200, 150, NA, 120, 130, 140, 110, 50, -10, 60, 70)
  , staff    = c(10, 12, 11, 8, 9, 10, 12, 11, 5, 6, NA, 4)
)

rules <- validator(turnover >= 0, staff >= 0, turnover >= staff)
cf <- confront(dat, rules)

# Release decision per region: release when the failure ratio is at most 5%
# and at least 6 items (rule x record) were effectively evaluated. Regions
# below the minimum effective volume are 'pending' (to be investigated).
d <- partition_decision(cf, by = dat$region, max_fail_ratio = 0.05, min_effective = 6)
d

# When confront is called with a key, the key can be used to partition.
cf <- confront(dat, rules, key = "region")
partition_decision(cf, by = "region", max_fail_ratio = 0.05, min_effective = 6)

# Rule errors are counted independently of fails and missings.
rules <- validator(turnover >= 0, nonexisting_variable > 0)
cf <- confront(dat, rules)
partition_decision(cf, dat$region, min_effective = 6)

# The decision table is directly consumable by a loading task, e.g.
# write.csv(d, "partition_decision.csv", row.names = FALSE)
