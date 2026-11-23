## Shared composite-key checks for shard-wise validated data ----

# A report of orders, delivered in separate files. Within each file the
# order numbers are unique, but across files there are duplicates.
part1 <- data.frame(order_id = c("A1","A2","A3"), amount = c(10,20,30))
part2 <- data.frame(order_id = c("A4","A5","A6"), amount = c(40,50,60))
part3 <- data.frame(order_id = c("A3","A7","A1"), amount = c(70,80,90))

rules <- validator(amount >= 0, is_unique(order_id))

cf1 <- confront(part1, rules, key="order_id")
cf2 <- confront(part2, rules, key="order_id")
cf3 <- confront(part3, rules, key="order_id")

# each file passes the within-file uniqueness check
expect_true(all(cf1))
expect_true(all(cf2))
expect_true(all(cf3))

store <- key_store()
expect_silent(register_keys(store, "part1.csv", keyset(cf1)))
expect_silent(register_keys(store, "part2.csv", keyset(cf2)))
expect_silent(register_keys(store, "part3.csv", keyset(cf3)))

# aggregation finds the cross-file duplicates, for each file involved
dups <- shared_duplicates(store)
expect_equal(names(dups), c("order_id","shard"))
expect_equal(dups$order_id, c("A1","A1","A3","A3"))
expect_equal(dups$shard, c("part1.csv","part3.csv","part1.csv","part3.csv"))

# flagging adds a rule to each file's own validation results
cf1 <- flag_shared_keys(cf1, store)
cf2 <- flag_shared_keys(cf2, store)
cf3 <- flag_shared_keys(cf3, store)

expect_true("shared_key_unique" %in% names(cf1))
expect_equal(summary(cf1)$fails[names(cf1) == "shared_key_unique"], 2)
expect_equal(summary(cf2)$fails[names(cf2) == "shared_key_unique"], 0)
expect_equal(summary(cf3)$fails[names(cf3) == "shared_key_unique"], 2)
expect_equivalent(values(cf1, simplify=FALSE)$shared_key_unique, c(FALSE,TRUE,FALSE))
expect_equivalent(values(cf3, simplify=FALSE)$shared_key_unique, c(FALSE,TRUE,FALSE))
# the flagged records are the ones with cross-file duplicate keys
expect_equal(keyset(cf1)$order_id[!values(cf1, simplify=FALSE)$shared_key_unique], c("A1","A3"))

# the appended rule shows up in derived results
expect_true("shared_key_unique" %in% rownames(aggregate(cf1, by="rule")))
expect_true("shared_key_unique" %in% as.data.frame(cf1)$name)

## composite keys ----
p1 <- data.frame(order_id=c("A1","A1","A2"), line_nr=c(1,2,1), amount=1:3)
p2 <- data.frame(order_id=c("A2","A3"),       line_nr=c(1,1),   amount=4:5)
r2 <- validator(amount >= 0, is_unique(order_id, line_nr))
c1 <- confront(p1, r2, key=c("order_id","line_nr"))
c2 <- confront(p2, r2, key=c("order_id","line_nr"))

store2 <- key_store()
register_keys(store2, "p1.csv", keyset(c1))
register_keys(store2, "p2.csv", keyset(c2))
d2 <- shared_duplicates(store2)
expect_equal(nrow(d2), 2)
expect_equal(d2$order_id, c("A2","A2"))
expect_equal(d2$line_nr, c(1,1))
# (A1,1) occurs twice within p1 only: not a shared duplicate
expect_false(any(d2$order_id == "A1"))
c1 <- flag_shared_keys(c1, store2)
expect_equivalent(values(c1, simplify=FALSE)$shared_key_unique, c(TRUE,TRUE,FALSE))

## load batches are independent of each other ----
batch1 <- key_store()
batch2 <- key_store()
register_keys(batch1, "x.csv", data.frame(id="K1"))
register_keys(batch2, "y.csv", data.frame(id="K1"))
expect_equal(nrow(shared_duplicates(batch1)), 0)
expect_equal(nrow(shared_duplicates(batch2)), 0)
register_keys(batch1, "z.csv", data.frame(id="K1"))
expect_equal(nrow(shared_duplicates(batch1)), 2)
expect_equal(nrow(shared_duplicates(batch2)), 0)

## after fixing one file, correct results can be regenerated ----
part3_fixed <- data.frame(order_id = c("A8","A7","A9"), amount = c(70,80,90))
cf3 <- confront(part3_fixed, rules, key="order_id")
# re-registering a shard replaces its keys
expect_silent(register_keys(store, "part3.csv", keyset(cf3)))
expect_equal(nrow(shared_duplicates(store)), 0)
# re-flagging replaces the previously added rule
cf1 <- flag_shared_keys(cf1, store)
cf3 <- flag_shared_keys(cf3, store)
expect_equal(sum(names(cf1) == "shared_key_unique"), 1)
expect_true(all(values(cf1, simplify=FALSE)$shared_key_unique))
expect_true(all(values(cf3, simplify=FALSE)$shared_key_unique))

## in_shared_duplicates ----
store3 <- key_store()
register_keys(store3, "a.csv", data.frame(id=c("K1","K2")))
register_keys(store3, "b.csv", data.frame(id=c("K2","K3")))
expect_equal( in_shared_duplicates(store3, data.frame(id=c("K1","K2","K3","K4")))
            , c(FALSE,TRUE,FALSE,FALSE) )

## edge cases ----
# empty store
empty <- key_store()
expect_equal(nrow(shared_duplicates(empty)), 0)
expect_equal(in_shared_duplicates(empty, data.frame(id="x")), FALSE)
cf_empty <- flag_shared_keys(confront(part1, rules, key="order_id"), empty)
expect_true(all(values(cf_empty, simplify=FALSE)$shared_key_unique))

# zero-row shard
expect_silent(register_keys(empty, "zero.csv", data.frame(order_id=character(0))))
register_keys(empty, "one.csv", data.frame(order_id="A1"))
expect_equal(nrow(shared_duplicates(empty)), 0)

# duplicates within a single shard only are not shared duplicates
solo <- key_store()
register_keys(solo, "only.csv", data.frame(id=c("K","K")))
expect_equal(nrow(shared_duplicates(solo)), 0)

# keys can be registered as named list or confrontation object
lst <- key_store()
register_keys(lst, "l1.csv", list(id="K1"))
register_keys(lst, "l2.csv", confront(data.frame(id="K1"), validator(id %in% c("K1","K2")), key="id"))
expect_equal(nrow(shared_duplicates(lst)), 2)

## error handling ----
expect_error(register_keys(store, "bad.csv", data.frame(wrong=1)))
expect_error(register_keys("notastore", "x.csv", data.frame(order_id=1)))
expect_error(register_keys(store, c("a","b"), data.frame(order_id=1)))
expect_error(register_keys(store, "x.csv", data.frame(1)))
expect_error(flag_shared_keys("notacf", store))
expect_error(flag_shared_keys(confront(part1, rules), store)) # no key= in confront
expect_error(flag_shared_keys(cf1, "notastore"))
expect_error(in_shared_duplicates(store, data.frame(wrong=1)))
