# Tests for provenance, verification, and replay of validation deliveries.

## helper: write a small validation script and run it as a delivery ------------
mk_script <- function(lines){
  f <- tempfile(fileext=".R")
  writeLines(lines, con=f)
  f
}

## A delivery without reference data ------------------------------------------
sf <- mk_script(c(
  'rules <- validator(height >= 0, weight >= 0)'
  , 'confront(women, rules)'
  , 'check_that(women, height/weight > 0.4)'
))
out <- run_validation_file(sf, verbose=FALSE)
prov <- provenance(out)

# A manifest is recorded and of the right class.
expect_true(inherits(prov, "provenance"))
expect_equal(prov$rules$n, 2)
expect_equal(nrow(prov$rules$table), 3)

# The rule identities are visible: name and expression.
expect_true(all(c("height >= 0","weight >= 0","height/weight > 0.4") %in% prov$rules$table$expression))

# Input identity is recorded with dimensions and content fingerprint.
expect_equal(unique(prov$inputs$nrow), 15)
expect_true(all(nzchar(prov$inputs$fingerprint)))

# No references: nothing to report, no missing versions.
expect_true(is.null(prov$references) || nrow(prov$references) == 0)
expect_true(is.null(prov$missing_reference_versions))

# The result summary has a fingerprint.
expect_true(nzchar(prov$results$fingerprint))


## Reference data WITHOUT a version is flagged with its source ----------------
sf2 <- mk_script(c(
  'ed <- women; attr(ed, "source") <- "hand-made/reference.csv"'
  , 'rules <- validator(weight == ref$weight)'
  , 'confront(women, rules, ref = ed)'
))
out2 <- run_validation_file(sf2, verbose=FALSE)
prov2 <- provenance(out2)
expect_equal(nrow(prov2$references), 1)
expect_true(prov2$references$version_missing[1])
expect_equal(nrow(prov2$missing_reference_versions), 1)
# The source of the missing version is reported (file and call).
expect_true(grepl("ref", prov2$missing_reference_versions$call[1]))


## Reference data WITH a version: version is detected -------------------------
edv <- women
attr(edv, "version") <- "2024-01"
sf3 <- mk_script(c(
  'edv <- women; attr(edv, "version") <- "2024-01"'
  , 'rules <- validator(weight == ref$weight)'
  , 'confront(women, rules, ref = edv)'
))
out3 <- run_validation_file(sf3, verbose=FALSE)
prov3 <- provenance(out3)
expect_false(prov3$references$version_missing[1])
expect_equal(prov3$references$version[1], "2024-01")


## verify_provenance: same data passes, different data fails ------------------
vsame <- verify_provenance(prov3, reference=edv, what="reference", on_mismatch="ignore")
expect_true(vsame$match)

edv_other <- women
attr(edv_other, "version") <- "2025-06"
vbad <- verify_provenance(prov3, reference=edv_other, what="reference", on_mismatch="ignore")
expect_false(vbad$match)

# ... and by default a mismatch stops.
expect_error(verify_provenance(prov3, reference=edv_other, what="reference"))


## Rules identity is stable across separate loads of the same source ---------
v_a <- validator(x > 0, y > 0)
v_b <- validator(x > 0, y > 0)
expect_equal(
    getFromNamespace("rules_identity","validate")(v_a)$fingerprint
  , getFromNamespace("rules_identity","validate")(v_b)$fingerprint
)
# ... but a changed rule yields a different identity.
v_c <- validator(x > 0, y >= 0)
expect_false(identical(
    getFromNamespace("rules_identity","validate")(v_a)$fingerprint
  , getFromNamespace("rules_identity","validate")(v_c)$fingerprint
))


## replay_validation: version is checked BEFORE the rules are re-run ----------
# A mismatching reference makes replay stop before execution.
expect_error(replay_validation(out3, reference=edv_other, verbose=FALSE))

# A matching delivery replays and reports identical results.
rep1 <- replay_validation(out3, reference=edv, verbose=FALSE)
expect_true(inherits(rep1, "validations"))
expect_true(all(attr(rep1,"replay")$comparison$match))


## Code lists pulled in by rules are identified with source and version ------
# Detection reads the literal arguments of the code-list call; it does not
# evaluate them, so no stubs need to exist.
vc <- validator(
    x %in% sdmx_codelist(endpoint="https://reg", agency_id="ESTAT"
                         , resource_id="CL_FREQ", version="2.0")
  , y %in% global_codelist("CL_SEX", version="1.1")
)
cr <- getFromNamespace("codelist_references", "validate")(vc)
expect_equal(length(cr), 2)
expect_true("CL_FREQ" %in% sapply(cr, function(z) z$resource_id))
expect_equal(cr[[1]]$version, "2.0")
# a code list without a version is flagged.
vlatest <- validator(x %in% global_codelist("CL_SEX"))
cr2 <- getFromNamespace("codelist_references", "validate")(vlatest)
expect_true(is.na(cr2[[1]]$version))


## Rules loaded from a .yaml file: the rule file itself is fingerprinted ------
yd <- tempfile(); dir.create(yd)
yamlf <- file.path(yd, "myrules.yaml")
scriptf <- file.path(yd, "validate_yml.R")
writeLines(c(
  '---'
  , 'rules:'
  , '  - name: R_wt'
  , '    expr: weight >= 0'
), yamlf)
writeLines(c(
  'rules <- validator(.file="myrules.yaml")'
  , 'confront(women, rules)'
), scriptf)
outy <- run_validation_file(scriptf, verbose=FALSE)
prov_y <- provenance(outy)
# both the validation script and the yaml rule file are checked for changes
checked <- sapply(prov_y$files, `[[`, "path")
expect_true(any(grepl("myrules.yaml$", checked)))
expect_true(any(grepl("validate_yml.R$", checked)))
expect_true(all(sapply(prov_y$files, function(f) nzchar(f$fingerprint))))
# the rule records the yaml file as its origin, the script line as its source
expect_equal(prov_y$rules$table$origin, "myrules.yaml")
expect_true(grepl("validate_yml.R:2", prov_y$rules$table$source))


## replay without reference supplied still checks rule source files ----------
rep2 <- replay_validation(out, verbose=FALSE)
expect_true(all(attr(rep2,"replay")$comparison$match))


## run_validation_dir delivers every script, not only the last one -----------
dd <- tempfile(); dir.create(dd)
writeLines('confront(women, validator(height > 0))', file.path(dd,"validate_one.R"))
writeLines('confront(women, validator(weight > 0))', file.path(dd,"validate_two.R"))
outd <- run_validation_dir(dd, verbose=FALSE)
expect_equal(length(outd), 2)
prov_d <- provenance(outd)
expect_equal(length(prov_d$scripts), 2)
expect_equal(nrow(prov_d$rules$table), 2)
repd <- replay_validation(outd, verbose=FALSE)
expect_true(all(attr(repd,"replay")$comparison$match))


## A delivery with no provenance cannot be replayed ---------------------------
fake <- structure(list(), class=c("validations","list"))
expect_error(replay_validation(fake, verbose=FALSE))
