#!/usr/bin/env Rscript

# Publication integrity suite for Exp9_manuscript.
#
# Two sets of tests live under tests/testthat/, and only one of them gates.
#
# GATING. Written or verified for this repository. These must pass.
#
#   test-publication-integrity.R        bundles verify against their manifests;
#                                       the registry is coherent (10 canonical,
#                                       2 withheld); all 10 canonical assembled
#                                       artefacts hash to the frozen values;
#                                       prose figure references resolve
#   test-no-analysis-in-manuscript.R    no model fitting, no inference, no live
#                                       cross-repository path
#   test-manuscript-figure3-utils.R     panel helper behaviour, self-contained
#   test-story-v5-figure-layer.R        panel layer behaviour, self-contained
#
# PENDING REPOINT. Imported byte-identical from pRoteomics in Phase 6C. They
# assert against the live analysis results tree - results/figures/manuscript/,
# results/tables/manuscript_candidates/, config/*.yml, pipeline.yml - which
# this repository deliberately cannot see, because a live cross-repository
# dependency is exactly what the split removed. They are retained rather than
# deleted, and must be repointed onto source_data/pRoteomics/ in Phase 6D.
# Their status is recorded in the migration classification audit in the
# pRoteomics repository.
#
# Running them now reports failures that mean "this test has not been
# repointed yet", not "the publication state is wrong". Conflating the two is
# how a suite stops being believed, so they do not gate.
#
# Run everything, including the pending set:
#   Rscript tests/testthat.R --all

library(testthat)

GATING <- c(
  "test-publication-integrity.R",
  "test-no-analysis-in-manuscript.R",
  "test-manuscript-figure3-utils.R",
  "test-story-v5-figure-layer.R"
)

args <- commandArgs(trailingOnly = TRUE)
all_files <- sort(list.files("tests/testthat", pattern = "^test-.*[.]R$"))
run <- if ("--all" %in% args) all_files else GATING
pending <- setdiff(all_files, GATING)

cat("Exp9_manuscript publication integrity suite\n")
cat("===========================================\n")
cat("gating tests :", length(GATING), "\n")
cat("pending repoint:", length(pending), "(not gating)\n\n")

rows <- list()
for (f in run) {
  r <- tryCatch(
    as.data.frame(test_file(file.path("tests/testthat", f),
                            reporter = "silent", package = NULL)),
    error = function(e) data.frame(test = NA, passed = 0, failed = 0,
                                   error = TRUE, skipped = 0))
  rows[[f]] <- data.frame(file = f, pass = sum(r$passed),
                          fail = sum(r$failed), err = sum(r$error))
  cat(sprintf("%-40s pass=%-4d fail=%-3d err=%-3d\n", f,
              sum(r$passed), sum(r$failed), sum(r$error)))
}
d <- do.call(rbind, rows)

cat("\n")
cat("files:", nrow(d), " pass:", sum(d$pass),
    " fail:", sum(d$fail), " err:", sum(d$err), "\n")

if (!("--all" %in% args) && length(pending)) {
  cat("\npending repoint (Phase 6D):\n")
  writeLines(paste0("  ", pending))
}

if (sum(d$fail) == 0L && sum(d$err) == 0L) {
  cat("\nRESULT: PASS\n")
  quit(status = 0L)
}
cat("\nRESULT: FAIL\n")
quit(status = 1L)
