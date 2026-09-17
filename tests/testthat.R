#!/usr/bin/env Rscript

# Publication integrity suite for Exp9_manuscript.
#
# Every test file in tests/testthat/ gates. There is no excluded set.
#
# Phase 6C imported sixteen renderer tests that could not run here, because
# they addressed the pRoteomics results tree. Phase 6D closed that gap rather
# than working around it:
#
#   * the panel libraries' own dependencies were completed - spatial grammar
#     moved here because nothing in pRoteomics used it, and nine genuinely
#     shared libraries were vendored byte-identical under R/vendor/ with a
#     manifest, all verified to contain zero inference calls;
#   * the render inputs the contracts declare were imported into this
#     repository's own gitignored results/ workspace at the paths the
#     contracts name, so no renderer and almost no test had to be rewritten;
#   * repo_path() learned the single-argument form repo_path("R/x.R"), so
#     callers that iterate repo-relative file lists resolve through the domain
#     layout like everyone else;
#   * assertions that were about the pRoteomics tree were removed with named
#     replacement coverage there, and assertions about scientific content were
#     delegated to the repository that owns it.
#
# The workspace is rebuilt by:
#   PROTEOMICS_ROOT=/path/to/proteomics Rscript tools/import_render_inputs.R
#
# That tool is the only thing in this repository that ever touches a sibling
# path, it is run by hand, and what the suite verifies against afterwards is
# the tracked manifest in provenance/source_manifests/.

library(testthat)

args <- commandArgs(trailingOnly = TRUE)
files <- sort(list.files("tests/testthat", pattern = "^test-.*[.]R$"))

cat("Exp9_manuscript publication integrity suite\n")
cat("===========================================\n")
cat("test files:", length(files), " (no excluded set)\n\n")

rows <- list()
for (f in files) {
  r <- tryCatch(
    as.data.frame(test_file(file.path("tests/testthat", f),
                            reporter = "silent", package = NULL)),
    error = function(e) data.frame(test = NA, passed = 0, failed = 0,
                                   error = TRUE, skipped = 0))
  rows[[f]] <- data.frame(file = f, pass = sum(r$passed), fail = sum(r$failed),
                          err = sum(r$error), skip = sum(r$skipped))
  cat(sprintf("%-44s pass=%-5d fail=%-3d err=%-3d skip=%-3d\n", f,
              sum(r$passed), sum(r$failed), sum(r$error), sum(r$skipped)))
}
d <- do.call(rbind, rows)

cat("\n")
cat("files  :", nrow(d), "\n")
cat("passed :", sum(d$pass), "\n")
cat("failed :", sum(d$fail), "\n")
cat("errors :", sum(d$err), "\n")
cat("skipped:", sum(d$skip), "\n")

if (sum(d$skip) > 0L) {
  cat("\nfiles reporting skips:\n")
  s <- d[d$skip > 0L, ]
  for (i in seq_len(nrow(s))) cat(sprintf("  %-44s %d\n", s$file[i], s$skip[i]))
}

if (sum(d$fail) == 0L && sum(d$err) == 0L) {
  cat("\nRESULT: PASS\n")
  quit(status = 0L)
}
cat("\nRESULT: FAIL\n")
b <- d[d$fail > 0L | d$err > 0L, ]
for (i in seq_len(nrow(b))) {
  cat(sprintf("  %-44s fail=%d err=%d\n", b$file[i], b$fail[i], b$err[i]))
}
quit(status = 1L)
