#!/usr/bin/env Rscript

# Build stats.xlsx in the paper folder: the statistics of the manuscript, one sheet per figure, then the numbers
# quoted only in the text, sensitivity analyses, other registered and post hoc analyses, and group sizes.
#
#   Rscript tools/build_stats.R             write <paper folder>/stats.xlsx (refuses uncommitted changes)
#   Rscript tools/build_stats.R --dry-run   resolve every row and print the counts per sheet, write nothing
#   Rscript tools/build_stats.R --out FILE  write FILE instead
#
# Every row is declared in figures/contracts/stats_map.csv and read from one stored cell (or one filtered table)
# of the pinned bundles or the promoted per-panel source data (R/workbooks.R); nothing is computed. The paper
# folder is outside this repository (paper_path() in R/paths.R; E9_PAPER_DIR).

suppressWarnings(source(file.path("R", "paths.R")))
source(repo_path("R", "workbooks.R"))

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
out <- { i <- which(args == "--out"); if (length(i)) args[i[1] + 1L] else NULL }

map <- utils::read.csv(repo_path("figures", "contracts", "stats_map.csv"), stringsAsFactors = FALSE,
                       colClasses = "character", na.strings = "")
tables <- stats_tables(map)
for (s in names(tables)) cat(sprintf("%-22s %4d rows\n", s, nrow(tables[[s]])))
if (dry_run) { cat("\n[DRY-RUN] nothing written\n"); quit(status = 0L) }

code <- git_code_state()
if (is.null(out) && !isTRUE(code$code_clean))
  stop("uncommitted changes; commit first (a build records the commit it was made from)", call. = FALSE)
out <- out %||% file.path(paper_path(), "stats.xlsx")
wb_stats(tables, out, list(commit = code$commit, clean = code$code_clean))
cat("\nstats ->", out, "\n")
