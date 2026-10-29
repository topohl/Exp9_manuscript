#!/usr/bin/env Rscript

# Build the source data of every published figure as Excel workbooks in the paper folder:
# <paper folder>/source_data/<id>.xlsx (figure_01.xlsx ... extended_data_10.xlsx), one sheet per panel holding
# the values the panel plots, and an 'about' sheet (title, panels, analysis and render commits, build commit).
#
#   Rscript tools/build_source_data.R             build all (refuses uncommitted changes)
#   Rscript tools/build_source_data.R --dry-run   check every figure's per-panel tables, write nothing
#   Rscript tools/build_source_data.R --out DIR   write to DIR instead of the paper folder
#
# Input: the promoted per-panel tables figures/main|extended_data/source_data/<id>/ with their 00_manifest.csv
# (tools/promote_manuscript_render.R); each is checked against its SHA-256 first. Withheld figures are skipped.
# Nothing is computed. The paper folder is outside this repository (paper_path() in R/paths.R; E9_PAPER_DIR).

suppressWarnings(source(file.path("R", "paths.R")))
source(repo_path("R", "workbooks.R"))

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
out_dir <- { i <- which(args == "--out"); if (length(i)) args[i[1] + 1L] else NULL }

reg <- utils::read.csv(repo_path("provenance", "publication_registry", "canonical_publication_registry.csv"),
                       stringsAsFactors = FALSE, colClasses = "character")
reg <- reg[reg$status == "CANONICAL", , drop = FALSE]
contract <- yaml::read_yaml(repo_path("figures", "contracts", "figure_contract.yml"))$figures
code <- git_code_state()

# Where a figure's numbers were computed, for the 'about' sheet. The registry's source_commit is the render commit
# for figures drawn here, so it is not used for them: a SISanalyzer figure names its pinned bundle and producer
# commit; a figure drawn here from pRoteomics exports names the pRoteomics commits recorded for the files its
# contract entry reads (render inputs, source_data/pRoteomics imports and the primary-tree imports of
# provenance/source_manifests/primary_inputs.csv).
sd_analysis_origin <- function(row, entry) {
  if (identical(row$originating_analysis, "SISanalyzer")) {
    pin <- yaml::read_yaml(repo_path("config", "conventional_bundle.yml"))
    return(list(analysis = paste0("SISanalyzer, bundle ", pin$bundle_id), commit = pin$producer_commit))
  }
  if (!identical(row$originating_analysis, "this repository"))
    return(list(analysis = row$originating_analysis, commit = row$source_commit))
  paths <- unique(unlist(lapply(entry$panels, function(p) unlist(p[c("figure_source", "primary_source", "input_dependencies")]))))
  ri <- utils::read.csv(repo_path("provenance", "source_manifests", "render_inputs_manifest.csv"), colClasses = "character")
  pr <- utils::read.csv(repo_path("source_data", "pRoteomics", "manifest.csv"), colClasses = "character")
  primary <- utils::read.csv(repo_path("provenance", "source_manifests", "primary_inputs.csv"), colClasses = "character")
  dirs <- unique(sub("^source_data/pRoteomics/([^/]+)/.*$", "\\1", grep("^source_data/pRoteomics/", paths, value = TRUE)))
  commits <- unique(c(ri$source_commit[ri$declared_path %in% paths], pr$source_commit[pr$publication_id %in% dirs],
                      primary$source_commit[primary$declared_path %in% paths]))
  if (!length(commits)) stop(row$publication_id, ": no pRoteomics commit recorded for its inputs", call. = FALSE)
  list(analysis = "topohl/pRoteomics (frozen exports; figure drawn by Exp9_manuscript)", commit = paste(commits, collapse = ", "))
}

missing <- character()
for (i in seq_len(nrow(reg))) {
  sd <- repo_path(reg$canonical_source_data[i])
  if (!startsWith(reg$canonical_source_data[i], "figures/") || !file.exists(file.path(sd, "00_manifest.csv")))
    missing <- c(missing, reg$publication_id[i])
}
if (length(missing))
  stop("no promoted per-panel source data for: ", paste(missing, collapse = ", "),
       " (render, then tools/promote_manuscript_render.R)", call. = FALSE)
if (!dry_run && is.null(out_dir) && !isTRUE(code$code_clean))
  stop("uncommitted changes; commit first (a build records the commit it was made from)", call. = FALSE)
if (!dry_run) out_dir <- out_dir %||% paper_path("source_data")

for (i in seq_len(nrow(reg))) {
  pid <- reg$publication_id[i]
  entry <- contract[[reg$contract_key[i]]]
  origin <- sd_analysis_origin(reg[i, ], entry)
  info <- list(title = reg$title[i], analysis = origin$analysis, source_commit = origin$commit,
               panels = stats::setNames(lapply(entry$panels, function(p) gsub("\\s+", " ", as.character(p$reader %||% p$description %||% ""))),
                                        vapply(entry$panels, function(p) as.character(p$id), "")))
  sd <- repo_path(reg$canonical_source_data[i])
  got <- wb_panel_tables(sd)
  if (dry_run) {
    cat(sprintf("%-18s %2d panels  %6d rows  ok\n", pid, nrow(got$manifest), sum(as.integer(got$manifest$rows))))
    next
  }
  f <- file.path(out_dir, paste0(pid, ".xlsx"))
  wb_source_data(pid, sd, f, info, list(commit = code$commit, clean = code$code_clean))
  cat(sprintf("%-18s -> %s\n", pid, f))
}
if (dry_run) cat("\n[DRY-RUN] nothing written\n")
