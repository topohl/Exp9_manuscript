#!/usr/bin/env Rscript
# ================================================================
# Script: figures/figure_01_panels.R
# Stage: manuscript_figures
# Scope: global
# Consumes: the pinned canonical behaviour bundle (config/behaviour_bundle.yml) and the pinned figure-support
#           bundle (config/figure_support_bundle.yml), source_data/MMMSociability/<bundle_id>/, through
#           R/behaviour_bundle.R and R/figure_support_bundle.R
# Produces: results/figures/manuscript/figure_01_panels/
# Notes: Rendering only. Fits no model, computes no statistic.
# ================================================================
#
# Figure 1 panels, in the layout Tobias assembled by hand (2026-10-12) and now drawn from the pipelines. The
# behavioural analysis is owned and frozen by MMMSociability (Stage 29 characterisation and its release, Stage 09
# registered prediction and its sex-stratified correlations, Stage 14 domain heatmap, post hoc run 29c; bundled by
# Stages 16b and 16c). This script plots stored values. Every printed number is declared in
# figures/figure_01_annotation_map.csv and resolved to exactly one bundle cell (ebb or fsb) by bundle_cell(); a
# number that is not in a bundle cannot reach the figure.
#
# Panels (builders in R/panels/behaviour_figure1_v2_panels.R; house style, annotation resolver and SVG device in
# R/panels/behaviour_figure_style.R):
#   a  design and timeline: the SIS design and the five tests with the postnatal ages (fsb T1)  f1v2_panel_timeline
#   b  later CombZ ("Integrated resilience") and its six components, per sex (fsb F2, ebb A2b)   f1v2_panel_combz
#   c  the Stage 14 behavioural domain heatmap, Hedges g and post hoc markers (fsb D1)          f1v2_panel_domains
#   d  early movement against later CombZ per sex: Stage 09 Spearman rho and BH q (fsb E1),    f1v2_panel_movement
#      the post hoc least-squares display line and band (fsb E2, run 29c), animals (ebb A2)
#   e  held-out prediction (ebb A2, C2) and its permutation null as an inset (ebb A3, E)       f1v2_panel_prediction
# Panels b, d and e use overlapping animals (RES/SUS are defined from later CombZ): complementary views, not
# independent replication.

suppressPackageStartupMessages({
  library(ggplot2)
})

paths_file <- if (file.exists(file.path("R", "paths.R"))) file.path("R", "paths.R") else file.path("..", "R", "paths.R")
source(paths_file)
source(repo_path("R", "behaviour_bundle.R"))
source(repo_path("R", "stage30_bundle.R"))
source(repo_path("R", "figure_support_bundle.R"))

PIN <- behaviour_bundle_pin()
behaviour_bundle_verify(PIN)
FSB <- fsb_bundle_pin()
fsb_bundle_verify(FSB)
OUT <- path_results("figures", "manuscript", "figure_01_panels")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

TAB <- new.env(parent = emptyenv())
getter <- function(prefix, read) function(name) {
  k <- paste(prefix, name, sep = ":")
  if (!exists(k, envir = TAB, inherits = FALSE)) assign(k, read(name), envir = TAB)
  get(k, envir = TAB, inherits = FALSE)
}
tab <- getter("ebb", function(n) behaviour_bundle_table(n, PIN))
fsb <- getter("fsb", function(n) fsb_bundle_table(n, FSB))

for (lib in c("behaviour_figure_style.R", "behaviour_figure1_panels.R", "behaviour_s30_light_panels.R", "behaviour_figure1_nature_panels.R",
              "behaviour_figure1_option3_panels.R", "behaviour_stage14_panels.R", "behaviour_figure1_v2_panels.R"))
  source(repo_path("R", "panels", lib))

# ---------------------------------------------------------------- annotation map
# Every printed number: one map row -> one stored bundle cell (bundle_cell), in the bundle the row names.
AN <- bh_annotation(repo_path("figures", "figure_01_annotation_map.csv"), tables = list(ebb = tab, fsb = fsb), cell = bundle_cell)

# ---------------------------------------------------------------- build a-e (boxes from figures/figure_contract.yml), then print
BOX <- local({
  e <- yaml::read_yaml(repo_path("figures", "figure_contract.yml"))$figures[["01"]]
  stats::setNames(lapply(e$panels, function(p) c(w = as.numeric(p$w), h = as.numeric(p$h))), vapply(e$panels, function(p) sub("^1", "", p$id), ""))
})
PANELS <- list(
  a = f1v2_panel_timeline(fsb, AN, BOX$a[["w"]], BOX$a[["h"]]),
  b = f1v2_panel_combz(tab, fsb, AN, BOX$b[["w"]], BOX$b[["h"]]),
  c = f1v2_panel_domains(fsb, AN, BOX$c[["w"]], BOX$c[["h"]]),
  d = f1v2_panel_movement(tab, fsb, AN, BOX$d[["w"]], BOX$d[["h"]]),
  e = f1v2_panel_prediction(tab, AN, BOX$e[["w"]], BOX$e[["h"]]))

# Authored at the exact box the absolute-layout assembler places (figures/figure_contract.yml), so scale = 1.
written <- vapply(names(PANELS), function(id) bh_save_svg(PANELS[[id]], file.path(OUT, paste0("figure_01", id, ".svg"))), character(1))
used <- unique(AN$map$key)
cat("Figure 1 panels ->", OUT, "\n  bundles", PIN$bundle_id, "(config", PIN$config_version, PIN$config_sha256, ")", FSB$bundle_id, "\n")
for (id in names(written)) cat(sprintf("  figure_01%s.svg  %7d bytes  %.0f x %.0f mm\n", id, file.size(written[[id]]), PANELS[[id]]$w_mm, PANELS[[id]]$h_mm))
cat("  every printed number resolved through figure_01_annotation_map.csv (", length(used), "keys )\n")
unused <- setdiff(used, AN$resolved())
if (length(unused)) stop("annotation keys declared but never printed: ", paste(unused, collapse = ", "), call. = FALSE)

# ---------------------------------------------------------------- render record
# Which commit drew these panels, whether that code was clean, and the hash of every panel, so that
# tools/promote_manuscript_render.R figure_01 can refuse panels drawn from other or uncommitted code.
code <- git_code_state()
record_dir <- path_results("logs", "manuscript_figures", "figure_01_panels")
dir.create(record_dir, recursive = TRUE, showWarnings = FALSE)
yaml::write_yaml(list(
  timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  producer = "figures/figure_01_panels.R",
  behaviour_bundle_id = PIN$bundle_id, behaviour_bundle_manifest_sha256 = PIN$manifest_sha256,
  figure_support_bundle_id = FSB$bundle_id, figure_support_bundle_manifest_sha256 = FSB$manifest_sha256,
  render_git_commit = code$commit, render_code_clean = code$code_clean, render_code_dirty = code$dirty,
  files = lapply(unname(written), function(x) list(path = relative_to(x), sha256 = unname(tools::sha256sum(x))))),
  file.path(record_dir, "render_record.yml"))
cat("  render record", relative_to(file.path(record_dir, "render_record.yml")), "at", substr(code$commit, 1, 7),
    if (isTRUE(code$code_clean)) "(clean code)\n" else "(UNCOMMITTED CODE)\n")
