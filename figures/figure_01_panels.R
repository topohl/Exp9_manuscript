#!/usr/bin/env Rscript
# ================================================================
# Script: figures/figure_01_panels.R
# Stage: manuscript_figures
# Scope: global
# Consumes: the pinned canonical behaviour bundle, source_data/MMMSociability/<bundle_id>/
#           (config/behaviour_bundle.yml), through R/behaviour_bundle.R
# Produces: results/figures/manuscript/figure_01_panels/
# Notes: Rendering only. Fits no model, computes no statistic.
# ================================================================
#
# Figure 1 panels. The behavioural analysis is owned and frozen by MMMSociability
# (Stage 29 characterisation, Stage 09 registered prediction, Stage 16b bundle).
# This script plots stored values. Every printed number is declared in
# figures/figure_01_annotation_map.csv and resolved to exactly one bundle cell by
# bundle_cell(); a number that is not in the bundle cannot reach the figure.
# Plotted group means and intervals are the bundle's model-based estimates from
# the sex-stratified fits (per-sex means come only from stratified fits); points
# are the bundle's animal values; CON is shown in grey as description only.
#
# Panels (builders in R/panels/behaviour_figure1_panels.R; house style, annotation
# resolver and SVG device in R/panels/behaviour_figure_style.R):
#   a  design and timeline                                         f1_panel_design
#   b  CombZ and the RES/SUS definition (definition, not validation) f1_panel_combz
#   c  first active phase after CC1: RFID position-change rate and   f1_panel_cc1
#      shared RFID-position occupancy, per-sex RES/SUS estimates, the
#      sex-difference contrast and its P-CC1 Holm p
#   d  CC1-CC4 active-phase trajectories, sex-stratified, with the   f1_panel_trajectory
#      Q2b P-TR Holm p
#   e  early position-change rate versus later CombZ (Stage 09)     f1_panel_association
#   f  held-out prediction and its permutation null (Stage 09)       f1_panel_prediction
# Panels c, d and e use overlapping animals and data (RES/SUS are defined from
# later CombZ): they are complementary views, not independent replication.
#
# VISUAL LANGUAGE. Descends from the original behaviour main figure (closest
# surviving ancestor MMMSociability Analysis/27_candidate_recompose_behavior_main_figure.R
# at 4b0f90f): the held-out prediction panel with its permutation null, the dashed
# identity line, the circle/triangle sex encoding and the point and histogram marks.
#
# ORDER. All six panels are built a-f and then printed a-f: the jitter in b and c
# draws its seed from the global RNG stream at print time (see the library header),
# so this order is part of what makes the SVGs byte-reproducible.

suppressPackageStartupMessages({
  library(ggplot2)
})

paths_file <- if (file.exists(file.path("R", "paths.R"))) file.path("R", "paths.R") else file.path("..", "R", "paths.R")
source(paths_file)
source(repo_path("R", "behaviour_bundle.R"))

PIN <- behaviour_bundle_pin()
behaviour_bundle_verify(PIN)
OUT <- path_results("figures", "manuscript", "figure_01_panels")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

TAB <- new.env(parent = emptyenv())
tab <- function(name) {
  if (!exists(name, envir = TAB, inherits = FALSE)) assign(name, behaviour_bundle_table(name, PIN), envir = TAB)
  get(name, envir = TAB, inherits = FALSE)
}

source(repo_path("R", "behaviour_figure_style.R"))
source(repo_path("R", "behaviour_figure1_panels.R"))

# ---------------------------------------------------------------- annotation map
# Every printed number: one map row -> one stored bundle cell (bundle_cell).
AN <- bh_annotation(repo_path("figures", "figure_01_annotation_map.csv"), tables = list(ebb = tab), cell = bundle_cell)

# ---------------------------------------------------------------- build a-f, then print a-f
PANELS <- list(
  a = f1_panel_design(tab, AN),
  b = f1_panel_combz(tab, AN),
  c = f1_panel_cc1(tab, AN),
  d = f1_panel_trajectory(tab, AN),
  e = f1_panel_association(tab, AN),
  f = f1_panel_prediction(tab, AN))

# Authored at the exact box the absolute-layout assembler places (figures/figure_contract.yml), so scale = 1.
written <- vapply(names(PANELS), function(id) bh_save_svg(PANELS[[id]], file.path(OUT, paste0("figure_01", id, ".svg"))), character(1))
used <- unique(AN$map$key)
cat("Figure 1 panels ->", OUT, "\n  bundle", PIN$bundle_id, "config", PIN$config_version, PIN$config_sha256, "\n")
for (id in names(written)) cat(sprintf("  figure_01%s.svg  %7d bytes  %.0f x %.0f mm\n", id, file.size(written[[id]]), PANELS[[id]]$w_mm, PANELS[[id]]$h_mm))
cat("  every printed number resolved through figure_01_annotation_map.csv (", length(used), "keys )\n")
unused <- setdiff(used, AN$resolved())
if (length(unused)) stop("annotation keys declared but never printed: ", paste(unused, collapse = ", "), call. = FALSE)
