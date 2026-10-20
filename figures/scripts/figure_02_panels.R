#!/usr/bin/env Rscript
# Editorial-v8. Ninth parallel layer; canonical and Parts 16-23 untouched.
# Final polish plus a VECTOR-PRESERVING PDF export.
source(file.path("R", "paths.R"))
source(repo_path("R", "integration_utils.R"))
source(repo_path("R", "proteomics_style.R"))
source(repo_path("R", "proteomics_panels_core.R"))
source(repo_path("R", "proteomics_panels_programmes.R"))
source(repo_path("R", "proteomics_panels_ed_support.R"))
source(repo_path("R", "spatial_grammar_utils.R"))
source(repo_path("R", "proteomics_panels_anatomy_bridge.R"))
source(repo_path("R", "proteomics_panels_spatial_ed.R"))
source(repo_path("R", "proteomics_panels_wgcna.R"))
source(repo_path("R", "proteomics_panels_figure2.R"))
source(repo_path("R", "proteomics_panels_figure3_extra.R"))
source(repo_path("R", "proteomics_panels_main.R"))
source(repo_path("R", "proteomics_panels_extended_data.R"))
source(repo_path("R", "proteomics_panels_revised.R"))
source(repo_path("R", "proteomics_page_export.R"))
source(repo_path("R", "proteomics_page_builder.R"))
suppressPackageStartupMessages({ library(readr); library(dplyr); library(ggplot2); library(patchwork); library(scales); library(grid) })
Sys.setenv(PROTEOMICS_SCRIPT_ID = "figures/scripts/figure_02_panels.R")
args <- commandArgs(trailingOnly = TRUE)
res <- s9f_build("figure_02", dry_run = "--dry-run" %in% args || is_dry_run())
if (!is.null(res)) {
  cat("\n===== Editorial-v8 figure_02 =====\n")
  print(res$panels[, c("panel_id", "box_w_mm", "box_h_mm", "status")], row.names = FALSE)
}
