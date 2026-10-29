#!/usr/bin/env Rscript

# Explicit manuscript entry point for the promoted proteomics Extended Data.
#
# Extended Data 5 to 8 (numbering of 2026-10-20): spatial validation, CA2-SLM
# QC, enrichment atlases and spatial networks; Extended Data 9 (2026-10-29): the
# stress analyses. This layer validates the declared
# panel assets, hashes them, republishes their source data into the manuscript
# namespace and assembles - exactly as it does for Figures 1 to 3. It computes
# nothing.
#
# The WGCNA and identity-location figures are withheld, as is the former
# spatial-identity panel c. Their defects are recorded in
# provenance/claims/extended_data_promotion_audit.csv and nothing was substituted for
# them.
source(file.path("R", "paths.R"))
source(repo_path("R", "manuscript_figure_utils.R"))

ALL <- c("ED_spatial_validation", "ED_ca2_slm_qc", "ED_enrichment_atlas",
         "ED_spatial_networks", "ED_stress_analyses")
# --figures=ED_stress_analyses assembles only those contract keys (e.g. a page whose
# siblings cannot be redrawn in this workspace)
sel <- sub("^--figures=", "", grep("^--figures=", commandArgs(trailingOnly = TRUE), value = TRUE))
figs <- if (length(sel)) strsplit(sel, ",", fixed = TRUE)[[1]] else ALL
if (length(setdiff(figs, ALL))) stop("not a proteomics Extended Data key: ", paste(setdiff(figs, ALL), collapse = ", "), call. = FALSE)
for (fig in figs) {
  manuscript_figure_main(fig)
}
