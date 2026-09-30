#!/usr/bin/env Rscript

# Importer for the canonical figure-support bundle (fsb).
#
# The figure-support bundle (descriptive CON cage means and the CombZ components as they
# enter CombZ) is owned by MMMSociability. Its exporter
# (Analysis/16c_figure_support_bundle.R) is the only writer of the bundle: a versioned,
# hash-frozen directory under
#   <analysis_ready>/canonical/figure_support_bundle/<bundle_id>/
# registered in BUNDLE_REGISTRY.csv next to it.
#
# This tool is run by hand. Through fsb_bundle_import() (R/figure_support_bundle.R) it:
#   * refuses a bundle that is not registered exactly once as FROZEN, whose manifest hash
#     differs from the registry, whose files differ from its manifest, that carries unlisted
#     files or lacks a required table, whose H_provenance is not FROZEN, disagrees with the
#     registry or declares more than the descriptive recomputation, or that was built from a
#     Stage 29 bundle other than the pinned behaviour bundle (config/behaviour_bundle.yml);
#   * copies the bundle, unchanged, into source_data/MMMSociability/<bundle_id>/
#     (never over an existing copy: an imported bundle is immutable);
#   * pins it in config/figure_support_bundle.yml and records every file hash in
#     provenance/source_manifests/figure_support_bundle_manifest.csv.
# Nothing at render time reads the upstream path; renderers read the pinned copy through
# R/figure_support_bundle.R.
#
# Usage (from the repository root):
#   MMM_FIGURE_SUPPORT_BUNDLE_ROOT=<.../analysis_ready/canonical/figure_support_bundle> \
#     Rscript tools/import_figure_support_bundle.R <bundle_id>

suppressWarnings(source(file.path("R", "paths.R")))
library(yaml)
source(repo_path("R", "behaviour_bundle.R"))
source(repo_path("R", "stage30_bundle.R"))
source(repo_path("R", "figure_support_bundle.R"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("usage: Rscript tools/import_figure_support_bundle.R <bundle_id>", call. = FALSE)
id <- args[[1]]
root <- Sys.getenv("MMM_FIGURE_SUPPORT_BUNDLE_ROOT", unset = "")
if (!nzchar(root) || !dir.exists(root))
  stop("Set MMM_FIGURE_SUPPORT_BUNDLE_ROOT to MMMSociability's analysis_ready/canonical/figure_support_bundle.", call. = FALSE)

bpin <- behaviour_bundle_pin()
behaviour_bundle_verify(bpin)
pin <- fsb_bundle_import(root, id, repo_dir = repo_root(), behaviour_pin = bpin)
fsb_bundle_verify(pin)
cat("Imported and pinned figure-support bundle", id, "->", fsb_bundle_dir(pin), "\n",
    " built from Stage 29 bundle", pin$stage29_bundle_id, "(matches the behaviour pin)\n")
