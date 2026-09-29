#!/usr/bin/env Rscript

# Importer for the canonical Stage 30 figure bundle.
#
# Stage 30 (the exploratory screen) is owned by MMMSociability. Its figure-bundle
# exporter (Analysis/30b_stage30_figure_bundle.R) is the only writer of the bundle:
# a versioned, hash-frozen directory under
#   <analysis_ready>/canonical/stage30_figure_bundle/<bundle_id>/
# registered in BUNDLE_REGISTRY.csv next to it.
#
# This tool is run by hand. Through stage30_bundle_import() (R/stage30_bundle.R) it:
#   * refuses a bundle that is not registered exactly once as FROZEN, whose
#     manifest hash differs from the registry, whose files differ from its
#     manifest, that carries unlisted files, whose H_provenance is not FROZEN or
#     disagrees with the registry, or that was built from a Stage 29 bundle other
#     than the pinned behaviour bundle (config/behaviour_bundle.yml);
#   * copies the bundle, unchanged, into source_data/MMMSociability/<bundle_id>/
#     (never over an existing copy: an imported bundle is immutable);
#   * pins it in config/stage30_bundle.yml and records every file hash in
#     provenance/source_manifests/stage30_bundle_manifest.csv.
# Nothing at render time reads the upstream path; renderers read the pinned copy
# through R/stage30_bundle.R.
#
# Usage (from the repository root):
#   MMM_STAGE30_BUNDLE_ROOT=<.../analysis_ready/canonical/stage30_figure_bundle> \
#     Rscript tools/import_stage30_bundle.R <bundle_id>

suppressWarnings(source(file.path("R", "paths.R")))
library(yaml)
source(repo_path("R", "behaviour_bundle.R"))
source(repo_path("R", "stage30_bundle.R"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("usage: Rscript tools/import_stage30_bundle.R <bundle_id>", call. = FALSE)
id <- args[[1]]
root <- Sys.getenv("MMM_STAGE30_BUNDLE_ROOT", unset = "")
if (!nzchar(root) || !dir.exists(root))
  stop("Set MMM_STAGE30_BUNDLE_ROOT to MMMSociability's analysis_ready/canonical/stage30_figure_bundle.", call. = FALSE)

bpin <- behaviour_bundle_pin()
behaviour_bundle_verify(bpin)
pin <- stage30_bundle_import(root, id, repo_dir = repo_root(), behaviour_pin = bpin)
stage30_bundle_verify(pin)
cat("Imported and pinned Stage 30 bundle", id, "->", stage30_bundle_dir(pin), "\n",
    " built from Stage 29 bundle", pin$stage29_bundle_id, "(matches the behaviour pin)\n")
