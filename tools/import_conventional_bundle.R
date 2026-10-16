#!/usr/bin/env Rscript

# Importer for the SISanalyzer conventional-tests figure bundle (ctp).
#
# The bundle (conventional behaviour tests, physiology, heterogeneity, exploratory locomotion) is owned by SISanalyzer.
# Its exporter, manuscript_bundle/export_bundle.R, is the only writer of the bundle: a versioned, hash-listed directory
#   Analysis/SIS_Analysis/bundles/<bundle_id>/
# registered in BUNDLE_REGISTRY.csv next to it.
#
# This tool is run by hand. Through ctp_import() (R/conventional_bundle.R) it refuses a bundle that is not registered
# exactly once as FROZEN, whose manifest hash differs from the registry, whose files differ from their manifest, that
# carries unlisted files or lacks a required table, or whose provenance is not FROZEN or not from a clean tree; copies it,
# unchanged, into source_data/SISanalyzer/<bundle_id>/ (never over an existing copy); pins it in
# config/conventional_bundle.yml and records every file hash in provenance/source_manifests/conventional_bundle_manifest.csv.
#
# Usage (from the repository root):
#   Rscript tools/import_conventional_bundle.R <bundles directory> <bundle_id>
#   e.g. Rscript tools/import_conventional_bundle.R S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/bundles ctp_v1_20261016_e276430

suppressWarnings(source(file.path("R", "paths.R")))
library(yaml)
source(repo_path("R", "conventional_bundle.R"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("usage: Rscript tools/import_conventional_bundle.R <bundles directory> <bundle_id>", call. = FALSE)
root <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
pin <- ctp_import(root, args[[2]], repo_dir = repo_root())
ctp_verify(pin)
cat("Imported and pinned conventional-tests bundle", pin$bundle_id, "->", ctp_dir(pin), "\n",
    " produced by SISanalyzer commit", substr(pin$producer_commit, 1, 7), "\n")
