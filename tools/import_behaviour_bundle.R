#!/usr/bin/env Rscript

# Importer for the canonical behaviour bundle.
#
# The behavioural analysis is owned by MMMSociability. Its Stage 16b
# (Analysis/16b_canonical_behavior_bundle.R) is the only writer of the bundle:
# a versioned, hash-frozen directory under
#   <analysis_ready>/canonical/behavior_bundle/<bundle_id>/
# registered in BUNDLE_REGISTRY.csv next to it.
#
# This tool is run by hand. It:
#   * refuses a bundle that is not registered FROZEN, whose manifest hash differs
#     from the registry, or whose files differ from its manifest;
#   * copies the bundle, unchanged, into source_data/MMMSociability/<bundle_id>/
#     (never over an existing copy: an imported bundle is immutable);
#   * pins it in config/behaviour_bundle.yml and records every file hash in
#     provenance/source_manifests/behaviour_bundle_manifest.csv.
# Nothing at render time reads the upstream path; renderers read the pinned copy
# through R/behaviour_bundle.R.
#
# Usage:
#   MMM_BEHAVIOUR_BUNDLE_ROOT=<.../analysis_ready/canonical/behavior_bundle> \
#     Rscript tools/import_behaviour_bundle.R <bundle_id>

suppressWarnings(source(file.path("R", "paths.R")))
library(yaml)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("usage: Rscript tools/import_behaviour_bundle.R <bundle_id>", call. = FALSE)
id <- args[[1]]
root <- Sys.getenv("MMM_BEHAVIOUR_BUNDLE_ROOT", unset = "")
if (!nzchar(root) || !dir.exists(root))
  stop("Set MMM_BEHAVIOUR_BUNDLE_ROOT to MMMSociability's analysis_ready/canonical/behavior_bundle.", call. = FALSE)
sha <- function(p) unname(tools::sha256sum(p))

reg <- utils::read.csv(file.path(root, "BUNDLE_REGISTRY.csv"), stringsAsFactors = FALSE)
row <- reg[reg$bundle_id == id, , drop = FALSE]
if (nrow(row) != 1L || !identical(row$status, "FROZEN"))
  stop("Bundle ", id, " is not registered exactly once as FROZEN.", call. = FALSE)
src <- file.path(root, id)
man_path <- file.path(src, "00_manifest.csv")
if (!identical(sha(man_path), row$manifest_sha256))
  stop("The bundle manifest does not match its registry hash.", call. = FALSE)
man <- utils::read.csv(man_path, stringsAsFactors = FALSE)
if (any(sha(file.path(src, man$file)) != man$sha256))
  stop("A bundle file differs from its manifest.", call. = FALSE)
extra <- setdiff(list.files(src), c(man$file, "00_manifest.csv"))
if (length(extra)) stop("Unlisted files in the bundle: ", paste(extra, collapse = ", "), call. = FALSE)

dst <- repo_path("source_data", "MMMSociability", id)
if (dir.exists(dst)) stop("Bundle already imported (immutable): ", dst, call. = FALSE)
dir.create(dst, recursive = TRUE)
files <- c(man$file, "00_manifest.csv")
if (!all(file.copy(file.path(src, files), file.path(dst, files), copy.mode = FALSE)))   # tracked copy: integrity is by hash, not file mode
  stop("Copy failed.", call. = FALSE)
if (any(sha(file.path(dst, man$file)) != man$sha256) || !identical(sha(file.path(dst, "00_manifest.csv")), row$manifest_sha256))
  stop("Imported copy does not verify.", call. = FALSE)

prov <- utils::read.csv(file.path(dst, "H_provenance.csv"), stringsAsFactors = FALSE)
pv <- function(k) prov$value[prov$key == k]
yaml::write_yaml(list(
  bundle_id = id,
  manifest_sha256 = row$manifest_sha256,
  config_id = pv("config_id"), config_version = pv("config_version"), config_sha256 = pv("config_sha256"),
  mmm_git_commit = pv("mmm_git_commit"), stage29_run_commit = pv("stage29_run_commit"),
  producer = "topohl/MMMSociability Analysis/16b_canonical_behavior_bundle.R",
  imported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  importer = "tools/import_behaviour_bundle.R"), repo_path("config", "behaviour_bundle.yml"))

mp <- repo_path("provenance", "source_manifests", "behaviour_bundle_manifest.csv")
rows <- data.frame(bundle_id = id, file = files, bytes = file.size(file.path(dst, files)),
                   sha256 = sha(file.path(dst, files)), stringsAsFactors = FALSE)
if (file.exists(mp)) rows <- rbind(utils::read.csv(mp, stringsAsFactors = FALSE), rows)
utils::write.csv(rows, mp, row.names = FALSE)
cat("Imported and pinned behaviour bundle", id, "(", length(files), "files ) ->", dst, "\n")
