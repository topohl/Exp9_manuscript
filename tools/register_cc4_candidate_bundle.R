# Record a CC4 candidate bundle that was imported before the CC4 pins and import manifest existed
# (R/cc4_candidate_bundles.R). The copy in source_data/MMMSociability_candidates/<bundle_id>/ must
# verify against its own manifest.csv, and that manifest must equal the upstream Stage 31 run's.
# A new import is recorded by its importer (tools/import_cc4_*.R); this tool is for older copies only.
#
#   Rscript tools/register_cc4_candidate_bundle.R <Stage31_run> <bundle_id> <imported_at>
#
# <imported_at> says when and in which commit the copy entered this repository.
source(file.path("R", "paths.R"))
source(repo_path("R", "cc4_candidate_bundles.R"))
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 3L)
    stop("Usage: Rscript tools/register_cc4_candidate_bundle.R <Stage31_run> <bundle_id> <imported_at>", call. = FALSE)
  pin <- cc4_candidate_register(args[1], args[2], imported_at = args[3])
  cat("Recorded CC4 candidate bundle ", pin$bundle_id, " (", pin$files, " files, manifest ", substr(pin$manifest_sha256, 1, 12), ")\n", sep = "")
}
