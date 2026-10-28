#!/usr/bin/env Rscript
# Import the proteomic figure inputs that differ in the regenerated primary analysis of pRoteomics (original centring,
# Analysis/proteomics_original, rebuilt on the corrected pipeline in October 2026). Every input the proteomic figures read
# was compared with its counterpart there (provenance/source_manifests/primary_input_check.csv): all others are byte- or
# value-identical, have no producer in the regenerated tree, or feed no promoted panel, and stay as imported. These come
# from the primary tree:
#   Figure 2d        marker classes per acquisition and animal         -> results/ render workspace (declared path)
#   Figure 2f-2i     GO programmes of regions and layers (selections)  -> results/ render workspace (declared path)
#   Figure 2j-2l     input territories, afferent sources, inhibitory share -> results/ render workspace (declared path)
#   Figure 3b-g      selection inventories                             -> source_data/pRoteomics/supplementary_selection_inventories
#   Ext. Data Fig. 7 theme-assignment table                            -> results/ render workspace (over the size ceiling)
# (Figure 2d and the GO panels, 2f-2i since 2026-10-28, were added on 2026-10-27; the former Figure 2h GO candidate left with its panel;
# Figure 2j-2l on 2026-10-28.) A results/spatial_validation table is imported only if it is the table its producing run wrote
# (SHA-256 in that run's manifest, where the run records it), with committed code that is unchanged at the current revision.
# The tree must hold the approved primary centring (config/normalisation.yml). Each import is recorded in
# provenance/source_manifests/primary_inputs.csv (declared path, source path, source commit, SHA-256, previous SHA-256),
# which the import-boundary tests and tools/import_proteomics_statistics.R read; the bundle manifest rows of the
# inventories are replaced by the primary tree's export rows. Re-running with unchanged inputs changes nothing.
# Usage (repository root): Rscript tools/import_primary_inputs.R <primary tree root, short path, e.g. R:/>

source(file.path("R", "paths.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !file.exists(file.path(args[[1]], "pipeline.yml")))
  stop("usage: Rscript tools/import_primary_inputs.R <pRoteomics primary tree root>", call. = FALSE)
p_root <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
norm <- yaml::read_yaml(file.path(p_root, "config", "normalisation.yml"))
if (!identical(norm$decision, "approved") || !identical(norm$sample_centring, norm$approved_sample_centring) ||
    !identical(norm$sample_centring, "all_observed_median"))
  stop("Not the approved primary tree (config/normalisation.yml): ", norm$sample_centring, " / ", norm$decision, call. = FALSE)
git <- function(...) system2("git", c("-C", shQuote(p_root), ...), stdout = TRUE)
rev <- git("rev-parse", "HEAD")
if (length(rev) != 1L || !grepl("^[0-9a-f]{40}$", rev)) stop("Could not read the pRoteomics revision.", call. = FALSE)
sha <- function(p) if (file.exists(p)) unname(tools::sha256sum(p)) else NA_character_
TREE <- "pRoteomics primary analysis (Analysis/proteomics_original)"
rows <- list()

# ---------------------------------------------------------------- render-workspace inputs (gitignored; recorded by hash)
WORK <- data.frame(stringsAsFactors = FALSE,
  declared = c("results/source_data/spatial_validation/score_compartment_markers/marker_class_scores_by_acquisition.csv",
               "results/source_data/spatial_validation/score_compartment_markers/marker_class_scores_by_animal.csv",
               "results/source_data/spatial_validation/score_compartment_markers/marker_class_compartment_contrasts.csv",
               "results/source_data/spatial_validation/map_anatomical_programmes/anatomical_programme_display_terms.csv",
               "results/source_data/spatial_validation/map_input_territories/input_territory_scores_by_animal.csv",
               "results/source_data/spatial_validation/map_input_territories/input_territory_tests.csv",
               "results/source_data/spatial_validation/map_input_territories/input_territory_sensitivity.csv",
               "results/source_data/spatial_validation/map_input_territories/input_territory_protein_contrasts.csv",
               "results/source_data/spatial_validation/map_afferent_sources/afferent_source_scores_by_animal.csv",
               "results/source_data/spatial_validation/map_afferent_sources/afferent_source_contrasts.csv",
               "results/source_data/spatial_validation/map_afferent_sources/afferent_source_sets.csv",
               "results/source_data/spatial_validation/compare_lamina_synapses/inhibitory_share_by_animal.csv",
               "results/source_data/spatial_validation/compare_lamina_synapses/inhibitory_share_contrasts.csv",
               "results/tables/10_biological_integration/gsea_wgcna_concordance/global/ontology_aware_gsea_theme_assignments_all_contrasts.csv"),
  source = c("results/spatial_validation/score_compartment_markers/global/tables/marker_class_scores_by_acquisition.csv",
             "results/spatial_validation/score_compartment_markers/global/tables/marker_class_scores_by_animal.csv",
             "results/spatial_validation/score_compartment_markers/global/tables/marker_class_compartment_contrasts.csv",
             "results/spatial_validation/map_anatomical_programmes/global/tables/anatomical_programme_display_terms.csv",
             "results/spatial_validation/map_input_territories/global/tables/input_territory_scores_by_animal.csv",
             "results/spatial_validation/map_input_territories/global/tables/input_territory_tests.csv",
             "results/spatial_validation/map_input_territories/global/tables/input_territory_sensitivity.csv",
             "results/spatial_validation/map_input_territories/global/tables/input_territory_protein_contrasts.csv",
             "results/spatial_validation/map_afferent_sources/global/tables/afferent_source_scores_by_animal.csv",
             "results/spatial_validation/map_afferent_sources/global/tables/afferent_source_contrasts.csv",
             "results/spatial_validation/map_afferent_sources/global/tables/afferent_source_sets.csv",
             "results/spatial_validation/compare_lamina_synapses/global/tables/inhibitory_share_by_animal.csv",
             "results/spatial_validation/compare_lamina_synapses/global/tables/inhibitory_share_contrasts.csv",
             "results/integration/test_enrichment_module_concordance/global/tables/ontology_aware_gsea_theme_assignments_all_contrasts.csv"),
  used_by = c("Figure 2d", "Figure 2d", "Figure 2d", "Figure 2f-2i", "Figure 2j", "Figure 2j", "Figure 2j", "Figure 2j", "Figure 2k",
              "Figure 2k", "Figure 2k", "Figure 2l", "Figure 2l", "Extended Data Fig. 7a-e"))
# A spatial_validation table must come from a finished run of committed code that the current revision has not changed:
# the run manifest's commit, the script and the R/spatial helpers it names unchanged since, and, where the run records output hashes,
# the table's SHA-256 among them.
run_checked <- function(source) {
  m <- regmatches(source, regexec("^results/spatial_validation/([^/]+)/global/tables/", source))[[1]]
  if (!length(m)) return(invisible(TRUE))
  man <- yaml::read_yaml(file.path(p_root, "results", "spatial_validation", m[2], "global", "manifests", "run_manifest.yml"))
  script <- file.path("analysis", "spatial_validation", paste0(m[2], ".R"))
  code <- paste(readLines(file.path(p_root, script), warn = FALSE), collapse = "
")
  helpers <- file.path("R", "spatial", Filter(function(f) grepl(f, code, fixed = TRUE),
                                              list.files(file.path(p_root, "R", "spatial"), pattern = "[.]R$")))
  same <- system2("git", c("-C", shQuote(p_root), "diff", "--quiet", man$git_commit, "HEAD", "--", script, helpers)) == 0L
  prov <- man$parameters$provenance
  if (!same) stop(script, " changed since its last run (", man$git_commit, "); rerun it before importing.", call. = FALSE)
  if (!is.null(prov)) {
    if (isTRUE(prov$code_uncommitted)) stop(script, " last ran with uncommitted code; rerun it at a commit.", call. = FALSE)
    if (!identical(prov$output_sha256[[basename(source)]], sha(file.path(p_root, source))))
      stop(source, " is not the table its run wrote (run manifest).", call. = FALSE)
  }
  invisible(TRUE)
}
for (i in seq_len(nrow(WORK))) {
  src <- file.path(p_root, WORK$source[i]); dst <- repo_path(WORK$declared[i])
  if (!file.exists(src)) stop("Missing in the primary tree: ", WORK$source[i], call. = FALSE)
  run_checked(WORK$source[i])
  new <- sha(src); old <- sha(dst)
  if (!identical(new, old)) {
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(src, dst, overwrite = TRUE) || !identical(sha(dst), new)) stop("Copy failed: ", WORK$declared[i], call. = FALSE)
  }
  rows[[length(rows) + 1L]] <- data.frame(declared_path = WORK$declared[i], used_by = WORK$used_by[i], source_tree = TREE,
    source_path = WORK$source[i], source_commit = rev, sha256 = new, previous_sha256 = old, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------- source-data bundle: selection inventories (tracked)
id <- "supplementary_selection_inventories"
pm <- utils::read.csv(file.path(p_root, "exports", "publication_source_data", "manifest.csv"), stringsAsFactors = FALSE)
pm <- pm[pm$publication_id == id, , drop = FALSE]
if (!nrow(pm)) stop("The primary tree has not exported ", id, " (tools/export_publication_source_data.R).", call. = FALSE)
mf <- repo_path("source_data", "pRoteomics", "manifest.csv")
m <- utils::read.csv(mf, stringsAsFactors = FALSE)
old_rows <- m[m$publication_id == id, , drop = FALSE]
if (!setequal(basename(old_rows$exported_file), basename(pm$exported_file))) stop("The ", id, " file set changed; review by hand.", call. = FALSE)
dest_dir <- repo_path("source_data", "pRoteomics", id)
for (i in seq_len(nrow(pm))) {
  src <- file.path(p_root, pm$exported_file[i]); dst <- file.path(dest_dir, basename(pm$exported_file[i]))
  if (!identical(sha(src), pm$sha256[i])) stop("Primary-tree export differs from its manifest: ", pm$exported_file[i], call. = FALSE)
  old <- sha(dst)
  if (!identical(old, pm$sha256[i]) && (!file.copy(src, dst, overwrite = TRUE) || !identical(sha(dst), pm$sha256[i])))
    stop("Copy failed: ", dst, call. = FALSE)
  rows[[length(rows) + 1L]] <- data.frame(declared_path = paste0("source_data/pRoteomics/", id, "/", basename(dst)), used_by = "Figure 3b-g; supplementary inventories",
    source_tree = TREE, source_path = pm$exported_file[i], source_commit = pm$source_commit[i], sha256 = pm$sha256[i],
    previous_sha256 = old_rows$sha256[match(pm$exported_file[i], old_rows$exported_file)], stringsAsFactors = FALSE)
}
pm <- pm[match(old_rows$exported_file, pm$exported_file), names(m)]
m[m$publication_id == id, ] <- pm   # replace the identity's rows in place, keeping row order
write_lf <- function(x, path) { con <- file(path, "wb"); on.exit(close(con)); utils::write.csv(x, con, row.names = FALSE) }   # LF, as committed
write_lf(m, mf)

# ---------------------------------------------------------------- record
rec <- do.call(rbind, rows)
prev <- repo_path("provenance", "source_manifests", "primary_inputs.csv")
if (file.exists(prev)) {   # keep the first previous_sha256 (the earlier import) across re-runs
  p0 <- utils::read.csv(prev, stringsAsFactors = FALSE)
  k <- match(rec$declared_path, p0$declared_path)
  rec$previous_sha256[!is.na(k)] <- p0$previous_sha256[k[!is.na(k)]]
}
write_lf(rec, prev)
cat("imported from", rev, ":", sum(rec$sha256 != rec$previous_sha256, na.rm = TRUE), "of", nrow(rec), "inputs changed ->", prev, "\n")
print(rec[, c("declared_path", "previous_sha256", "sha256")], row.names = FALSE)
