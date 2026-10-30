#!/usr/bin/env Rscript
# Freeze one version of the WGCNA theme composition candidate source exported by pRoteomics
# (tools/export_wgcna_theme_composition_candidate.R there, publication id
# wgcna_theme_composition_candidate), checking every hash on both sides.
#   v1 (2026-10-12)  source_data/pRoteomics/wgcna_theme_composition_candidate/; rows copied unchanged
#   v2, v3, ...      source_data/pRoteomics/wgcna_theme_composition_candidate_<version>/, from the
#                    approved primary tree only. The files are byte-identical; the rows keep every
#                    field except publication_id and exported_file, which name the versioned copy so
#                    the import-boundary checks resolve each row to it (source_table keeps the path
#                    pRoteomics wrote, with source_commit and sha256). An imported version, or an
#                    export already imported, is never replaced.
# Usage (repository root): Rscript tools/import_wgcna_theme_composition.R <pRoteomics_root> [v1|v2|...]
source(file.path("R", "paths.R"))
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) %in% 1:2 || !dir.exists(args[[1]]) ||
    (length(args) == 2L && !grepl("^v[0-9]+$", args[[2]])))
  stop("Usage: Rscript tools/import_wgcna_theme_composition.R <pRoteomics_root> [v1|v2|...]",
       call. = FALSE)
p_root <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(p_root, "pipeline.yml")))
  stop("Argument is not the pRoteomics checkout.", call. = FALSE)
version <- if (length(args) == 2L) args[[2]] else "v1"
id <- "wgcna_theme_composition_candidate"                       # the pRoteomics publication id
dest_id <- if (version == "v1") id else paste0(id, "_", version)
files <- c("theme_entity_composition_long.csv", "theme_composition_summary.csv",
           "theme_composition_summary_all_variants.csv", "theme_partition_agreement.csv",
           "curated_mapping_composition_audit.csv", "wgcna_module_wheel_structure.csv",
           "composition_universe_audit.csv", "theme_index.csv", "input_manifest.csv")
if (version != "v1") {   # later versions come from the approved primary tree only
  norm <- yaml::read_yaml(file.path(p_root, "config", "normalisation.yml"))
  if (!identical(norm$decision, "approved") || !identical(norm$sample_centring, norm$approved_sample_centring))
    stop("Not the approved primary tree (config/normalisation.yml): ", norm$sample_centring, " / ",
         norm$decision, call. = FALSE)
}
source_manifest <- file.path(p_root, "exports", "publication_source_data", "manifest.csv")
dest_manifest <- repo_path("source_data", "pRoteomics", "manifest.csv")
dest_dir <- repo_path("source_data", "pRoteomics", dest_id)
if (file.exists(dest_dir)) stop("Manuscript import already exists: ", dest_dir, call. = FALSE)
src_rows <- read.csv(source_manifest, stringsAsFactors = FALSE)
src_rows <- src_rows[src_rows$publication_id == id, , drop = FALSE]
if (!nrow(src_rows)) stop("pRoteomics manifest has no rows for ", id, call. = FALSE)
if (anyDuplicated(basename(src_rows$exported_file)) || !setequal(basename(src_rows$exported_file), files))
  stop("pRoteomics manifest rows for ", id, " do not list the nine candidate files once each.", call. = FALSE)
have <- read.csv(dest_manifest, stringsAsFactors = FALSE)
if (any(have$publication_id == dest_id)) stop("Manuscript manifest already lists ", dest_id, call. = FALSE)
if (length(unique(src_rows$source_commit)) != 1L ||
    system2("git", c("-C", shQuote(p_root), "cat-file", "-e",
                     paste0(src_rows$source_commit[[1]], "^{commit}"))) != 0L)
  stop("Manifest rows do not name one pRoteomics commit present here.", call. = FALSE)
earlier <- have[have$publication_id == id | startsWith(have$publication_id, paste0(id, "_v")), , drop = FALSE]
if (src_rows$source_commit[[1]] %in% earlier$source_commit)
  stop("The pRoteomics export at ", substr(src_rows$source_commit[[1]], 1, 7), " is already imported.",
       call. = FALSE)
origin <- file.path(p_root, src_rows$exported_file)
if (!all(file.exists(origin)) || !identical(unname(tools::sha256sum(origin)), src_rows$sha256))
  stop("pRoteomics export does not match its manifest.", call. = FALSE)
dir.create(dest_dir, recursive = TRUE)
frozen <- file.path(dest_dir, basename(src_rows$exported_file))
if (!all(file.copy(origin, frozen)) || !identical(unname(tools::sha256sum(frozen)), src_rows$sha256))
  stop("Manuscript source copy incomplete or altered.", call. = FALSE)
rows <- src_rows
rows$publication_id <- dest_id
rows$exported_file <- file.path("exports", "publication_source_data", dest_id, basename(src_rows$exported_file))
lines <- character()
buffer <- textConnection("lines", "w", local = TRUE)
write.table(rows, buffer, sep = ",", row.names = FALSE,
            col.names = FALSE, quote = TRUE, qmethod = "double")
close(buffer)
con <- file(dest_manifest, open = "ab")
writeBin(charToRaw(paste0(paste(lines, collapse = "\n"), "\n")), con)   # the manifest is LF
close(con)
cat("Imported ", nrow(rows), " frozen composition source files (", version, ", pRoteomics ",
    substr(src_rows$source_commit[[1]], 1, 7), ") into ", dest_dir, "\n", sep = "")
