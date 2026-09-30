#!/usr/bin/env Rscript
# Freeze the downstream Figure 3 atlas source produced by pRoteomics.
source(file.path("R", "paths.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !dir.exists(args[[1]]))
  stop("Usage: Rscript tools/import_figure_03_go_atlas_appendix.R <pRoteomics_root>",
       call. = FALSE)
p_root <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(p_root, "pipeline.yml")))
  stop("Argument is not the pRoteomics checkout.", call. = FALSE)
id <- "figure_03_go_atlas_appendix"
names <- c("theme_term_index.csv", "selected_contexts.csv",
           "running_enrichment_curves.csv", "regional_exact_term_inventory.csv",
           "protein_zoom_values.csv", "input_manifest.csv")
source_dir <- file.path(p_root, "exports", "publication_source_data", id)
dest_dir <- repo_path("source_data", "pRoteomics", id)
source_manifest <- file.path(p_root, "exports", "publication_source_data",
                             "manifest.csv")
dest_manifest <- repo_path("source_data", "pRoteomics", "manifest.csv")
if (!all(file.exists(file.path(source_dir, names))) ||
    file.exists(dest_dir))
  stop("Source files missing or manuscript import already exists.", call. = FALSE)
for (path in c(source_manifest, dest_manifest)) {
  m <- read.csv(path, stringsAsFactors = FALSE)
  if (any(m$publication_id == id))
    stop("Manifest already contains this appendix: ", path, call. = FALSE)
}
rev <- system2("git", c("-C", shQuote(p_root), "rev-parse", "HEAD"),
               stdout = TRUE)
if (length(rev) != 1L || !grepl("^[0-9a-f]{40}$", rev))
  stop("Could not read pRoteomics Git revision.", call. = FALSE)
dir.create(dest_dir, recursive = TRUE)
copied <- file.copy(file.path(source_dir, names), file.path(dest_dir, names))
if (!all(copied)) stop("Manuscript source copy incomplete.", call. = FALSE)
rows <- lapply(names, function(name) {
  origin <- file.path(source_dir, name)
  frozen <- file.path(dest_dir, name)
  hash <- unname(tools::sha256sum(origin))
  if (!identical(hash, unname(tools::sha256sum(frozen))))
    stop("Copied source hash mismatch: ", name, call. = FALSE)
  tab <- read.csv(frozen, stringsAsFactors = FALSE)
  relative <- paste0("exports/publication_source_data/", id, "/", name)
  data.frame(publication_id = id, source_repo = "topohl/pRoteomics",
             source_commit = rev,
             source_analysis = "tools/export_figure_03_go_atlas_appendix.R",
             source_table = relative, exported_file = relative,
             rows = nrow(tab), columns = ncol(tab), sha256 = hash,
             contract_version = "publication_source_data_v1")
})
entry <- do.call(rbind, rows)
lines <- character()
buffer <- textConnection("lines", "w", local = TRUE)
write.table(entry, buffer, sep = ",", row.names = FALSE,
            col.names = FALSE, quote = TRUE, qmethod = "double")
close(buffer)
for (path in c(source_manifest, dest_manifest)) {
  con <- file(path, open = "ab")
  writeBin(charToRaw(paste0(paste(lines, collapse = "\n"), "\n")), con)
  close(con)
}
cat("Imported ", length(names), " frozen atlas source files into ",
    dest_dir, "\n", sep = "")
