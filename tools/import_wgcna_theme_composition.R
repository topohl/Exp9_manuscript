#!/usr/bin/env Rscript
# Freeze the WGCNA theme composition candidate source exported by pRoteomics
# (tools/export_wgcna_theme_composition_candidate.R there). The export already
# wrote its rows to the pRoteomics manifest, so this copies the files and those
# rows unchanged, checking every hash on both sides.
source(file.path("R", "paths.R"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !dir.exists(args[[1]]))
  stop("Usage: Rscript tools/import_wgcna_theme_composition.R <pRoteomics_root>",
       call. = FALSE)
p_root <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(p_root, "pipeline.yml")))
  stop("Argument is not the pRoteomics checkout.", call. = FALSE)
id <- "wgcna_theme_composition_candidate"
source_manifest <- file.path(p_root, "exports", "publication_source_data", "manifest.csv")
dest_manifest <- repo_path("source_data", "pRoteomics", "manifest.csv")
dest_dir <- repo_path("source_data", "pRoteomics", id)
if (file.exists(dest_dir)) stop("Manuscript import already exists: ", dest_dir, call. = FALSE)
src_rows <- read.csv(source_manifest, stringsAsFactors = FALSE)
src_rows <- src_rows[src_rows$publication_id == id, , drop = FALSE]
if (!nrow(src_rows)) stop("pRoteomics manifest has no rows for ", id, call. = FALSE)
if (any(read.csv(dest_manifest, stringsAsFactors = FALSE)$publication_id == id))
  stop("Manuscript manifest already lists ", id, call. = FALSE)
if (length(unique(src_rows$source_commit)) != 1L ||
    system2("git", c("-C", shQuote(p_root), "cat-file", "-e",
                     paste0(src_rows$source_commit[[1]], "^{commit}"))) != 0L)
  stop("Manifest rows do not name one pRoteomics commit present here.", call. = FALSE)
origin <- file.path(p_root, src_rows$exported_file)
if (!all(file.exists(origin)) ||
    !identical(unname(tools::sha256sum(origin)), src_rows$sha256))
  stop("pRoteomics export does not match its manifest.", call. = FALSE)
dir.create(dest_dir, recursive = TRUE)
frozen <- file.path(dest_dir, basename(src_rows$exported_file))
if (!all(file.copy(origin, frozen)) ||
    !identical(unname(tools::sha256sum(frozen)), src_rows$sha256))
  stop("Manuscript source copy incomplete or altered.", call. = FALSE)
lines <- character()
buffer <- textConnection("lines", "w", local = TRUE)
write.table(src_rows, buffer, sep = ",", row.names = FALSE,
            col.names = FALSE, quote = TRUE, qmethod = "double")
close(buffer)
con <- file(dest_manifest, open = "ab")
writeBin(charToRaw(paste0(paste(lines, collapse = "\n"), "\n")), con)
close(con)
cat("Imported ", nrow(src_rows), " frozen composition source files into ",
    dest_dir, "\n", sep = "")
