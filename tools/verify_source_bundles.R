#!/usr/bin/env Rscript

# Verify every imported source-data bundle against its manifest.
#
# This is the only trust boundary in the manuscript repository. Nothing here
# recomputes a scientific value; it recomputes hashes and compares them to what
# the producing repository recorded. If this fails, no rendered figure in this
# repository should be believed.
#
# Exit status 0 when all bundles verify, 1 otherwise.

# Locate the bootstrap by walking upward, so the script works from any
# working directory (tests invoke it from tests/testthat).
.boot <- NULL
.dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
repeat {
  cand <- file.path(.dir, "R", "paths.R")
  if (file.exists(cand)) { .boot <- cand; break }
  parent <- dirname(.dir)
  if (identical(parent, .dir)) break
  .dir <- parent
}
if (is.null(.boot)) stop("Could not locate R/paths.R from ", getwd(), call. = FALSE)
source(.boot)
rm(.boot, .dir)

problems <- 0L
note_fail <- function(...) {
  cat("FAIL:", ..., "\n")
  problems <<- problems + 1L
}

sha <- function(p) {
  if (!file.exists(p) || dir.exists(p)) return(NA_character_)
  unname(tools::sha256sum(p))
}

cat("Exp9_manuscript source-bundle verification\n")
cat("==========================================\n\n")

## ------------------------------------------------- pRoteomics science bundle
p_root <- repo_path("source_data", "pRoteomics")
p_manifest <- file.path(p_root, "manifest.csv")

if (!file.exists(p_manifest)) {
  note_fail("pRoteomics bundle manifest is missing:", p_manifest)
} else {
  m <- utils::read.csv(p_manifest, stringsAsFactors = FALSE)
  required <- c("publication_id", "source_repo", "source_commit", "source_analysis",
                "source_table", "exported_file", "rows", "columns", "sha256",
                "contract_version")
  miss <- setdiff(required, names(m))
  if (length(miss)) note_fail("manifest is missing columns:", paste(miss, collapse = ", "))

  cat("pRoteomics bundle\n")
  cat("  manifest rows      :", nrow(m), "\n")
  cat("  publication ids    :", length(unique(m$publication_id)), "\n")
  cat("  source commit(s)   :", paste(unique(m$source_commit), collapse = ", "), "\n")
  cat("  contract version(s):", paste(unique(m$contract_version), collapse = ", "), "\n")

  ## exported_file is recorded relative to the producing repository root.
  local_path <- file.path(p_root, sub("^results/publication_source_data/", "",
                                      m$exported_file))
  present <- file.exists(local_path)
  cat("  files present      :", sum(present), "/", nrow(m), "\n")
  if (any(!present)) {
    note_fail(sum(!present), "manifest file(s) absent from the bundle")
    writeLines(paste0("    missing: ", head(m$exported_file[!present], 10)))
  }

  actual <- vapply(local_path, function(p) if (file.exists(p)) sha(p) else NA_character_,
                   character(1), USE.NAMES = FALSE)
  match_ok <- present & !is.na(actual) & actual == m$sha256
  cat("  hash-verified      :", sum(match_ok), "/", nrow(m), "\n")
  if (any(present & !match_ok)) {
    note_fail(sum(present & !match_ok), "file(s) do not match the recorded sha256")
    writeLines(paste0("    mismatch: ", head(m$exported_file[present & !match_ok], 10)))
  }

  ## Nothing may sit in the bundle that the manifest does not cover.
  on_disk <- setdiff(list.files(p_root, recursive = TRUE), "manifest.csv")
  covered <- sub("^results/publication_source_data/", "", m$exported_file)
  orphan <- setdiff(on_disk, covered)
  cat("  uncovered files    :", length(orphan), "\n")
  if (length(orphan)) {
    note_fail(length(orphan), "bundle file(s) are not covered by the manifest")
    writeLines(paste0("    orphan: ", head(orphan, 10)))
  }
}

## --------------------------------------------- MMMSociability behaviour bridge
cat("\nMMMSociability bridge\n")
b_root <- repo_path("source_data", "MMMSociability")
b_manifest <- repo_path("provenance", "source_manifests", "figure1_bridge_import_manifest.csv")

if (!file.exists(b_manifest)) {
  note_fail("behaviour bridge import manifest is missing:", b_manifest)
} else {
  b <- utils::read.csv(b_manifest, stringsAsFactors = FALSE)
  path_col <- intersect(c("imported_file", "repository_relative_path", "path", "file"), names(b))[1]
  hash_col <- intersect(c("sha256", "sha256sum", "hash"), names(b))[1]
  if (is.na(path_col) || is.na(hash_col)) {
    note_fail("cannot find path/hash columns in the bridge manifest; columns are:",
              paste(names(b), collapse = ", "))
  } else {
    ## The bridge was imported from manuscript/figure1_bridge_mmmsociability/
    ## and now lives at source_data/MMMSociability/.
    rel <- sub("^manuscript/figure1_bridge_mmmsociability/", "", b[[path_col]])
    local_path <- file.path(b_root, rel)
    present <- file.exists(local_path)
    cat("  manifest rows      :", nrow(b), "\n")
    cat("  files present      :", sum(present), "/", nrow(b), "\n")
    actual <- vapply(local_path, function(p) if (file.exists(p)) sha(p) else NA_character_,
                     character(1), USE.NAMES = FALSE)
    match_ok <- present & !is.na(actual) & actual == b[[hash_col]]
    cat("  hash-verified      :", sum(match_ok), "/", nrow(b), "\n")
    if (any(!present)) {
      note_fail(sum(!present), "bridge file(s) absent")
      writeLines(paste0("    missing: ", head(rel[!present], 10)))
    }
    if (any(present & !match_ok)) {
      note_fail(sum(present & !match_ok), "bridge file(s) are not byte-exact")
      writeLines(paste0("    mismatch: ", head(rel[present & !match_ok], 10)))
    }
  }
}

cat("\n")
if (problems == 0L) {
  cat("RESULT: PASS - every imported bundle matches its manifest.\n")
  quit(status = 0L)
}
cat("RESULT: FAIL -", problems, "problem(s).\n")
quit(status = 1L)
