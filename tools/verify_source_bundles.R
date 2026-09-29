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

  ## exported_file is recorded relative to the producing repository root, at
  ## its declared export boundary. Phase 6F moved that boundary from
  ## results/publication_source_data/ to exports/publication_source_data/;
  ## both prefixes are stripped so a manifest from either era resolves.
  local_path <- file.path(p_root, sub("^(exports|results)/publication_source_data/", "",
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
  covered <- sub("^(exports|results)/publication_source_data/", "", m$exported_file)
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

## --------------------------------------------- MMMSociability canonical behaviour bundle
cat("\nMMMSociability canonical behaviour bundle (Stage 16b)\n")
pin_path <- repo_path("config", "behaviour_bundle.yml")
bb_manifest <- repo_path("provenance", "source_manifests", "behaviour_bundle_manifest.csv")
if (!file.exists(pin_path)) {
  note_fail("no behaviour bundle is pinned:", pin_path)
} else {
  pin <- yaml::read_yaml(pin_path)
  bd <- repo_path("source_data", "MMMSociability", pin$bundle_id)
  cat("  pinned bundle      :", pin$bundle_id, "(config", pin$config_version, substr(pin$config_sha256, 1, 12), ")\n")
  own <- file.path(bd, "00_manifest.csv")
  if (!file.exists(own)) {
    note_fail("pinned bundle is not imported:", bd)
  } else {
    if (!identical(sha(own), pin$manifest_sha256)) note_fail("00_manifest.csv differs from the pinned manifest hash")
    m <- utils::read.csv(own, stringsAsFactors = FALSE)
    act <- vapply(file.path(bd, m$file), sha, character(1), USE.NAMES = FALSE)
    ok <- !is.na(act) & act == m$sha256
    cat("  hash-verified      :", sum(ok), "/", nrow(m), "\n")
    if (any(!ok)) note_fail(sum(!ok), "bundle file(s) are not byte-exact:", paste(head(m$file[!ok], 10), collapse = ", "))
    extra <- setdiff(list.files(bd), c(m$file, "00_manifest.csv"))
    if (length(extra)) note_fail("unlisted file(s) in the bundle copy:", paste(extra, collapse = ", "))
    if (!file.exists(bb_manifest)) note_fail("behaviour bundle import manifest is missing:", bb_manifest) else {
      im <- utils::read.csv(bb_manifest, stringsAsFactors = FALSE)
      im <- im[im$bundle_id == pin$bundle_id, , drop = FALSE]
      if (!setequal(im$file, list.files(bd)) || any(im$sha256 != vapply(file.path(bd, im$file), sha, character(1), USE.NAMES = FALSE)))
        note_fail("the import manifest does not match the pinned copy")
    }
  }
}

## --------------------------------------------- MMMSociability canonical Stage 30 figure bundle
## Verified only when one is pinned (config/stage30_bundle.yml); until the first
## Stage 30 import there is nothing to check.
cat("\nMMMSociability canonical Stage 30 figure bundle\n")
s30_pin_path <- repo_path("config", "stage30_bundle.yml")
if (!file.exists(s30_pin_path)) {
  cat("  pinned bundle      : none (config/stage30_bundle.yml absent; not verified)\n")
} else {
  source(repo_path("R", "stage30_bundle.R"))
  s30_manifest <- repo_path("provenance", "source_manifests", "stage30_bundle_manifest.csv")
  s30 <- tryCatch(stage30_bundle_pin(), error = function(e) { note_fail(conditionMessage(e)); NULL })
  if (!is.null(s30)) {
    sd30 <- stage30_bundle_dir(s30)
    cat("  pinned bundle      :", s30$bundle_id, "(Stage 30 run", substr(s30$stage30_run_commit %||% "", 1, 7),
        "; built from", s30$stage29_bundle_id, ")\n")
    ok30 <- tryCatch(stage30_bundle_verify(s30), error = function(e) { note_fail(conditionMessage(e)); FALSE })
    if (isTRUE(ok30)) {
      n30 <- nrow(utils::read.csv(file.path(sd30, "00_manifest.csv"), stringsAsFactors = FALSE))
      cat("  hash-verified      :", n30, "/", n30, "\n")
      prov30 <- tryCatch(s30_provenance(file.path(sd30, "H_provenance.csv")), error = function(e) { note_fail(conditionMessage(e)); NULL })
      if (!is.null(prov30) && (!identical(prov30$status, "FROZEN") || !identical(prov30$bundle_id, s30$bundle_id)))
        note_fail("the Stage 30 bundle's H_provenance is not FROZEN for", s30$bundle_id)
    }
    if (!file.exists(s30_manifest)) note_fail("Stage 30 bundle import manifest is missing:", s30_manifest) else {
      im30 <- utils::read.csv(s30_manifest, stringsAsFactors = FALSE)
      im30 <- im30[im30$bundle_id == s30$bundle_id, , drop = FALSE]
      if (!nrow(im30) || !setequal(im30$file, list.files(sd30)) ||
          any(im30$sha256 != vapply(file.path(sd30, im30$file), sha, character(1), USE.NAMES = FALSE)))
        note_fail("the Stage 30 import manifest does not match the pinned copy")
    }
    ## Stage 30 figures combine the Stage 29 bundle and the Stage 30 bundle, so the
    ## Stage 30 bundle must have been built from the Stage 29 bundle that is pinned.
    if (file.exists(pin_path)) {
      bpin <- yaml::read_yaml(pin_path)
      if (!identical(s30$stage29_bundle_id, bpin$bundle_id) || !identical(s30$stage29_bundle_manifest_sha256, bpin$manifest_sha256))
        note_fail("the Stage 30 bundle was built from", s30$stage29_bundle_id, "but the pinned behaviour bundle is", bpin$bundle_id)
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
