# CC4 candidate bundles: the repository-side record of the MMMSociability Stage 31 runs imported
# for the CC4 candidate figures (tools/import_cc4_phase_groups.R, tools/import_cc4_phase_statistics.R,
# tools/import_cc4_phenotypes.R).
#
# Stage 31b, 31c and 31d write each run once into a new directory and write its manifest.csv
# (File, SHA256) last. They keep no bundle registry and stamp no commit, so a run is identified by
# its directory name and the hash of its manifest. The importers copy a run byte-exact into
#   source_data/MMMSociability_candidates/<bundle_id>/
# (beside source_data/MMMSociability, whose top level holds only the frozen ebb, s30b and fsb
# bundles). This file records each copy the way the frozen bundles are recorded:
#   config/cc4_candidate_bundles.yml        one pin per bundle: family, Stage 31 step, upstream run
#                                           directory, sha256 of its manifest.csv, file count and bytes;
#   provenance/source_manifests/cc4_candidate_bundles_manifest.csv
#                                           every imported file with its bytes and sha256.
# cc4_candidate_verify() checks a copy against both and against its own manifest.csv, and refuses
# any file that the manifest does not list. tools/verify_source_bundles.R runs it for every pin and
# fails on a copy that is not pinned.
#
# Every function takes `repo_dir` (default: this repository's root), so the registry logic can be
# exercised on a synthetic fixture (tests/testthat/test-cc4-candidate-bundles.R).

CC4_CANDIDATE_DIR <- "source_data/MMMSociability_candidates"
CC4_CANDIDATE_PIN_FILE <- "config/cc4_candidate_bundles.yml"
CC4_CANDIDATE_MANIFEST <- "provenance/source_manifests/cc4_candidate_bundles_manifest.csv"
CC4_CANDIDATE_FAMILIES <- data.frame(
  family = c("phase_groups", "phase_statistics", "phenotypes"),
  stage = c("31b", "31c", "31d"),
  producer = paste("topohl/MMMSociability", c("Analysis/31b_cc4_phase_groups.R", "Analysis/31c_cc4_phase_statistics.R",
                                              "Analysis/31d_cc4_phenotype_statistics.R")),
  importer = c("tools/import_cc4_phase_groups.R", "tools/import_cc4_phase_statistics.R", "tools/import_cc4_phenotypes.R"),
  stringsAsFactors = FALSE)
CC4_CANDIDATE_PIN_FIELDS <- c("bundle_id", "family", "stage", "upstream_run", "manifest_sha256", "files", "bytes",
                              "producer", "importer", "imported_at")
CC4_CANDIDATE_PIN_HEADER <- c(
  "# CC4 candidate bundles imported from MMMSociability Stage 31 (R/cc4_candidate_bundles.R).",
  "# Written by the importers (tools/import_cc4_*.R); verified by tools/verify_source_bundles.R.",
  "# Candidate inputs only: no canonical figure, contract or registry row reads them.")

cc4_candidate_stop <- function(...) stop(..., call. = FALSE)

cc4_candidate_sha <- function(paths) {
  vapply(paths, function(p) if (file.exists(p) && !dir.exists(p)) unname(tools::sha256sum(p)) else NA_character_,
         character(1), USE.NAMES = FALSE)
}

#' The family row of a CC4 candidate bundle id (cc4_phase_groups_*, cc4_phase_statistics_* or
#' cc4_phenotypes_*).
cc4_candidate_family <- function(bundle_id) {
  hit <- vapply(CC4_CANDIDATE_FAMILIES$family, function(f) grepl(paste0("^cc4_", f, "_[A-Za-z0-9_-]+$"), bundle_id),
                logical(1))
  if (length(bundle_id) != 1L || sum(hit) != 1L) cc4_candidate_stop("Not a CC4 candidate bundle id: ", paste(bundle_id, collapse = ", "))
  CC4_CANDIDATE_FAMILIES[hit, , drop = FALSE]
}

#' Every file of a directory, recursively, as paths relative to it (hidden files included).
cc4_candidate_files <- function(dir) sort(list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE))

#' A run's own manifest.csv (File, SHA256): complete, unique, relative paths without traversal.
cc4_candidate_own_manifest <- function(dir) {
  p <- file.path(dir, "manifest.csv")
  if (!file.exists(p)) cc4_candidate_stop("No manifest.csv in ", dir)
  m <- utils::read.csv(p, stringsAsFactors = FALSE, colClasses = "character")
  if (!identical(names(m), c("File", "SHA256")) || !nrow(m) || anyNA(m) || any(!nzchar(as.matrix(m))) ||
      anyDuplicated(m$File) || "manifest.csv" %in% m$File ||
      any(grepl("(^[/\\\\]|:|(^|[/\\\\])[.][.]([/\\\\]|$))", m$File)))
    cc4_candidate_stop("Invalid manifest.csv in ", dir)
  m
}

#' Check a run directory against its own manifest: every listed file present with its hash and
#' nothing unlisted. Returns the manifest and the full file list (the listed files and manifest.csv).
cc4_candidate_check_copy <- function(dir) {
  if (!dir.exists(dir)) cc4_candidate_stop("No such CC4 bundle directory: ", dir)
  m <- cc4_candidate_own_manifest(dir)
  now <- cc4_candidate_sha(file.path(dir, m$File))
  bad <- m$File[is.na(now) | now != m$SHA256]
  if (length(bad)) cc4_candidate_stop("Files differ from manifest.csv in ", basename(dir), ": ", paste(bad, collapse = ", "))
  extra <- setdiff(cc4_candidate_files(dir), c(m$File, "manifest.csv"))
  if (length(extra)) cc4_candidate_stop("Files that manifest.csv does not list in ", basename(dir), ": ", paste(extra, collapse = ", "))
  list(manifest = m, files = sort(c(m$File, "manifest.csv")))
}

#' The pinned CC4 candidate bundles (a list of pins; empty when nothing is pinned yet).
cc4_candidate_pins <- function(repo_dir = repo_root()) {
  p <- file.path(repo_dir, CC4_CANDIDATE_PIN_FILE)
  if (!file.exists(p)) return(list())
  y <- yaml::read_yaml(p)
  pins <- y$bundles
  if (is.null(pins)) return(list())
  for (pin in pins) {
    miss <- setdiff(CC4_CANDIDATE_PIN_FIELDS, names(pin))
    if (length(miss)) cc4_candidate_stop("A CC4 pin lacks ", paste(miss, collapse = ", "), " (", CC4_CANDIDATE_PIN_FILE, ").")
    fam <- cc4_candidate_family(pin$bundle_id)
    if (!identical(pin$family, fam$family) || !identical(pin$stage, fam$stage))
      cc4_candidate_stop("The CC4 pin of ", pin$bundle_id, " names the wrong family or Stage 31 step.")
  }
  ids <- vapply(pins, `[[`, "", "bundle_id")
  if (anyDuplicated(ids)) cc4_candidate_stop("A CC4 bundle is pinned twice: ", paste(ids[duplicated(ids)], collapse = ", "))
  stats::setNames(pins, ids)
}

cc4_candidate_import_manifest <- function(repo_dir = repo_root()) {
  p <- file.path(repo_dir, CC4_CANDIDATE_MANIFEST)
  if (!file.exists(p)) return(data.frame(bundle_id = character(0), file = character(0), bytes = numeric(0),
                                         sha256 = character(0), stringsAsFactors = FALSE))
  utils::read.csv(p, stringsAsFactors = FALSE, colClasses = c(bundle_id = "character", file = "character",
                                                              bytes = "numeric", sha256 = "character"))
}

#' Record an imported copy: pin it and list every file with its bytes and sha256. The copy must
#' already be in place (the importer copies it) and verify against its own manifest; its
#' manifest.csv must equal the upstream run's (`source_dir`). Refuses a bundle that is recorded already.
cc4_candidate_register <- function(source_dir, bundle_id, repo_dir = repo_root(),
                                   imported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")) {
  fam <- cc4_candidate_family(bundle_id)
  dst <- file.path(repo_dir, CC4_CANDIDATE_DIR, bundle_id)
  chk <- cc4_candidate_check_copy(dst)
  up <- cc4_candidate_sha(file.path(source_dir, "manifest.csv"))
  man_sha <- cc4_candidate_sha(file.path(dst, "manifest.csv"))
  if (is.na(up) || !identical(up, man_sha)) cc4_candidate_stop("The copy's manifest.csv is not the upstream run's: ", source_dir)
  pins <- cc4_candidate_pins(repo_dir)
  im <- cc4_candidate_import_manifest(repo_dir)
  if (bundle_id %in% names(pins) || bundle_id %in% im$bundle_id) cc4_candidate_stop("CC4 bundle already recorded: ", bundle_id)

  bytes <- file.size(file.path(dst, chk$files))
  rows <- data.frame(bundle_id = bundle_id, file = chk$files, bytes = bytes,
                     sha256 = cc4_candidate_sha(file.path(dst, chk$files)), stringsAsFactors = FALSE)
  total <- sum(bytes)
  pin <- list(bundle_id = bundle_id, family = fam$family, stage = fam$stage,
              upstream_run = basename(normalizePath(source_dir, winslash = "/", mustWork = TRUE)),
              manifest_sha256 = man_sha, files = length(chk$files),
              bytes = if (total <= .Machine$integer.max) as.integer(total) else total,
              producer = fam$producer, importer = fam$importer, imported_at = imported_at)
  pins[[bundle_id]] <- pin
  pin_path <- file.path(repo_dir, CC4_CANDIDATE_PIN_FILE)
  dir.create(dirname(pin_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(CC4_CANDIDATE_PIN_HEADER, yaml::as.yaml(list(bundles = unname(pins)))), pin_path)
  mp <- file.path(repo_dir, CC4_CANDIDATE_MANIFEST)
  dir.create(dirname(mp), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(rbind(im, rows), mp, row.names = FALSE)
  invisible(pin)
}

#' Verify one pinned copy: against its own manifest (nothing unlisted), the pinned manifest hash,
#' file count and bytes, and the import manifest's rows.
cc4_candidate_verify <- function(pin, repo_dir = repo_root()) {
  dst <- file.path(repo_dir, CC4_CANDIDATE_DIR, pin$bundle_id)
  chk <- cc4_candidate_check_copy(dst)
  if (!identical(cc4_candidate_sha(file.path(dst, "manifest.csv")), pin$manifest_sha256))
    cc4_candidate_stop(pin$bundle_id, ": manifest.csv does not match the pinned hash.")
  bytes <- file.size(file.path(dst, chk$files))
  if (!identical(as.numeric(length(chk$files)), as.numeric(pin$files)) || !identical(as.numeric(sum(bytes)), as.numeric(pin$bytes)))
    cc4_candidate_stop(pin$bundle_id, ": the copy's file count or bytes differ from the pin.")
  im <- cc4_candidate_import_manifest(repo_dir)
  im <- im[im$bundle_id == pin$bundle_id, , drop = FALSE]
  im <- im[order(im$file), , drop = FALSE]
  if (!identical(im$file, chk$files) || !identical(as.numeric(im$bytes), as.numeric(bytes)) ||
      !identical(im$sha256, cc4_candidate_sha(file.path(dst, chk$files))))
    cc4_candidate_stop(pin$bundle_id, ": the import manifest does not match the copy.")
  invisible(TRUE)
}

#' Verify every pinned copy, and refuse a copy or an import-manifest entry that is not pinned.
#' One row per bundle: bundle_id, files, ok, problem.
cc4_candidate_verify_all <- function(repo_dir = repo_root()) {
  pins <- cc4_candidate_pins(repo_dir)
  on_disk <- list.dirs(file.path(repo_dir, CC4_CANDIDATE_DIR), recursive = FALSE, full.names = FALSE)
  ids <- sort(unique(c(names(pins), on_disk, cc4_candidate_import_manifest(repo_dir)$bundle_id)))
  out <- lapply(ids, function(id) {
    problem <- if (!id %in% names(pins)) "imported or recorded but not pinned" else
      tryCatch({ cc4_candidate_verify(pins[[id]], repo_dir); "" }, error = function(e) conditionMessage(e))
    data.frame(bundle_id = id, files = if (id %in% names(pins)) as.integer(pins[[id]]$files) else NA_integer_,
               ok = !nzchar(problem), problem = problem, stringsAsFactors = FALSE)
  })
  if (!length(out)) return(data.frame(bundle_id = character(0), files = integer(0), ok = logical(0), problem = character(0)))
  do.call(rbind, out)
}
