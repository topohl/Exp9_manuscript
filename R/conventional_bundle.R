# Conventional-tests figure bundle (ctp) resolver and importer logic.
#
# The bundle is produced and frozen in SISanalyzer by manuscript_bundle/export_bundle.R, which writes one immutable,
# hash-listed directory per bundle,
#   Analysis/SIS_Analysis/bundles/<bundle_id>/
# registered in BUNDLE_REGISTRY.csv beside it (bundle_id, status, created_at, producer_commit, manifest_sha256).
# It carries the conventional behaviour tests, the physiology measures, the heterogeneity analysis and the exploratory
# locomotion analysis: per-animal values, batch-adjusted group means, the contrasts as stored in the SISanalyzer result
# workbooks, and provenance. Every statistic is computed upstream; nothing is recomputed here.
# tools/import_conventional_bundle.R copies one FROZEN bundle, unchanged, into source_data/SISanalyzer/<bundle_id>/,
# pins it in config/conventional_bundle.yml and records every file hash in
# provenance/source_manifests/conventional_bundle_manifest.csv. Renderers read the pinned copy only.
#
# Every function takes `repo_dir` (default: this repository's root) so that the logic can be exercised on a copy.

CTP_ID_PATTERN <- "^ctp_v[0-9]+_[0-9]{8}_[0-9a-f]{7}$"
CTP_IMPORT_DIR <- "source_data/SISanalyzer"
CTP_PIN_FILE <- "config/conventional_bundle.yml"
CTP_IMPORT_MANIFEST <- "provenance/source_manifests/conventional_bundle_manifest.csv"
CTP_REGISTRY_COLUMNS <- c("bundle_id", "status", "created_at", "producer_commit", "manifest_sha256")
CTP_MANIFEST_COLUMNS <- c("file", "rows", "bytes", "sha256")
CTP_REQUIRED_FILES <- c("A_measures.csv", "B_values.csv", "C_group_means.csv", "D_contrasts.csv", "E_heterogeneity_summary.csv",
                        "E_heterogeneity_tails.csv", "F_locomotion_sessions.csv", "H_provenance.csv")
CTP_PROVENANCE_KEYS <- c("bundle_id", "status", "producer", "producer_commit", "producer_tree_clean", "created_at")

ctp_sha <- function(p) unname(tools::sha256sum(p))
ctp_stop <- function(...) stop(..., call. = FALSE)
ctp_list <- function(d) sort(list.files(d, all.files = TRUE, no.. = TRUE))

ctp_read_manifest <- function(path) {
  man <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = c(bytes = "numeric"))
  miss <- setdiff(CTP_MANIFEST_COLUMNS, names(man))
  if (length(miss)) ctp_stop("00_manifest.csv is missing column(s): ", paste(miss, collapse = ", "))
  if (!nrow(man) || anyDuplicated(man$file)) ctp_stop("00_manifest.csv is empty or lists a file twice.")
  if (any(!grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", man$file)) || "00_manifest.csv" %in% man$file)
    ctp_stop("00_manifest.csv lists a file name that is not a plain bundle file.")
  man
}

ctp_provenance <- function(path) {
  prov <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character")
  if (!identical(names(prov), c("key", "value"))) ctp_stop("H_provenance.csv must have columns key,value.")
  out <- lapply(CTP_PROVENANCE_KEYS, function(k) { v <- prov$value[prov$key == k]
    if (length(v) != 1L || is.na(v) || !nzchar(v)) ctp_stop("H_provenance.csv does not record '", k, "' exactly once."); v })
  stats::setNames(out, CTP_PROVENANCE_KEYS)
}

#' Check an upstream bundle before import: id form; registered exactly once as FROZEN; manifest hash = registry; every
#' listed file byte-exact; nothing unlisted; the required tables present; H_provenance FROZEN, naming this bundle and the
#' registry commit, produced from a clean tree.
ctp_check_source <- function(root, id) {
  if (!grepl(CTP_ID_PATTERN, id)) ctp_stop("Not a conventional-tests bundle id (ctp_v<N>_<YYYYMMDD>_<commit7>): ", id)
  reg_path <- file.path(root, "BUNDLE_REGISTRY.csv")
  if (!file.exists(reg_path)) ctp_stop("No BUNDLE_REGISTRY.csv in ", root)
  reg <- utils::read.csv(reg_path, stringsAsFactors = FALSE, colClasses = "character")
  miss <- setdiff(CTP_REGISTRY_COLUMNS, names(reg)); if (length(miss)) ctp_stop("BUNDLE_REGISTRY.csv is missing column(s): ", paste(miss, collapse = ", "))
  row <- reg[reg$bundle_id == id, , drop = FALSE]
  if (nrow(row) != 1L || !identical(row$status, "FROZEN")) ctp_stop("Bundle ", id, " is not registered exactly once as FROZEN.")
  src <- file.path(root, id); man_path <- file.path(src, "00_manifest.csv")
  if (!file.exists(man_path)) ctp_stop("The bundle has no 00_manifest.csv: ", src)
  if (!identical(ctp_sha(man_path), row$manifest_sha256)) ctp_stop("The bundle manifest does not match its registry hash.")
  man <- ctp_read_manifest(man_path); paths <- file.path(src, man$file)
  if (any(!file.exists(paths))) ctp_stop("A file listed in the bundle manifest is missing: ", paste(man$file[!file.exists(paths)], collapse = ", "))
  if (any(ctp_sha(paths) != man$sha256) || any(file.size(paths) != man$bytes)) ctp_stop("A bundle file differs from its manifest.")
  extra <- setdiff(ctp_list(src), c(man$file, "00_manifest.csv")); if (length(extra)) ctp_stop("Unlisted files in the bundle: ", paste(extra, collapse = ", "))
  absent <- setdiff(CTP_REQUIRED_FILES, man$file); if (length(absent)) ctp_stop("The bundle lacks the required table(s): ", paste(absent, collapse = ", "))
  prov <- ctp_provenance(file.path(src, "H_provenance.csv"))
  if (!identical(prov$status, "FROZEN") || !identical(prov$bundle_id, id)) ctp_stop("H_provenance is not FROZEN for ", id, ".")
  if (!identical(prov$producer_commit, row$producer_commit)) ctp_stop("H_provenance and the registry name different producing commits.")
  if (!identical(prov$producer_tree_clean, "TRUE")) ctp_stop("The bundle was not produced from a clean SISanalyzer tree.")
  list(row = row, manifest = man, provenance = prov, src = src)
}

#' Import one FROZEN bundle into `repo_dir`: immutable copy, pin, import manifest. Returns the pin invisibly.
ctp_import <- function(root, id, repo_dir = repo_root(), imported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")) {
  chk <- ctp_check_source(root, id); man <- chk$manifest; files <- c(man$file, "00_manifest.csv")
  dst <- file.path(repo_dir, CTP_IMPORT_DIR, id)
  if (dir.exists(dst) || file.exists(dst)) ctp_stop("Bundle already imported (immutable): ", dst)
  mp <- file.path(repo_dir, CTP_IMPORT_MANIFEST)
  old <- if (file.exists(mp)) utils::read.csv(mp, stringsAsFactors = FALSE, colClasses = c(bytes = "numeric")) else NULL
  if (!is.null(old) && id %in% old$bundle_id) ctp_stop("The import manifest already records ", id, " but no copy exists; resolve by hand.")
  if (max(nchar(file.path(normalizePath(dirname(dst), winslash = "/", mustWork = FALSE), id, files))) >= 260L)
    ctp_stop("A destination path would be 260 characters or longer (Windows R cannot open it).")
  stage <- file.path(dirname(dst), paste0(".", id, ".partial")); unlink(stage, recursive = TRUE); dir.create(stage, recursive = TRUE)
  ok <- FALSE; on.exit(if (!ok) unlink(stage, recursive = TRUE), add = TRUE)
  if (!all(file.copy(file.path(chk$src, files), file.path(stage, files), copy.mode = FALSE))) ctp_stop("Copy failed.")
  if (any(ctp_sha(file.path(stage, man$file)) != man$sha256) || !identical(ctp_sha(file.path(stage, "00_manifest.csv")), chk$row$manifest_sha256))
    ctp_stop("Imported copy does not verify.")
  if (!file.rename(stage, dst)) ctp_stop("Could not move the verified copy into place: ", dst)
  ok <- TRUE
  pin <- list(bundle_id = id, manifest_sha256 = chk$row$manifest_sha256, registry_status = chk$row$status,
              producer_commit = chk$provenance$producer_commit, producer = chk$provenance$producer, created_at = chk$row$created_at,
              scientific_recomputation = "none in this repository; every statistic is read from the bundle",
              imported_at = imported_at, importer = "tools/import_conventional_bundle.R")
  dir.create(dirname(file.path(repo_dir, CTP_PIN_FILE)), recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(pin, file.path(repo_dir, CTP_PIN_FILE))
  rows <- data.frame(bundle_id = id, file = files, bytes = file.size(file.path(dst, files)), sha256 = ctp_sha(file.path(dst, files)), stringsAsFactors = FALSE)
  if (!is.null(old)) rows <- rbind(old, rows)
  dir.create(dirname(mp), recursive = TRUE, showWarnings = FALSE); utils::write.csv(rows, mp, row.names = FALSE)
  invisible(pin)
}

# ---------------------------------------------------------------- readers of the pinned copy
ctp_pin <- function(repo_dir = repo_root()) {
  p <- file.path(repo_dir, CTP_PIN_FILE)
  if (!file.exists(p)) ctp_stop("No conventional-tests bundle is pinned (", CTP_PIN_FILE, "). Run tools/import_conventional_bundle.R.")
  pin <- yaml::read_yaml(p); if (!grepl(CTP_ID_PATTERN, pin$bundle_id %||% "")) ctp_stop("The pin does not name a ctp bundle.")
  pin
}
ctp_dir <- function(pin = ctp_pin(repo_dir), repo_dir = repo_root()) file.path(repo_dir, CTP_IMPORT_DIR, pin$bundle_id)

#' Verify the pinned copy byte-for-byte against its own manifest, the pinned manifest hash and the import manifest.
ctp_verify <- function(pin = ctp_pin(repo_dir), repo_dir = repo_root()) {
  d <- ctp_dir(pin, repo_dir); man_path <- file.path(d, "00_manifest.csv")
  if (!file.exists(man_path)) ctp_stop("Pinned conventional-tests bundle is not imported: ", d)
  if (!identical(ctp_sha(man_path), pin$manifest_sha256)) ctp_stop("00_manifest.csv does not match the pinned manifest hash.")
  man <- ctp_read_manifest(man_path); now <- ctp_sha(file.path(d, man$file))
  bad <- man$file[is.na(now) | now != man$sha256]; if (length(bad)) ctp_stop("Bundle files differ from the manifest: ", paste(bad, collapse = ", "))
  extra <- setdiff(ctp_list(d), c(man$file, "00_manifest.csv")); if (length(extra)) ctp_stop("Unlisted files in the bundle copy: ", paste(extra, collapse = ", "))
  im <- file.path(repo_dir, CTP_IMPORT_MANIFEST)
  if (!file.exists(im)) ctp_stop("The import manifest is missing: ", CTP_IMPORT_MANIFEST)
  im <- utils::read.csv(im, stringsAsFactors = FALSE); im <- im[im$bundle_id == pin$bundle_id, , drop = FALSE]
  if (!setequal(im$file, c(man$file, "00_manifest.csv")) || any(im$sha256[match(man$file, im$file)] != man$sha256))
    ctp_stop("The import manifest does not match the pinned copy.")
  invisible(TRUE)
}

ctp_table <- function(name, pin = ctp_pin(repo_dir), repo_dir = repo_root()) {
  p <- file.path(ctp_dir(pin, repo_dir), paste0(name, ".csv"))
  if (!file.exists(p)) ctp_stop("Bundle table not found: ", name)
  utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE, na.strings = "", encoding = "UTF-8")
}
