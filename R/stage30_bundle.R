# Canonical Stage 30 figure-bundle resolver and importer logic.
#
# Stage 30 (the exploratory screen) is produced and frozen in MMMSociability. Its
# figure-bundle exporter (Analysis/30b_stage30_figure_bundle.R) writes one
# immutable, hash-frozen directory per bundle,
#   <analysis_ready>/canonical/stage30_figure_bundle/<bundle_id>/
# registered in BUNDLE_REGISTRY.csv beside it (bundle_id, status,
# manifest_sha256, stage30_run_commit, mmm_git_commit, created_at).
# tools/import_stage30_bundle.R copies one FROZEN bundle, unchanged, into
# source_data/MMMSociability/<bundle_id>/, pins it in config/stage30_bundle.yml
# and records every file hash in provenance/source_manifests/stage30_bundle_manifest.csv.
#
# Mirrors R/behaviour_bundle.R. It fits nothing and recomputes nothing: tables
# are read from the pinned copy and single values are taken with bundle_cell()
# (R/behaviour_bundle.R), which returns exactly one stored cell or fails.
#
# Every function takes `repo_dir` (default: this repository's root) so that the
# importer logic can be exercised on a synthetic fixture in a temporary directory
# (tests/testthat/test-stage30-bundle-import.R) without touching real paths.

if (!exists("bundle_cell", mode = "function")) source(repo_path("R", "behaviour_bundle.R"))

S30_ID_PATTERN <- "^s30b_v[0-9]+_[0-9]{8}_[0-9a-f]{7}$"
# The local import directory of the upstream bundles (a directory of this repository).
S30_IMPORT_DIR <- "source_data/MMMSociability"
S30_PIN_FILE <- "config/stage30_bundle.yml"
S30_IMPORT_MANIFEST <- "provenance/source_manifests/stage30_bundle_manifest.csv"
S30_REGISTRY_COLUMNS <- c("bundle_id", "status", "manifest_sha256", "stage30_run_commit", "mmm_git_commit", "created_at")
S30_MANIFEST_COLUMNS <- c("file", "bytes", "sha256", "schema_version")
S30_PROVENANCE_KEYS <- c("bundle_id", "status", "mmm_git_commit", "stage30_run_commit", "stage30_registry_sha256",
                         "stage29_bundle_id", "stage29_bundle_manifest_sha256")

s30_sha <- function(p) unname(tools::sha256sum(p))
s30_stop <- function(...) stop(..., call. = FALSE)
s30_list <- function(d) sort(list.files(d, all.files = TRUE, no.. = TRUE))

#' key -> value from a bundle's H_provenance.csv; every key in `keys` must occur exactly once, non-empty.
s30_provenance <- function(path, keys = S30_PROVENANCE_KEYS) {
  prov <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character")
  if (!identical(names(prov), c("key", "value"))) s30_stop("H_provenance.csv must have columns key,value.")
  out <- lapply(keys, function(k) {
    v <- prov$value[prov$key == k]
    if (length(v) != 1L || is.na(v) || !nzchar(v)) s30_stop("H_provenance.csv does not record '", k, "' exactly once.")
    v
  })
  stats::setNames(out, keys)
}

#' Read and check a bundle's own manifest (00_manifest.csv): columns, plain file names, unique entries.
s30_read_manifest <- function(man_path) {
  man <- utils::read.csv(man_path, stringsAsFactors = FALSE, colClasses = c(bytes = "numeric"))
  miss <- setdiff(S30_MANIFEST_COLUMNS, names(man))
  if (length(miss)) s30_stop("00_manifest.csv is missing column(s): ", paste(miss, collapse = ", "))
  if (!nrow(man)) s30_stop("00_manifest.csv lists no files.")
  if (anyDuplicated(man$file)) s30_stop("00_manifest.csv lists a file twice.")
  if (any(!grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", man$file)) || "00_manifest.csv" %in% man$file)
    s30_stop("00_manifest.csv lists a file name that is not a plain bundle file.")
  man
}

#' Check an upstream bundle before import. Refuses unless: the id has the s30b form; the
#' registry lists it exactly once as FROZEN; its 00_manifest.csv hashes to the registry value;
#' every listed file matches its bytes and sha256; nothing unlisted is present; H_provenance
#' is FROZEN, names this bundle and agrees with the registry commits; and (if given) the
#' Stage 29 bundle it was built from is the pinned behaviour bundle.
stage30_bundle_check_source <- function(root, id, behaviour_pin = NULL) {
  if (!grepl(S30_ID_PATTERN, id)) s30_stop("Not a Stage 30 figure-bundle id (s30b_v<N>_<YYYYMMDD>_<commit7>): ", id)
  reg_path <- file.path(root, "BUNDLE_REGISTRY.csv")
  if (!file.exists(reg_path)) s30_stop("No BUNDLE_REGISTRY.csv in ", root)
  reg <- utils::read.csv(reg_path, stringsAsFactors = FALSE, colClasses = "character")
  miss <- setdiff(S30_REGISTRY_COLUMNS, names(reg))
  if (length(miss)) s30_stop("BUNDLE_REGISTRY.csv is missing column(s): ", paste(miss, collapse = ", "))
  row <- reg[reg$bundle_id == id, , drop = FALSE]
  if (nrow(row) != 1L || !identical(row$status, "FROZEN"))
    s30_stop("Bundle ", id, " is not registered exactly once as FROZEN.")
  src <- file.path(root, id)
  man_path <- file.path(src, "00_manifest.csv")
  if (!file.exists(man_path)) s30_stop("The bundle has no 00_manifest.csv: ", src)
  if (!identical(s30_sha(man_path), row$manifest_sha256))
    s30_stop("The bundle manifest does not match its registry hash.")
  man <- s30_read_manifest(man_path)
  paths <- file.path(src, man$file)
  if (any(!file.exists(paths))) s30_stop("A file listed in the bundle manifest is missing: ", paste(man$file[!file.exists(paths)], collapse = ", "))
  if (any(s30_sha(paths) != man$sha256) || any(file.size(paths) != man$bytes))
    s30_stop("A bundle file differs from its manifest.")
  extra <- setdiff(s30_list(src), c(man$file, "00_manifest.csv"))
  if (length(extra)) s30_stop("Unlisted files in the bundle: ", paste(extra, collapse = ", "))
  if (!"H_provenance.csv" %in% man$file) s30_stop("The bundle has no H_provenance.csv.")
  prov <- s30_provenance(file.path(src, "H_provenance.csv"))
  if (!identical(prov$status, "FROZEN")) s30_stop("H_provenance does not record status FROZEN.")
  if (!identical(prov$bundle_id, id)) s30_stop("H_provenance names a different bundle: ", prov$bundle_id)
  if (!identical(prov$mmm_git_commit, row$mmm_git_commit) || !identical(prov$stage30_run_commit, row$stage30_run_commit))
    s30_stop("H_provenance commits disagree with the registry.")
  if (!is.null(behaviour_pin)) {
    if (!identical(prov$stage29_bundle_id, behaviour_pin$bundle_id) ||
        !identical(prov$stage29_bundle_manifest_sha256, behaviour_pin$manifest_sha256))
      s30_stop("The Stage 30 bundle was built from Stage 29 bundle ", prov$stage29_bundle_id,
               ", not the pinned behaviour bundle ", behaviour_pin$bundle_id, ".")
  }
  list(row = row, manifest = man, provenance = prov, src = src)
}

#' Import one FROZEN Stage 30 figure bundle into `repo_dir`: immutable copy, pin, import manifest.
#' Returns the pin (a list) invisibly.
stage30_bundle_import <- function(root, id, repo_dir = repo_root(), behaviour_pin = NULL,
                                  imported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")) {
  chk <- stage30_bundle_check_source(root, id, behaviour_pin)
  man <- chk$manifest
  files <- c(man$file, "00_manifest.csv")
  dst <- file.path(repo_dir, S30_IMPORT_DIR, id)
  if (dir.exists(dst) || file.exists(dst)) s30_stop("Bundle already imported (immutable): ", dst)
  mp <- file.path(repo_dir, S30_IMPORT_MANIFEST)
  old <- if (file.exists(mp)) utils::read.csv(mp, stringsAsFactors = FALSE, colClasses = c(bytes = "numeric")) else NULL
  if (!is.null(old) && id %in% old$bundle_id) s30_stop("The import manifest already records ", id, " but no copy exists; resolve by hand.")
  if (max(nchar(file.path(normalizePath(dirname(dst), winslash = "/", mustWork = FALSE), id, files))) >= 260L)
    s30_stop("A destination path would be 260 characters or longer (Windows R cannot open it).")
  # Copy into a staging directory and move it into place only once it verifies.
  stage <- file.path(dirname(dst), paste0(".", id, ".partial"))
  unlink(stage, recursive = TRUE)
  dir.create(stage, recursive = TRUE)
  ok <- FALSE
  on.exit(if (!ok) unlink(stage, recursive = TRUE), add = TRUE)
  if (!all(file.copy(file.path(chk$src, files), file.path(stage, files), copy.mode = FALSE)))   # tracked copy: integrity is by hash, not file mode
    s30_stop("Copy failed.")
  if (any(s30_sha(file.path(stage, man$file)) != man$sha256) || !identical(s30_sha(file.path(stage, "00_manifest.csv")), chk$row$manifest_sha256))
    s30_stop("Imported copy does not verify.")
  if (!file.rename(stage, dst)) s30_stop("Could not move the verified copy into place: ", dst)
  ok <- TRUE

  prov <- chk$provenance
  pin <- list(
    bundle_id = id,
    manifest_sha256 = chk$row$manifest_sha256,
    registry_status = chk$row$status,
    mmm_git_commit = prov$mmm_git_commit,
    stage30_run_commit = prov$stage30_run_commit,
    stage30_registry_sha256 = prov$stage30_registry_sha256,
    stage29_bundle_id = prov$stage29_bundle_id,
    stage29_bundle_manifest_sha256 = prov$stage29_bundle_manifest_sha256,
    created_at = chk$row$created_at,
    producer = "topohl/MMMSociability Analysis/30b_stage30_figure_bundle.R",
    imported_at = imported_at,
    importer = "tools/import_stage30_bundle.R")
  dir.create(dirname(file.path(repo_dir, S30_PIN_FILE)), recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(pin, file.path(repo_dir, S30_PIN_FILE))

  rows <- data.frame(bundle_id = id, file = files, bytes = file.size(file.path(dst, files)),
                     sha256 = s30_sha(file.path(dst, files)), stringsAsFactors = FALSE)
  if (!is.null(old)) rows <- rbind(old, rows)
  dir.create(dirname(mp), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(rows, mp, row.names = FALSE)
  invisible(pin)
}

# ---------------------------------------------------------------- readers of the pinned copy
stage30_bundle_pin <- function(repo_dir = repo_root()) {
  p <- file.path(repo_dir, S30_PIN_FILE)
  if (!file.exists(p)) s30_stop("No Stage 30 bundle is pinned (", S30_PIN_FILE, "). Run tools/import_stage30_bundle.R.")
  pin <- yaml::read_yaml(p)
  if (!grepl(S30_ID_PATTERN, pin$bundle_id %||% "")) s30_stop("The Stage 30 pin does not name an s30b bundle.")
  pin
}

stage30_bundle_dir <- function(pin = stage30_bundle_pin(repo_dir), repo_dir = repo_root()) {
  file.path(repo_dir, S30_IMPORT_DIR, pin$bundle_id)
}

#' Verify the pinned copy byte-for-byte against its own manifest and the pinned manifest hash.
stage30_bundle_verify <- function(pin = stage30_bundle_pin(repo_dir), repo_dir = repo_root()) {
  d <- stage30_bundle_dir(pin, repo_dir)
  man_path <- file.path(d, "00_manifest.csv")
  if (!file.exists(man_path)) s30_stop("Pinned Stage 30 bundle is not imported: ", d)
  if (!identical(s30_sha(man_path), pin$manifest_sha256)) s30_stop("00_manifest.csv does not match the pinned manifest hash.")
  man <- s30_read_manifest(man_path)
  now <- s30_sha(file.path(d, man$file))
  bad <- man$file[is.na(now) | now != man$sha256]
  if (length(bad)) s30_stop("Stage 30 bundle files differ from the manifest: ", paste(bad, collapse = ", "))
  extra <- setdiff(s30_list(d), c(man$file, "00_manifest.csv"))
  if (length(extra)) s30_stop("Files in the Stage 30 bundle copy that the manifest does not list: ", paste(extra, collapse = ", "))
  invisible(TRUE)
}

stage30_bundle_table <- function(name, pin = stage30_bundle_pin(repo_dir), repo_dir = repo_root()) {
  p <- file.path(stage30_bundle_dir(pin, repo_dir), paste0(name, ".csv"))
  if (!file.exists(p)) s30_stop("Stage 30 bundle table not found: ", name)
  utils::read.csv(p, stringsAsFactors = FALSE, na.strings = c("NA", ""))
}
