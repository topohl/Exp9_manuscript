# Canonical figure-support bundle (fsb) resolver and importer logic.
#
# The figure-support bundle is produced and frozen in MMMSociability. Its exporter
# (Analysis/16c_figure_support_bundle.R) writes one immutable, hash-frozen directory
# per bundle,
#   <analysis_ready>/canonical/figure_support_bundle/<bundle_id>/
# registered in BUNDLE_REGISTRY.csv beside it (bundle_id, status, manifest_sha256,
# stage29_bundle_id, combz_table_sha256, mmm_git_commit, created_at). It carries
# descriptive values only: the CON cage means per measure x sex x cage change (F1), the
# CON reference means copied from the Stage 29 bundle (F1b), and the six CombZ
# components per animal as they enter CombZ (F2, F2b). No model is fitted upstream for
# it, and nothing is recomputed here.
# tools/import_figure_support_bundle.R copies one FROZEN bundle, unchanged, into
# source_data/MMMSociability/<bundle_id>/, pins it in config/figure_support_bundle.yml
# and records every file hash in provenance/source_manifests/figure_support_bundle_manifest.csv.
#
# Mirrors R/stage30_bundle.R and reuses its manifest and provenance readers. Tables
# are read from the pinned copy; single values are taken with bundle_cell()
# (R/behaviour_bundle.R), which returns exactly one stored cell or fails.
#
# Every function takes `repo_dir` (default: this repository's root) so that the
# importer logic can be exercised on a synthetic fixture in a temporary directory
# (tests/testthat/test-figure-support-bundle-import.R) without touching real paths.

if (!exists("bundle_cell", mode = "function")) source(repo_path("R", "behaviour_bundle.R"))
if (!exists("s30_read_manifest", mode = "function")) source(repo_path("R", "stage30_bundle.R"))

FSB_ID_PATTERN <- "^fsb_v[0-9]+_[0-9]{8}_[0-9a-f]{7}$"
# The local import directory of the upstream bundles (a directory of this repository).
FSB_IMPORT_DIR <- "source_data/MMMSociability"
FSB_PIN_FILE <- "config/figure_support_bundle.yml"
FSB_IMPORT_MANIFEST <- "provenance/source_manifests/figure_support_bundle_manifest.csv"
FSB_REGISTRY_COLUMNS <- c("bundle_id", "status", "manifest_sha256", "stage29_bundle_id", "combz_table_sha256",
                          "mmm_git_commit", "created_at")
FSB_PROVENANCE_KEYS <- c("bundle_id", "status", "mmm_git_commit", "stage29_bundle_id", "stage29_bundle_manifest_sha256",
                         "combz_table_sha256", "scientific_recomputation")
# The tables a figure-support bundle must carry (OPTION3_SPEC 1).
FSB_REQUIRED_FILES <- c("F1_con_cage_means.csv", "F1b_con_reference_means.csv", "F2_combz_components.csv",
                        "F2b_combz_definition.csv", "H_provenance.csv", "H2_inputs.csv")
# H_provenance scientific_recomputation must open with the specification's wording.
FSB_RECOMPUTATION_PREFIX <- "none; descriptive cage means and the frozen CombZ standardisation reproduced exactly; no model"

#' Check an upstream figure-support bundle before import. Refuses unless: the id has the fsb
#' form; the registry lists it exactly once as FROZEN; its 00_manifest.csv hashes to the
#' registry value; every listed file matches its bytes and sha256; nothing unlisted is present;
#' the required tables are listed; H_provenance is FROZEN, names this bundle, agrees with the
#' registry (commit, Stage 29 bundle, CombZ table hash) and declares no recomputation beyond
#' the descriptive one; and (if given) the Stage 29 bundle it was built from is the pinned
#' behaviour bundle.
fsb_bundle_check_source <- function(root, id, behaviour_pin = NULL) {
  if (!grepl(FSB_ID_PATTERN, id)) s30_stop("Not a figure-support bundle id (fsb_v<N>_<YYYYMMDD>_<commit7>): ", id)
  reg_path <- file.path(root, "BUNDLE_REGISTRY.csv")
  if (!file.exists(reg_path)) s30_stop("No BUNDLE_REGISTRY.csv in ", root)
  reg <- utils::read.csv(reg_path, stringsAsFactors = FALSE, colClasses = "character")
  miss <- setdiff(FSB_REGISTRY_COLUMNS, names(reg))
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
  absent <- setdiff(FSB_REQUIRED_FILES, man$file)
  if (length(absent)) s30_stop("The bundle does not carry the required table(s): ", paste(absent, collapse = ", "))
  prov <- s30_provenance(file.path(src, "H_provenance.csv"), keys = FSB_PROVENANCE_KEYS)
  if (!identical(prov$status, "FROZEN")) s30_stop("H_provenance does not record status FROZEN.")
  if (!identical(prov$bundle_id, id)) s30_stop("H_provenance names a different bundle: ", prov$bundle_id)
  if (!identical(prov$mmm_git_commit, row$mmm_git_commit) || !identical(prov$stage29_bundle_id, row$stage29_bundle_id) ||
      !identical(prov$combz_table_sha256, row$combz_table_sha256))
    s30_stop("H_provenance disagrees with the registry (commit, Stage 29 bundle or CombZ table hash).")
  if (!startsWith(prov$scientific_recomputation, FSB_RECOMPUTATION_PREFIX))
    s30_stop("H_provenance does not declare the descriptive-only recomputation ('", FSB_RECOMPUTATION_PREFIX, "').")
  if (!is.null(behaviour_pin)) {
    if (!identical(prov$stage29_bundle_id, behaviour_pin$bundle_id) ||
        !identical(prov$stage29_bundle_manifest_sha256, behaviour_pin$manifest_sha256))
      s30_stop("The figure-support bundle was built from Stage 29 bundle ", prov$stage29_bundle_id,
               ", not the pinned behaviour bundle ", behaviour_pin$bundle_id, ".")
  }
  list(row = row, manifest = man, provenance = prov, src = src)
}

#' Import one FROZEN figure-support bundle into `repo_dir`: immutable copy, pin, import manifest.
#' Returns the pin (a list) invisibly.
fsb_bundle_import <- function(root, id, repo_dir = repo_root(), behaviour_pin = NULL,
                              imported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")) {
  chk <- fsb_bundle_check_source(root, id, behaviour_pin)
  man <- chk$manifest
  files <- c(man$file, "00_manifest.csv")
  dst <- file.path(repo_dir, FSB_IMPORT_DIR, id)
  if (dir.exists(dst) || file.exists(dst)) s30_stop("Bundle already imported (immutable): ", dst)
  mp <- file.path(repo_dir, FSB_IMPORT_MANIFEST)
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
    stage29_bundle_id = prov$stage29_bundle_id,
    stage29_bundle_manifest_sha256 = prov$stage29_bundle_manifest_sha256,
    combz_table_sha256 = prov$combz_table_sha256,
    scientific_recomputation = prov$scientific_recomputation,
    created_at = chk$row$created_at,
    producer = "topohl/MMMSociability Analysis/16c_figure_support_bundle.R",
    imported_at = imported_at,
    importer = "tools/import_figure_support_bundle.R")
  dir.create(dirname(file.path(repo_dir, FSB_PIN_FILE)), recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(pin, file.path(repo_dir, FSB_PIN_FILE))

  rows <- data.frame(bundle_id = id, file = files, bytes = file.size(file.path(dst, files)),
                     sha256 = s30_sha(file.path(dst, files)), stringsAsFactors = FALSE)
  if (!is.null(old)) rows <- rbind(old, rows)
  dir.create(dirname(mp), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(rows, mp, row.names = FALSE)
  invisible(pin)
}

# ---------------------------------------------------------------- readers of the pinned copy
fsb_bundle_pin <- function(repo_dir = repo_root()) {
  p <- file.path(repo_dir, FSB_PIN_FILE)
  if (!file.exists(p)) s30_stop("No figure-support bundle is pinned (", FSB_PIN_FILE, "). Run tools/import_figure_support_bundle.R.")
  pin <- yaml::read_yaml(p)
  if (!grepl(FSB_ID_PATTERN, pin$bundle_id %||% "")) s30_stop("The figure-support pin does not name an fsb bundle.")
  pin
}

fsb_bundle_dir <- function(pin = fsb_bundle_pin(repo_dir), repo_dir = repo_root()) {
  file.path(repo_dir, FSB_IMPORT_DIR, pin$bundle_id)
}

#' Verify the pinned copy byte-for-byte against its own manifest and the pinned manifest hash.
fsb_bundle_verify <- function(pin = fsb_bundle_pin(repo_dir), repo_dir = repo_root()) {
  d <- fsb_bundle_dir(pin, repo_dir)
  man_path <- file.path(d, "00_manifest.csv")
  if (!file.exists(man_path)) s30_stop("Pinned figure-support bundle is not imported: ", d)
  if (!identical(s30_sha(man_path), pin$manifest_sha256)) s30_stop("00_manifest.csv does not match the pinned manifest hash.")
  man <- s30_read_manifest(man_path)
  now <- s30_sha(file.path(d, man$file))
  bad <- man$file[is.na(now) | now != man$sha256]
  if (length(bad)) s30_stop("Figure-support bundle files differ from the manifest: ", paste(bad, collapse = ", "))
  extra <- setdiff(s30_list(d), c(man$file, "00_manifest.csv"))
  if (length(extra)) s30_stop("Files in the figure-support bundle copy that the manifest does not list: ", paste(extra, collapse = ", "))
  invisible(TRUE)
}

fsb_bundle_table <- function(name, pin = fsb_bundle_pin(repo_dir), repo_dir = repo_root()) {
  p <- file.path(fsb_bundle_dir(pin, repo_dir), paste0(name, ".csv"))
  if (!file.exists(p)) s30_stop("Figure-support bundle table not found: ", name)
  utils::read.csv(p, stringsAsFactors = FALSE, na.strings = c("NA", ""))
}
