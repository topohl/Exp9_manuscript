# Guards for the Stage 30 figure-bundle import boundary.
#
# The Stage 30 exploratory-screen results enter this repository only as one
# pinned, hash-frozen bundle written by MMMSociability's Analysis/30b exporter and
# copied unchanged by tools/import_stage30_bundle.R. The importer logic lives in
# R/stage30_bundle.R so it can be exercised here on a synthetic FROZEN bundle and
# registry built in tempdir(): no real upstream path is read and nothing in this
# repository is written. If a real bundle is pinned, it is verified as well.

repo <- function(...) file.path(testthat::test_path("..", ".."), ...)
source(repo("R", "paths.R"))
source(repo("R", "behaviour_bundle.R"))
source(repo("R", "stage30_bundle.R"))
IMPORTER <- repo("tools", "import_stage30_bundle.R")
LIBRARY <- repo("R", "stage30_bundle.R")
VERIFIER <- repo("tools", "verify_source_bundles.R")
have_pin <- file.exists(repo("config", "stage30_bundle.yml"))
code_of <- function(p) { src <- readLines(p, warn = FALSE); src[!grepl("^[[:space:]]*#", src)] }

# ---------------------------------------------------------------- synthetic fixture
ID <- "s30b_v10_20260930_abcdef1"
ID2 <- "s30b_v10_20261001_1234567"
EBB <- list(bundle_id = "ebb_v101_20260929_b2ce507", manifest_sha256 = strrep("e", 64))
MMM_COMMIT <- strrep("a", 40)
RUN_COMMIT <- "be71e2f"
sha <- function(p) unname(tools::sha256sum(p))

# An empty upstream root (bundle registry location) and an empty target repository.
make_env <- function() {
  base <- tempfile("s30_")
  e <- list(base = base, root = file.path(base, "up"), repo = file.path(base, "rp"))
  dir.create(e$root, recursive = TRUE)
  dir.create(file.path(e$repo, "source_data", "MMMSociability"), recursive = TRUE)
  e
}

# One FROZEN bundle in the upstream root: three tables, H_provenance, 00_manifest, a registry row.
add_bundle <- function(e, id = ID, status = "FROZEN", prov = list()) {
  src <- file.path(e$root, id)
  dir.create(src)
  utils::write.csv(data.frame(id = c("SLEEP-CAT-IA40L-F", "COOKIE-CAT-F"), estimate = c(-0.0123, -5.62),
                              ci_low = c(-0.02, -15.7), ci_high = c(-0.004, 4.49), p_raw = c(0.004, 0.27),
                              classification = c("B", "C"), row_order = 1:2),
                   file.path(src, "S0_master_hypotheses.csv"), row.names = FALSE)
  utils::write.csv(data.frame(AnimalNum = c("F01", "F02", "M01"), Sex = c("Female", "Female", "Male"),
                              Group = c("RES", "SUS", "RES"), posinact40_light = c(0.995, 0.981, 0.999)),
                   file.path(src, "S1_light_animals_cc1.csv"), row.names = FALSE)
  utils::write.csv(data.frame(measure_col = "light_phase_crossing_rate",
                              display_label = "light-phase RFID position-change rate", unit = "position changes/h"),
                   file.path(src, "S5_display_labels.csv"), row.names = FALSE)
  p <- list(bundle_id = id, status = "FROZEN", generated_at = "2026-09-30T10:00:00+0200",
            generator = "Analysis/30b_stage30_figure_bundle.R", mmm_git_commit = MMM_COMMIT,
            stage30_run_commit = RUN_COMMIT, stage30_registry_sha256 = strrep("5", 64),
            stage29_bundle_id = EBB$bundle_id, stage29_bundle_manifest_sha256 = EBB$manifest_sha256,
            scientific_recomputation = "none; copies of frozen values")
  p[names(prov)] <- prov
  utils::write.csv(data.frame(key = names(p), value = unlist(p), stringsAsFactors = FALSE),
                   file.path(src, "H_provenance.csv"), row.names = FALSE)
  f <- setdiff(list.files(src), "00_manifest.csv")
  utils::write.csv(data.frame(file = f, bytes = file.size(file.path(src, f)), sha256 = sha(file.path(src, f)), schema_version = 1L),
                   file.path(src, "00_manifest.csv"), row.names = FALSE)
  reg_path <- file.path(e$root, "BUNDLE_REGISTRY.csv")
  row <- data.frame(bundle_id = id, status = status, manifest_sha256 = sha(file.path(src, "00_manifest.csv")),
                    stage30_run_commit = RUN_COMMIT, mmm_git_commit = MMM_COMMIT, created_at = "2026-09-30T10:00:01+0200")
  if (file.exists(reg_path)) row <- rbind(utils::read.csv(reg_path, stringsAsFactors = FALSE, colClasses = "character"), row)
  utils::write.csv(row, reg_path, row.names = FALSE)
  src
}

# Nothing may have been written to the target repository by a refused import.
expect_untouched <- function(e, id = ID) {
  expect_false(dir.exists(file.path(e$repo, "source_data", "MMMSociability", id)))
  expect_equal(list.files(file.path(e$repo, "source_data", "MMMSociability"), all.files = TRUE, no.. = TRUE), character(0))
  expect_false(file.exists(file.path(e$repo, "config", "stage30_bundle.yml")))
  expect_false(file.exists(file.path(e$repo, "provenance", "source_manifests", "stage30_bundle_manifest.csv")))
}
refuses <- function(e, pattern, id = ID, bpin = EBB) {
  expect_error(stage30_bundle_import(e$root, id, repo_dir = e$repo, behaviour_pin = bpin), pattern, fixed = TRUE)
  expect_untouched(e, id)
}

test_that("a FROZEN bundle imports byte-exact, is pinned and is recorded", {
  e <- make_env(); withr::defer(unlink(e$base, recursive = TRUE))
  src <- add_bundle(e)
  pin <- stage30_bundle_import(e$root, ID, repo_dir = e$repo, behaviour_pin = EBB, imported_at = "2026-09-30T12:00:00+0200")
  dst <- file.path(e$repo, "source_data", "MMMSociability", ID)
  expect_true(dir.exists(dst))
  expect_equal(sort(list.files(dst)), sort(list.files(src)))
  expect_equal(sha(file.path(dst, list.files(src))), sha(file.path(src, list.files(src))))
  # no staging directory is left behind
  expect_equal(list.files(dirname(dst), all.files = TRUE, no.. = TRUE), ID)
  # the pin
  on_disk <- yaml::read_yaml(file.path(e$repo, "config", "stage30_bundle.yml"))
  expect_equal(on_disk, pin)
  expect_equal(pin$bundle_id, ID)
  expect_equal(pin$manifest_sha256, sha(file.path(src, "00_manifest.csv")))
  expect_equal(pin$registry_status, "FROZEN")
  expect_equal(pin$mmm_git_commit, MMM_COMMIT)
  expect_equal(pin$stage30_run_commit, RUN_COMMIT)
  expect_equal(pin$stage29_bundle_id, EBB$bundle_id)
  expect_equal(pin$stage29_bundle_manifest_sha256, EBB$manifest_sha256)
  expect_equal(pin$importer, "tools/import_stage30_bundle.R")
  # the import manifest: every file of the copy, including 00_manifest.csv
  man <- utils::read.csv(file.path(e$repo, "provenance", "source_manifests", "stage30_bundle_manifest.csv"), stringsAsFactors = FALSE)
  expect_equal(names(man), c("bundle_id", "file", "bytes", "sha256"))
  expect_true(all(man$bundle_id == ID))
  expect_setequal(man$file, list.files(dst))
  expect_equal(man$sha256, sha(file.path(dst, man$file)))
  expect_equal(man$bytes, as.numeric(file.size(file.path(dst, man$file))))
  # readers of the pinned copy
  expect_equal(stage30_bundle_pin(e$repo), pin)
  expect_equal(normalizePath(stage30_bundle_dir(pin, e$repo)), normalizePath(dst))
  expect_true(stage30_bundle_verify(pin, e$repo))
  s0 <- stage30_bundle_table("S0_master_hypotheses", pin, e$repo)
  expect_equal(bundle_cell(s0, "estimate", id = "COOKIE-CAT-F"), -5.62)          # bundle_cell is reused
  expect_error(bundle_cell(s0, "estimate", id = "NOT-A-ROW"), "resolves 0 rows", fixed = TRUE)
  expect_error(stage30_bundle_table("S9_absent", pin, e$repo), "table not found", fixed = TRUE)
})

test_that("the importer refuses a bundle it cannot trust, and writes nothing", {
  # malformed or dry-run ids
  e <- make_env(); withr::defer(unlink(e$base, recursive = TRUE)); add_bundle(e)
  refuses(e, "Not a Stage 30 figure-bundle id", id = "s30b_dry_20260930_abcdef1")
  refuses(e, "Not a Stage 30 figure-bundle id", id = "ebb_v101_20260929_b2ce507")
  # not registered / registered twice / not FROZEN
  refuses(e, "is not registered exactly once as FROZEN", id = ID2)
  e2 <- make_env(); withr::defer(unlink(e2$base, recursive = TRUE)); add_bundle(e2)
  reg <- utils::read.csv(file.path(e2$root, "BUNDLE_REGISTRY.csv"), stringsAsFactors = FALSE, colClasses = "character")
  utils::write.csv(rbind(reg, reg), file.path(e2$root, "BUNDLE_REGISTRY.csv"), row.names = FALSE)
  refuses(e2, "is not registered exactly once as FROZEN")
  e3 <- make_env(); withr::defer(unlink(e3$base, recursive = TRUE)); add_bundle(e3, status = "DRAFT")
  refuses(e3, "is not registered exactly once as FROZEN")
  # manifest hash differs from the registry
  e4 <- make_env(); withr::defer(unlink(e4$base, recursive = TRUE)); add_bundle(e4)
  reg <- utils::read.csv(file.path(e4$root, "BUNDLE_REGISTRY.csv"), stringsAsFactors = FALSE, colClasses = "character")
  reg$manifest_sha256 <- strrep("0", 64)
  utils::write.csv(reg, file.path(e4$root, "BUNDLE_REGISTRY.csv"), row.names = FALSE)
  refuses(e4, "manifest does not match its registry hash")
  # a listed file altered, a listed file missing, an unlisted file present
  e5 <- make_env(); withr::defer(unlink(e5$base, recursive = TRUE)); s5 <- add_bundle(e5)
  cat("tampered\n", file = file.path(s5, "S1_light_animals_cc1.csv"), append = TRUE)
  refuses(e5, "differs from its manifest")
  e6 <- make_env(); withr::defer(unlink(e6$base, recursive = TRUE)); s6 <- add_bundle(e6)
  unlink(file.path(s6, "S5_display_labels.csv"))
  refuses(e6, "is missing: S5_display_labels.csv")
  e7 <- make_env(); withr::defer(unlink(e7$base, recursive = TRUE)); s7 <- add_bundle(e7)
  writeLines("scratch", file.path(s7, "notes.txt"))
  refuses(e7, "Unlisted files in the bundle: notes.txt")
  # H_provenance not FROZEN, naming another bundle, or disagreeing with the registry commits
  e8 <- make_env(); withr::defer(unlink(e8$base, recursive = TRUE)); add_bundle(e8, prov = list(status = "DRY_RUN"))
  refuses(e8, "H_provenance does not record status FROZEN")
  e9 <- make_env(); withr::defer(unlink(e9$base, recursive = TRUE)); add_bundle(e9, prov = list(bundle_id = ID2))
  refuses(e9, "H_provenance names a different bundle")
  e10 <- make_env(); withr::defer(unlink(e10$base, recursive = TRUE)); add_bundle(e10, prov = list(mmm_git_commit = strrep("b", 40)))
  refuses(e10, "H_provenance commits disagree with the registry")
  e11 <- make_env(); withr::defer(unlink(e11$base, recursive = TRUE)); add_bundle(e11, prov = list(stage29_bundle_manifest_sha256 = ""))
  refuses(e11, "does not record 'stage29_bundle_manifest_sha256' exactly once")
  # built from a Stage 29 bundle other than the pinned behaviour bundle
  e12 <- make_env(); withr::defer(unlink(e12$base, recursive = TRUE)); add_bundle(e12)
  refuses(e12, "not the pinned behaviour bundle", bpin = list(bundle_id = "ebb_v100_20260927_95e5dc8", manifest_sha256 = strrep("f", 64)))
})

test_that("an imported bundle is immutable and later imports append to the record", {
  e <- make_env(); withr::defer(unlink(e$base, recursive = TRUE))
  add_bundle(e)
  stage30_bundle_import(e$root, ID, repo_dir = e$repo, behaviour_pin = EBB)
  before <- utils::read.csv(file.path(e$repo, "provenance", "source_manifests", "stage30_bundle_manifest.csv"), stringsAsFactors = FALSE)
  expect_error(stage30_bundle_import(e$root, ID, repo_dir = e$repo, behaviour_pin = EBB), "already imported (immutable)", fixed = TRUE)
  add_bundle(e, id = ID2)
  pin2 <- stage30_bundle_import(e$root, ID2, repo_dir = e$repo, behaviour_pin = EBB)
  after <- utils::read.csv(file.path(e$repo, "provenance", "source_manifests", "stage30_bundle_manifest.csv"), stringsAsFactors = FALSE)
  expect_equal(after[after$bundle_id == ID, ], before)
  expect_setequal(unique(after$bundle_id), c(ID, ID2))
  # the pin moves to the newer bundle; the earlier copy stays as provenance
  expect_equal(stage30_bundle_pin(e$repo)$bundle_id, ID2)
  expect_true(dir.exists(file.path(e$repo, "source_data", "MMMSociability", ID)))
  expect_true(stage30_bundle_verify(pin2, e$repo))
})

test_that("verification catches an altered, extended or mis-pinned copy", {
  e <- make_env(); withr::defer(unlink(e$base, recursive = TRUE))
  add_bundle(e)
  pin <- stage30_bundle_import(e$root, ID, repo_dir = e$repo, behaviour_pin = EBB)
  dst <- stage30_bundle_dir(pin, e$repo)
  f <- file.path(dst, "S0_master_hypotheses.csv")
  keep <- readBin(f, "raw", file.size(f))
  cat("1\n", file = f, append = TRUE)
  expect_error(stage30_bundle_verify(pin, e$repo), "differ from the manifest: S0_master_hypotheses.csv", fixed = TRUE)
  writeBin(keep, f)
  expect_true(stage30_bundle_verify(pin, e$repo))
  writeLines("x", file.path(dst, "extra.csv"))
  expect_error(stage30_bundle_verify(pin, e$repo), "does not list: extra.csv", fixed = TRUE)
  unlink(file.path(dst, "extra.csv"))
  bad <- pin; bad$manifest_sha256 <- strrep("0", 64)
  expect_error(stage30_bundle_verify(bad, e$repo), "does not match the pinned manifest hash", fixed = TRUE)
  expect_error(stage30_bundle_pin(file.path(e$base, "nowhere")), "No Stage 30 bundle is pinned", fixed = TRUE)
})

test_that("the importer tool delegates to the tested logic and names no upstream path", {
  tool <- code_of(IMPORTER)
  for (must in c("stage30_bundle_import(", "MMM_STAGE30_BUNDLE_ROOT", "behaviour_bundle_pin()", "behaviour_bundle_verify(", "stage30_bundle_verify("))
    expect_true(any(grepl(must, tool, fixed = TRUE)), info = must)
  # no absolute path (drive letter or UNC share): the upstream root comes only from the environment
  for (src in c(IMPORTER, LIBRARY)) {
    code <- code_of(src)
    for (f in c("[\"'][A-Za-z]:[/\\\\]", "[\"'](//|\\\\\\\\)[A-Za-z]"))
      expect_equal(grep(f, code, value = TRUE), character(0), info = paste(basename(src), "names an absolute path:", f))
  }
  # the library carries every refusal the importer promises
  lib <- paste(readLines(LIBRARY, warn = FALSE), collapse = "\n")
  for (must in c('identical(row$status, "FROZEN")', "manifest does not match its registry hash", "differs from its manifest",
                 "Unlisted files in the bundle", "already imported (immutable)", "Imported copy does not verify"))
    expect_true(grepl(must, lib, fixed = TRUE), info = must)
  # verify_source_bundles.R checks the Stage 30 pin when one exists
  ver <- paste(code_of(VERIFIER), collapse = "\n")
  expect_true(grepl('repo_path("config", "stage30_bundle.yml")', ver, fixed = TRUE))
  expect_true(grepl("stage30_bundle_verify(", ver, fixed = TRUE))
})

test_that("the pinned Stage 30 bundle, if any, is FROZEN, verifies and matches the behaviour pin", {
  skip_if_not(have_pin, "no Stage 30 bundle pinned")
  pin <- stage30_bundle_pin()
  expect_match(pin$bundle_id, S30_ID_PATTERN)
  expect_false(grepl("dry", pin$bundle_id, fixed = TRUE))
  expect_true(stage30_bundle_verify(pin))
  d <- stage30_bundle_dir(pin)
  expect_equal(list.files(d, pattern = "[.][Rr]$"), character(0))
  man <- utils::read.csv(repo("provenance", "source_manifests", "stage30_bundle_manifest.csv"), stringsAsFactors = FALSE)
  man <- man[man$bundle_id == pin$bundle_id, , drop = FALSE]
  expect_setequal(man$file, list.files(d))
  expect_equal(unname(tools::sha256sum(file.path(d, man$file))), man$sha256)
  prov <- s30_provenance(file.path(d, "H_provenance.csv"))
  expect_equal(prov$status, "FROZEN")
  expect_equal(prov$bundle_id, pin$bundle_id)
  bpin <- behaviour_bundle_pin()
  expect_equal(pin$stage29_bundle_id, bpin$bundle_id)
  expect_equal(pin$stage29_bundle_manifest_sha256, bpin$manifest_sha256)
})
