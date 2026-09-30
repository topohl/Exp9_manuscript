# Guards for the figure-support bundle (fsb) import boundary.
#
# The descriptive figure-support values (CON cage means, the CombZ components as they
# enter CombZ) enter this repository only as one pinned, hash-frozen bundle written by
# MMMSociability's Analysis/16c exporter and copied unchanged by
# tools/import_figure_support_bundle.R. The importer logic lives in
# R/figure_support_bundle.R so it can be exercised here on a synthetic FROZEN bundle and
# registry built in tempdir(): no real upstream path is read and nothing in this repository
# is written. If a real bundle is pinned, it is verified as well.

repo <- function(...) file.path(testthat::test_path("..", ".."), ...)
source(repo("R", "paths.R"))
source(repo("R", "behaviour_bundle.R"))
source(repo("R", "stage30_bundle.R"))
source(repo("R", "figure_support_bundle.R"))
IMPORTER <- repo("tools", "import_figure_support_bundle.R")
LIBRARY <- repo("R", "figure_support_bundle.R")
VERIFIER <- repo("tools", "verify_source_bundles.R")
have_pin <- file.exists(repo("config", "figure_support_bundle.yml"))
code_of <- function(p) { src <- readLines(p, warn = FALSE); src[!grepl("^[[:space:]]*#", src)] }

# ---------------------------------------------------------------- synthetic fixture
ID <- "fsb_v1_20260930_abcdef1"
ID2 <- "fsb_v1_20261001_1234567"
EBB <- list(bundle_id = "ebb_v101_20260929_b2ce507", manifest_sha256 = strrep("e", 64))
MMM_COMMIT <- strrep("a", 40)
COMBZ_SHA <- strrep("1", 64)
RECOMP <- "none; descriptive cage means and the frozen CombZ standardisation reproduced exactly; no model. Stored z-scores carried verbatim."
sha <- function(p) unname(tools::sha256sum(p))

make_env <- function() {
  base <- tempfile("fsb_")
  e <- list(base = base, root = file.path(base, "up"), repo = file.path(base, "rp"))
  dir.create(e$root, recursive = TRUE)
  dir.create(file.path(e$repo, "source_data", "MMMSociability"), recursive = TRUE)
  e
}

# One FROZEN bundle in the upstream root: the four F tables, H2, H_provenance, 00_manifest, a registry row.
add_bundle <- function(e, id = ID, status = "FROZEN", prov = list(), drop = character(0)) {
  src <- file.path(e$root, id)
  dir.create(src)
  utils::write.csv(data.frame(measure = "crossing_rate", Sex = "Female", CC = "CC1", Batch = c("B3", "B4", "B6"),
                              CageEpisodeID = c("B3|sys.3|CC1", "B4|sys.3|CC1", "B6|sys.2|CC1"), Group = "CON",
                              n_animals = 4L, cage_mean = c(20.5, 24.25, 28.125)),
                   file.path(src, "F1_con_cage_means.csv"), row.names = FALSE)
  utils::write.csv(data.frame(measure = "crossing_rate", Sex = "Female", CC = "CC1", Group = "CON", n = 12L, mean = 24.2916),
                   file.path(src, "F1b_con_reference_means.csv"), row.names = FALSE)
  utils::write.csv(data.frame(AnimalNum = c("F01", "F01"), Sex = "Female", Group = "SUS", component = c("NOR", "delta_cort"),
                              z = c(-1.5, 0.5), direction = c(1L, -1L), signed_z = c(-1.5, -0.5), present = TRUE,
                              CombZ = -1.0),
                   file.path(src, "F2_combz_components.csv"), row.names = FALSE)
  utils::write.csv(data.frame(component = c("NOR", "delta_cort"), direction = c(1L, -1L), display_label = c("NOR", "cort")),
                   file.path(src, "F2b_combz_definition.csv"), row.names = FALSE)
  utils::write.csv(data.frame(input = "x.csv", bytes = 1L, sha256 = strrep("9", 64), role = "combz_table"),
                   file.path(src, "H2_inputs.csv"), row.names = FALSE)
  p <- list(bundle_id = id, status = "FROZEN", generated_at = "2026-09-30T10:00:00+0200",
            generator = "Analysis/16c_figure_support_bundle.R", mmm_git_commit = MMM_COMMIT,
            stage29_bundle_id = EBB$bundle_id, stage29_bundle_manifest_sha256 = EBB$manifest_sha256,
            combz_table_sha256 = COMBZ_SHA, scientific_recomputation = RECOMP)
  p[names(prov)] <- prov
  utils::write.csv(data.frame(key = names(p), value = unlist(p), stringsAsFactors = FALSE),
                   file.path(src, "H_provenance.csv"), row.names = FALSE)
  unlink(file.path(src, drop))
  f <- setdiff(list.files(src), "00_manifest.csv")
  utils::write.csv(data.frame(file = f, bytes = file.size(file.path(src, f)), sha256 = sha(file.path(src, f)), schema_version = 1L),
                   file.path(src, "00_manifest.csv"), row.names = FALSE)
  reg_path <- file.path(e$root, "BUNDLE_REGISTRY.csv")
  row <- data.frame(bundle_id = id, status = status, manifest_sha256 = sha(file.path(src, "00_manifest.csv")),
                    stage29_bundle_id = EBB$bundle_id, combz_table_sha256 = COMBZ_SHA,
                    mmm_git_commit = MMM_COMMIT, created_at = "2026-09-30T10:00:01+0200")
  if (file.exists(reg_path)) row <- rbind(utils::read.csv(reg_path, stringsAsFactors = FALSE, colClasses = "character"), row)
  utils::write.csv(row, reg_path, row.names = FALSE)
  src
}

expect_untouched <- function(e, id = ID) {
  expect_false(dir.exists(file.path(e$repo, "source_data", "MMMSociability", id)))
  expect_equal(list.files(file.path(e$repo, "source_data", "MMMSociability"), all.files = TRUE, no.. = TRUE), character(0))
  expect_false(file.exists(file.path(e$repo, "config", "figure_support_bundle.yml")))
  expect_false(file.exists(file.path(e$repo, "provenance", "source_manifests", "figure_support_bundle_manifest.csv")))
}
refuses <- function(e, pattern, id = ID, bpin = EBB) {
  expect_error(fsb_bundle_import(e$root, id, repo_dir = e$repo, behaviour_pin = bpin), pattern, fixed = TRUE)
  expect_untouched(e, id)
}

test_that("a FROZEN figure-support bundle imports byte-exact, is pinned and is recorded", {
  e <- make_env(); withr::defer(unlink(e$base, recursive = TRUE))
  src <- add_bundle(e)
  pin <- fsb_bundle_import(e$root, ID, repo_dir = e$repo, behaviour_pin = EBB, imported_at = "2026-09-30T12:00:00+0200")
  dst <- file.path(e$repo, "source_data", "MMMSociability", ID)
  expect_true(dir.exists(dst))
  expect_equal(sort(list.files(dst)), sort(list.files(src)))
  expect_equal(sha(file.path(dst, list.files(src))), sha(file.path(src, list.files(src))))
  expect_equal(list.files(dirname(dst), all.files = TRUE, no.. = TRUE), ID)
  on_disk <- yaml::read_yaml(file.path(e$repo, "config", "figure_support_bundle.yml"))
  expect_equal(on_disk, pin)
  expect_equal(pin$bundle_id, ID)
  expect_equal(pin$manifest_sha256, sha(file.path(src, "00_manifest.csv")))
  expect_equal(pin$registry_status, "FROZEN")
  expect_equal(pin$mmm_git_commit, MMM_COMMIT)
  expect_equal(pin$stage29_bundle_id, EBB$bundle_id)
  expect_equal(pin$stage29_bundle_manifest_sha256, EBB$manifest_sha256)
  expect_equal(pin$combz_table_sha256, COMBZ_SHA)
  expect_equal(pin$scientific_recomputation, RECOMP)
  expect_equal(pin$importer, "tools/import_figure_support_bundle.R")
  man <- utils::read.csv(file.path(e$repo, "provenance", "source_manifests", "figure_support_bundle_manifest.csv"), stringsAsFactors = FALSE)
  expect_equal(names(man), c("bundle_id", "file", "bytes", "sha256"))
  expect_true(all(man$bundle_id == ID))
  expect_setequal(man$file, list.files(dst))
  expect_equal(man$sha256, sha(file.path(dst, man$file)))
  expect_equal(fsb_bundle_pin(e$repo), pin)
  expect_equal(normalizePath(fsb_bundle_dir(pin, e$repo)), normalizePath(dst))
  expect_true(fsb_bundle_verify(pin, e$repo))
  f1 <- fsb_bundle_table("F1_con_cage_means", pin, e$repo)
  expect_equal(bundle_cell(f1, "cage_mean", Batch = "B4"), 24.25)          # bundle_cell is reused
  expect_error(bundle_cell(f1, "cage_mean", Batch = "B1"), "resolves 0 rows", fixed = TRUE)
  expect_error(fsb_bundle_table("F9_absent", pin, e$repo), "table not found", fixed = TRUE)
})

test_that("the importer refuses a figure-support bundle it cannot trust, and writes nothing", {
  e <- make_env(); withr::defer(unlink(e$base, recursive = TRUE)); add_bundle(e)
  refuses(e, "Not a figure-support bundle id", id = "fsb_dry_20260930_abcdef1")
  refuses(e, "Not a figure-support bundle id", id = "s30b_v10_20260929_5394f2f")
  refuses(e, "is not registered exactly once as FROZEN", id = ID2)
  e2 <- make_env(); withr::defer(unlink(e2$base, recursive = TRUE)); add_bundle(e2)
  reg <- utils::read.csv(file.path(e2$root, "BUNDLE_REGISTRY.csv"), stringsAsFactors = FALSE, colClasses = "character")
  utils::write.csv(rbind(reg, reg), file.path(e2$root, "BUNDLE_REGISTRY.csv"), row.names = FALSE)
  refuses(e2, "is not registered exactly once as FROZEN")
  e3 <- make_env(); withr::defer(unlink(e3$base, recursive = TRUE)); add_bundle(e3, status = "DRY_RUN_NOT_FOR_USE")
  refuses(e3, "is not registered exactly once as FROZEN")
  e4 <- make_env(); withr::defer(unlink(e4$base, recursive = TRUE)); add_bundle(e4)
  reg <- utils::read.csv(file.path(e4$root, "BUNDLE_REGISTRY.csv"), stringsAsFactors = FALSE, colClasses = "character")
  reg$manifest_sha256 <- strrep("0", 64)
  utils::write.csv(reg, file.path(e4$root, "BUNDLE_REGISTRY.csv"), row.names = FALSE)
  refuses(e4, "manifest does not match its registry hash")
  e5 <- make_env(); withr::defer(unlink(e5$base, recursive = TRUE)); s5 <- add_bundle(e5)
  cat("tampered\n", file = file.path(s5, "F1_con_cage_means.csv"), append = TRUE)
  refuses(e5, "differs from its manifest")
  e6 <- make_env(); withr::defer(unlink(e6$base, recursive = TRUE)); s6 <- add_bundle(e6)
  unlink(file.path(s6, "F2b_combz_definition.csv"))
  refuses(e6, "is missing: F2b_combz_definition.csv")
  e7 <- make_env(); withr::defer(unlink(e7$base, recursive = TRUE)); s7 <- add_bundle(e7)
  writeLines("scratch", file.path(s7, "notes.txt"))
  refuses(e7, "Unlisted files in the bundle: notes.txt")
  e8 <- make_env(); withr::defer(unlink(e8$base, recursive = TRUE)); add_bundle(e8, drop = "F2_combz_components.csv")
  refuses(e8, "does not carry the required table(s): F2_combz_components.csv")
  e9 <- make_env(); withr::defer(unlink(e9$base, recursive = TRUE)); add_bundle(e9, prov = list(status = "DRY_RUN"))
  refuses(e9, "H_provenance does not record status FROZEN")
  e10 <- make_env(); withr::defer(unlink(e10$base, recursive = TRUE)); add_bundle(e10, prov = list(bundle_id = ID2))
  refuses(e10, "H_provenance names a different bundle")
  e11 <- make_env(); withr::defer(unlink(e11$base, recursive = TRUE)); add_bundle(e11, prov = list(mmm_git_commit = strrep("b", 40)))
  refuses(e11, "H_provenance disagrees with the registry")
  e12 <- make_env(); withr::defer(unlink(e12$base, recursive = TRUE)); add_bundle(e12, prov = list(combz_table_sha256 = strrep("2", 64)))
  refuses(e12, "H_provenance disagrees with the registry")
  e13 <- make_env(); withr::defer(unlink(e13$base, recursive = TRUE)); add_bundle(e13, prov = list(scientific_recomputation = "model refit"))
  refuses(e13, "does not declare the descriptive-only recomputation")
  e14 <- make_env(); withr::defer(unlink(e14$base, recursive = TRUE)); add_bundle(e14, prov = list(stage29_bundle_manifest_sha256 = ""))
  refuses(e14, "does not record 'stage29_bundle_manifest_sha256' exactly once")
  e15 <- make_env(); withr::defer(unlink(e15$base, recursive = TRUE)); add_bundle(e15)
  refuses(e15, "not the pinned behaviour bundle", bpin = list(bundle_id = "ebb_v100_20260927_95e5dc8", manifest_sha256 = strrep("f", 64)))
})

test_that("an imported figure-support bundle is immutable and later imports append to the record", {
  e <- make_env(); withr::defer(unlink(e$base, recursive = TRUE))
  add_bundle(e)
  fsb_bundle_import(e$root, ID, repo_dir = e$repo, behaviour_pin = EBB)
  before <- utils::read.csv(file.path(e$repo, "provenance", "source_manifests", "figure_support_bundle_manifest.csv"), stringsAsFactors = FALSE)
  expect_error(fsb_bundle_import(e$root, ID, repo_dir = e$repo, behaviour_pin = EBB), "already imported (immutable)", fixed = TRUE)
  add_bundle(e, id = ID2)
  pin2 <- fsb_bundle_import(e$root, ID2, repo_dir = e$repo, behaviour_pin = EBB)
  after <- utils::read.csv(file.path(e$repo, "provenance", "source_manifests", "figure_support_bundle_manifest.csv"), stringsAsFactors = FALSE)
  expect_equal(after[after$bundle_id == ID, ], before)
  expect_setequal(unique(after$bundle_id), c(ID, ID2))
  expect_equal(fsb_bundle_pin(e$repo)$bundle_id, ID2)
  expect_true(dir.exists(file.path(e$repo, "source_data", "MMMSociability", ID)))
  expect_true(fsb_bundle_verify(pin2, e$repo))
})

test_that("verification catches an altered, extended or mis-pinned figure-support copy", {
  e <- make_env(); withr::defer(unlink(e$base, recursive = TRUE))
  add_bundle(e)
  pin <- fsb_bundle_import(e$root, ID, repo_dir = e$repo, behaviour_pin = EBB)
  dst <- fsb_bundle_dir(pin, e$repo)
  f <- file.path(dst, "F2_combz_components.csv")
  keep <- readBin(f, "raw", file.size(f))
  cat("1\n", file = f, append = TRUE)
  expect_error(fsb_bundle_verify(pin, e$repo), "differ from the manifest: F2_combz_components.csv", fixed = TRUE)
  writeBin(keep, f)
  expect_true(fsb_bundle_verify(pin, e$repo))
  writeLines("x", file.path(dst, "extra.csv"))
  expect_error(fsb_bundle_verify(pin, e$repo), "does not list: extra.csv", fixed = TRUE)
  unlink(file.path(dst, "extra.csv"))
  bad <- pin; bad$manifest_sha256 <- strrep("0", 64)
  expect_error(fsb_bundle_verify(bad, e$repo), "does not match the pinned manifest hash", fixed = TRUE)
  expect_error(fsb_bundle_pin(file.path(e$base, "nowhere")), "No figure-support bundle is pinned", fixed = TRUE)
})

test_that("the figure-support importer delegates to the tested logic and names no upstream path", {
  tool <- code_of(IMPORTER)
  for (must in c("fsb_bundle_import(", "MMM_FIGURE_SUPPORT_BUNDLE_ROOT", "behaviour_bundle_pin()", "behaviour_bundle_verify(", "fsb_bundle_verify("))
    expect_true(any(grepl(must, tool, fixed = TRUE)), info = must)
  for (src in c(IMPORTER, LIBRARY)) {
    code <- code_of(src)
    for (f in c("[\"'][A-Za-z]:[/\\\\]", "[\"'](//|\\\\\\\\)[A-Za-z]"))
      expect_equal(grep(f, code, value = TRUE), character(0), info = paste(basename(src), "names an absolute path:", f))
  }
  lib <- paste(readLines(LIBRARY, warn = FALSE), collapse = "\n")
  for (must in c('identical(row$status, "FROZEN")', "manifest does not match its registry hash", "differs from its manifest",
                 "Unlisted files in the bundle", "already imported (immutable)", "Imported copy does not verify",
                 "does not declare the descriptive-only recomputation"))
    expect_true(grepl(must, lib, fixed = TRUE), info = must)
  expect_identical(FSB_ID_PATTERN, "^fsb_v[0-9]+_[0-9]{8}_[0-9a-f]{7}$")
  ver <- paste(code_of(VERIFIER), collapse = "\n")
  expect_true(grepl('repo_path("config", "figure_support_bundle.yml")', ver, fixed = TRUE))
  expect_true(grepl("fsb_bundle_verify(", ver, fixed = TRUE))
})

test_that("the pinned figure-support bundle, if any, is FROZEN, verifies and matches the behaviour pin", {
  skip_if_not(have_pin, "no figure-support bundle pinned")
  pin <- fsb_bundle_pin()
  expect_match(pin$bundle_id, FSB_ID_PATTERN)
  expect_false(grepl("dry", pin$bundle_id, fixed = TRUE))
  expect_true(fsb_bundle_verify(pin))
  d <- fsb_bundle_dir(pin)
  expect_equal(list.files(d, pattern = "[.][Rr]$"), character(0))
  expect_true(all(FSB_REQUIRED_FILES %in% list.files(d)))
  man <- utils::read.csv(repo("provenance", "source_manifests", "figure_support_bundle_manifest.csv"), stringsAsFactors = FALSE)
  man <- man[man$bundle_id == pin$bundle_id, , drop = FALSE]
  expect_setequal(man$file, list.files(d))
  expect_equal(unname(tools::sha256sum(file.path(d, man$file))), man$sha256)
  prov <- s30_provenance(file.path(d, "H_provenance.csv"), keys = FSB_PROVENANCE_KEYS)
  expect_equal(prov$status, "FROZEN")
  expect_equal(prov$bundle_id, pin$bundle_id)
  expect_true(startsWith(prov$scientific_recomputation, FSB_RECOMPUTATION_PREFIX))
  bpin <- behaviour_bundle_pin()
  expect_equal(pin$stage29_bundle_id, bpin$bundle_id)
  expect_equal(pin$stage29_bundle_manifest_sha256, bpin$manifest_sha256)
})
