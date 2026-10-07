# Guards for the CC4 candidate-bundle record (R/cc4_candidate_bundles.R): the pins and import manifest
# that tools/import_cc4_*.R write for the MMMSociability Stage 31 runs, and the verification that
# tools/verify_source_bundles.R runs on every copy. Synthetic fixtures in tempdir(); the real
# record is checked last.

repo <- function(...) file.path(testthat::test_path("..", ".."), ...)
source(repo("R", "paths.R"))
source(repo("R", "cc4_candidate_bundles.R"))

# A fixture repository and an upstream Stage 31 run with a nested file, its manifest written last.
fixture <- function() {
  root <- tempfile("cc4_fixture_")
  repo_dir <- file.path(root, "repo"); run <- file.path(root, "phase_groups_20990101_v1")
  dir.create(file.path(run, "bootstrap"), recursive = TRUE)
  writeLines(c("a,b", "1,2"), file.path(run, "group_phase_summary.csv"))
  writeLines(c("draw,value", "1,0.5"), file.path(run, "bootstrap", "draws.csv"))
  files <- c("bootstrap/draws.csv", "group_phase_summary.csv")
  utils::write.csv(data.frame(File = files, SHA256 = unname(tools::sha256sum(file.path(run, files)))),
                   file.path(run, "manifest.csv"), row.names = FALSE)
  list(root = root, repo = repo_dir, run = run, id = "cc4_phase_groups_20990101_v1")
}
# What an importer does before it records the copy: copy the listed files and manifest.csv.
copy_run <- function(fx, id = fx$id) {
  dst <- file.path(fx$repo, CC4_CANDIDATE_DIR, id)
  m <- utils::read.csv(file.path(fx$run, "manifest.csv"), stringsAsFactors = FALSE)
  for (f in c(m$File, "manifest.csv")) {
    dir.create(dirname(file.path(dst, f)), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(fx$run, f), file.path(dst, f))
  }
  dst
}

test_that("a recorded copy is pinned, listed file by file and verifies", {
  fx <- fixture(); on.exit(unlink(fx$root, recursive = TRUE), add = TRUE)
  copy_run(fx)
  pin <- cc4_candidate_register(fx$run, fx$id, repo_dir = fx$repo, imported_at = "fixture")
  expect_equal(pin$family, "phase_groups")
  expect_equal(pin$stage, "31b")
  expect_equal(pin$upstream_run, basename(fx$run))
  expect_equal(pin$files, 3L)
  expect_equal(pin$manifest_sha256, unname(tools::sha256sum(file.path(fx$run, "manifest.csv"))))
  pins <- cc4_candidate_pins(fx$repo)
  expect_equal(names(pins), fx$id)
  im <- cc4_candidate_import_manifest(fx$repo)
  expect_setequal(im$file, c("bootstrap/draws.csv", "group_phase_summary.csv", "manifest.csv"))
  expect_true(cc4_candidate_verify(pins[[1]], repo_dir = fx$repo))
  res <- cc4_candidate_verify_all(fx$repo)
  expect_equal(nrow(res), 1L)
  expect_true(res$ok)
})

test_that("recording refuses a second record, a foreign manifest and an unknown family", {
  fx <- fixture(); on.exit(unlink(fx$root, recursive = TRUE), add = TRUE)
  copy_run(fx)
  cc4_candidate_register(fx$run, fx$id, repo_dir = fx$repo, imported_at = "fixture")
  expect_error(cc4_candidate_register(fx$run, fx$id, repo_dir = fx$repo), "already recorded")
  # an upstream run whose manifest is not the copy's
  other <- file.path(fx$root, "other_run"); dir.create(other)
  writeLines("File,SHA256", file.path(other, "manifest.csv"))
  id2 <- "cc4_phase_groups_20990101_v2"
  copy_run(fx, id2)
  expect_error(cc4_candidate_register(other, id2, repo_dir = fx$repo), "not the upstream run's")
  expect_error(cc4_candidate_family("cc4_grid_exposure_20990101_v1"), "Not a CC4 candidate bundle id")
  expect_equal(cc4_candidate_family("cc4_phenotypes_20990101_v1")$stage, "31d")
  expect_equal(cc4_candidate_family("cc4_phase_statistics_20990101_v1")$stage, "31c")
})

test_that("verification fails on a changed, missing or unlisted file, and on an unpinned copy", {
  fx <- fixture(); on.exit(unlink(fx$root, recursive = TRUE), add = TRUE)
  dst <- copy_run(fx)
  cc4_candidate_register(fx$run, fx$id, repo_dir = fx$repo, imported_at = "fixture")
  pin <- cc4_candidate_pins(fx$repo)[[1]]
  f <- file.path(dst, "group_phase_summary.csv")
  keep <- readBin(f, "raw", file.size(f))
  writeLines(c("a,b", "1,3"), f)
  expect_error(cc4_candidate_verify(pin, repo_dir = fx$repo), "differ from manifest.csv")
  unlink(f)
  expect_error(cc4_candidate_verify(pin, repo_dir = fx$repo), "differ from manifest.csv")
  writeBin(keep, f)
  expect_true(cc4_candidate_verify(pin, repo_dir = fx$repo))
  writeLines("x", file.path(dst, "notes.txt"))
  expect_error(cc4_candidate_verify(pin, repo_dir = fx$repo), "does not list")
  unlink(file.path(dst, "notes.txt"))
  # an import-manifest row that no longer matches the copy
  mp <- file.path(fx$repo, CC4_CANDIDATE_MANIFEST)
  im <- utils::read.csv(mp, stringsAsFactors = FALSE)
  im$bytes[1] <- im$bytes[1] + 1
  utils::write.csv(im, mp, row.names = FALSE)
  expect_error(cc4_candidate_verify(pin, repo_dir = fx$repo), "import manifest does not match")
  # a copy that nothing pins
  copy_run(fx, "cc4_phase_groups_20990101_v3")
  res <- cc4_candidate_verify_all(fx$repo)
  expect_equal(res$problem[res$bundle_id == "cc4_phase_groups_20990101_v3"], "imported or recorded but not pinned")
})

test_that("every CC4 candidate copy in this repository is pinned and verifies", {
  on_disk <- list.dirs(repo(CC4_CANDIDATE_DIR), recursive = FALSE, full.names = FALSE)
  skip_if(!length(on_disk), "no CC4 candidate bundle imported")
  res <- cc4_candidate_verify_all(normalizePath(repo(), winslash = "/"))
  expect_setequal(res$bundle_id, on_disk)
  expect_true(all(res$ok), info = paste(res$bundle_id[!res$ok], res$problem[!res$ok], collapse = "; "))
  pins <- cc4_candidate_pins(normalizePath(repo(), winslash = "/"))
  for (pin in pins) expect_true(pin$importer %in% CC4_CANDIDATE_FAMILIES$importer, info = pin$bundle_id)
  # the importers record what they copy
  for (f in CC4_CANDIDATE_FAMILIES$importer)
    expect_true(any(grepl("cc4_candidate_register(", readLines(repo(f), warn = FALSE), fixed = TRUE)), info = f)
})
