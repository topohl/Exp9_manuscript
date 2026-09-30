# Guards for the behaviour-bundle import boundary.
#
# The canonical behavioural results enter this repository only as one pinned,
# hash-frozen bundle written by MMMSociability's Stage 16b and copied unchanged by
# tools/import_behaviour_bundle.R. These guards check that the pinned copy is
# exactly that bundle, that its provenance is recorded, and that the importer
# keeps its refusals.

repo <- function(...) file.path(testthat::test_path("..", ".."), ...)
source(repo("R", "paths.R"))
source(repo("R", "behaviour_bundle.R"))
PIN_PATH <- repo("config", "behaviour_bundle.yml")
MANIFEST <- repo("provenance", "source_manifests", "behaviour_bundle_manifest.csv")
IMPORTER <- repo("tools", "import_behaviour_bundle.R")
have_pin <- file.exists(PIN_PATH)

test_that("the importer keeps its refusals", {
  code <- paste(readLines(IMPORTER, warn = FALSE), collapse = "\n")
  for (must in c('identical(row$status, "FROZEN")', "manifest does not match its registry hash", "differs from its manifest",
                 "Unlisted files in the bundle", "already imported (immutable)", "Imported copy does not verify"))
    expect_true(grepl(must, code, fixed = TRUE), info = must)
})

test_that("the pin names a FROZEN bundle and its provenance", {
  skip_if_not(have_pin, "no behaviour bundle pinned")
  pin <- behaviour_bundle_pin()
  for (k in c("bundle_id", "manifest_sha256", "config_version", "config_sha256", "mmm_git_commit", "stage29_run_commit"))
    expect_true(nzchar(pin[[k]] %||% ""), info = k)
  expect_match(pin$bundle_id, "^ebb_v[0-9]+_[0-9]{8}_[0-9a-f]{7}$")
  expect_false(grepl("dry", pin$bundle_id, fixed = TRUE))
  prov <- behaviour_bundle_table("H_provenance")
  expect_equal(prov$value[prov$key == "status"], "FROZEN")
  expect_equal(prov$value[prov$key == "config_version"], pin$config_version)
})

test_that("the pinned copy matches its manifest and the recorded import manifest", {
  skip_if_not(have_pin, "no behaviour bundle pinned")
  pin <- behaviour_bundle_pin()
  expect_true(behaviour_bundle_verify(pin))
  man <- utils::read.csv(MANIFEST, stringsAsFactors = FALSE)
  man <- man[man$bundle_id == pin$bundle_id, , drop = FALSE]
  d <- behaviour_bundle_dir(pin)
  expect_setequal(man$file, list.files(d))
  expect_equal(unname(tools::sha256sum(file.path(d, man$file))), man$sha256)
})

test_that("the bundle carries the frozen design's families and counts", {
  skip_if_not(have_pin, "no behaviour bundle pinned")
  e <- behaviour_bundle_table("E_multiplicity")
  fam <- function(id) e[e$family_id == id, , drop = FALSE]
  for (id in c("P-CC1", "P-TR")) {
    f <- fam(id)
    expect_equal(nrow(f), 2L)
    expect_true(all(f$declared_m == 2 & f$realised_m == 2 & f$method == "holm"))
    expect_setequal(sub("^[^|]+[|]", "", f$member), c("crossing_rate", "shared_zone_use"))
  }
  g <- behaviour_bundle_table("G_sample_sizes")
  cc1 <- g[g$CC == "CC1" & g$Group %in% c("RES", "SUS"), ]
  counts <- stats::setNames(tapply(cc1$n_animals, paste(cc1$Sex, cc1$Group), sum), NULL)
  expect_equal(sort(as.integer(counts)), sort(c(28L, 18L, 25L, 16L)))
  expect_equal(sum(g$n_windows[g$CC != "all"]), 444L)
})

test_that("nothing in the manuscript renders behaviour from the retired bridge path", {
  code <- readLines(repo("figures", "figure_01_panels.R"), warn = FALSE)
  code <- code[!grepl("^[[:space:]]*#", code)]
  expect_false(any(grepl("figure1_bridge_mmmsociability", code, fixed = TRUE)))
  expect_false(any(grepl("source_data/MMMSociability/source_data", code, fixed = TRUE)))
})
