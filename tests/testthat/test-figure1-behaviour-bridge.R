# Guards for the imported Figure 1 behavioural evidence bridge.
#
# The behavioural analysis lives in a separate repository and is not reproduced
# here, so the risk this phase carries is not a wrong model but a wrong quote:
# a number drifting away from the frozen contract, or a superseded Phase-2B
# assertion surviving. The manuscript text is kept with the Word file, outside
# this repository (docs/WHERE_THINGS_LIVE.md); tools/check_claims.R checks its numbers.

repo <- function(...) file.path(testthat::test_path("..", ".."), ...)
BRIDGE <- repo("source_data", "MMMSociability")
rd <- function(p) utils::read.csv(p, stringsAsFactors = FALSE)

# The frozen manifests record the pRoteomics prefix
# manuscript/figure1_bridge_mmmsociability/. The bundle still mirrors that
# internal layout; only the prefix moved to source_data/MMMSociability/.
bridge_resolve <- function(recorded) {
  file.path(testthat::test_path("..", ".."), "source_data", "MMMSociability",
            sub("^manuscript/figure1_bridge_mmmsociability/", "", recorded))
}

# The five Figure 1 contracts, plus the two tables the behavioural Extended Data
# figure renders. All seven come from the same upstream freeze and are pinned by
# the same import manifest; the two later additions are listed separately so it
# stays visible which files arrived for which figure.
FIGURE1_BUNDLE_FILES <- c("figure1_claim_contract.csv",
                          "figure1_methods_contract.csv",
                          "figure1_repo_provenance.csv",
                          "figure1_source_data_manifest.csv",
                          "figure1_timeline_contract.csv")
EXTENDED_DATA_BUNDLE_FILES <- c("behavior_prediction_model_ladder.csv",
                                "behavior_sex_effect_contract.csv",
                                "early_behavior_later_outcome_association.csv")
BUNDLE_FILES <- c(FIGURE1_BUNDLE_FILES, EXTENDED_DATA_BUNDLE_FILES)

test_that("the bridge is a byte-exact mirror of exactly the frozen bundle", {
  skip_if_not(dir.exists(BRIDGE), "figure 1 bridge not imported")
  # Exactly these files and nothing else - an extra entry here would be a
  # proteomics-side artefact masquerading as frozen upstream evidence. The
  # source_data/ subdirectory is the one permitted addition: it is the frozen
  # Figure 1 panel source data, itself a byte-exact upstream mirror, and it is
  # pinned to its own expected contents below rather than waved through.
  # Pinned canonical behaviour bundles (ebb_v*, MMMSociability Stage 16b) are the
  # second permitted addition; each is hash-verified by test-behaviour-bundle-import.R.
  # Pinned Stage 30 figure bundles (s30b_v*, MMMSociability Analysis/30b) are the
  # third; each is hash-verified by test-stage30-bundle-import.R.
  # Pinned figure-support bundles (fsb_v*, MMMSociability Analysis/16c) are the
  # fourth; each is hash-verified by test-figure-support-bundle-import.R.
  here <- list.files(BRIDGE)
  expect_setequal(here[!grepl("^ebb_v[0-9]+_[0-9]{8}_[0-9a-f]{7}$", here) & !grepl("^s30b_v[0-9]+_[0-9]{8}_[0-9a-f]{7}$", here) &
                         !grepl("^fsb_v[0-9]+_[0-9]{8}_[0-9a-f]{7}$", here)],
                  c(BUNDLE_FILES, "source_data"))
  expect_true(dir.exists(file.path(BRIDGE, "source_data")))
  expect_setequal(
    list.files(file.path(BRIDGE, "source_data")),
    c("figure1a_timeline_source.csv", "figure1b_combz_classification_source.csv",
      "figure1c_movement_combz_source.csv", "figure1d_loao_predictions_source.csv",
      "figure1e_permutation_source.csv", "figure1f_repeated_cv_source.csv",
      "figure1_panel_statistics.csv", "figure1_panel_source_manifest.csv"))
  # and nothing executable may live in the evidence interface
  expect_equal(list.files(BRIDGE, pattern = "[.][Rr]$", recursive = TRUE),
               character(0))

  man <- rd(repo("provenance", "source_manifests", "figure1_bridge_import_manifest.csv"))
  expect_equal(nrow(man), length(BUNDLE_FILES))
  skip_if_not(requireNamespace("digest", quietly = TRUE), "digest unavailable")
  for (i in seq_len(nrow(man))) {
    p <- bridge_resolve(man$imported_file[i])
    expect_true(file.exists(p))
    expect_equal(file.size(p), man$bytes[i])
    expect_equal(digest::digest(p, algo = "sha256", file = TRUE),
                 man$sha256[i])
  }
})

test_that("the bridge records the full upstream provenance chain", {
  skip_if_not(dir.exists(BRIDGE), "figure 1 bridge not imported")
  p <- rd(repo("provenance", "claims", "figure1_bridge_provenance.csv"))
  val <- function(k) p$value[match(k, p$element)]
  expect_equal(val("source_repository"), "topohl/MMMSociability")
  # The commits the import is pinned to. These are full 40-character shas and
  # move whenever the upstream bundle is re-frozen, so assert their shape and
  # their mutual consistency rather than literal values that would have to be
  # edited on every resync.
  for (k in c("analysis_commit", "bundle_commit", "verified_source_HEAD"))
    expect_match(val(k), "^[0-9a-f]{40}$")
  # the re-freeze is itself the verified head, so these two agree
  expect_equal(val("bundle_commit"), val("verified_source_HEAD"))
  # the facts that make the quote trustworthy at all
  expect_equal(val("behavioural_code_copied"), "NONE")
  expect_equal(val("behavioural_results_recomputed"), "NONE")
  expect_equal(val("scientific_values_changed_by_this_resync"), "NONE")
  # the bundle must have been exported from a clean source tree; the first
  # freeze recorded "dirty at export time" and that is what FC-10 was about
  expect_equal(val("bundle_internal_worktree_state"), "clean")
  expect_equal(val("source_worktree_state_at_import"), "clean")
})

test_that("the imported contract carries the corrected prediction seeds", {
  skip_if_not(dir.exists(BRIDGE), "figure 1 bridge not imported")
  fm <- rd(file.path(BRIDGE, "figure1_methods_contract.csv"))
  design <- fm$value[fm$methods_id == "FM-07"]
  # 521 assigns the CV folds. 123 is the association-bootstrap seed and must
  # never appear in the prediction-design row: that substitution is exactly the
  # defect this bridge was re-frozen to repair.
  expect_true(grepl("seed 521", design, fixed = TRUE))
  expect_false(grepl("123", design, fixed = TRUE))
  assoc <- fm$value[fm$methods_id == "FM-06"]
  expect_true(grepl("seed 123", assoc, fixed = TRUE))
  expect_false(grepl("521", assoc, fixed = TRUE))
})

test_that("no behavioural analysis code was copied into this repository", {
  # The bridge is a CSV evidence interface. An .R file appearing under it, or a
  # copy of the upstream analysis entrypoint anywhere here, would mean the
  # separation had been broken.
  skip_if_not(dir.exists(BRIDGE), "figure 1 bridge not imported")
  expect_equal(list.files(BRIDGE, pattern = "[.][Rr]$"), character(0))
  upstream <- c("09_early_prediction_model_ladder",
                "build_figure1_export_bundle", "build_figure1_manuscript_bridge",
                "build_later_outcome_combz")
  here <- list.files(repo("."), pattern = "[.][Rr]$", recursive = TRUE)
  here <- here[!grepl("^(\\.git|renv)/", here)]
  for (u in upstream) expect_false(any(grepl(u, basename(here), fixed = TRUE)))
})

test_that("line-ending normalisation is disabled for the bridge", {
  # .gitattributes sets * text=auto repo-wide. Under that rule git would store
  # these CRLF files as LF and check them out differently on another platform,
  # so the recorded SHA-256 would not reproduce and the integrity guard above
  # would fail for a reason that has nothing to do with the evidence.
  ga <- readLines(repo(".gitattributes"), warn = FALSE)
  expect_true(any(grepl("source_data/\\*\\*\\s+-text", ga)))
})

# ----------------------------------------------------- provenance completeness

test_that("superseded Phase-2B assertions are marked, not silently left true", {
  skip_if_not(dir.exists(BRIDGE), "figure 1 bridge not imported")
  # the contracts that declared Figure 1 unresolved must no longer say so
  bp <- rd(repo("provenance", "claims", "behavior_prediction_contract.csv"))
  expect_false(any(grepl("NO PREDICTION ANALYSIS EXISTS", bp$value)))
  os <- rd(repo("provenance", "claims", "outcome_score_contract.csv"))
  expect_false(any(grepl("\\[UNRESOLVED\\]", os$value)))
  # every point of disagreement is recorded rather than quietly resolved
  fc <- rd(repo("provenance", "claims", "figure1_bridge_conflicts.csv"))
  expect_gt(nrow(fc), 0)
  expect_true(all(nzchar(fc$authority_applied)))
  expect_true(all(nzchar(fc$resolution_in_manuscript)))
})
