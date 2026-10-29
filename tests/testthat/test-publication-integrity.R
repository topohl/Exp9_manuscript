source(testthat::test_path("..", "..", "R", "paths.R"))

# Publication integrity suite.
#
# Verifies that this repository reproduces the frozen publication state and
# that its only inputs are manifest-covered imports. Nothing here recomputes a
# scientific value.

registry_path <- function() {
  repo_path("provenance", "publication_registry", "canonical_publication_registry.csv")
}

testthat::test_that("imported source bundles match their manifests", {
  out <- suppressWarnings(system2(
    "Rscript", shQuote(repo_path("tools", "verify_source_bundles.R")),
    stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  testthat::expect_true(is.null(status) || status == 0L,
                        info = paste(utils::tail(out, 12), collapse = "\n"))
  testthat::expect_true(any(grepl("RESULT: PASS", out, fixed = TRUE)),
                        info = paste(utils::tail(out, 12), collapse = "\n"))
})

testthat::test_that("the publication registry is internally coherent", {
  testthat::expect_true(file.exists(registry_path()))
  r <- utils::read.csv(registry_path(), stringsAsFactors = FALSE)

  # Identities are unique and stable.
  testthat::expect_false(anyDuplicated(r$publication_id) > 0L)
  # a numbered identity carries its number; a withheld one has none (since 2026-10-20)
  testthat::expect_true(all(grepl("^(figure|extended_data)_[0-9]{2}$|^withheld_[a-z_]+$", r$publication_id)))

  # Every row is either canonical with an artefact and a hash, or withheld
  # with its identity reserved and no artefact.
  canon <- r$status == "CANONICAL"
  testthat::expect_true(all(nzchar(r$rendered_artifact[canon])))
  testthat::expect_true(all(nzchar(r$hash[canon])))
  testthat::expect_true(all(nchar(r$hash[canon]) == 64L))
  testthat::expect_true(all(!nzchar(r$rendered_artifact[!canon])))

  # The publication layer: Figures 1-3 and Extended Data 1-10 (eight since the
  # merges of 2026-10-20, ED9 the stress analyses since 2026-10-29, ED10 the home-cage
  # record since 2026-10-30); 2 withheld.
  testthat::expect_identical(sum(canon), 13L)
  testthat::expect_identical(sum(!canon), 2L)
  testthat::expect_setequal(r$publication_id[!canon],
                            c("withheld_wgcna", "withheld_identity_locations"))
})

testthat::test_that("canonical figures reproduce the frozen artefact hashes", {
  r <- utils::read.csv(registry_path(), stringsAsFactors = FALSE)
  canon <- r[r$status == "CANONICAL", , drop = FALSE]

  bundle <- repo_path("source_data", "pRoteomics")
  checked <- 0L
  for (i in seq_len(nrow(canon))) {
    pid <- canon$publication_id[i]
    # Identities with a promoted, tracked render (tools/promote_manuscript_render.R)
    # publish it from figures/main/ or figures/extended_data/.
    promoted <- grepl("^figures/(main|extended_data)/", canon$rendered_artifact[i])
    svg <- if (promoted) repo_path(canon$rendered_artifact[i]) else file.path(bundle, pid, "assembled", paste0(pid, ".svg"))
    if (!file.exists(svg)) next
    testthat::expect_identical(unname(tools::sha256sum(svg)), canon$hash[i],
                               info = paste("assembled artefact changed:", pid))
    checked <- checked + 1L
  }
  # Figures 1-3 and the canonical Extended Data must all be covered.
  testthat::expect_identical(checked, nrow(canon))
})

testthat::test_that("the repository holds no manuscript text", {
  # The text, snapshots, notes, legend drafts and claims register are kept with the Word file (docs/WHERE_THINGS_LIVE.md).
  testthat::expect_false(dir.exists(repo_path("manuscript")))
  testthat::expect_false(file.exists(repo_path("provenance", "claims", "docx_claims.csv")))
})

testthat::test_that("no biological model is fitted anywhere in this repository", {
  # Delegated to the structural guard so the rule lives in one place.
  testthat::expect_true(file.exists(
    testthat::test_path("test-no-analysis-in-manuscript.R")))
})
