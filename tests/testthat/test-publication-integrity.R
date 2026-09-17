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
  testthat::expect_true(all(grepl("^(figure|extended_data)_[0-9]{2}$", r$publication_id)))

  # Every row is either canonical with an artefact and a hash, or withheld
  # with its identity reserved and no artefact.
  canon <- r$status == "CANONICAL"
  testthat::expect_true(all(nzchar(r$rendered_artifact[canon])))
  testthat::expect_true(all(nzchar(r$hash[canon])))
  testthat::expect_true(all(nchar(r$hash[canon]) == 64L))
  testthat::expect_true(all(!nzchar(r$rendered_artifact[!canon])))

  # The frozen publication layer: 10 canonical identities, 2 withheld.
  testthat::expect_identical(sum(canon), 10L)
  testthat::expect_identical(sum(!canon), 2L)
  testthat::expect_setequal(r$publication_id[!canon],
                            c("extended_data_04", "extended_data_07"))
})

testthat::test_that("canonical figures reproduce the frozen artefact hashes", {
  r <- utils::read.csv(registry_path(), stringsAsFactors = FALSE)
  canon <- r[r$status == "CANONICAL", , drop = FALSE]

  bundle <- repo_path("source_data", "pRoteomics")
  checked <- 0L
  for (i in seq_len(nrow(canon))) {
    pid <- canon$publication_id[i]
    svg <- file.path(bundle, pid, "assembled", paste0(pid, ".svg"))
    if (!file.exists(svg)) next
    testthat::expect_identical(unname(tools::sha256sum(svg)), canon$hash[i],
                               info = paste("assembled artefact changed:", pid))
    checked <- checked + 1L
  }
  # Figures 1-3 and the canonical Extended Data must all be covered.
  testthat::expect_identical(checked, nrow(canon))
})

testthat::test_that("the manuscript draft and legends are present and unedited by this suite", {
  draft <- repo_path("manuscript", "manuscript_draft.md")
  testthat::expect_true(file.exists(draft))
  txt <- readLines(draft, warn = FALSE)
  testthat::expect_gt(length(txt), 100L)

  legends <- list.files(repo_path("manuscript", "legends"), pattern = "[.]md$")
  testthat::expect_gt(length(legends), 0L)
})

testthat::test_that("manuscript figure references resolve to registry identities", {
  draft <- repo_path("manuscript", "manuscript_draft.md")
  txt <- paste(readLines(draft, warn = FALSE), collapse = "\n")
  r <- utils::read.csv(registry_path(), stringsAsFactors = FALSE)

  # Every Figure/Extended Data number named in the prose must exist as a
  # registry identity, canonical or explicitly withheld.
  nums <- unique(unlist(regmatches(txt, gregexpr("Figure[[:space:]]+[0-9]+", txt))))
  fig_ids <- sprintf("figure_%02d", as.integer(gsub("[^0-9]", "", nums)))
  testthat::expect_true(all(fig_ids %in% r$publication_id),
                        info = paste("unresolved figure reference:",
                                     paste(setdiff(fig_ids, r$publication_id), collapse = ", ")))

  ed <- unique(unlist(regmatches(txt, gregexpr("Extended Data Figure[[:space:]]+[0-9]+", txt))))
  ed_ids <- sprintf("extended_data_%02d", as.integer(gsub("[^0-9]", "", ed)))
  testthat::expect_true(all(ed_ids %in% r$publication_id),
                        info = paste("unresolved Extended Data reference:",
                                     paste(setdiff(ed_ids, r$publication_id), collapse = ", ")))
})

testthat::test_that("no biological model is fitted anywhere in this repository", {
  # Delegated to the structural guard so the rule lives in one place.
  testthat::expect_true(file.exists(
    testthat::test_path("test-no-analysis-in-manuscript.R")))
})
