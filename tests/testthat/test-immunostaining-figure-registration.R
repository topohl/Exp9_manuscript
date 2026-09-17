source(testthat::test_path("..", "..", "R", "paths.R"))

# Extracted from pRoteomics tests/testthat/test-immunostaining-candidate-comparison.R
# in Phase 6C. Those two assertions read figures/figure_contract.yml, which is
# the journal figure assembly contract and moved to this repository. The other
# 169 assertions in that file are about the candidate comparison analysis and
# stayed in pRoteomics.
#
# Assertion text and direction are unchanged; only the contract is now read
# from this repository rather than from a sibling one.

library(testthat)

contract <- function() {
  testthat::skip_if_not_installed("yaml")
  yaml::read_yaml(repo_path("figures", "figure_contract.yml"))
}

test_that("the figure is registered as manuscript-supporting, not a numbered figure", {
  y <- contract()
  entry <- y$figures$S_immunostaining_candidates
  expect_false(is.null(entry))
  expect_false(isTRUE(entry$is_numbered_manuscript_figure))
  expect_equal(length(entry$panels), 2L)
  for (p in entry$panels) {
    expect_equal(p$biological_unit, "animal")
    expect_true(grepl("animal_level", p$hemisphere_handling))
    expect_true(all(grepl("^results/source_data/", unlist(p$input_dependencies))))
  }
})

test_that("the separation panel is registered as manuscript-supporting", {
  y <- contract()
  entry <- y$figures$S_immunostaining_separation
  expect_false(is.null(entry))
  expect_false(isTRUE(entry$is_numbered_manuscript_figure))
  expect_equal(length(entry$panels), 2L)
  for (p in entry$panels) {
    expect_equal(p$biological_unit, "animal")
    expect_true(grepl("animal_level", p$hemisphere_handling))
    expect_true(all(grepl("^results/source_data/", unlist(p$input_dependencies))))
  }
  # the contract must record that Z' did not gate, matching the provenance row
  eff <- entry$panels[[2]]
  expect_false(isTRUE(eff$z_factor_used_as_gate))
  expect_true(all(c("fully_observed_only", "complete_separation") %in%
                    unlist(eff$selection_gates)))
})
