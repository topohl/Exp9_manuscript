# Guards for the canonical publication registry and the Extended Data
# canonicalisation performed in Phase 6A.
#
# The point of this phase was to remove every remaining ambiguity about WHICH
# artefact is the publication before the repository is physically restructured.
# These tests encode that: one canonical identity per figure, no competing
# generation, and the three withheld Extended Data figures recorded rather than
# quietly dropped.

repo_rel <- function(...) file.path(testthat::test_path("..", ".."), ...)
pub_contract <- function() {
  source(testthat::test_path("..", "..", "R", "paths.R"))
source(testthat::test_path("..", "..", "R", "frozen_inputs.R"))
  yaml::read_yaml(repo_rel("figures", "contracts", "figure_contract.yml"))
}

testthat::test_that("every numbered publication has exactly one canonical identity", {
  testthat::skip_if_not_installed("yaml")
  y <- pub_contract()
  numbered <- Filter(function(f) isTRUE(f$is_numbered_manuscript_figure), y$figures)

  ids <- vapply(numbered, function(f) {
    as.character(f$canonical_publication_id %||%
      if (!is.null(f$extended_data_number))
        sprintf("extended_data_%02d", as.integer(f$extended_data_number))
      else NA_character_)
  }, character(1))

  testthat::expect_false(anyDuplicated(ids) > 0L)
  testthat::expect_setequal(ids, c(
    "figure_01", "figure_02", "figure_03",
    sprintf("extended_data_%02d", 1:8)))
})

testthat::test_that("no panel id is reused across publications", {
  testthat::skip_if_not_installed("yaml")
  y <- pub_contract()
  all_ids <- unlist(lapply(y$figures, function(f)
    vapply(f$panels, function(p) as.character(p$id), character(1))), use.names = FALSE)
  dup <- unique(all_ids[duplicated(all_ids)])
  # Extended Data ids are zero-padded precisely so ED1 panel a cannot be confused
  # with Figure 1 panel a. Before that padding both were "1a".
  testthat::expect_identical(dup, character(0))
})

testthat::test_that("the registry matches the contract and records the withheld figures", {
  reg <- repo_rel("provenance", "publication_registry", "canonical_publication_registry.csv")
  testthat::skip_if_not(file.exists(reg), "registry not generated")
  r <- utils::read.csv(reg, stringsAsFactors = FALSE)

  testthat::expect_identical(sum(r$status == "CANONICAL"), 11L)   # Figures 1-3 and Extended Data 1-8
  withheld <- r[r$status != "CANONICAL", ]
  # Since the renumbering by first citation (2026-10-20) a withheld identity has no
  # number; it keeps its former id in previous_publication_id.
  testthat::expect_setequal(withheld$publication_id,
    c("withheld_wgcna", "withheld_identity_locations"))
  testthat::expect_setequal(withheld$previous_publication_id,
    c("extended_data_04", "extended_data_07"))
  testthat::expect_true(all(is.na(withheld$hash) | !nzchar(withheld$hash)))
  testthat::expect_true(all(grepl("^WITHHELD_", withheld$status)))

  y <- pub_contract()
  numbered <- Filter(function(f) isTRUE(f$is_numbered_manuscript_figure), y$figures)
  testthat::expect_identical(length(numbered), 11L)
})

testthat::test_that("the Extended Data promotion audit adjudicates all nine identities", {
  aud <- repo_rel("provenance", "claims", "extended_data_promotion_audit.csv")
  testthat::skip_if_not(file.exists(aud), "promotion audit not generated")
  a <- utils::read.csv(aud, stringsAsFactors = FALSE)

  testthat::expect_identical(nrow(a), 9L)
  testthat::expect_true(all(a$promote %in% c("YES", "NO")))
  testthat::expect_identical(sum(a$promote == "YES"), 7L)
  testthat::expect_setequal(a$canonical_publication_id[a$promote == "NO"],
    c("extended_data_04", "extended_data_07"))
  # Every withheld figure must carry a stated defect, never a bare NO.
  for (i in which(a$promote == "NO")) {
    testthat::expect_true(nchar(a$defect_or_note[i]) > 120L,
      info = paste("withheld without a stated defect:", a$ed_figure[i]))
  }
})

testthat::test_that("the promoted Extended Data figures resolve to real assets", {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  testthat::skip_if_not_installed("yaml")
  y <- pub_contract()
  for (k in c("ED_spatial_validation", "ED_ca2_slm_qc", "ED_enrichment_atlas",
              "ED_spatial_networks", "ED_behaviour_models")) {
    f <- y$figures[[k]]
    testthat::expect_false(isTRUE(f$rendering_repository_computes_statistics))
    for (p in f$panels) {
      testthat::expect_true(file.exists(repo_path(as.character(p$figure_source))),
                            info = paste("missing panel asset:", p$id))
      testthat::expect_true(frozen_input_present(repo_path(as.character(p$primary_source))),
                            info = paste("missing source data:", p$id))
    }
  }
})

testthat::test_that("the pre-restructure freeze manifest is complete", {
  man <- repo_rel("provenance", "source_manifests", "prerestructure_freeze_manifest.csv")
  testthat::skip_if_not(file.exists(man), "freeze manifest not generated")
  m <- utils::read.csv(man, stringsAsFactors = FALSE)

  # The manifest is an equivalence oracle for a structural migration. An object
  # with no hash cannot serve as one.
  testthat::expect_true(all(m$exists))
  testthat::expect_false(any(is.na(m$sha256) | !nzchar(m$sha256)))
  for (cls in c("configuration_contract", "manuscript_provenance",
                "frozen_upstream_import", "canonical_figure_panel",
                "publication_source_data", "canonical_analysis_table",
                "protected_scientific_state", "guard_test")) {
    testthat::expect_gt(sum(m$object_class == cls), 0L,
                        label = paste("objects of class", cls))
  }
  # Every canonical publication must be represented.
  reg <- repo_rel("provenance", "publication_registry", "canonical_publication_registry.csv")
  testthat::skip_if_not(file.exists(reg), "registry not generated")
  r <- utils::read.csv(reg, stringsAsFactors = FALSE)
  # Identities created after the freeze (Extended Data 10-12, drawn from the SISanalyzer bundle) cannot be in it.
  # the freeze predates the renumbering of 2026-10-20, so an identity is looked up under the id it had
  # then; a merged identity lists each of its former ids, separated by ";"
  keep <- r$status == "CANONICAL" & r$originating_analysis != "SISanalyzer"
  canon <- unlist(strsplit(ifelse(nzchar(r$previous_publication_id[keep]),
                                  r$previous_publication_id[keep], r$publication_id[keep]), ";", fixed = TRUE))
  testthat::expect_true(all(canon %in% m$publication_id[!is.na(m$publication_id)]))
})

testthat::test_that("the spatial-identity panels are promoted without the completeness misrepresentation", {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  testthat::skip_if_not_installed("yaml")
  y <- pub_contract()
  f <- y$figures$ED_spatial_validation

  # Bilateral agreement and precision (a, b), then the spatial identity (c, d).
  # The former identity panel c drew 14 of 2,826 FDR-supported positive GO
  # terms under a complete-inventory caption and is deliberately absent.
  testthat::expect_identical(
    vapply(f$panels, function(p) as.character(p$id), character(1)), paste0("05", letters[1:4]))
  testthat::expect_true(nzchar(as.character(f$withheld_panel %||% "")))

  src <- paste(readLines(repo_path("R", "proteomics_panels_extended_data.R"), warn = FALSE),
               collapse = "\n")
  testthat::expect_false(grepl("v9_ed_internal_full", 
    paste(vapply(f$panels, function(p) as.character(p$v9_panel_id), character(1)),
          collapse = " "), fixed = TRUE))
})

testthat::test_that("MT-04: the external panel uses the signature-family correction", {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  src <- paste(readLines(repo_path("R", "proteomics_panels_extended_data.R"), warn = FALSE),
               collapse = "\n")
  # The panel must not mark significance from p_adjust, which is the raw
  # single-set value: Benjamini-Hochberg over a family of one is a no-op.
  testthat::expect_false(grepl("k$sig <- is.finite(k$p_adjust)", src, fixed = TRUE))
  testthat::expect_true(grepl("k$sig <- is.finite(k$signature_FDR)", src, fixed = TRUE))

  sd <- path_results("source_data", "manuscript_candidates", "final_truth_v9",
                     "extended_data", "v9_ed_external_full_source_data.csv")
  testthat::skip_if_not(file.exists(sd), "ED2b sidecar not rendered here")
  d <- utils::read.csv(sd, stringsAsFactors = FALSE)
  testthat::expect_identical(nrow(d), 30L)
  testthat::expect_true("signature_FDR" %in% names(d))
  # The uncorrected value may be carried, but never under a name that implies
  # adjustment.
  testthat::expect_false("p_adjust" %in% names(d))
  testthat::expect_true("single_set_p_unadjusted" %in% names(d))
  testthat::expect_identical(sum(d$signature_FDR < 0.05), 28L)
  testthat::expect_identical(sum(d$kind == "expected pairing"), 10L)
})

testthat::test_that("MT-04: Figure 2g marks significance from the signature-family FDR", {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  src <- paste(readLines(repo_path("R", "proteomics_panels_main.R"), warn = FALSE),
               collapse = "\n")
  testthat::expect_false(grepl("k$sig <- is.finite(k$p_adjust)", src, fixed = TRUE))
  testthat::expect_true(grepl("k$sig <- is.finite(k$signature_FDR)", src, fixed = TRUE))
  sd <- repo_path("figures", "main", "source_data", "figure_02", "figure_02g_source_data.csv")
  testthat::skip_if_not(file.exists(sd), "Figure 2g source data not promoted here")
  d <- utils::read.csv(sd, stringsAsFactors = FALSE)
  testthat::expect_false("p_adjust" %in% names(d))
  testthat::expect_true(all(c("signature_FDR", "single_set_p_unadjusted") %in% names(d)))
})

testthat::test_that("no artefact still points Figure 2h at a withdrawn panel", {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  for (f in c(repo_path("figures", "contracts", "figure_contract.yml"),
              repo_path("figures", "contracts", "proteomics_panels_contract.yml"),
              repo_path("R", "proteomics_panels_main.R"),
              repo_path("figures", "scripts", "proteomics_legends.R"))) {
    s <- paste(readLines(f, warn = FALSE), collapse = " ")
    testthat::expect_false(grepl("complete evidence lives in ED2", s, fixed = TRUE),
                           info = basename(f))
    testthat::expect_false(grepl("complete term inventory is in ED2", s, fixed = TRUE),
                           info = basename(f))
    testthat::expect_false(grepl("one canonical term per contrast; the complete inventory is ED2",
                                 s, fixed = TRUE), info = basename(f))
  }
})
