# Guards for the behavioural Extended Data figures (ED5, ED9).
#
# Since 2026-10-17 the panels are drawn from the bundles Figure 1 is drawn from: the pinned canonical behaviour bundle
# (ebb: Stage 09 rows of C2_estimates, E_multiplicity PR1-PR3) and the pinned figure-support bundle (fsb: E1, and E3
# from fsb_v5 on). Three things have to stay true, and none of them is checked by simply re-running the renderer: the
# pinned bundles still verify, this repository still computes nothing, and the panels still say only what the stored
# results permit. The bridge contracts of MMMSociability 53bc7e9 that the panels read before stay imported as history.

ed_bridge_path <- function(...) {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  repo_path("source_data", "MMMSociability", ...)
}
ed_bundles <- function() {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  source(repo_path("R", "behaviour_bundle.R")); source(repo_path("R", "stage30_bundle.R")); source(repo_path("R", "figure_support_bundle.R"))
  pin <- behaviour_bundle_pin(); behaviour_bundle_verify(pin); fsb <- fsb_bundle_pin(); fsb_bundle_verify(fsb)
  list(pin = pin, fsb = fsb, C2 = behaviour_bundle_table("C2_estimates", pin), EM = behaviour_bundle_table("E_multiplicity", pin),
       A2 = behaviour_bundle_table("A2_prediction_animals", pin), E1 = fsb_bundle_table("E1_movement_combz_spearman_by_sex", fsb),
       E3 = tryCatch(fsb_bundle_table("E3_early_window_coverage", fsb), error = function(e) NULL))
}

testthat::test_that("the retired bridge contracts are still the recorded imports (history)", {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  testthat::skip_if_not_installed("digest")
  manifest <- utils::read.csv(repo_path("provenance", "source_manifests", "figure1_bridge_import_manifest.csv"), stringsAsFactors = FALSE)
  rows <- manifest[basename(manifest$imported_file) %in% c("behavior_prediction_model_ladder.csv", "behavior_sex_effect_contract.csv"), , drop = FALSE]
  testthat::expect_identical(nrow(rows), 2L)
  for (i in seq_len(nrow(rows))) {
    p <- ed_bridge_path(sub("^manuscript/figure1_bridge_mmmsociability/", "", rows$imported_file[[i]]))
    testthat::expect_identical(digest::digest(file = p, algo = "sha256"), rows$sha256[[i]])
  }
})

testthat::test_that("the model ladder: five registered models on the same 111 animals, permutation only where run", {
  B <- ed_bundles(); s09 <- B$C2[B$C2$source == "stage09", ]
  models <- c("mean_only", "movement_mean", "primary_behavior_family", "movement_mean_sex", "primary_behavior_family_sex")
  for (m in models) for (e in c("LOAO_R2", "repeated_CV_mean_R2"))
    testthat::expect_identical(sum(s09$model_id == paste0("stage09_", m) & s09$estimand == e), 1L, info = paste(m, e))
  testthat::expect_identical(unique(s09$n), 111L)
  pr2 <- B$EM[B$EM$family_id == "PR2", ]
  testthat::expect_setequal(pr2$member, paste0("permutation|", c("movement_mean", "primary_behavior_family")))
  cv <- s09[s09$estimand == "repeated_CV_mean_R2", ]
  testthat::expect_true(all(cv$estimate >= cv$ci_low & cv$estimate <= cv$ci_high))   # the tick sits inside its bar
})

testthat::test_that("no feature-by-sex interaction is rejected (PR3), which is what licenses ED9 c", {
  B <- ed_bundles()
  pr3 <- B$EM[B$EM$family_id == "PR3", ]
  testthat::expect_identical(nrow(pr3), 3L)
  testthat::expect_false(any(as.logical(pr3$reject_at_alpha_adjusted)))
  ix <- B$C2[B$C2$model_id == "stage09_feature_x_sex", ]
  testthat::expect_identical(nrow(ix), 3L)
  testthat::expect_true(all(ix$ci_low < 0 & ix$ci_high > 0))   # zero inside every interval, the visual claim of ED9 b
  testthat::expect_setequal(paste(B$E1$Sex, B$E1$feature), paste(rep(c("Female", "Male"), each = 3), rep(c("Movement_mean", "Movement_rmssd", "Entropy_acf1"), 2)))
})

testthat::test_that("the contract declares the panels the renderer writes, from the pinned bundles", {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  source(repo_path("R", "manuscript_figure_utils.R"))
  testthat::skip_if_not_installed("yaml")
  B <- ed_bundles()
  cov <- manuscript_figure_contract("ED_behaviour_coverage"); sec <- manuscript_figure_contract("ED_behaviour_secondary")
  testthat::expect_identical(vapply(cov$panels, function(x) as.character(x$id), character(1)), c("05a", "05b"))
  testthat::expect_identical(vapply(sec$panels, function(x) as.character(x$id), character(1)), c("09a", "09b", "09c"))
  testthat::expect_identical(as.character(cov$canonical_publication_id), "extended_data_05")
  testthat::expect_identical(as.character(sec$canonical_publication_id), "extended_data_09")
  for (fig in list(cov, sec)) {
    testthat::expect_identical(as.character(fig$behaviour_bundle_id), B$pin$bundle_id)
    testthat::expect_identical(as.character(fig$figure_support_bundle_id), B$fsb$bundle_id)
    testthat::expect_false(isTRUE(fig$rendering_repository_computes_statistics))
    for (panel in fig$panels) {
      testthat::expect_identical(as.character(panel$producer_script), "figures/extended_data_behaviour_panels.R")
      src <- as.character(panel$primary_source)
      testthat::expect_true(startsWith(src, paste0("source_data/MMMSociability/", B$pin$bundle_id, "/")) ||
                              startsWith(src, paste0("source_data/MMMSociability/", B$fsb$bundle_id, "/")), info = src)
      if (!(panel$id == "05a" && is.null(B$E3))) testthat::expect_true(file.exists(repo_path(src)), info = src)
    }
  }
})

testthat::test_that("ED9 a is honest about what it can draw: effect sizes, no per-animal secondary features", {
  B <- ed_bundles()
  as <- B$C2[B$C2$model_id == "stage09_association", ]
  testthat::expect_setequal(as$construct, c("Movement_mean", "Movement_rmssd", "Entropy_acf1"))
  testthat::expect_identical(unique(as$n), 111L)
  testthat::expect_false(any(grepl("rmssd|entropy", names(B$A2), ignore.case = TRUE)))
  testthat::expect_setequal(B$EM$member[B$EM$family_id == "PR1"], paste0("spearman|", c("Movement_mean", "Movement_rmssd", "Entropy_acf1")))
})

testthat::test_that("ED5 a states the coverage aggregate Stage 09 exports (fsb E3, fsb_v5 on)", {
  B <- ed_bundles()
  testthat::skip_if(is.null(B$E3), "the pinned figure-support bundle has no E3 (fsb_v5 on)")
  testthat::expect_identical(nrow(B$E3), 1L)
  testthat::expect_identical(as.integer(B$E3$n_animals), 111L)
  testthat::expect_identical(as.integer(B$E3$expected_target_slots_per_animal), 72L)
})

testthat::test_that("the Extended Data namespace resolves and still fails closed", {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  source(repo_path("R", "output_namespace_utils.R"))
  paths <- output_namespace_manuscript_figure_paths("/tmp/root", "ED_behaviour_secondary")
  testthat::expect_true(grepl("extended_data_09$", paths$figures))
  testthat::expect_true(grepl("extended_data_09$", paths$source_data))
  testthat::expect_true(grepl("figure_01$", output_namespace_manuscript_figure_paths("/tmp/root", "01")$figures))
  testthat::expect_error(output_namespace_manuscript_figure_paths("/tmp/root", "07"), "must be one of")
  testthat::expect_error(output_namespace_manuscript_figure_paths("/tmp/root", "ED_nonsense"), "must be one of")
})

testthat::test_that("the renderer computes no statistic and reads only the pinned bundles", {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  src <- readLines(repo_path("figures", "extended_data_behaviour_panels.R"), warn = FALSE)
  code <- paste(sub("#.*$", "", src), collapse = "\n")   # match calls, not prose
  forbidden <- c("cor", "cor.test", "lm", "glm", "lmer", "t.test", "wilcox.test", "p.adjust", "sample", "boot", "quantile", "aov", "anova",
                 "fisher.test", "chisq.test", "predict", "residuals")
  for (fn in forbidden) {
    pattern <- paste0("(^|[^[:alnum:]._$])", gsub(".", "\\.", fn, fixed = TRUE), "[[:space:]]*\\(")
    testthat::expect_false(grepl(pattern, code, perl = TRUE), info = paste0("renderer appears to call ", fn, "()"))
  }
  testthat::expect_true(grepl("behaviour_bundle_pin()", code, fixed = TRUE) && grepl("fsb_bundle_pin()", code, fixed = TRUE))
  testthat::expect_false(grepl("figure1_bridge_mmmsociability|behavior_prediction_model_ladder|behavior_sex_effect_contract", code))
  testthat::expect_true(grepl("reject_at_alpha_adjusted", code, fixed = TRUE))   # the PR3 guard is why ED9 c may exist
})

testthat::test_that("prohibited sex wording appears on no rendered panel", {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  panel_dir <- path_results("figures", "manuscript", "extended_data_behaviour_panels")
  testthat::skip_if_not(dir.exists(panel_dir), "panels not rendered in this environment")
  svgs <- list.files(panel_dir, pattern = "[.]svg$", full.names = TRUE)
  testthat::expect_gt(length(svgs), 0L)
  for (f in svgs) {
    text <- paste(readLines(f, warn = FALSE), collapse = " ")
    for (phrase in c("female-specific", "sex-specific", "stronger in females", "driven by females"))
      testthat::expect_false(grepl(phrase, text, ignore.case = TRUE), info = paste0(basename(f), " renders prohibited wording: ", phrase))
  }
})

testthat::test_that("Results cites the Extended Data panels and no retired Figure 1 panel", {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  text <- paste(readLines(repo_path("manuscript", "manuscript_draft.md"), warn = FALSE), collapse = "\n")
  testthat::expect_true(grepl("Extended Data Fig. 9a", text, fixed = TRUE))
  testthat::expect_true(grepl("Extended Data Fig. 9b", text, fixed = TRUE))
  testthat::expect_true(grepl("Extended Data Fig. 9c", text, fixed = TRUE))
  testthat::expect_false(grepl("Fig. 1e", text, fixed = TRUE))
  testthat::expect_false(grepl("Fig. 1f", text, fixed = TRUE))
})
