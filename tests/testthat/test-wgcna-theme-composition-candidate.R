# The WGCNA module wheel and theme composition candidate (figures/wgcna_theme_composition_candidate.R).
#
# Portable: frozen source data and the panel library only. The entry script is never sourced, so
# nothing is rendered; each check compares what the panels would draw with the frozen export.

source(testthat::test_path("..", "..", "R", "paths.R"))
for (f in c("house_v2_figure_utils.R", "house_final_v7_panels.R", "spatial_grammar_utils.R",
            "final_truth_v9_panels.R", "figure3_adaptation_panels.R", "wgcna_theme_composition_panels.R"))
  source(repo_path("R", "panels", f))

X <- wtc_read()

testthat::test_that("the frozen export is the one the manifest records", {
  mf <- utils::read.csv(repo_path("source_data", "pRoteomics", "manifest.csv"), stringsAsFactors = FALSE)
  rows <- mf[mf$publication_id == WTC_ID, , drop = FALSE]
  testthat::expect_setequal(basename(rows$exported_file), WTC_FILES)
  testthat::expect_identical(length(unique(rows$source_commit)), 1L)
  testthat::expect_true(all(rows$contract_version == "publication_source_data_v1"))
  testthat::expect_true(all(X$long$evidence_role == "structural_composition_not_phenotype"))
})

testthat::test_that("the wheel places every module once, compartment by compartment", {
  w <- wtc_layout(X$wheel)
  testthat::expect_identical(nrow(w), nrow(X$wheel))
  testthat::expect_false(anyDuplicated(paste(w$dataset, w$module_id)) > 0)
  testthat::expect_true(all(w$t0 >= 0 & w$t1 <= 360 & w$t0 < w$t1))
  testthat::expect_false(is.unsorted(w$t0, strictly = TRUE))
  testthat::expect_identical(unique(w$dataset), WTC_DATASETS)
  # a supermodule's modules are adjacent on the wheel
  runs <- rle(paste(w$dataset, w$supermodule_id))$values
  testthat::expect_false(anyDuplicated(runs) > 0)
})

testthat::test_that("a theme ring is coloured exactly where the export marks the module as carrying it", {
  cells <- wtc_wheel_cells(X)
  themes <- wtc_themes(X$themes)
  drawn <- cells[grepl("^theme_", cells$ring) & cells$value == "TRUE", , drop = FALSE]
  prim <- X$long[X$long$is_primary_variant %in% TRUE & X$long$level == "module" &
                   X$long$composition_supported %in% TRUE, , drop = FALSE]
  testthat::expect_setequal(paste(drawn$dataset, drawn$module_id, drawn$theme_id),
                            paste(prim$dataset, prim$entity_id, prim$theme_id))
  testthat::expect_identical(drawn$fill, themes$colour[match(drawn$theme_id, themes$theme_id)])
  testthat::expect_true(all(cells$fill[grepl("^theme_", cells$ring) & cells$value == "FALSE"] == WTC_EMPTY))
  # one cell per module and ring
  testthat::expect_false(anyDuplicated(cells[c("dataset", "module_id", "ring")]) > 0)
})

testthat::test_that("the themes carry the Figure 3 labels, colours and registry order", {
  themes <- wtc_themes(X$themes)
  p <- f3a_programmes()
  testthat::expect_identical(themes$theme_id, p$source_theme_id)
  testthat::expect_identical(themes$colour, p$colour)
  testthat::expect_identical(themes$theme_id, X$themes$theme_id[order(X$themes$display_order)])
})

testthat::test_that("the dot matrix shows every module and every multi-module supermodule, nothing else", {
  m <- wtc_matrix_data(X)
  prim <- X$long[X$long$is_primary_variant %in% TRUE, , drop = FALSE]
  want <- prim[prim$level == "module" | grepl(";", prim$member_modules, fixed = TRUE), , drop = FALSE]
  testthat::expect_identical(nrow(m), nrow(want))
  testthat::expect_true(all(m$level[grepl(";", m$member_modules, fixed = TRUE)] == "supermodule"))
  testthat::expect_identical(sum(m$level == "module"), nrow(X$wheel) * nrow(X$themes))
  # the values drawn are the exported ones, unchanged
  k <- match(paste(m$dataset, m$level, m$entity_id, m$theme_id),
             paste(want$dataset, want$level, want$entity_id, want$theme_id))
  testthat::expect_false(anyNA(k))
  for (col in c("frac_theme_in_entity", "fold_enrichment", "composition_supported"))
    testthat::expect_identical(m[[col]], want[[col]][k], info = col)
})

testthat::test_that("the coherence tiles restate the summary and the pips its sensitivity columns", {
  cd <- wtc_coherence_data(X)
  testthat::expect_identical(nrow(cd$tiles), length(WTC_DATASETS) * nrow(X$themes))
  testthat::expect_true(all(cd$tiles$class_label[cd$tiles$n_carriers == 0] == "no carrier"))
  # a pip exists only where there is a top carrier to check
  with_carrier <- paste(cd$tiles$dataset, cd$tiles$theme_id)[cd$tiles$n_carriers > 0]
  testthat::expect_true(all(paste(cd$pips$dataset, cd$pips$theme_id) %in% with_carrier))
  testthat::expect_true(all(table(paste(cd$pips$dataset, cd$pips$theme_id)) == length(WTC_SENSITIVITIES)))
})

testthat::test_that("the candidate computes no statistic and draws no group effect", {
  files <- c(repo_path("R", "panels", "wgcna_theme_composition_panels.R"),
             repo_path("figures", "wgcna_theme_composition_candidate.R"))
  testthat::expect_true(hv_assert_no_model_fitting(files))
  code <- unlist(lapply(files, function(f) { x <- readLines(f, warn = FALSE); x[!grepl("^\\s*#", x)] }))
  for (pat in BEHAVIOUR_FORBIDDEN_STATS)
    testthat::expect_false(any(grepl(pat, code, perl = TRUE)), info = pat)
  for (pat in c("fisher", "phyper", "p\\.adjust", "estimate_group", "SUS - CON", "RES - CON"))
    testthat::expect_false(any(grepl(pat, code, ignore.case = TRUE)), info = pat)
  # reader-facing wording: no boundary claim the composition cannot support
  testthat::expect_false(any(grepl("spatially[ -](restricted|specific)|selectiv|exclusiv", code,
                                   ignore.case = TRUE)))
})
