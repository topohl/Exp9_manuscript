source(testthat::test_path("..", "..", "R", "paths.R"))

testthat::test_that("supplementary GO candidate is selection-disclosed and source-backed", {
  base <- repo_path("source_data", "pRoteomics",
                    "supplementary_go_pathways_candidate")
  sel <- utils::read.csv(file.path(base, "selection.csv"),
                         stringsAsFactors = FALSE)
  curves <- utils::read.csv(file.path(base, "running_enrichment_curves.csv"),
                            stringsAsFactors = FALSE)
  regions <- utils::read.csv(file.path(base, "regional_exact_term_inventory.csv"),
                             stringsAsFactors = FALSE)
  protein <- utils::read.csv(repo_path(
    "source_data", "pRoteomics", "supplementary_go_protein_zooms_candidate",
    "protein_zoom_values.csv"), stringsAsFactors = FALSE)
  supported <- utils::read.csv(repo_path(
    "source_data", "pRoteomics", "supplementary_selection_inventories",
    "pathway_enrichment_inventory_fdr_supported.csv"),
    stringsAsFactors = FALSE)

  testthat::expect_identical(sel$GO_ID,
    c("GO:0006412", "GO:0006325", "GO:0031175", "GO:0006914"))
  testthat::expect_identical(sel$display_order, 1:4)
  testthat::expect_identical(nrow(regions), 216L)
  testthat::expect_false(anyDuplicated(regions[c("GO_ID", "dataset",
                                                  "spatial_unit", "contrast")]) > 0)
  testthat::expect_setequal(unique(regions$contrast),
                           c("RES - CON", "SUS - CON", "SUS - RES"))
  # the strips and the regional matrix use the manuscript's fixed NES colour limit
  # (config/manuscript_palette.yml diverging_limits, palette v3.2), the one Figure 3
  # and Extended Data 6 use; a value beyond it takes full colour and stays uncapped
  # in the regional source data
  testthat::expect_false(anyNA(regions$NES))
  code <- readLines(repo_path("figures", "figure_03_supplementary_go.R"), warn = FALSE)
  code <- code[!grepl("^\\s*#", code)]
  testthat::expect_true(any(grepl('nes_limit <- nv_diverging_limit("nes")', code, fixed = TRUE)))
  testthat::expect_true(any(grepl('nv_diverging(measure = "nes"', code, fixed = TRUE)))
  testthat::expect_identical(nrow(protein), 84L)
  testthat::expect_false(anyDuplicated(protein[c("GO_ID", "gene",
                                                  "contrast")]) > 0)
  testthat::expect_false(anyNA(protein$ProteinGroupID))
  testthat::expect_true(all(protein$BH_FDR >= 0.05))
  for (i in seq_len(nrow(sel))) {
    s <- sel[i, ]
    eligible <- supported[supported$GO_ID == s$GO_ID &
                            supported$theme_id == s$theme_id &
                            supported$contrast == "SUS - RES", , drop = FALSE]
    eligible <- eligible[order(eligible$BH_FDR, eligible$dataset,
                               eligible$spatial_unit), , drop = FALSE]
    testthat::expect_gt(nrow(eligible), 0L)
    testthat::expect_identical(s$dataset, eligible$dataset[[1]])
    testthat::expect_identical(s$spatial_unit, eligible$spatial_unit[[1]])
    testthat::expect_equal(s$BH_FDR, eligible$BH_FDR[[1]], tolerance = 1e-12)
    z <- curves[curves$GO_ID == s$GO_ID, , drop = FALSE]
    testthat::expect_identical(z$rank, seq_len(s$n_ranked))
    testthat::expect_equal(sum(z$hit), s$setSize)
    testthat::expect_identical(sum(z$peak), 1L)
    testthat::expect_equal(z$running_ES[z$peak], s$enrichmentScore,
                           tolerance = 1e-10)
    tri <- regions[regions$GO_ID == s$GO_ID &
                     regions$dataset == s$dataset &
                     regions$spatial_unit == s$spatial_unit, , drop = FALSE]
    testthat::expect_setequal(tri$contrast,
                             c("RES - CON", "SUS - CON", "SUS - RES"))
    testthat::expect_equal(tri$NES[tri$contrast == "SUS - RES"],
                           s$NES, tolerance = 1e-12)
    p <- protein[protein$GO_ID == s$GO_ID, , drop = FALSE]
    testthat::expect_identical(nrow(p), 21L)
    testthat::expect_identical(length(unique(p$gene)), 7L)
    testthat::expect_setequal(unique(p$contrast),
                             c("RES - CON", "SUS - CON", "SUS - RES"))
  }
})
