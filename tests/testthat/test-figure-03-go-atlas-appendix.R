source(testthat::test_path("..", "..", "R", "paths.R"))

testthat::test_that("Figure 3 GO appendix covers all supported atlas terms", {
  base <- repo_path("source_data", "pRoteomics", "figure_03_go_atlas_appendix")
  ix <- read.csv(file.path(base, "theme_term_index.csv"),
                 stringsAsFactors = FALSE)
  sel <- read.csv(file.path(base, "selected_contexts.csv"),
                  stringsAsFactors = FALSE)
  curves <- read.csv(file.path(base, "running_enrichment_curves.csv"),
                     stringsAsFactors = FALSE)
  regional <- read.csv(file.path(base, "regional_exact_term_inventory.csv"),
                       stringsAsFactors = FALSE)
  protein <- read.csv(file.path(base, "protein_zoom_values.csv"),
                      stringsAsFactors = FALSE)
  supported <- read.csv(repo_path(
    "source_data", "pRoteomics", "supplementary_selection_inventories",
    "pathway_enrichment_inventory_fdr_supported.csv"),
    stringsAsFactors = FALSE)
  themes <- c("rna_processing_splicing_rnp", "ribosome_translation",
              "chromatin_organization", "mitochondrial_respiration_oxphos",
              "synaptic_signaling_vesicle", "neuron_projection_development",
              "autophagy_lysosome_endosome")
  supported <- supported[supported$contrast == "SUS - RES" &
                           supported$theme_claim_eligible %in% TRUE, ,
                         drop = FALSE]
  membership <- unique(do.call(rbind, lapply(seq_len(nrow(supported)),
    function(i) {
      ids <- strsplit(supported$theme_id[[i]], ";", fixed = TRUE)[[1]]
      data.frame(theme_id = ids[ids %in% themes],
                 GO_ID = supported$GO_ID[[i]])
    })))
  testthat::expect_setequal(paste(ix$theme_id, ix$GO_ID),
                            paste(membership$theme_id, membership$GO_ID))
  testthat::expect_identical(unique(ix$theme_id), themes)
  testthat::expect_false(anyDuplicated(ix[c("theme_id", "GO_ID")]) > 0L)
  testthat::expect_identical(ix$display_order, seq_len(nrow(ix)))
  testthat::expect_setequal(ix$GO_ID, sel$GO_ID)
  testthat::expect_setequal(curves$GO_ID, sel$GO_ID)
  testthat::expect_setequal(protein$GO_ID, sel$GO_ID)
  testthat::expect_false(anyDuplicated(regional[
    c("GO_ID", "dataset", "spatial_unit", "contrast")]) > 0L)
  testthat::expect_false(anyDuplicated(protein[
    c("GO_ID", "gene", "contrast")]) > 0L)
  for (i in seq_len(nrow(sel))) {
    s <- sel[i, ]
    eligible <- supported[supported$GO_ID == s$GO_ID, , drop = FALSE]
    eligible <- eligible[order(eligible$BH_FDR, eligible$dataset,
                               eligible$spatial_unit), , drop = FALSE]
    testthat::expect_identical(s$dataset, eligible$dataset[[1]])
    testthat::expect_identical(s$spatial_unit, eligible$spatial_unit[[1]])
    testthat::expect_equal(s$BH_FDR, eligible$BH_FDR[[1]], tolerance = 1e-12)
    z <- curves[curves$GO_ID == s$GO_ID, , drop = FALSE]
    testthat::expect_identical(z$rank, seq_len(s$n_ranked))
    testthat::expect_identical(sum(z$hit), s$setSize)
    testthat::expect_identical(sum(z$peak), 1L)
    testthat::expect_equal(z$running_ES[z$peak], s$enrichmentScore,
                           tolerance = 1e-10)
    tri <- regional[regional$GO_ID == s$GO_ID &
                      regional$dataset == s$dataset &
                      regional$spatial_unit == s$spatial_unit, , drop = FALSE]
    testthat::expect_setequal(tri$contrast,
                              c("RES - CON", "SUS - CON", "SUS - RES"))
    p <- protein[protein$GO_ID == s$GO_ID, , drop = FALSE]
    testthat::expect_gte(length(unique(p$gene)), 1L)
    testthat::expect_lte(length(unique(p$gene)), 7L)
    testthat::expect_identical(nrow(p), 3L * length(unique(p$gene)))
    testthat::expect_false(anyNA(p[c("ProteinGroupID", "log2FC", "BH_FDR")]))
  }
})
