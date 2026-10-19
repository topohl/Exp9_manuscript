# Guards for the spatial grammar (R/panels/spatial_grammar_utils.R) and for the spatial_v6 tables that the
# current Figure 2 and Extended Data contracts read as primary sources
# (results/tables/manuscript_candidates/spatial_v6/). These checks were part of
# test-spatial-v6-figure-layer.R. They moved here when the spatial_v6 candidate generation was retired on
# 2026-10-07, because both the grammar and the tables are still live.

source(testthat::test_path("..", "..", "R", "paths.R"))
source(repo_path("R", "integration_utils.R"))
source(repo_path("R", "house_v2_figure_utils.R"))
source(repo_path("R", "spatial_grammar_utils.R"))

rd <- function(p) utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
have <- function(p) file.exists(p)

# ------------------------------------------------------ the spatial contract

testthat::test_that("the spatial order contract declares exactly 18 units", {
  u <- sg_units()
  testthat::expect_identical(nrow(u), 18L)
  testthat::expect_identical(sum(u$dataset == "neuron_neuropil"), 10L)
  testthat::expect_identical(sum(u$dataset == "neuron_soma"), 4L)
  testthat::expect_identical(sum(u$dataset == "microglia"), 4L)
  testthat::expect_identical(anyDuplicated(u$analysis_key), 0L)
  testthat::expect_identical(u$order, seq_len(18L))
})

testthat::test_that("region-level compartments never carry laminar resolution", {
  u <- sg_units()
  # soma carries an sp/sg token naming the layer that was dissected, but only
  # one layer per region was sampled, so it is NOT laminar resolution
  testthat::expect_true(all(u$layer_is_resolution[u$dataset == "neuron_neuropil"]))
  testthat::expect_false(any(u$layer_is_resolution[u$dataset == "neuron_soma"]))
  testthat::expect_false(any(u$layer_is_resolution[u$dataset == "microglia"]))
  # and the DISPLAY label of a region-level unit must not gain a layer suffix
  soma <- u[u$dataset == "neuron_soma", ]
  testthat::expect_identical(sort(soma$display), c("CA1", "CA2", "CA3", "DG"))
  mg <- u[u$dataset == "microglia", ]
  testthat::expect_identical(sort(mg$display), c("CA1", "CA2", "CA3", "DG"))
})

testthat::test_that("CA3 has no SLM and DG layers belong to neuropil", {
  u <- sg_units()
  ca3 <- u[u$dataset == "neuron_neuropil" & u$region == "CA3", ]
  testthat::expect_false("slm" %in% ca3$layer)
  testthat::expect_identical(sort(ca3$layer), c("so", "sr"))
  dg_np <- u[u$dataset == "neuron_neuropil" & u$region == "DG", ]
  testthat::expect_identical(sort(dg_np$layer), c("mo", "po"))
  testthat::expect_identical(u$layer[u$dataset == "neuron_soma" & u$region == "DG"], "sg")
})

testthat::test_that("both spelling vocabularies resolve, disambiguated by dataset", {
  # the atlas spells soma AND microglia units ca1/ca2/ca3/dg, so the unit
  # string alone is ambiguous and the pair (dataset, unit) is the real key
  testthat::expect_identical(sg_resolve_unit("ca1", "neuron_soma"), "CA1_sp")
  testthat::expect_identical(sg_resolve_unit("ca1", "microglia"), "CA1")
  testthat::expect_identical(sg_resolve_unit("CA1_sp", "neuron_soma"), "CA1_sp")
  testthat::expect_identical(sg_resolve_unit("CA1", "microglia"), "CA1")
  testthat::expect_identical(sg_resolve_unit("ca1_slm", "neuron_neuropil"), "CA1_slm")
  testthat::expect_identical(sg_resolve_unit("CA1_slm", "neuron_neuropil"), "CA1_slm")
  # fails closed rather than silently dropping a row
  testthat::expect_error(sg_resolve_unit("CA1_slm", "neuron_soma"), "unrecognised")
  testthat::expect_error(sg_resolve_unit("nonsense", "microglia"), "unrecognised")
})

testthat::test_that("the axis never repeats what a header already carries", {
  u <- sg_units()
  b <- sg_blocks(u$analysis_key, u$dataset)
  testthat::expect_identical(nrow(b$order), 18L)
  testthat::expect_identical(b$compartment$label, c("Neuropil", "Soma", "Microglia ROI"))
  testthat::expect_identical(b$compartment$start, c(1L, 11L, 15L))
  testthat::expect_identical(b$compartment$end, c(10L, 14L, 18L))
  lab <- sg_axis_labels(b)
  # neuropil shows only the layer token; region-level compartments show nothing
  # because the region strip above them IS the label
  testthat::expect_identical(lab[1:10], c("SO", "SR", "SLM", "SO", "SR", "SLM", "SO", "SR", "MO", "PO"))
  testthat::expect_true(all(lab[11:18] == ""))
  testthat::expect_false(any(grepl("neuron_|microglia", lab)))
})

testthat::test_that("sg_blocks deduplicates long input", {
  u <- sg_units()
  # a tidy table repeats each unit once per row; building one axis position per
  # ROW instead of per UNIT silently garbles the axis
  testthat::expect_identical(nrow(sg_blocks(rep(u$analysis_key, 12), rep(u$dataset, 12))$order), 18L)
})

# ------------------------------------------- the live spatial_v6 input tables

testthat::test_that("the Figure-2 fingerprint selection is phenotype-blind", {
  p <- path_results("tables", "manuscript_candidates", "spatial_v6", "figure2_spatial_fingerprint_selection.csv")
  testthat::skip_if_not(have(p), "selection table not materialised")
  z <- rd(p)
  testthat::expect_true(all(z$phenotype_blind == "YES"))
  testthat::expect_true(all(nzchar(z$selection_rule)))
  testthat::expect_true(all(nzchar(z$phenotype_blind_evidence)))
  # the rule must never mention a stress contrast as a selection criterion
  testthat::expect_false(any(grepl("SUS *- *RES|SUS-RES|sus_res", z$selection_rule)))
  # and the contaminated source must be explicitly excluded, not silently unused
  testthat::expect_true(all(grepl("protein_baseline_spatial_profile", z$excluded_source_note, fixed = TRUE)))
  testthat::expect_true(all(grepl("SUS - RES", z$excluded_source_note, fixed = TRUE)))
  testthat::expect_true(all(z$display_form %in% c("A_individual_proteins", "B_signature_scores")))
})

testthat::test_that("the CON baseline profile uses CON animals only", {
  p <- path_results("tables", "manuscript_candidates", "spatial_v6", "spatial_v6_con_baseline_profile_long.csv")
  testthat::skip_if_not(have(p), "baseline profile not materialised")
  z <- rd(p)
  testthat::expect_true(all(z$n_con_animals == 3L))
  testthat::expect_identical(sort(unique(z$dataset)), c("microglia", "neuron_neuropil", "neuron_soma"))
  testthat::expect_true(all(grepl("CON", z$value_definition)))
  # every unit must resolve through the shared grammar
  testthat::expect_false(anyNA(sg_resolve_unit(z$spatial_unit, z$dataset)))
  # neuropil must have 10 units, soma and microglia 4 each - never more
  for (d in unique(z$dataset)) {
    n <- length(unique(z$spatial_unit[z$dataset == d]))
    testthat::expect_identical(n, if (d == "neuron_neuropil") 10L else 4L, label = d)
  }
})
