source(testthat::test_path("..", "..", "R", "paths.R"))
source(repo_path("R", "final_truth_v9_panels.R"))
source(repo_path("R", "figure3_adaptation_panels.R"))

f3a_inventory_for_test <- function() {
  utils::read.csv(repo_path(
    "source_data", "pRoteomics", "supplementary_selection_inventories",
    "pathway_enrichment_inventory.csv"), stringsAsFactors = FALSE,
    check.names = FALSE)
}

testthat::test_that("Figure 3 programmes are the seven v3 registry themes", {
  p <- f3a_programmes()
  testthat::expect_identical(p$programme_order, 1:7)
  testthat::expect_identical(p$source_theme_id, f9_atlas_themes()$theme_id)
  testthat::expect_identical(p$programme_label, c(
    "RNA processing", "Translation / ribosome",
    "Chromatin / epigenetic regulation", "Mitochondrial respiration",
    "Synaptic signalling / vesicle", "Neuron projection development",
    "Autophagy / endolysosomal"))
  testthat::expect_false(anyDuplicated(p$programme_id) > 0L)
  testthat::expect_false(anyDuplicated(p$colour) > 0L)
  testthat::expect_true(all(f3a_exemplars()$programme_id %in% p$programme_id))

  # Membership is the registry's claim-eligible theme assignment and nothing
  # else: no hand-picked GO IDs, no unclassified terms.
  inv <- f3a_inventory_for_test()
  z <- f3a_programme_terms(inv)
  testthat::expect_true(all(z$theme_claim_eligible %in% TRUE))
  testthat::expect_true(all(mapply(function(ids, theme)
    theme %in% strsplit(ids, ";", fixed = TRUE)[[1]],
    z$theme_id, p$source_theme_id[match(z$programme_id, p$programme_id)])))
})

testthat::test_that("the SUS-RES programme atlas equals the frozen v3 theme atlas", {
  # Figure 3b and the Extended Data Fig. 6 atlases must be the same theme
  # aggregation. The frozen SUS-RES atlas (the former panel 3b, drawn by
  # f9_gsea_atlas) is the reference, cell for cell.
  cells <- f3a_programme_cells(f3a_inventory_for_test())
  cells <- cells[cells$contrast == "SUS - RES", , drop = FALSE]
  cells$theme_id <- f3a_programmes()$source_theme_id[
    match(cells$programme_id, f3a_programmes()$programme_id)]
  ref <- utils::read.csv(repo_path(
    "source_data", "pRoteomics", "figure_03", "figure_03b_source_data.csv"),
    stringsAsFactors = FALSE)
  m <- merge(cells, ref, by = c("dataset", "spatial_unit", "theme_id"),
             suffixes = c("", "_ref"))
  testthat::expect_identical(nrow(cells), nrow(ref))
  testthat::expect_identical(nrow(m), nrow(ref))
  testthat::expect_equal(m$median_NES, m$median_NES_ref, tolerance = 1e-12)
  testthat::expect_identical(as.integer(m$n_fdr_supported),
                             as.integer(m$n_fdr))
})

testthat::test_that("programme summaries cover every context and contrast once", {
  cells <- f3a_programme_cells(f3a_inventory_for_test())
  testthat::expect_identical(nrow(cells), 7L * 18L * 3L)
  testthat::expect_false(anyDuplicated(cells[
    c("programme_id", "dataset", "spatial_unit", "contrast")]) > 0L)
  testthat::expect_identical(sort(unique(cells$contrast)),
    sort(c("RES - CON", "SUS - CON", "SUS - RES")))
  testthat::expect_true(all(cells$n_constituent_terms > 0L))
  testthat::expect_true(all(is.finite(cells$median_NES)))
})

testthat::test_that("five-state adaptation classification is exhaustive and transparent", {
  z <- f3a_adaptation_states(f3a_inventory_for_test())
  testthat::expect_identical(nrow(z), 7L * 18L)
  expected <- c(
    "resilience-specific remodeling", "susceptibility-specific remodeling",
    "shared / parallel", "divergent / opposing",
    "little detectable adaptation")
  testthat::expect_setequal(z$adaptation_pattern, expected)
  observed <- table(factor(z$adaptation_pattern, levels = expected))
  testthat::expect_identical(as.integer(observed), c(25L, 26L, 12L, 3L, 60L))

  res <- z$RES_CON_n_fdr > 0L
  sus <- z$SUS_CON_n_fdr > 0L
  testthat::expect_true(all(z$adaptation_pattern[!res & !sus] ==
                              "little detectable adaptation"))
  testthat::expect_true(all(z$adaptation_pattern[res & !sus] ==
                              "resilience-specific remodeling"))
  testthat::expect_true(all(z$adaptation_pattern[!res & sus] ==
                              "susceptibility-specific remodeling"))
  both <- res & sus
  same <- sign(z$RES_CON_median_NES) == sign(z$SUS_CON_median_NES)
  testthat::expect_true(all(z$adaptation_pattern[both & same] ==
                              "shared / parallel"))
  testthat::expect_true(all(z$adaptation_pattern[both & !same] ==
                              "divergent / opposing"))
  testthat::expect_true(all(grepl("never passive", z$classification_rule,
                                  fixed = TRUE)))
})

testthat::test_that("exemplar cards and frozen exact-context drill-downs agree", {
  inv <- f3a_inventory_for_test()
  ex <- f3a_exemplars()
  selection <- utils::read.csv(repo_path(
    "source_data", "pRoteomics", "figure_03_adaptation", "selection.csv"),
    stringsAsFactors = FALSE)
  curves <- utils::read.csv(repo_path(
    "source_data", "pRoteomics", "figure_03_adaptation",
    "running_enrichment_curves.csv"), stringsAsFactors = FALSE)
  proteins <- utils::read.csv(repo_path(
    "source_data", "pRoteomics", "figure_03_adaptation",
    "protein_zoom_values.csv"), stringsAsFactors = FALSE)
  testthat::expect_identical(selection$exemplar, 1:3)
  testthat::expect_identical(selection$dataset, ex$dataset)
  testthat::expect_identical(selection$spatial_unit, ex$spatial_unit)
  testthat::expect_identical(selection$GO_ID, ex$term_id)
  for (i in 1:3) {
    card <- f3a_card_data(inv, i)
    testthat::expect_identical(nrow(card), 9L)
    testthat::expect_setequal(card$contrast,
      c("RES - CON", "SUS - CON", "SUS - RES"))
    testthat::expect_identical(unique(card$GO_ID[card$term_order == 1L]),
                               ex$term_id[[i]])
    curve <- curves[curves$exemplar == i, , drop = FALSE]
    testthat::expect_identical(nrow(curve), selection$n_ranked[[i]])
    testthat::expect_identical(sum(curve$peak), 1L)
    protein <- proteins[proteins$exemplar == i, , drop = FALSE]
    testthat::expect_identical(nrow(protein),
                              3L * length(unique(protein$gene)))
    testthat::expect_setequal(protein$contrast,
      c("RES - CON", "SUS - CON", "SUS - RES"))
  }
})

testthat::test_that("canonical Figure 3 contract is a through m", {
  testthat::skip_if_not_installed("yaml")
  producing <- yaml::read_yaml(repo_path(
    "figures", "figure_final_truth_v9_contract.yml"))
  f <- Filter(function(x) x$figure_key == "figure_03", producing$figures)[[1]]
  testthat::expect_identical(
    vapply(f$layout, function(x) as.character(x$label), character(1)),
    letters[1:13])
  manuscript <- yaml::read_yaml(repo_path("figures", "figure_contract.yml"))
  testthat::expect_identical(
    as.character(manuscript$figures[["03"]]$assembled_pdf_source),
    paste0("results/figures/manuscript_candidates/final_truth_v9/figure_03/",
           "assembled/F3_NATURE_FINAL_V9.pdf"))
  testthat::expect_identical(
    vapply(manuscript$figures[["03"]]$panels,
           function(x) as.character(x$id), character(1)),
    paste0("3", letters[1:13]))

  final_pdf <- repo_path("results", "figures", "manuscript", "figure_03",
                         "assembled", "figure_03.pdf")
  producer_pdf <- repo_path(
    "results", "figures", "manuscript_candidates", "final_truth_v9",
    "figure_03", "assembled", "F3_NATURE_FINAL_V9.pdf")
  if (file.exists(final_pdf) && file.exists(producer_pdf))
    testthat::expect_identical(unname(tools::sha256sum(final_pdf)),
                               unname(tools::sha256sum(producer_pdf)))
})

testthat::test_that("the registered Figure 3 is the promoted a-m render", {
  # Tracked files only, so this runs without a results/ workspace. It pins the
  # canonical identity and the counts the manuscript text cites.
  reg <- utils::read.csv(repo_path("provenance", "publication_registry",
                                   "canonical_publication_registry.csv"),
                         stringsAsFactors = FALSE, colClasses = "character")
  r <- reg[reg$publication_id == "figure_03", , drop = FALSE]
  testthat::expect_identical(nrow(r), 1L)
  testthat::expect_identical(r$status, "CANONICAL")
  testthat::expect_identical(r$panels,
                             paste(paste0("3", letters[1:13]), collapse = ","))
  testthat::expect_identical(r$rendered_artifact, "figures/main/figure_03.svg")
  testthat::expect_identical(r$canonical_source_data,
                             "figures/main/source_data/figure_03")
  for (ext in c(".svg", ".pdf", ".png"))
    testthat::expect_true(file.exists(repo_path("figures", "main",
                                                paste0("figure_03", ext))))
  sha <- function(p) unname(tools::sha256sum(p))
  testthat::expect_identical(sha(repo_path(r$rendered_artifact)), r$hash)
  # the frozen a-i figure is history, not the registered one
  testthat::expect_false(identical(r$hash, sha(repo_path(
    "source_data", "pRoteomics", "figure_03", "assembled", "figure_03.svg"))))

  sd <- repo_path(r$canonical_source_data)
  man <- utils::read.csv(file.path(sd, "00_manifest.csv"),
                         stringsAsFactors = FALSE, colClasses = "character")
  testthat::expect_identical(man$file,
                             paste0("figure_03", letters[1:13], "_source_data.csv"))
  testthat::expect_setequal(list.files(sd), c(man$file, "00_manifest.csv"))
  testthat::expect_identical(sha(file.path(sd, man$file)), man$sha256)
  testthat::expect_identical(unique(man$render_git_commit), r$source_commit)
  testthat::expect_true(grepl("^[0-9a-f]{40}$", r$source_commit))

  rd <- function(panel) utils::read.csv(
    file.path(sd, paste0("figure_03", panel, "_source_data.csv")),
    stringsAsFactors = FALSE)
  b <- rd("b")
  testthat::expect_identical(nrow(b), 126L)
  testthat::expect_identical(sum(b$n_fdr_supported > 0), 51L)
  c3 <- rd("c")
  classes <- c("resilience-specific remodeling",
               "susceptibility-specific remodeling", "shared / parallel",
               "divergent / opposing", "little detectable adaptation")
  testthat::expect_identical(
    as.integer(table(factor(c3$adaptation_pattern, levels = classes))),
    c(25L, 26L, 12L, 3L, 60L))
  ex <- c3[!is.na(c3$exemplar), , drop = FALSE]
  testthat::expect_identical(ex$adaptation_pattern[order(ex$exemplar)],
                             classes[c(4L, 2L, 3L)])
  d <- rd("d")
  n_of <- function(comp) d$n[d$compartment == comp][
    match(classes, d$adaptation_pattern[d$compartment == comp])]
  testthat::expect_identical(as.integer(n_of("Neuropil")), c(19L, 15L, 6L, 1L, 29L))
  testthat::expect_identical(as.integer(n_of("Soma")), c(3L, 6L, 2L, 0L, 17L))
  testthat::expect_identical(as.integer(n_of("Microglia ROI")), c(3L, 5L, 4L, 2L, 14L))
})

testthat::test_that("the manuscript text describes the promoted a-m Figure 3", {
  draft <- readLines(repo_path("manuscript", "manuscript_draft.md"), warn = FALSE)
  # the a-i panel ranges are gone, and no reference is split across a line,
  # which the line-based reference guards could not see
  testthat::expect_false(any(grepl("Fig\\. ?3(c[–-]f|d[–-]f|g[–-]i)", draft)))
  testthat::expect_false(any(grepl("Fig\\.$", draft)))
  # the pattern counts in the text are the promoted source data's
  txt <- gsub("\\s+", " ", paste(draft, collapse = " "))
  c3 <- utils::read.csv(repo_path("figures", "main", "source_data", "figure_03",
                                  "figure_03c_source_data.csv"),
                        stringsAsFactors = FALSE)
  n <- table(c3$adaptation_pattern)
  quoted <- sprintf(paste0(
    "Of the %d cells, %d were supported against controls only in resilient ",
    "animals (resilience-specific), %d only in susceptible animals ",
    "(susceptibility-specific), %d in both with medians of the same sign ",
    "(shared / parallel) and %d in both with medians of opposite sign ",
    "(divergent / opposing); the remaining %d were supported in neither"),
    nrow(c3), n[["resilience-specific remodeling"]],
    n[["susceptibility-specific remodeling"]], n[["shared / parallel"]],
    n[["divergent / opposing"]], n[["little detectable adaptation"]])
  testthat::expect_true(grepl(quoted, txt, fixed = TRUE))
})
