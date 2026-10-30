# The WGCNA module wheel and theme composition candidate (figures/candidates/wgcna_theme_composition_candidate.R).
#
# Portable: frozen source data and the panel library only. The entry script is never sourced, so
# nothing is rendered; each check compares what the panels would draw with the frozen export.
# Every imported version (WTC_VERSIONS) is checked; v1 stays the import of 2026-10-12.

source(testthat::test_path("..", "..", "R", "paths.R"))
for (f in c("proteomics_style.R", "proteomics_panels_figure2.R", "spatial_grammar_utils.R",
            "proteomics_panels_main.R", "figure3_adaptation_panels.R", "wgcna_theme_composition_panels.R"))
  source(repo_path("R", "panels", f))

MF <- utils::read.csv(repo_path("source_data", "pRoteomics", "manifest.csv"), stringsAsFactors = FALSE)
XS <- lapply(stats::setNames(names(WTC_VERSIONS), names(WTC_VERSIONS)), wtc_read)

testthat::test_that("each frozen version is the export the manifest records", {
  boundary <- file.path("exports", "publication_source_data")
  for (v in names(XS)) {
    rows <- MF[MF$publication_id == wtc_bundle_id(v), , drop = FALSE]
    testthat::expect_setequal(basename(rows$exported_file), WTC_FILES)
    testthat::expect_true(all(dirname(rows$exported_file) == file.path(boundary, wtc_bundle_id(v))), info = v)
    # source_table keeps the path pRoteomics wrote
    testthat::expect_true(all(dirname(rows$source_table) == file.path(boundary, WTC_ID)), info = v)
    testthat::expect_identical(length(unique(rows$source_commit)), 1L, info = v)
    testthat::expect_true(all(rows$contract_version == "publication_source_data_v1"), info = v)
    testthat::expect_true(all(XS[[v]]$long$evidence_role == "structural_composition_not_phenotype"), info = v)
  }
  commits <- vapply(names(XS), function(v) MF$source_commit[MF$publication_id == wtc_bundle_id(v)][[1]], "")
  testthat::expect_false(anyDuplicated(commits) > 0)   # every version is a different export
})

testthat::test_that("the wheel places every module once, compartment by compartment", {
  for (v in names(XS)) {
    X <- XS[[v]]
    w <- wtc_layout(X$wheel)
    testthat::expect_identical(nrow(w), nrow(X$wheel), info = v)
    testthat::expect_false(anyDuplicated(paste(w$dataset, w$module_id)) > 0, info = v)
    testthat::expect_true(all(w$t0 >= 0 & w$t1 <= 360 & w$t0 < w$t1), info = v)
    testthat::expect_false(is.unsorted(w$t0, strictly = TRUE), info = v)
    testthat::expect_identical(unique(w$dataset), WTC_DATASETS, info = v)
    # a supermodule's modules are adjacent on the wheel
    runs <- rle(paste(w$dataset, w$supermodule_id))$values
    testthat::expect_false(anyDuplicated(runs) > 0, info = v)
  }
})

testthat::test_that("the wheel's supermodules are the member sets the composition tested", {
  for (v in names(XS)) {
    X <- XS[[v]]
    sm <- tapply(X$wheel$module_id, paste(X$wheel$dataset, X$wheel$supermodule_id),
                 function(m) paste(sort(m), collapse = ";"))
    sm <- sm[grepl(";", sm, fixed = TRUE)]
    tested <- unique(X$long[X$long$level == "supermodule", c("dataset", "entity_id", "member_modules")])
    tested <- tested[grepl(";", tested$member_modules, fixed = TRUE), , drop = FALSE]
    testthat::expect_setequal(paste(names(sm), sm),
                              paste(tested$dataset, tested$entity_id, tested$member_modules))
  }
})

testthat::test_that("a theme ring is coloured exactly where the export marks the module as carrying it", {
  for (v in names(XS)) {
    X <- XS[[v]]
    cells <- wtc_wheel_cells(X)
    themes <- wtc_themes(X$themes)
    drawn <- cells[grepl("^theme_", cells$ring) & cells$value == "TRUE", , drop = FALSE]
    prim <- X$long[X$long$is_primary_variant %in% TRUE & X$long$level == "module" &
                     X$long$composition_supported %in% TRUE, , drop = FALSE]
    testthat::expect_setequal(paste(drawn$dataset, drawn$module_id, drawn$theme_id),
                              paste(prim$dataset, prim$entity_id, prim$theme_id))
    testthat::expect_identical(drawn$fill, themes$colour[match(drawn$theme_id, themes$theme_id)], info = v)
    testthat::expect_true(all(cells$fill[grepl("^theme_", cells$ring) & cells$value == "FALSE"] == WTC_EMPTY),
                          info = v)
    # one cell per module and ring
    testthat::expect_false(anyDuplicated(cells[c("dataset", "module_id", "ring")]) > 0, info = v)
  }
})

testthat::test_that("the themes carry the Figure 3 labels, colours and registry order", {
  p <- f3a_programmes()
  for (v in names(XS)) {
    X <- XS[[v]]
    themes <- wtc_themes(X$themes)
    testthat::expect_identical(themes$theme_id, p$source_theme_id, info = v)
    testthat::expect_identical(themes$colour, p$colour, info = v)
    testthat::expect_identical(themes$theme_id, X$themes$theme_id[order(X$themes$display_order)], info = v)
  }
})

testthat::test_that("the dot matrix shows every module and every multi-module supermodule, nothing else", {
  for (v in names(XS)) {
    X <- XS[[v]]
    m <- wtc_matrix_data(X)
    prim <- X$long[X$long$is_primary_variant %in% TRUE, , drop = FALSE]
    want <- prim[prim$level == "module" | grepl(";", prim$member_modules, fixed = TRUE), , drop = FALSE]
    testthat::expect_identical(nrow(m), nrow(want), info = v)
    testthat::expect_true(all(m$level[grepl(";", m$member_modules, fixed = TRUE)] == "supermodule"), info = v)
    testthat::expect_identical(sum(m$level == "module"), nrow(X$wheel) * nrow(X$themes), info = v)
    # the values drawn are the exported ones, unchanged
    k <- match(paste(m$dataset, m$level, m$entity_id, m$theme_id),
               paste(want$dataset, want$level, want$entity_id, want$theme_id))
    testthat::expect_false(anyNA(k), info = v)
    for (col in c("frac_theme_in_entity", "fold_enrichment", "composition_supported"))
      testthat::expect_identical(m[[col]], want[[col]][k], info = paste(v, col))
  }
})

testthat::test_that("the coherence tiles restate the summary and the pips its sensitivity columns", {
  for (v in names(XS)) {
    X <- XS[[v]]
    cd <- wtc_coherence_data(X)
    testthat::expect_identical(nrow(cd$tiles), length(WTC_DATASETS) * nrow(X$themes), info = v)
    testthat::expect_true(all(cd$tiles$class_label[cd$tiles$n_carriers == 0] == "no carrier"), info = v)
    # a pip exists only where there is a top carrier to check
    with_carrier <- paste(cd$tiles$dataset, cd$tiles$theme_id)[cd$tiles$n_carriers > 0]
    testthat::expect_true(all(paste(cd$pips$dataset, cd$pips$theme_id) %in% with_carrier), info = v)
    testthat::expect_true(all(table(paste(cd$pips$dataset, cd$pips$theme_id)) == length(WTC_SENSITIVITIES)),
                          info = v)
  }
})

testthat::test_that("v2 keeps v1's module-level composition and theme summary", {
  # docs/WGCNA_THEME_COMPOSITION_CANDIDATE.md: v2 differs from v1 through the supermodule cut heights only
  testthat::expect_true(all(c("v1", "v2") %in% names(XS)))
  mod <- function(X) {
    d <- X$long[X$long$level == "module", , drop = FALSE]
    rownames(d) <- NULL
    d
  }
  testthat::expect_identical(mod(XS$v2), mod(XS$v1))
  testthat::expect_identical(XS$v2$summary, XS$v1$summary)
})

testthat::test_that("the candidate computes no statistic and draws no group effect", {
  files <- c(repo_path("R", "panels", "wgcna_theme_composition_panels.R"),
             repo_path("figures", "candidates", "wgcna_theme_composition_candidate.R"))
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
