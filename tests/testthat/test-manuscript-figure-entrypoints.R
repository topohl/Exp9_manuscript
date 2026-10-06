testthat::test_that("manuscript figure contract has exact unique panel identities", {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  source(repo_path("R", "manuscript_figure_utils.R"))
  testthat::skip_if_not_installed("yaml")

  fig2 <- manuscript_figure_contract("02")
  fig3 <- manuscript_figure_contract("03")
  ids2 <- vapply(fig2$panels, function(x) as.character(x$id), character(1))
  ids3 <- vapply(fig3$panels, function(x) as.character(x$id), character(1))

  # Figure 2 remains a-h; the adaptation redesign expands Figure 3 to a-m.
  testthat::expect_identical(ids2, paste0("2", letters[1:8]))
  testthat::expect_identical(ids3, paste0("3", letters[1:13]))
  testthat::expect_false(anyDuplicated(c(ids2, ids3)) > 0L)
  testthat::expect_identical(fig2$contract_version,
                             "manuscript_figures_v4_figure3_adaptation")
  testthat::expect_identical(fig3$contract_version,
                             "manuscript_figures_v4_figure3_adaptation")
})

testthat::test_that("Figure 2a is a rendered anatomy schematic, not a placeholder", {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  source(repo_path("R", "manuscript_figure_utils.R"))
  fig2 <- manuscript_figure_contract("02")
  panel_2a <- fig2$panels[[1]]

  # Before the promotion this slot was deferred_to_illustrator with no asset at
  # all. The promoted generation draws it, so it must now behave like every
  # other automated panel.
  testthat::expect_identical(panel_2a$id, "2a")
  testthat::expect_identical(as.character(panel_2a$renderer), "f9_schematic")
  testthat::expect_true(manuscript_figure_panel_is_automated(panel_2a))
  testthat::expect_true(nzchar(as.character(panel_2a$figure_source)))
  testthat::expect_true(nzchar(as.character(panel_2a$primary_source)))
})

testthat::test_that("contract pins canonical panels and does not use newest-file selection", {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  source(repo_path("R", "manuscript_figure_utils.R"))
  contract_text <- paste(readLines(manuscript_figure_contract_path(), warn = FALSE), collapse = "\n")
  helper_text <- paste(readLines(repo_path("R", "manuscript_figure_utils.R"), warn = FALSE), collapse = "\n")

  # Each promoted panel names one exact asset produced by the v9 entry points.
  for (asset in c("v9_schematic.svg", "v9_depth.svg", "v9_pca.svg",
                  "v9_fingerprint.svg", "v9_compartment.svg", "v9_bilateral_main.svg",
                  "v9_external_main.svg", "v9_internal_main.svg",
                  "v9_dap_track.svg", "v9_atlas.svg",
                  "v9_adaptation_state.svg", "v9_adaptation_burden.svg",
                  "v9_card_syn.svg", "v9_card_rna.svg", "v9_card_ox.svg",
                  "v9_curve_syn.svg", "v9_curve_rna.svg", "v9_curve_ox.svg",
                  "v9_prot_syn.svg", "v9_prot_rna.svg", "v9_prot_ox.svg")) {
    testthat::expect_match(contract_text, asset, fixed = TRUE)
  }
  testthat::expect_false(grepl("latest_input_candidate", helper_text, fixed = TRUE))
  testthat::expect_false(grepl("list.files", helper_text, fixed = TRUE))
})

testthat::test_that("hemisphere and biological-unit declarations stay explicit", {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  source(repo_path("R", "manuscript_figure_utils.R"))
  # Every promoted panel must still declare both fields. The promoted generation
  # is animal-level and bilaterally aggregated throughout; the point of the test
  # is that the declaration is never silently dropped, which is how the previous
  # generation lost it on the one panel that was entirely about hemispheres.
  for (k in c("02", "03")) {
    fig <- manuscript_figure_contract(k)
    for (p in fig$panels) {
      testthat::expect_true(nzchar(as.character(p$biological_unit %||% "")),
                            info = paste("missing biological_unit:", p$id))
      testthat::expect_true(nzchar(as.character(p$hemisphere_handling %||% "")),
                            info = paste("missing hemisphere_handling:", p$id))
    }
  }
})

testthat::test_that("incomplete candidates require an explicit output root", {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  source(repo_path("R", "manuscript_figure_utils.R"))
  canonical <- manuscript_figure_args("--allow-incomplete")
  isolated <- manuscript_figure_args(c("--allow-incomplete", "--output-root", tempfile("figure-candidate-")))
  testthat::expect_true(canonical$allow_incomplete)
  testthat::expect_false(canonical$output_explicit)
  testthat::expect_true(isolated$allow_incomplete)
  testthat::expect_true(isolated$output_explicit)
})

testthat::test_that("SVG assembly is self-contained and preserves panel letters", {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  source(repo_path("R", "manuscript_figure_utils.R"))
  testthat::skip_if_not_installed("base64enc")
  root <- tempfile("figure-assembly-")
  dir.create(root)
  a <- file.path(root, "a.svg")
  b <- file.path(root, "b.svg")
  writeLines('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10"><rect width="10" height="10" fill="red"/></svg>', a)
  writeLines('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10"><rect width="10" height="10" fill="blue"/></svg>', b)
  panels <- list(
    list(id = "9a", row = 1L, col = 1L),
    list(id = "9b", row = 1L, col = 2L)
  )
  target <- file.path(root, "assembled.svg")
  manuscript_figure_assemble_svg(
    c("9a" = a, "9b" = b), panels,
    list(width_mm = 183, height_mm = 80), target
  )
  text <- paste(readLines(target, warn = FALSE), collapse = "\n")
  testthat::expect_match(text, "data:image/svg+xml;base64", fixed = TRUE)
  testthat::expect_match(text, ">a</text>", fixed = TRUE)
  testthat::expect_match(text, ">b</text>", fixed = TRUE)
  testthat::expect_false(grepl(a, text, fixed = TRUE))
})

testthat::test_that("the figure entry points live here and fit no models", {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  # Was: registered as downstream-only steps in the pRoteomics registry. That
  # registration was removed in Phase 6C and pRoteomics now asserts no
  # renderer is registered. What matters here is that the entry points exist
  # in the repository that owns them and remain assembly-only; the inference
  # ban itself is enforced across the whole layer by
  # tests/testthat/test-no-analysis-in-manuscript.R.
  for (s in c("figures/figure_02.R", "figures/figure_03.R")) {
    testthat::expect_true(file.exists(repo_path(s)), label = s)
    code <- readLines(repo_path(s), warn = FALSE)
    code <- sub("#.*$", "", code)
    testthat::expect_false(any(grepl("(^|[^A-Za-z0-9._])(lm|glm|p[.]adjust|fgsea)[[:space:]]*\\(",
                                     code, perl = TRUE)), label = s)
  }
})

testthat::test_that("an assembled page is rasterised with each panel drawn at the page resolution", {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  source(repo_path("R", "manuscript_figure_utils.R"))
  testthat::skip_if_not_installed("magick")
  testthat::skip_if_not_installed("base64enc")
  dir <- tempfile("raster_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  panel <- file.path(dir, "panel.svg")
  writeLines(c('<svg xmlns="http://www.w3.org/2000/svg" width="40mm" height="20mm" viewBox="0 0 40 20">',
               '<rect width="100%" height="100%" fill="white"/>',
               '<rect x="10" y="5" width="20" height="10" fill="black"/>', '</svg>'), panel)
  page <- file.path(dir, "page.svg")
  manuscript_figure_assemble_svg(list("9a" = panel), list(list(id = "9a", x = 5, y = 5, w = 40, h = 20)),
                                 list(width_mm = 50, height_mm = 30, layout_mode = "absolute"), page)
  img <- manuscript_figure_rasterize(page, dpi = 300)
  grey <- as.integer(magick::image_data(magick::image_convert(img, colorspace = "gray"), "gray"))[, , 1]
  testthat::expect_identical(dim(grey), c(354L, 591L))
  # The black box spans x = 15-35 mm of the page (pixels 177.2-413.4) and the
  # row is its middle. Read as one document, the embedded panel was scaled up
  # from a low-resolution raster and this edge smeared over about 12 pixels.
  row <- grey[178, ]
  testthat::expect_lt(row[181], 30)
  testthat::expect_gt(row[175], 225)
  testthat::expect_lt(row[412], 30)
  testthat::expect_gt(row[416], 225)
  # the panel letter is drawn over the panel's top-left corner
  testthat::expect_lt(min(grey[60:95, 59:95]), 60)
})
