# A panel extending beyond the canvas can pass source hashes while clipping both
# the data and its annotations. The assembler scales each panel into its declared
# box, so the boxes must lie on the page, and the page must draw every panel.
test_that("behavioural Extended Data panels stay inside the assembled page", {
  root <- file.path(testthat::test_path("..", ".."), "results", "figures", "manuscript")
  figs <- yaml::read_yaml(file.path(testthat::test_path("..", ".."), "figures", "figure_contract.yml"))$figures
  for (key in c("ED_behaviour_models", "ED_behaviour_secondary")) {
    f <- figs[[key]]
    for (p in f$panels) {
      expect_gte(p$x, 0, label = p$id); expect_gte(p$y, 0, label = p$id)
      expect_lte(p$x + p$w, f$width_mm, label = p$id)
      expect_lte(p$y + p$h, f$height_mm, label = p$id)
    }
    id <- as.character(f$canonical_publication_id)
    file <- file.path(root, id, "assembled", paste0(id, ".svg"))
    expect_true(file.exists(file), info = id)
    if (!file.exists(file)) next
    text <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    expect_equal(lengths(regmatches(text, gregexpr('<g id="panel_', text, fixed = TRUE))), length(f$panels))
  }
})
