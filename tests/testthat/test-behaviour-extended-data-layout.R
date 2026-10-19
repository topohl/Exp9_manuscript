# Check the assembled artifacts: a panel extending beyond the canvas can pass
# source hashes while clipping both the data and its annotations.
test_that("behavioural Extended Data panels stay inside the assembled page", {
  root <- file.path(testthat::test_path("..", ".."), "results", "figures", "manuscript")
  attribute <- function(line, name) {
    as.numeric(sub(paste0('.*\\s', name, '="([-0-9.]+)".*'), "\\1", line))
  }
  for (id in c("extended_data_05", "extended_data_09")) {
    file <- file.path(root, id, "assembled", paste0(id, ".svg"))
    expect_true(file.exists(file), info = id)
    if (!file.exists(file)) next
    text <- readLines(file, warn = FALSE, encoding = "UTF-8")
    svg <- text[grepl("^<svg ", text)]
    view <- as.numeric(strsplit(sub('.*viewBox="([^"]+)".*', "\\1", svg), " ")[[1]])
    panels <- text[grepl("^<image ", text)]
    expect_equal(length(panels), if (id == "extended_data_05") 2L else 3L)
    for (panel in panels) {
      x <- attribute(panel, "x"); y <- attribute(panel, "y")
      width <- attribute(panel, "width"); height <- attribute(panel, "height")
      expect_gte(x, 0, label = id); expect_gte(y, 0, label = id)
      expect_lte(x + width, view[3], label = id)
      expect_lte(y + height, view[4], label = id)
    }
  }
})
