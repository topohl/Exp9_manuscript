# The manuscript palette (config/manuscript_palette.yml, v3.2): one colour source for every figure.
#
#   1. the contract: version, group and diverging colours, a white midpoint, ends distinct from it and each other;
#   2. one fixed colour limit per signed measure, drawn by nv_diverging(measure = ...): the limit, the <= / >= end
#      labels, full colour beyond the limit, and a missing value that is never white (white is zero);
#   3. a panel's own limits (no measure) keep ggplot's censor, so an out-of-range value stays visible;
#   4. no canonical or active renderer repeats a palette colour as a literal or keeps a replaced one, every signed
#      fill goes through nv_diverging(), and every behaviour figure takes its group colours from the palette file.
#
# Portable: repository files only, nothing rendered.

source(testthat::test_path("..", "..", "R", "paths.R"))
source(repo_path("R", "panels", "nature_v2_figure_utils.R"))
repo <- function(...) file.path(testthat::test_path("..", ".."), ...)

PAL <- yaml::read_yaml(repo("config", "manuscript_palette.yml"))
LE <- intToUtf8(0x2264); GE <- intToUtf8(0x2265); MINUS <- intToUtf8(0x2212)

testthat::test_that("the palette contract holds the v3 colours", {
  testthat::expect_identical(PAL$palette_version, "manuscript_palette_v3.2")
  testthat::expect_identical(unlist(PAL$group), c(CON = "#6B7296", RES = "#BFBCB4", SUS = "#C74C56"))
  testthat::expect_identical(unlist(PAL$diverging), c(low = "#6679D9", mid = "#FFFFFF", high = "#F2CA4E"))
  testthat::skip_if_not_installed("farver")
  de <- function(a, b) as.numeric(farver::compare_colour(farver::decode_colour(a), farver::decode_colour(b),
                                                         from_space = "rgb", method = "cie2000"))
  d <- unlist(PAL$diverging)
  # the white midpoint was chosen knowing the yellow arm is the weaker one, so this checks distinctness, not balance
  testthat::expect_gt(de(d[["low"]], d[["mid"]]), 25)
  testthat::expect_gt(de(d[["high"]], d[["mid"]]), 25)
  testthat::expect_gt(de(d[["low"]], d[["high"]]), 50)
})

testthat::test_that("each signed measure has one fixed colour limit", {
  testthat::expect_identical(unlist(PAL$diverging_limits),
                             c(smd = 1, correlation = 0.6, nes = 2, z = 2, z_set_mean = 1))
  testthat::expect_identical(nv_diverging_limit("nes"), 2)
  testthat::expect_error(nv_diverging_limit("log2fc"), "no fixed diverging limit")
})

testthat::test_that("nv_diverging(measure =) draws the fixed limit, squishes beyond it and marks the ends", {
  s <- nv_diverging(measure = "nes", name = "NES")
  testthat::expect_equal(s$limits, c(-2, 2))
  testthat::expect_identical(s$get_labels(s$get_breaks()),
                             c(paste0(LE, MINUS, "2"), paste0(MINUS, "1"), "0", "1", paste0(GE, "2")))
  s$train(c(-2, 2))
  testthat::expect_identical(toupper(s$map(c(-5, 0, 5))), toupper(unname(unlist(PAL$diverging))))
  testthat::expect_false(toupper(s$na.value) %in% c("WHITE", "#FFFFFF", "GREY100", "GRAY100"))
  r <- nv_diverging(measure = "correlation")
  testthat::expect_identical(r$get_labels(r$get_breaks()),
                             c(paste0(LE, MINUS, "0.6"), paste0(MINUS, "0.3"), "0", "0.3", paste0(GE, "0.6")))
  z <- nv_diverging(measure = "z_set_mean", guide = "none")
  testthat::expect_equal(z$limits, c(-1, 1))
  testthat::expect_error(nv_diverging(limits = c(-1, 1), measure = "nes"), "not both")
  # a caller's own breaks are labelled too, ends marked only at the limit
  b <- nv_diverging(measure = "nes", breaks = c(-2, 0, 2))
  testthat::expect_identical(b$get_labels(b$get_breaks()),
                             c(paste0(LE, MINUS, "2"), "0", paste0(GE, "2")))
  testthat::expect_error(nv_diverging(measure = "nes", oob = scales::censor), "always squishes")
})

testthat::test_that("a panel's own limits keep ggplot's censor, so an out-of-range value stays visible", {
  s <- nv_diverging(limits = c(-1, 1), name = NULL)
  s$train(c(-1, 1))
  testthat::expect_identical(s$map(5), s$na.value)
  testthat::expect_false(toupper(s$na.value) %in% c("WHITE", "#FFFFFF"))
})

# canonical and active renderers (figures/FIGURE_INDEX.md); superseded generations keep their own code
PANELS <- c("nature_v2_figure_utils.R", "final_truth_v9_panels.R", "final_truth_v9_fidelity_panels.R",
            "final_truth_v9_ed_panels.R", "final_truth_v9_figure_utils.R", "figure3_adaptation_panels.R",
            "manuscript_figure_utils.R", "behaviour_figure_style.R", "behaviour_figure1_panels.R",
            "behaviour_figure1_nature_panels.R", "behaviour_figure1_option3_panels.R",
            "behaviour_figure1_option3b_panels.R", "behaviour_s30_cookie_panels.R", "behaviour_s30_edx_panels.R",
            "behaviour_s30_light_panels.R", "behaviour_s30_screen_panels.R", "cc4_phase_groups_candidate.R",
            "cc4_phase_groups_nature.R", "cc4_phenotype_candidate.R")
FIGURES <- c("figure_01_panels.R", "extended_data_behaviour_panels.R", "behaviour_v101_s30_candidates.R",
             "figure_03_supplementary_go.R", "figure_03_go_atlas_appendix.R", "final_truth_v9_legends.R",
             "manuscript_supporting_immunostaining_candidates.R", "manuscript_supporting_immunostaining_panel.R")
FILES <- c(file.path("R", "panels", PANELS), file.path("figures", FIGURES))
code_of <- function(f) { x <- readLines(repo(f), warn = FALSE); x[!grepl("^\\s*#", x)] }
HEXES <- toupper(c(unlist(PAL$group), PAL$diverging$low, PAL$diverging$high,  # current (white excepted)
                   "#8A8A8A", "#2E7D91",                                      # replaced v1 CON, RES
                   "#3E3C6F", "#C6C3BB", "#E63A48",                           # replaced behavioural trio (v2)
                   "#4C566A", "#D8D2C7", "#D98B3A", "#96460A"))              # replaced diverging ends (v1, v2)
# (#D1543A, the v1 SUS, stays in use as an evidence-class colour of ED3/ED8 and the YAML claimability set)
HAND_BUILT <- "scale_(fill|colour|color)_(gradient2|gradientn)\\("
hex_hits <- function(code) HEXES[vapply(HEXES, function(h) any(grepl(h, toupper(code), fixed = TRUE)), logical(1))]

testthat::test_that("no canonical or active renderer repeats a palette colour or keeps a replaced one", {
  for (f in FILES) testthat::expect_equal(unname(hex_hits(code_of(f))), character(0), info = f)
})

testthat::test_that("every signed fill goes through nv_diverging()", {
  # behaviour option 3 b reads the palette and the fixed z limit itself (the behaviour layer does not source the
  # proteomics helpers); manuscript_figure_utils.R keeps the legacy m12 render mode that no contract panel reaches
  allowed <- file.path("R", "panels", c("nature_v2_figure_utils.R", "behaviour_figure1_option3_panels.R",
                                        "manuscript_figure_utils.R"))
  for (f in setdiff(FILES, allowed))
    testthat::expect_false(any(grepl(HAND_BUILT, code_of(f))), info = f)
  o3 <- code_of(file.path("R", "panels", "behaviour_figure1_option3_panels.R"))
  testthat::expect_true(any(grepl("F1O3N_Z_CAP <- as.numeric(F1O3N_PALETTE$diverging_limits$z)", o3, fixed = TRUE)))
  testthat::expect_false(any(grepl('na.value = "white"', o3, fixed = TRUE)))
})

testthat::test_that("everything a contract renderer reaches is held to the same rules, function by function", {
  # The contracts also name renderers that live in older modules beside superseded code (2c and 2f in
  # nature_final_v7, ED3c and ED8b in story_v5, the ED4 cell-type panel in nature_v2_figure_panels), and every
  # renderer calls helpers. Every function a contract renderer can reach through R/panels, outside the files
  # checked whole above, is checked on its own, so the superseded code beside it keeps its own colours.
  renderers <- character()
  walk <- function(x) if (is.list(x)) {
    if (!is.null(x$renderer)) renderers <<- c(renderers, as.character(x$renderer))
    for (e in x) walk(e)
  }
  for (f in c("figure_final_truth_v9_contract.yml", "figure_contract.yml")) walk(yaml::read_yaml(repo("figures", f)))
  renderers <- unique(renderers[grepl("^[A-Za-z.][A-Za-z0-9._]*$", renderers)])
  defs <- list()
  for (f in list.files(repo("R", "panels"), pattern = "[.]R$", full.names = TRUE)) {
    ex <- parse(f, keep.source = TRUE)
    for (i in seq_along(ex)) {
      e <- ex[[i]]
      if (is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
          is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function")))
        defs[[as.character(e[[2]])]] <- c(defs[[as.character(e[[2]])]], list(list(
          file = basename(f), body = e[[3]], code = as.character(attr(ex, "srcref")[[i]]))))
    }
  }
  testthat::expect_identical(setdiff(renderers, names(defs)), character(0))
  reached <- character()
  todo <- renderers
  while (length(todo)) {
    n <- todo[1]
    todo <- todo[-1]
    if (n %in% reached) next
    reached <- c(reached, n)
    for (d in defs[[n]]) todo <- c(todo, setdiff(intersect(all.names(d$body), names(defs)), reached))
  }
  outside <- reached[!vapply(reached, function(n) all(vapply(defs[[n]], `[[`, "", "file") %in% PANELS), logical(1))]
  testthat::expect_true(all(c("nf_pca_compact", "nf_bilateral_main", "s5_ed_ca2_displacement",
                              "s5_ed_network_distance", "nvp_ed_celltype") %in% outside))
  for (n in outside) for (d in defs[[n]]) {
    code <- d$code[!grepl("^\\s*#", d$code)]
    testthat::expect_equal(unname(hex_hits(code)), character(0), info = paste(n, d$file))
    testthat::expect_false(any(grepl(HAND_BUILT, code)), info = paste(n, d$file))
  }
})

testthat::test_that("every behaviour figure takes its group colours from the palette file", {
  testthat::expect_true(any(grepl(
    'GROUP_COL <- unlist(yaml::read_yaml(repo_path("config", "manuscript_palette.yml"))$group)',
    code_of(file.path("R", "panels", "behaviour_figure_style.R")), fixed = TRUE)))
  for (f in c("cc4_phase_groups_candidate.R", "cc4_phase_groups_nature.R", "cc4_phenotype_candidate.R"))
    testthat::expect_true(any(grepl("manuscript_palette.yml", code_of(file.path("R", "panels", f)), fixed = TRUE)),
                          info = f)
})
