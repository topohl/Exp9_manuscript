testthat::local_edition(3)

source(testthat::test_path("..", "..", "R", "paths.R"))

# Figure 2j-l source data (input territories, afferent sources, inhibitory share): the row types each
# panel deposits, control n, the replication animals (named, never a control animal, n matching), and the
# columns the legend text promises. Read-only; skips where the figure is not promoted.

sd_path <- function(p) repo_path("figures", "main", "source_data", "figure_02", paste0("figure_02", p, "_source_data.csv"))
read_sd <- function(p) {
  f <- sd_path(p)
  testthat::skip_if_not(file.exists(f), paste("Figure 2", p, "source data not promoted here"))
  utils::read.csv(f, stringsAsFactors = FALSE)
}
check_animals <- function(d, control_types, replication_types) {
  con <- d[d$row_type %in% control_types, , drop = FALSE]
  rep <- d[d$row_type %in% replication_types, , drop = FALSE]
  testthat::expect_true(nrow(con) > 0 && nrow(rep) > 0)
  testthat::expect_true(all(con$n_animals == 3))
  con_animals <- unique(unlist(strsplit(con$animals, "; ", fixed = TRUE)))
  testthat::expect_length(con_animals, 3)
  for (i in seq_len(nrow(rep))) {
    an <- strsplit(rep$animals[i], "; ", fixed = TRUE)[[1]]
    testthat::expect_equal(length(an), rep$n_animals[i])
    testthat::expect_false(any(an %in% con_animals))
  }
}

testthat::test_that("2j deposits animals, tests, replication, sensitivity analyses and protein contrasts", {
  d <- read_sd("j")
  testthat::expect_setequal(unique(d$row_type),
                            c("animal", "comparison", "replication_comparison", "sensitivity", "protein_contrast"))
  check_animals(d, "comparison", "replication_comparison")
  testthat::expect_true(all(c("null_q025_excess", "null_q975_excess", "excess_over_random") %in% names(d)))
  s <- d[d$row_type == "sensitivity", , drop = FALSE]
  testthat::expect_true("lamina-label permutation" %in% s$analysis)
  testthat::expect_true(all(is.finite(s$p_lamina_permutation[s$analysis == "lamina-label permutation"])))
  testthat::expect_true(all(is.finite(s$ci_low[s$analysis != "lamina-label permutation"])))
})

testthat::test_that("2k deposits tiles, the animal values behind them, contrasts, replication and class members", {
  d <- read_sd("k")
  testthat::expect_setequal(unique(d$row_type),
                            c("tile", "animal_tile", "contrast", "replication_contrast", "class_member"))
  check_animals(d, "contrast", "replication_contrast")
  tiles <- d[d$row_type == "tile", , drop = FALSE]
  at <- d[d$row_type == "animal_tile", , drop = FALSE]
  means <- stats::aggregate(excess ~ origin_class + lamina, data = at, FUN = mean)
  k <- match(paste(tiles$origin_class, tiles$lamina), paste(means$origin_class, means$lamina))
  testthat::expect_equal(tiles$excess, means$excess[k], tolerance = 1e-9)   # a tile is the mean of its animals
})

testthat::test_that("2l deposits the VGLUT2 and GAD2 + VGAT columns its legend names, and the replication", {
  d <- read_sd("l")
  testthat::expect_setequal(unique(d$row_type), c("animal_lamina", "lamina_comparison", "replication_lamina_comparison"))
  testthat::expect_true(all(c("gad2_vgat_share_log2", "vglut2_minus_vglut1_log2", "vglut2_acquisitions_observed",
                              "estimate_gad2_vgat_only") %in% names(d)))
  check_animals(d, "lamina_comparison", "replication_lamina_comparison")
})
