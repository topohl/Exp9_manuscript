# source_data/proteomics_statistics: rows imported from the preserved original-centring archive by
# tools/import_proteomics_statistics.R for stats.xlsx. Every file matches its manifest and nothing else is in the folder.
testthat::test_that("the proteomic statistics import matches its manifest", {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  d <- repo_path("source_data", "proteomics_statistics")
  m <- utils::read.csv(file.path(d, "00_manifest.csv"), stringsAsFactors = FALSE)
  testthat::expect_setequal(list.files(d), c(m$file, "00_manifest.csv"))
  for (i in seq_len(nrow(m))) {
    f <- file.path(d, m$file[i])
    testthat::expect_identical(unname(tools::sha256sum(f)), m$sha256[i], info = m$file[i])
    testthat::expect_identical(nrow(utils::read.csv(f)), m$rows[i], info = m$file[i])
  }
  p <- utils::read.csv(file.path(d, "protein_statistics.csv"), stringsAsFactors = FALSE)
  testthat::expect_identical(nrow(p), 63L)
  # the stored P is the moderated t on residual df (60 neuropil, 24 soma and microglia) plus a per-protein prior df of about 2
  dfs <- mapply(function(t, pv) stats::uniroot(function(d) 2 * stats::pt(-abs(t), d) - pv, c(1, 1000))$root, p$t, p$pval)
  np <- p$dataset == "neuron_neuropil"
  testthat::expect_true(all(dfs[np] > 60 & dfs[np] < 63) && all(dfs[!np] > 24 & dfs[!np] < 27))
})
