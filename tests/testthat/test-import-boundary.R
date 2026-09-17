source(testthat::test_path("..", "..", "R", "paths.R"))

# Phase 6F: this repository imports scientific source data from exactly one
# place in pRoteomics, its export boundary, and from nowhere else.
#
# Before Phase 6F the source-data manifest pointed at
# results/publication_source_data/, a path in the middle of the analysis
# repository's canonical results tree that had no registered writer at all.
# The bundle now lives at exports/publication_source_data/ and that is the
# declared interface.

BOUNDARY <- "exports/publication_source_data/"

testthat::test_that("every source-data pointer resolves to the export boundary", {
  mf <- repo_path("source_data", "pRoteomics", "manifest.csv")
  testthat::expect_true(file.exists(mf))
  d <- utils::read.csv(mf, stringsAsFactors = FALSE)
  testthat::expect_gt(nrow(d), 0L)
  testthat::expect_true(all(c("publication_id", "exported_file", "sha256") %in% names(d)))

  off <- d$exported_file[!startsWith(d$exported_file, BOUNDARY)]
  if (length(off)) {
    testthat::fail(paste0("source-data pointers outside the export boundary:\n  ",
                          paste(unique(off), collapse = "\n  ")))
  }
  testthat::expect_length(off, 0L)
})

testthat::test_that("the local bundle still matches the manifest hashes", {
  # The pointer says where the data came from; this checks that what we hold
  # is that data. Resolution is local, so the suite never needs pRoteomics.
  mf <- repo_path("source_data", "pRoteomics", "manifest.csv")
  d <- utils::read.csv(mf, stringsAsFactors = FALSE)
  bundle <- repo_path("source_data", "pRoteomics")

  checked <- 0L
  for (i in seq_len(nrow(d))) {
    rel <- sub(BOUNDARY, "", d$exported_file[i], fixed = TRUE)
    f <- file.path(bundle, rel)
    if (!file.exists(f)) next
    testthat::expect_identical(unname(tools::sha256sum(f)), d$sha256[i],
                               info = d$exported_file[i])
    checked <- checked + 1L
  }
  testthat::expect_gt(checked, 0L)
})

testthat::test_that("no source-data pointer names a work or legacy path", {
  mf <- repo_path("source_data", "pRoteomics", "manifest.csv")
  d <- utils::read.csv(mf, stringsAsFactors = FALSE)
  txt <- paste(d$exported_file, collapse = "\n")
  # work/ is regenerable and must never be cited
  testthat::expect_false(grepl("(^|[^A-Za-z0-9_])work/", txt))
  # nor may a pointer reach into the analysis repository's results tree
  testthat::expect_false(any(startsWith(d$exported_file, "results/")))
})

testthat::test_that("the render-input bridge is a closed historical record", {
  # The 196 render inputs are this repository's own renderer outputs, which
  # happened to live in pRoteomics before the split. pRoteomics produces none
  # of them now, so they are provenance, not a live import path. The manifest
  # must therefore record a source commit and never be silently re-resolved.
  ri <- repo_path("provenance", "source_manifests", "render_inputs_manifest.csv")
  testthat::skip_if(!file.exists(ri))
  d <- utils::read.csv(ri, stringsAsFactors = FALSE)
  testthat::expect_gt(nrow(d), 0L)
  testthat::expect_true("source_commit" %in% names(d))
  testthat::expect_true(all(nzchar(d$source_commit)))
  testthat::expect_true(all(d$disposition %in%
    c("IMPORTED", "IMPORTED_DIRECTORY", "PROVENANCE_ONLY")))
  # the three over the size ceiling are recorded by hash rather than copied
  testthat::expect_identical(sum(d$disposition == "PROVENANCE_ONLY"), 3L)
})
