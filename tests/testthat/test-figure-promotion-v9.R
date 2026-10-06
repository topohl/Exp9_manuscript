# Guards for the Phase 5B promotion of final_truth_v9 to manuscript Figures 2 and 3.
#
# Three defects held the promotion back and each has a test here, because each
# was invisible to the checks that existed at the time: a column misregistration
# that no audit measured, a provenance dependency that no test declared, and a
# scientific constant hard-coded into a legend where re-running could not
# correct it.

repo_rel <- function(...) file.path(testthat::test_path("..", ".."), ...)
contract <- function() {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  yaml::read_yaml(repo_rel("figures", "figure_contract.yml"))
}

testthat::test_that("exactly one canonical generation is declared per manuscript figure", {
  testthat::skip_if_not_installed("yaml")
  y <- contract()
  testthat::expect_identical(y$contract_version,
                             "manuscript_figures_v4_figure3_adaptation")

  for (k in c("02", "03")) {
    f <- y$figures[[k]]
    testthat::expect_identical(as.character(f$canonical_generation), "final_truth_v9")
    testthat::expect_identical(as.character(f$superseded_status), "SUPERSEDED_FOR_MANUSCRIPT")
    testthat::expect_false(isTRUE(f$rendering_repository_computes_statistics))
    testthat::expect_identical(as.character(f$layout_mode), "absolute")
  }

  # Figure 2 remains a-h; the adaptation redesign expands Figure 3 to a-m.
  testthat::expect_identical(
    vapply(y$figures[["02"]]$panels, function(p) as.character(p$id), character(1)),
    paste0("2", letters[1:8]))
  testthat::expect_identical(
    vapply(y$figures[["03"]]$panels, function(p) as.character(p$id), character(1)),
    paste0("3", letters[1:13]))

  # Figure 1 was untouched by the v9 promotion. It is now rendered from the pinned
  # canonical behaviour bundle (six panels, a-f; test-figure-01-renderer.R).
  testthat::expect_identical(
    vapply(y$figures[["01"]]$panels, function(p) as.character(p$id), character(1)),
    paste0("1", letters[1:6]))
})

testthat::test_that("PB-02: no manuscript figure depends on the superseded export namespace", {
  testthat::skip_if_not_installed("yaml")
  src <- paste(readLines(repo_rel("figures", "figure_final_truth_v9_contract.yml"),
                         warn = FALSE), collapse = "\n")
  # The producing contract must not read anything the manuscript layer writes,
  # which is what made promoting-and-retiring impossible before.
  testthat::expect_false(grepl("results/source_data/manuscript/figure_0", src, fixed = TRUE))

  y <- contract()
  for (k in c("02", "03")) for (p in y$figures[[k]]$panels) {
    deps <- as.character(unlist(p$input_dependencies %||% character()))
    for (d in deps) {
      testthat::expect_false(grepl("^results/source_data/manuscript/figure_0", d),
        info = paste(p$id, "still depends on the v2 export namespace:", d))
    }
  }
})

testthat::test_that("PB-02: the promoted panels resolve to canonical stage outputs", {
  testthat::skip_if_not_installed("yaml")
  v9 <- yaml::read_yaml(repo_rel("figures", "figure_final_truth_v9_contract.yml"))
  byid <- setNames(v9$panels, vapply(v9$panels, function(p) as.character(p$id), character(1)))

  testthat::expect_identical(as.character(byid[["v9_depth"]]$primary_source),
                             "data/raw/pg_matrix/quicksearch.stats.annotated.xlsx")
  testthat::expect_identical(as.character(byid[["v9_pca"]]$primary_source),
    "results/tables/03_qc_exploration/00b_joint_compartment_qc/global/joint_primary_pca_scores.csv")
  for (id in c("v9_depth", "v9_pca")) {
    testthat::expect_true(file.exists(repo_rel(as.character(byid[[id]]$primary_source))),
                          info = paste("canonical source missing for", id))
  }
})

testthat::test_that("Figure 3 overview and atlas retain the same full-width box", {
  testthat::skip_if_not_installed("yaml")
  v9 <- yaml::read_yaml(repo_rel("figures", "figure_final_truth_v9_contract.yml"))
  f3 <- Filter(function(x) x$name == "F3_NATURE_FINAL_V9", v9$figures)[[1]]
  boxes <- setNames(f3$layout, vapply(f3$layout, function(x) as.character(x$panel), character(1)))
  testthat::expect_identical(as.numeric(boxes[["v9_dap_track"]]$x),
                             as.numeric(boxes[["v9_atlas"]]$x))
  testthat::expect_identical(as.numeric(boxes[["v9_dap_track"]]$w),
                             as.numeric(boxes[["v9_atlas"]]$w))
})

testthat::test_that("the redesigned overview and atlas render without placeholders", {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  pan <- path_results("figures", "manuscript_candidates", "final_truth_v9", "figure_03", "panels")
  a <- file.path(pan, "v9_dap_track.svg"); b <- file.path(pan, "v9_atlas.svg")
  testthat::skip_if_not(file.exists(a) && file.exists(b), "Figure 3 panels not rendered here")

  for (f in c(a, b)) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    testthat::expect_false(grepl("RENDER ERROR", txt, fixed = TRUE))
    testthat::expect_gt(file.info(f)$size, 5000)
  }
})

testthat::test_that("PB-03: the eps-floor disclosure is derived, not hard-coded", {
  src <- paste(readLines(repo_rel("figures", "final_truth_v9_legends.R"), warn = FALSE),
               collapse = "\n")
  testthat::expect_true(grepl("f9_eps_floor_disclosure", src, fixed = TRUE))
  # The stale literal must not come back as a live string. It survives only
  # inside the comment that explains why it was removed.
  live <- sub("(?s)# This disclosure used to be.*?derived\\.", "", src, perl = TRUE)
  testthat::expect_false(grepl("90 of the 851", live, fixed = TRUE))
  testthat::expect_false(grepl('"851"', live, fixed = TRUE))
  # The floor itself is declared once and checked against the data at run time.
  testthat::expect_true(grepl("GSEA_EPS <- 1e-10", src, fixed = TRUE))
  testthat::expect_true(grepl("is not the declared floor", src, fixed = TRUE))
})

testthat::test_that("SR-21: the shipped Figure 3a is the QC-aware panel", {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  sh <- path_results("manuscript", "figure_3", "panels", "figure_03a.svg")
  testthat::skip_if_not(file.exists(sh), "Figure 3 not exported here")

  lab <- local({
    s <- readLines(sh, warn = FALSE)
    t <- unlist(regmatches(s, gregexpr("<text[^>]*>[^<]*</text>", s)))
    sub("^<text[^>]*>(.*)</text>$", "\\1", t)
  })
  # Two rows, and the second one is what the pre-QC panel never had.
  testthat::expect_true(any(grepl("FDR-supported DAPs", lab, fixed = TRUE)))
  testthat::expect_true(any(grepl("Robustness-qualified", lab, fixed = TRUE)))
  testthat::expect_true(any(grepl("(37 total)", lab, fixed = TRUE)))
  testthat::expect_true(any(grepl("(15 total)", lab, fixed = TRUE)))
  nums <- suppressWarnings(as.integer(lab[grepl("^[0-9]+$", lab)]))
  testthat::expect_identical(sum(nums, na.rm = TRUE), 52L)   # 37 + 15
  testthat::expect_true(28L %in% nums)
  testthat::expect_true(6L %in% nums)
  testthat::expect_false(any(grepl("hotspot", lab, ignore.case = TRUE)))

  sd <- path_results("source_data", "manuscript", "figure_03", "figure_03a_source_data.csv")
  testthat::skip_if_not(file.exists(sd), "Figure 3a source data not exported here")
  d <- utils::read.csv(sd, stringsAsFactors = FALSE)
  testthat::expect_true(all(c("canonical", "claimable") %in% names(d)))
  testthat::expect_identical(sum(d$canonical), 37L)
  testthat::expect_identical(sum(d$claimable), 15L)
})

testthat::test_that("every manuscript figure-panel reference resolves", {
  source(testthat::test_path("..", "..", "R", "paths.R"))
  testthat::skip_if_not_installed("yaml")
  y <- contract()
  declared <- unlist(lapply(c("01", "02", "03"), function(k)
    vapply(y$figures[[k]]$panels, function(p) as.character(p$id), character(1))))

  draft <- readLines(repo_rel("manuscript", "manuscript_draft.md"), warn = FALSE)
  hits <- unlist(regmatches(draft, gregexpr(
    "Fig[.][ ]?[123][a-m](([,–-])[a-m])*", draft, perl = TRUE)))
  testthat::expect_gt(length(hits), 0L)

  for (hh in unique(hits)) {
    fign <- sub("^Fig[.][ ]?([123]).*$", "\\1", hh)
    tail_ <- sub("^Fig[.][ ]?[123]", "", hh)
    ls_ <- regmatches(tail_, gregexpr("[a-m]", tail_))[[1]]
    idx <- match(ls_, letters)
    if (grepl("[–-]", tail_) && length(idx) >= 2L && !anyNA(idx)) {
      ls_ <- letters[seq(min(idx), max(idx))]
    }
    for (L in ls_) {
      testthat::expect_true(paste0(fign, L) %in% declared,
        info = paste("manuscript cites", hh, "-> undeclared panel", paste0(fign, L)))
    }
  }
})

testthat::test_that("Figure 2 and the proteomics Extended Data are registered from their promoted renders", {
  # Tracked files only. Like Figure 3, each identity is published from figures/
  # with its per-panel source data, rendered at one commit on clean code.
  source(testthat::test_path("..", "..", "R", "paths.R"))
  reg <- utils::read.csv(repo_path("provenance", "publication_registry",
                                   "canonical_publication_registry.csv"),
                         stringsAsFactors = FALSE, colClasses = "character")
  sha <- function(p) unname(tools::sha256sum(p))
  panels <- list(figure_02 = paste0("2", letters[1:8]),
                 extended_data_01 = paste0("01", letters[1:2]),
                 extended_data_02 = paste0("02", letters[1:2]),
                 extended_data_03 = paste0("03", letters[1:5]),
                 extended_data_06 = paste0("06", letters[1:5]),
                 extended_data_08 = paste0("08", letters[1:4]))
  for (pid in names(panels)) {
    r <- reg[reg$publication_id == pid, , drop = FALSE]
    testthat::expect_identical(nrow(r), 1L, info = pid)
    testthat::expect_identical(r$status, "CANONICAL", info = pid)
    testthat::expect_identical(r$panels, paste(panels[[pid]], collapse = ","), info = pid)
    dir <- if (startsWith(pid, "figure_")) "main" else "extended_data"
    testthat::expect_identical(r$rendered_artifact,
                               paste0("figures/", dir, "/", pid, ".svg"), info = pid)
    testthat::expect_identical(r$canonical_source_data,
                               paste0("figures/", dir, "/source_data/", pid), info = pid)
    for (ext in c(".svg", ".pdf", ".png"))
      testthat::expect_true(file.exists(repo_path("figures", dir, paste0(pid, ext))),
                            info = paste0(pid, ext))
    testthat::expect_identical(sha(repo_path(r$rendered_artifact)), r$hash, info = pid)
    testthat::expect_true(grepl("^[0-9a-f]{40}$", r$source_commit), info = pid)
    sd <- repo_path(r$canonical_source_data)
    man <- utils::read.csv(file.path(sd, "00_manifest.csv"), stringsAsFactors = FALSE,
                           colClasses = "character")
    testthat::expect_identical(man$file, paste0(pid, sub("^[0-9]+", "", panels[[pid]]),
                                                "_source_data.csv"), info = pid)
    testthat::expect_setequal(list.files(sd), c(man$file, "00_manifest.csv"))
    testthat::expect_identical(sha(file.path(sd, man$file)), man$sha256, info = pid)
    testthat::expect_identical(unique(man$render_git_commit), r$source_commit, info = pid)
  }
  # one producer per key and one render commit, so one promotion
  testthat::expect_length(unique(reg$source_commit[reg$publication_id %in% names(panels)]), 1L)

  # the published source data carry the fixed colour limits the panels were drawn with
  rd <- function(pid, panel) utils::read.csv(repo_path(
    "figures", if (startsWith(pid, "figure_")) "main" else "extended_data", "source_data", pid,
    paste0(pid, panel, "_source_data.csv")), stringsAsFactors = FALSE)
  testthat::expect_equal(unique(rd("figure_02", "d")$colour_limit), 2)
  testthat::expect_equal(unique(rd("extended_data_02", "a")$colour_limit), 1)
  testthat::expect_equal(unique(rd("extended_data_08", "a")$colour_limit), 0.6)
  for (p in c("a", "b"))
    testthat::expect_equal(unique(rd("extended_data_06", p)$shared_NES_scale_limit), 2)
  # ED6 c-e: the curves of the frozen Figure 3 export, on the shared strip limit
  curves <- do.call(rbind, lapply(c("c", "d", "e"), function(p) rd("extended_data_06", p)))
  testthat::expect_equal(curves$shared_NES_strip_limit, c(2, 2, 2))
  testthat::expect_identical(unique(curves$curve_source),
    "source_data/pRoteomics/figure_03_adaptation/running_enrichment_curves.csv")
  testthat::expect_identical(curves$set_size, c(507L, 297L, 105L))
  testthat::expect_identical(curves$leading_edge_n, c(255L, 112L, 48L))
})
