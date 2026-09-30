# Phase 6D: the Part-B tests that asserted properties of the pRoteomics tree
# were removed from this file. Each has named replacement coverage in
# pRoteomics: the producer-layer dependency guard in
# test-analysis-publication-boundary.R, and registry existence plus audit-root
# exclusion in test-pipeline-registry.R. The verification-state guards read a
# pRoteomics audit script and stayed with it.

# Publication-hardening guards.
#
# Part A fixed what the manuscript may claim; Part B established which parts of
# the tree may depend on which. Both are properties that decay silently, so each
# one that can be checked mechanically is checked here rather than described in
# a document. Every expectation below corresponds to a finding or a verified
# clean result in results/tables/publication_hardening/.

repo <- function(...) file.path(testthat::test_path("..", ".."), ...)
tracked_scripts <- function() {
  f <- suppressWarnings(system2("git", c("-C", shQuote(repo()), "ls-files"),
                                stdout = TRUE, stderr = FALSE))
  grep("[.][Rr]$", f, value = TRUE)
}
layer_of <- function(p) {
  top <- sub("/.*", "", p)
  if (top == "99_audits") "audit layer"
  else if (top == "99_deprecated") "deprecated"
  else if (top == "90_testing") "testing scaffold"
  else if (grepl("^[0-9]{2}_", top)) "numbered analysis stage"
  else if (top == "R") "shared helper library"
  else if (top == "figures") "figure / manuscript layer"
  else if (top == "tests") "test suite"
  else "other"
}

# ---------------------------------------------------------------- B27 guards

# ------------------------------------------------- B28 freeze protection

test_that("the frozen v9 contract still reaches every renderer it names", {
  # PH-001: five v9 panels are rendered from superseded layer files. That is
  # accepted and documented, but the renderers must remain DEFINED - moving or
  # renaming one would break the frozen figures' reproducibility silently.
  ct <- yaml::read_yaml(repo("figures", "figure_final_truth_v9_contract.yml"))
  used <- unique(vapply(ct$panels, function(p)
    as.character(p$renderer %||% ""), character(1)))
  used <- used[nzchar(used)]
  # R/ is organised into panels/ and vendor/, so the scan is recursive.
  # A non-recursive glob would find almost nothing defined and the
  # assertion would pass vacuously.
  srcs <- c(list.files(repo("R"), pattern = "[.]R$", recursive = TRUE,
                       full.names = TRUE),
            Sys.glob(repo("figures", "*.R")))
  defined <- unlist(lapply(srcs, function(f)
    sub(" <- function.*", "",
        grep("^[a-z][A-Za-z0-9_]* <- function", readLines(f, warn = FALSE),
             value = TRUE))))
  expect_equal(setdiff(used, defined), character(0))
})

test_that("out-of-layer v9 renderers are exactly the five that are documented", {
  # If this count changes, a panel silently moved between generations. The
  # f3a_ renderers are the v9 layer's own Figure 3 adaptation panels, sourced
  # by s9f_renderer_sources(), not an older generation.
  ct <- yaml::read_yaml(repo("figures", "figure_final_truth_v9_contract.yml"))
  used <- unique(vapply(ct$panels, function(p)
    as.character(p$renderer %||% ""), character(1)))
  used <- used[nzchar(used)]
  out_of_layer <- sort(used[!grepl("^(f9_|s9f_|f3a_)", used)])
  expect_equal(out_of_layer,
               c("nf_bilateral_main", "nf_pca_compact", "nvp_ed_celltype",
                 "s5_ed_ca2_displacement", "s5_ed_network_distance"))
})

test_that("the specificity gate matches the adjective, not only the adverb", {
  # PH-002. "spatially selective" is the spatial form of the banned
  # susceptibility-specific claim; matching only "selectively" let it through.
  sem <- readLines(repo("figures", "final_truth_v9_semantics.R"), warn = FALSE)
  pat <- grep("selectiv", sem, value = TRUE)
  expect_true(any(grepl("selectiv[|]exclusiv", pat)))
  expect_true(any(grepl('RULE\\("selective"', sem)))
})

test_that("every primary atlas theme has exactly one display label", {
  reg <- utils::read.csv(repo("config", "manuscript_go_theme_registry.tsv"),
                         sep = "\t", stringsAsFactors = FALSE)
  prim <- unique(reg$theme_id[reg$theme_role == "primary"])
  pan <- readLines(repo("R", "panels", "final_truth_v9_panels.R"), warn = FALSE)
  b <- grep("^  SHORT <- c\\(", pan)
  e <- b + which(grepl("\\)\\s*$", pan[b:(b + 20)]))[1] - 1L
  src <- paste(pan[b:e], collapse = " ")
  ids <- gsub(" =.*", "",
              regmatches(src, gregexpr("[a-z0-9_]+ = \"[^\"]*\"", src))[[1]])
  expect_equal(sort(ids), sort(prim))
})

test_that("no manuscript prose claims a theme-level p-value or FDR", {
  # The atlas is a descriptive aggregation with no multiple-testing family, so
  # this phrasing can never become correct by rerunning anything.
  rep_dir <- repo("results", "reports", "manuscript_candidates",
                  "final_truth_v9")
  skip_if_not(dir.exists(rep_dir), "v9 report layer not built")
  files <- list.files(rep_dir, pattern = "[.]md$", full.names = TRUE)
  # the rules file and the claim-chain audit exist to quote banned wording
  files <- files[!basename(files) %in% c("manuscript_semantic_rules.md",
                                         "claim_chain_audit.md")]
  # The repository states this prohibition explicitly in its own legends -
  # "no theme-level p-value or FDR is computed or implied" - so the guard has to
  # separate an assertion from a denial, exactly as the semantics layer does.
  DENIAL <- paste0("(^|[^[:alnum:]])(no|never|not|neither|without)([^[:alnum:]]",
                   "[^.]{0,80})?(theme[- ]level|FDR[- ]significant)")
  hits <- unlist(lapply(files, function(f) {
    ln <- readLines(f, warn = FALSE)
    h <- grep("theme[- ]level (p|FDR)|FDR[- ]significant theme", ln,
              ignore.case = TRUE, value = TRUE)
    h[!grepl(DENIAL, h, ignore.case = TRUE, perl = TRUE)]
  }))
  expect_equal(hits, character(0))
})

# ------------------------------------------------- PH-008 spatial wording

test_that("reader-facing prose uses spatially resolved, not restricted", {
  # PH-008. "Resolved" describes what the design achieves; "restricted" and
  # "specific" assert a boundary only a heterogeneity test could draw, and the
  # only such test is at WGCNA level and is FDR-negative.
  rep_dir <- repo("results", "reports", "manuscript_candidates",
                  "final_truth_v9")
  skip_if_not(dir.exists(rep_dir), "v9 report layer not built")
  files <- list.files(rep_dir, pattern = "[.]md$", full.names = TRUE)
  # these two exist in order to quote the wording they ban
  files <- files[!basename(files) %in% c("manuscript_semantic_rules.md",
                                         "claim_chain_audit.md")]
  # A stated count of supported units is factual, not an inferential claim.
  LICENCE <- "of 18|of 10|of 15|FDR-supported in|units in which|subset of units"
  # The repository's standing convention: text that instructs against a phrase,
  # or quotes it in order to replace it, necessarily contains it. The semantics
  # layer exempts such lines and so must this guard, or the corrected core
  # story's own explanation of the correction would fail it.
  PROHIBITION <- paste0("never|must not|do not |does not|cannot|prohibited|",
                        "instead of|rather than|avoid|banned|forbidden|",
                        "reworded|not adopted|say spatially resolved|",
                        "\\bNOT\\b|\\bNEVER\\b")
  hits <- unlist(lapply(files, function(f) {
    ln <- readLines(f, warn = FALSE)
    h <- grep("spatially[ -](restricted|specific)", ln, ignore.case = TRUE,
              value = TRUE)
    h <- h[!grepl(LICENCE, h, ignore.case = TRUE)]
    h[!grepl(PROHIBITION, h, perl = TRUE)]
  }))
  expect_equal(hits, character(0))
})

test_that("the spatial wording contract is declared in the rulebook", {
  sem <- readLines(repo("figures", "final_truth_v9_semantics.R"), warn = FALSE)
  expect_true(any(grepl('RULE("spatial wording"', sem, fixed = TRUE)))
  # the S9 scan must carry the guard, anchored to the adverb so that a
  # restricted INTERPRETATION or a specificity INVENTORY is not flagged
  expect_true(any(grepl("spatially[ -](restricted|specific)", sem,
                        fixed = TRUE)))
})

# ------------------------------------------- PH-010 verification provenance
