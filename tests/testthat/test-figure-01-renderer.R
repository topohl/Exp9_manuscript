# Guards for the manuscript Figure 1 renderer.
#
# Figure 1's science is computed and frozen in MMMSociability (Stage 29
# characterisation, Stage 09 registered prediction) and bundled by its Stage 16b.
# The risk here is a wrong quote or a panel that implies more than the frozen
# analysis supports. These guards pin the separation: the renderer reads only the
# pinned bundle, every printed number is declared in the annotation map and
# resolves to exactly one bundle cell, and the rendered SVGs carry those values.

repo <- function(...) file.path(testthat::test_path("..", ".."), ...)
source(repo("R", "paths.R"))
source(repo("R", "behaviour_bundle.R"))
PANELS <- repo("results", "figures", "manuscript", "figure_01_panels")
FIG <- repo("results", "figures", "manuscript", "figure_01")
RENDERER <- repo("figures", "figure_01_panels.R")
# The renderer is a thin caller; its house style, annotation resolver and panel
# builders live in the behaviour panel libraries R/panels/behaviour_*.R.
BEHAVIOUR_LIBS <- sort(list.files(repo("R", "panels"), pattern = "^behaviour_.*[.][Rr]$", full.names = TRUE))
ENTRY <- repo("figures", "figure_01.R")
AMAP_PATH <- repo("figures", "figure_01_annotation_map.csv")
LEGEND <- repo("manuscript", "legends", "figure1_legend.md")
rd <- function(p) utils::read.csv(p, stringsAsFactors = FALSE)
have_pin <- file.exists(repo("config", "behaviour_bundle.yml"))
PANEL_IDS <- c("a", "b", "c", "d", "e", "f")
code_of <- function(p) { src <- readLines(p, warn = FALSE); src[!grepl("^[[:space:]]*#", src)] }

# The renderer's own resolution of one annotation key, re-implemented here only
# as string handling so the test does not source the renderer.
resolve <- function(r) {
  kv <- strsplit(strsplit(r$filters, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)
  f <- stats::setNames(lapply(kv, function(x) if (identical(x[[2]], "NA")) NA else x[[2]]), vapply(kv, `[[`, "", 1L))
  v <- do.call(bundle_cell, c(list(behaviour_bundle_table(r$table), r$column), f))
  if (grepl("%d", r$format, fixed = TRUE)) v <- as.integer(round(v))
  sub("^-", "\u2212", sprintf(r$format, v))
}

test_that("the renderer reads only the pinned bundle and computes nothing", {
  # The renderer and every behaviour panel library it (or a behaviour candidate)
  # sources: R/panels/behaviour_*.R, whatever files that glob holds.
  expect_true(all(c("behaviour_figure_style.R", "behaviour_figure1_panels.R") %in% basename(BEHAVIOUR_LIBS)))
  FORBIDDEN <- c("analysis_ready", "data/raw", "data/processed", "figure1_bridge_mmmsociability",
                 "canonical/later_outcome_combz", "S:/", "read_excel")
  STATS <- c("\\blm\\(", "\\bglm\\(", "\\blmer\\(", "cor\\.test\\(", "\\bcor\\(", "p\\.adjust\\(", "\\bboot\\(", "replicate\\(",
             "geom_smooth\\(", "stat_smooth\\(", "stat_summary\\(", "\\bt\\.test\\(", "wilcox\\.test\\(", "\\bmean\\(", "\\bquantile\\(",
             "\\bmedian\\(", "\\bsd\\(", "\\bvar\\(", "\\bpredict\\(", "\\bfitted\\(", "\\bcoef\\(", "\\bresiduals\\(",
             "\\bdensity\\(", "\\becdf\\(")
  for (src in c(RENDERER, BEHAVIOUR_LIBS)) {
    code <- code_of(src)
    for (f in FORBIDDEN)
      expect_equal(grep(f, code, value = TRUE, fixed = TRUE), character(0),
                   info = paste(basename(src), "reaches outside the bundle:", f))
    for (f in STATS)
      expect_equal(grep(f, code, value = TRUE), character(0), info = paste(basename(src), "computes a statistic:", f))
  }
  code <- code_of(RENDERER)
  expect_true(any(grepl("behaviour_bundle_verify(PIN)", code, fixed = TRUE)))
  expect_true(any(grepl("bundle_cell", code, fixed = TRUE)))
})

test_that("the entry point is a thin contract-driven assembler", {
  code <- code_of(ENTRY); code <- code[nzchar(trimws(code))]
  expect_lte(length(code), 5L)
  expect_true(any(grepl('manuscript_figure_main("01")', code, fixed = TRUE)))
})

test_that("no behavioural analysis code exists in this repository", {
  upstream <- c("09_early_prediction_model_ladder", "29_canonical_behavior_characterization", "16b_canonical_behavior_bundle",
                "rfid_canonical_inference", "rfid_binfree_metrics", "rfid_event_stream", "build_later_outcome_combz",
                "build_figure1_panel_source_data")
  here <- list.files(repo("."), pattern = "[.][Rr]$", recursive = TRUE)
  here <- here[!grepl("^(\\.git|renv)/", here)]
  for (u in upstream) expect_false(any(grepl(u, basename(here), fixed = TRUE)), info = u)
  skip_if_not(have_pin, "no behaviour bundle pinned")
  expect_equal(list.files(behaviour_bundle_dir(), pattern = "[.][Rr]$"), character(0))
})

test_that("the pinned bundle is FROZEN and verifies byte-for-byte", {
  skip_if_not(have_pin, "no behaviour bundle pinned")
  pin <- behaviour_bundle_pin()
  expect_true(behaviour_bundle_verify(pin))
  prov <- behaviour_bundle_table("H_provenance")
  expect_equal(prov$value[prov$key == "status"], "FROZEN")
  expect_equal(prov$value[prov$key == "bundle_id"], pin$bundle_id)
  expect_equal(prov$value[prov$key == "config_sha256"], pin$config_sha256)
  expect_equal(readLines(file.path(behaviour_bundle_dir(pin), "I_config_sha256.txt"))[1], pin$config_sha256)
})

test_that("every annotation key resolves to exactly one bundle cell", {
  skip_if_not(have_pin, "no behaviour bundle pinned")
  amap <- rd(AMAP_PATH)
  expect_equal(anyDuplicated(amap$key), 0L)
  expect_true(all(sub("^1", "", amap$panel) %in% PANEL_IDS))
  # (that every key is also printed is proven by the value-for-value test below)
  for (i in seq_len(nrow(amap))) {
    v <- resolve(amap[i, , drop = FALSE])
    expect_true(nzchar(v) && !grepl("NA", v, fixed = TRUE), info = amap$key[i])
  }
})

test_that("the rendered panels print exactly the bundle values (value-for-value)", {
  skip_if_not(have_pin, "no behaviour bundle pinned")
  skip_if_not(dir.exists(PANELS), "figure 1 panels not rendered")
  amap <- rd(AMAP_PATH)
  for (i in seq_len(nrow(amap))) {
    id <- sub("^1", "", amap$panel[i])
    svg <- paste(readLines(file.path(PANELS, paste0("figure_01", id, ".svg")), warn = FALSE, encoding = "UTF-8"), collapse = "")
    v <- enc2utf8(resolve(amap[i, , drop = FALSE]))
    expect_true(grepl(v, svg, fixed = TRUE), info = sprintf("%s (%s) does not print %s", amap$key[i], amap$panel[i], v))
  }
})

test_that("per-sex means come only from stratified fits", {
  skip_if_not(have_pin, "no behaviour bundle pinned")
  c2 <- behaviour_bundle_table("C2_estimates")
  m <- c2[c2$source == "stage29" & grepl("^mean_(RES|SUS)_", c2$estimand) & c2$construct %in% c("crossing_rate", "shared_zone_use"), ]
  expect_equal(nrow(m), 2L * (4L + 16L))
  expect_true(all(grepl("^(CC1_BY_SEX|TR_BY_SEX)\\|", m$model_id)))
  expect_true(all(m$sex %in% c("Female", "Male")))
})

test_that("the rendered panels and assembly are vector, not rasterised", {
  skip_if_not(dir.exists(PANELS), "figure 1 panels not rendered")
  for (id in PANEL_IDS) {
    p <- file.path(PANELS, paste0("figure_01", id, ".svg"))
    expect_true(file.exists(p))
    txt <- readLines(p, warn = FALSE)
    expect_true(any(grepl("</svg>", txt, fixed = TRUE)))
    expect_true(any(grepl("<text", txt, fixed = TRUE)))
    expect_false(any(grepl("<image", txt, fixed = TRUE)))
  }
  skip_if_not(dir.exists(file.path(FIG, "assembled")), "figure 1 not assembled")
  txt <- paste(readLines(file.path(FIG, "assembled", "figure_01.svg"), warn = FALSE), collapse = "")
  expect_false(grepl("data:image/png", txt, fixed = TRUE))
  expect_false(grepl("data:image/jpeg", txt, fixed = TRUE))
  expect_true(grepl("data:image/svg+xml", txt, fixed = TRUE))
})

test_that("the figure contract registers the six bundle-backed panels", {
  y <- yaml::read_yaml(repo("figures", "figure_contract.yml"))
  e <- y$figures[["01"]]
  expect_true(isTRUE(e$is_numbered_manuscript_figure))
  expect_false(isTRUE(e$rendering_repository_computes_statistics))
  expect_equal(e$layout_mode, "absolute")
  ids <- vapply(e$panels, function(p) as.character(p$id), "")
  expect_setequal(ids, paste0("1", PANEL_IDS))
  skip_if_not(have_pin, "no behaviour bundle pinned")
  pin <- behaviour_bundle_pin()
  expect_equal(e$behaviour_bundle_id, pin$bundle_id)
  for (p in e$panels) {
    expect_equal(as.character(p$producer_script), "figures/figure_01_panels.R")
    expect_true(startsWith(as.character(p$primary_source), paste0("source_data/MMMSociability/", pin$bundle_id, "/")))
    expect_true(file.exists(repo(as.character(p$primary_source))))
  }
})

test_that("Figure 1 uses the same group palette as Figures 2 and 3", {
  source(repo("R", "vendor", "plotting_nature.R"))
  expect_equal(unname(NATURE_SEMANTIC_PALETTES$group[c("CON", "RES", "SUS")]), c("#3E3C6F", "#C6C3BB", "#E63A48"))
  # GROUP_COL is defined in R/panels/behaviour_figure_style.R, which the renderer sources.
  code <- unlist(lapply(c(RENDERER, BEHAVIOUR_LIBS), code_of))
  expect_true(any(grepl("NATURE_SEMANTIC_PALETTES$group", code, fixed = TRUE)))
  skip_if_not(dir.exists(PANELS), "figure 1 panels not rendered")
  ink <- paste(readLines(file.path(PANELS, "figure_01f.svg"), warn = FALSE), collapse = "")
  for (h in c("#3E3C6F", "#C6C3BB", "#E63A48")) expect_true(grepl(h, ink, fixed = TRUE), info = paste("missing group ink", h))
})

test_that("the association panel is not coloured by outcome group", {
  skip_if_not(dir.exists(PANELS), "figure 1 panels not rendered")
  ink <- paste(readLines(file.path(PANELS, "figure_01e.svg"), warn = FALSE), collapse = "")
  for (h in c("#3E3C6F", "#C6C3BB", "#E63A48")) expect_false(grepl(h, ink, fixed = TRUE), info = paste("panel e carries group ink", h))
})

test_that("the legend avoids prohibited wording", {
  skip_if_not(file.exists(LEGEND), "legend absent")
  ln <- readLines(LEGEND, warn = FALSE)
  DENIAL <- "never|cannot|prohibited|instead of|rather than|avoid|\\b[Nn][Oo][Tt]\\b|\\b[Nn]o "
  BANNED <- c("predicts susceptibility", "predicts resilience", "female-specific", "sex-specific", "external validation",
              "independent validation", "independent replication", "biomarker", "sociability", "social coordination", "huddling",
              "confirmatory", "preregistered", "induced the", "acute", "immediate response")
  for (b in BANNED) {
    hits <- grep(paste0("\\b", b, "\\b"), ln, ignore.case = TRUE, perl = TRUE, value = TRUE)   # whole words: "MMMSociability" is not "sociability"
    hits <- hits[!grepl(DENIAL, hits, perl = TRUE)]
    expect_equal(hits, character(0), info = paste("legend uses:", b))
  }
})
