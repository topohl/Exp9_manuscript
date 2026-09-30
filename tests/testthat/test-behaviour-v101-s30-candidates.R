# Guards for the behaviour candidate generation behaviour_v101_s30
# (figures/behaviour_v101_s30_candidates.R, figures/figure_behaviour_v101_s30_contract.yml,
# figures/behaviour_v101_s30_annotation_map.csv; CANDIDATE_SPEC D).
#
# The candidates plot stored values of two pinned bundles: the Stage 29 v1.0.1 behaviour bundle
# (ebb) and the Stage 30 figure bundle (s30b). The risks are a statistic computed here, a number
# on a panel that is not a stored cell, wording the manuscript bans, an output that differs from
# its receipt, and a candidate that looks promoted. These guards pin each of them.

repo <- function(...) file.path(testthat::test_path("..", ".."), ...)
source(repo("R", "paths.R"))
source(repo("R", "behaviour_bundle.R"))
source(repo("R", "stage30_bundle.R"))

GEN <- "behaviour_v101_s30"
CONTRACT_PATH <- repo("figures", "figure_behaviour_v101_s30_contract.yml")
ENTRY <- repo("figures", "behaviour_v101_s30_candidates.R")
MAP_PATH <- repo("figures", "behaviour_v101_s30_annotation_map.csv")
CANON_MAP_PATH <- repo("figures", "figure_01_annotation_map.csv")
BEHAVIOUR_LIBS <- sort(list.files(repo("R", "panels"), pattern = "^behaviour_.*[.][Rr]$", full.names = TRUE))
CT <- yaml::read_yaml(CONTRACT_PATH)
KEYS <- names(CT$candidates)
FIGROOT <- repo("results", "figures", "manuscript_candidates", GEN)
RECEIPT <- repo("results", "reports", "manuscript_candidates", GEN, "RECEIPT.csv")
TRACKED_RECEIPT <- repo("provenance", "candidates", "behaviour_v101_s30_receipt.csv")
# The candidate legends and Results text (DESIGN 6), committed beside the manuscript, not in it.
TEXTS <- repo("manuscript", "candidates", GEN, c("legends.md", "results_text.md"))
# Wording banned in any displayed text (DESIGN 0, CANDIDATE_SPEC A).
BANNED <- c("antenna", "crossing", "sleep", "cookie approach", "investigation", "consumption", "time near cookie",
            "habituation", "confirmatory", "preregistered", "pre-registered", "female-specific", "sex-specific",
            "significant")
have_pins <- file.exists(repo("config", "behaviour_bundle.yml")) && file.exists(repo("config", "stage30_bundle.yml"))
rendered <- file.exists(RECEIPT)
code_of <- function(p) { src <- readLines(p, warn = FALSE, encoding = "UTF-8"); src[!grepl("^[[:space:]]*#", src)] }
rd <- function(p) utils::read.csv(p, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0), encoding = "UTF-8")
sha <- function(p) unname(tools::sha256sum(p))

# ---------------------------------------------------------------- independent resolution
# The builders' resolution of one annotation key (bh_annotation), re-implemented here only as
# string handling on bundle_cell, so the test does not source the renderer or the libraries.
TABLE_CACHE <- new.env(parent = emptyenv())
bundle_table <- function(bundle, name) {
  k <- paste(bundle, name)
  if (!exists(k, envir = TABLE_CACHE, inherits = FALSE))
    assign(k, switch(bundle, ebb = behaviour_bundle_table(name), s30b = stage30_bundle_table(name),
                     stop("unknown bundle ", bundle)), envir = TABLE_CACHE)
  get(k, envir = TABLE_CACHE, inherits = FALSE)
}
filters_of <- function(s) {
  if (!nzchar(s)) return(list())
  kv <- strsplit(strsplit(s, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)
  stats::setNames(lapply(kv, function(x) if (identical(x[[2]], "NA")) NA else x[[2]]), vapply(kv, `[[`, "", 1L))
}
resolve <- function(r) {
  v <- do.call(bundle_cell, c(list(bundle_table(r$bundle, r$table), r$column), filters_of(r$filters)))
  if (grepl("%d", r$format, fixed = TRUE)) v <- as.integer(round(v))
  out <- sub("^-", "\u2212", sprintf(r$format, v))
  # the candidate typography writes an e-notation exponent with U+2212 and no leading zeros
  if (grepl("e$", r$format)) out <- sub("e-0*([0-9])", "e\u2212\\1", out)
  out
}
all_panels <- function() unlist(lapply(KEYS, function(k) lapply(CT$candidates[[k]]$panels, function(s) c(s, list(candidate = k)))), recursive = FALSE)

# ---------------------------------------------------------------- SVG text
svg_text_nodes <- function(path) {
  doc <- xml2::xml_ns_strip(xml2::read_xml(path))
  nodes <- xml2::xml_find_all(doc, "//text")
  style <- xml2::xml_attr(nodes, "style")
  if (anyNA(style) || !all(grepl("font-size", style, fixed = TRUE))) stop(basename(path), ": a text node without a font size")
  data.frame(text = enc2utf8(xml2::xml_text(nodes)),
             pt = as.numeric(sub(".*font-size: *([0-9.]+)px.*", "\\1", style)),
             family = sub('.*font-family: *"?([^";]+)"?;.*', "\\1", style), stringsAsFactors = FALSE)
}
# Label words and registered constants that carry digits but are not printed values. Each is
# removed from a text node before its numbers are compared with the resolved annotation values.
LABEL_WORDS <- c(
  "axis starts at 0\\.975",    # the one permitted statement (CANDIDATE_SPEC A)
  "\u2265 ?0\\.99",            # the threshold the stored S1c column n_ge_0.99 names (also an axis break)
  "Stage (29|30)",             # tier labels
  "\u2265 ?40 s",              # the registered inactivity bout length
  "EPM\\+1",                   # the registered test day
  "\\b1[678]:00\\b",           # the registered cookie windows (the builder checks them against S5)
  "\\(18:30\u201306:30\\)",    # the registered active phase (Figure 1a caption, en dash)
  "95% CI",                    # interval label
  "\\b5-fold\\b",              # the registered cross-validation design (Figure 1f text)
  "F = 1\\b")                  # the L reference line of the screen (always an axis break)
NUMBER <- "(?<![A-Za-z0-9.])\u2212?[0-9]+(?:\\.[0-9]+)?(?:e[-+\u2212]?[0-9]+)?"
PURE_NUMBER <- "^\u2212?[0-9]+(\\.[0-9]+)?$"
SUPERSCRIPT <- c("\u2212", "1", "2", "\u22121")   # plotmath h^-1 and R^2, drawn below 6 pt
strip_labels <- function(x) { for (p in LABEL_WORDS) x <- gsub(p, " ", x, perl = TRUE); x }
numbers_in <- function(x) regmatches(x, gregexpr(NUMBER, x, perl = TRUE))[[1]]
panel_svg <- function(s) file.path(FIGROOT, s$candidate, "panels", sprintf("%s_%s.svg", s$candidate, s$id))

# The Nature-layout candidate (NATURE_REDESIGN_SPEC): one vector page, and a text floor of exactly
# 5.0 pt with no superscript exception (the other candidates keep 6 pt with the plotmath exception).
NATURE <- "figure_01_option2_nature"
vector_page <- function(k) identical(CT$candidates[[k]]$assembly, "vector_page")
TEXT_FLOOR_PT <- c(figure_01_option2_nature = 5.0)
# Fill and stroke colours an SVG draws (svglite style attributes), upper-case hex.
svg_colours <- function(path) {
  txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  hits <- regmatches(txt, gregexpr("(fill|stroke): *#[0-9A-Fa-f]{6}", txt))[[1]]
  unique(toupper(sub(".*#", "#", hits)))
}
# The dictionaries of a PDF as text: through qpdf (object streams expanded) when it is installed,
# else the raw bytes (a cairo PDF writes its font dictionaries uncompressed; image XObjects are
# streams, which PDF never puts in object streams, so their dictionaries are always visible).
pdf_text <- function(path) {
  qpdf <- Sys.which("qpdf")
  if (nzchar(qpdf)) {
    tmp <- tempfile(fileext = ".pdf")
    on.exit(unlink(tmp), add = TRUE)
    status <- system2(qpdf, c("--qdf", "--object-streams=disable", shQuote(path), shQuote(tmp)), stdout = FALSE, stderr = FALSE)
    if (identical(as.integer(status), 0L) && file.exists(tmp)) path <- tmp
  }
  b <- readBin(path, "raw", file.size(path))
  b[b == as.raw(0)] <- as.raw(32)
  out <- rawToChar(b)
  Encoding(out) <- "bytes"   # binary streams: every match below is bytewise (useBytes)
  out
}
pdf_hits <- function(pdf, pattern, perl = FALSE)
  regmatches(pdf, gregexpr(pattern, pdf, perl = perl, useBytes = TRUE))[[1]]

# ================================================================ tests
test_that("the entry script and the behaviour libraries read only the pinned bundles and compute nothing", {
  expect_true(file.exists(ENTRY))
  expect_true(all(c("behaviour_figure_style.R", "behaviour_figure1_panels.R", "behaviour_s30_light_panels.R",
                    "behaviour_s30_cookie_panels.R", "behaviour_s30_screen_panels.R", "behaviour_s30_edx_panels.R")
                  %in% basename(BEHAVIOUR_LIBS)))
  # the same list as test-figure-01-renderer.R (helper-behaviour-forbidden-statistics.R)
  expect_gte(length(BEHAVIOUR_FORBIDDEN_STATS), 29L)
  FORBIDDEN <- c("analysis_ready", "data/raw", "data/processed", "figure1_bridge_mmmsociability",
                 "canonical/later_outcome_combz", "S:/", "read_excel", "proteomics")
  for (src in c(ENTRY, BEHAVIOUR_LIBS)) {
    code <- code_of(src)
    for (f in FORBIDDEN)
      expect_equal(code[grepl(tolower(f), tolower(code), fixed = TRUE)], character(0),
                   info = paste(basename(src), "reaches outside the bundles:", f))
    for (f in BEHAVIOUR_FORBIDDEN_STATS)
      expect_equal(grep(f, code, value = TRUE), character(0), info = paste(basename(src), "computes a statistic:", f))
  }
  code <- code_of(ENTRY)
  for (tok in c("behaviour_bundle_verify(PIN)", "stage30_bundle_verify(S30)", "bundle_cell", "bh_annotation(",
                "manuscript_figure_assemble_svg(", "manuscript_figure_raster_companions("))
    expect_true(any(grepl(tok, code, fixed = TRUE)), info = tok)
  # the entry script draws and labels nothing itself: every mark and every printed string is a
  # library builder's, and every number in them comes from the annotation map
  CALL <- "(^|[^A-Za-z0-9_.])"
  for (tok in c("geom_[a-z0-9_]+", "annotate", "annotation_custom", "labs", "ggtitle", "xlab", "ylab", "scale_[a-z_]+",
                "theme", "sec_axis", "guides", "set\\.seed"))
    expect_false(any(grepl(paste0(CALL, tok, "\\s*\\("), code, perl = TRUE)), info = paste("entry script uses", tok))
})

test_that("the candidate contract is candidate-only and declares every panel completely", {
  expect_equal(CT$generation, GEN)
  expect_equal(CT$status, "candidate_only_not_promoted")
  expect_false(isTRUE(CT$rendering_repository_computes_statistics))
  expect_equal(CT$scientific_recomputation, "none")
  expect_equal(CT$entry_point, "figures/behaviour_v101_s30_candidates.R")
  expect_equal(CT$annotation_map, "figures/behaviour_v101_s30_annotation_map.csv")
  expect_setequal(KEYS, c("figure_01_option1", "figure_01_option2", "figure_01_option2_nature", "light_phase_panel", "light_dependence_ed",
                          "cookie_ed", "screen_summary", "ed_behaviour_longitudinal_light", "ed_behaviour_cookie"))
  lib_code <- unlist(lapply(BEHAVIOUR_LIBS, readLines, warn = FALSE))
  JITTERED <- c("f1_panel_combz", "f1_panel_cc1", "s30_panel_light_measure", "s30_panel_light_phase", "s30_panel_cookie_rs")
  for (k in KEYS) {
    cand <- CT$candidates[[k]]
    expect_false(isTRUE(cand$is_numbered_manuscript_figure), info = k)
    expect_true(identical(cand$is_numbered_manuscript_figure, FALSE), info = paste(k, "must say is_numbered_manuscript_figure: false"))
    expect_equal(cand$promotion_status, "not_promoted", info = k)
    expect_equal(cand$layout_mode, "absolute", info = k)
    ids <- vapply(cand$panels, function(s) s$id, "")
    expect_equal(anyDuplicated(ids), 0L, info = k)
    letters_used <- Filter(nzchar, vapply(cand$panels, function(s) s$letter, ""))
    expect_true(all(letters_used %in% letters[1:8]), info = k)
    expect_equal(anyDuplicated(letters_used), 0L, info = k)
    boxes <- lapply(cand$panels, function(s) as.numeric(c(s$x, s$y, s$w, s$h)))
    margin <- if (is.null(cand$page_margin_mm)) CT$page_margin_mm else cand$page_margin_mm
    for (i in seq_along(cand$panels)) {
      s <- cand$panels[[i]]; b <- boxes[[i]]
      expect_true(nzchar(s$description), info = paste(k, s$id))
      expect_true(any(startsWith(lib_code, paste0(s$builder, " <- function("))), info = paste(k, s$id, "builder", s$builder))
      expect_true(length(s$inputs) > 0 && all(grepl("^(ebb|s30b)/[A-Za-z0-9_]+$", unlist(s$inputs))), info = paste(k, s$id))
      # a panel prints map values, or (the Nature-layout design panel) has moved all of them to its legend
      expect_true(length(s$annotation_panels) > 0 || length(s$annotation_panels_legend) > 0, info = paste(k, s$id))
      if (s$builder %in% JITTERED)
        expect_true(!is.null(s$jitter_seed) && !anyNA(unlist(s$jitter_seed)), info = paste(k, s$id, "needs an explicit jitter seed"))
      # inside the page with the page margin (5 mm; the candidate's own where it declares one)
      expect_true(b[1] >= margin && b[2] >= margin &&
                    b[1] + b[3] <= cand$width_mm - margin + 1e-9 &&
                    b[2] + b[4] <= cand$height_mm - margin + 1e-9, info = paste(k, s$id, "box leaves the page margin"))
      for (j in seq_along(boxes)) if (j > i) {
        o <- boxes[[j]]
        overlap <- b[1] < o[1] + o[3] && o[1] < b[1] + b[3] && b[2] < o[2] + o[4] && o[2] < b[2] + b[4]
        expect_false(overlap, info = paste(k, s$id, "overlaps", cand$panels[[j]]$id))
      }
    }
  }
  # the candidate keys are not figures of the canonical contract
  canon <- yaml::read_yaml(repo("figures", "figure_contract.yml"))
  expect_equal(intersect(KEYS, names(canon$figures)), character(0))
  skip_if_not(have_pins, "bundles not pinned")
  expect_equal(CT$bundles$ebb$bundle_id, behaviour_bundle_pin()$bundle_id)
  expect_equal(CT$bundles$ebb$manifest_sha256, behaviour_bundle_pin()$manifest_sha256)
  expect_equal(CT$bundles$s30b$bundle_id, stage30_bundle_pin()$bundle_id)
  expect_equal(CT$bundles$s30b$manifest_sha256, stage30_bundle_pin()$manifest_sha256)
})

test_that("no candidate key or the generation appears in the publication registry or the canonical contract", {
  reg <- list.files(repo("provenance", "publication_registry"), full.names = TRUE)
  expect_gt(length(reg), 0L)
  for (f in c(reg, repo("figures", "figure_contract.yml"))) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    for (k in c(KEYS, GEN)) expect_false(grepl(k, txt, fixed = TRUE), info = paste(basename(f), "names", k))
  }
})

test_that("every annotation key resolves to exactly one stored cell", {
  m <- rd(MAP_PATH)
  expect_identical(names(m), c("key", "panel", "bundle", "table", "column", "filters", "format", "meaning"))
  expect_equal(anyDuplicated(m$key), 0L)
  expect_true(all(m$bundle %in% c("ebb", "s30b")))
  # every map panel is printed, plotted or quoted in a legend by some contract panel, and every
  # declared map panel exists
  declared <- unique(unlist(lapply(all_panels(), function(s)
    c(s$annotation_panels, s$annotation_panels_plotted, s$annotation_panels_legend))))
  expect_setequal(unique(m$panel), declared)
  # a panel's legend map panels are never also printed by that panel (their keys are not on its
  # SVG); another panel may print them (option 2 d's legend quotes the light panel's CIs)
  for (s in all_panels())
    expect_length(intersect(unlist(s$annotation_panels_legend), unlist(s$annotation_panels)), 0L)
  # a map panel that no contract panel prints or plots is legend-only: its keys are quoted, not drawn
  drawn <- unique(unlist(lapply(all_panels(), function(s) c(s$annotation_panels, s$annotation_panels_plotted))))
  legend_only <- setdiff(unique(unlist(lapply(all_panels(), function(s) s$annotation_panels_legend))), drawn)
  expect_true(all(c("light_legend", "cookie_text") %in% legend_only))
  # the option-panel keys are copies of the canonical Figure 1 keys (same cell, same format)
  canon <- rd(CANON_MAP_PATH)
  opt <- m[grepl("^f1o([12]|2n)_", m$key), , drop = FALSE]
  expect_equal(nrow(opt), 3L * nrow(canon[!grepl("^d_", canon$key), ]))
  i <- match(sub("^f1o([12]|2n)_", "", opt$key), canon$key)
  expect_false(anyNA(i))
  for (col in c("table", "column", "filters", "format")) expect_identical(opt[[col]], canon[[col]][i], info = col)
  skip_if_not(have_pins, "bundles not pinned")
  for (r in seq_len(nrow(m))) {
    v <- tryCatch(resolve(m[r, , drop = FALSE]), error = function(e) paste("ERROR", conditionMessage(e)))
    expect_true(nzchar(v) && !grepl("NA|ERROR", v), info = paste(m$key[r], v))
  }
})

test_that("every number printed on a panel is a resolved annotation value, and every declared value is printed", {
  skip_if_not(have_pins, "bundles not pinned")
  skip_if_not(rendered, "candidates not rendered")
  skip_if_not_installed("xml2")
  m <- rd(MAP_PATH)
  value <- stats::setNames(vapply(seq_len(nrow(m)), function(r) resolve(m[r, , drop = FALSE]), ""), m$key)
  for (s in all_panels()) {
    where <- paste(s$candidate, s$id)
    path <- panel_svg(s)
    expect_true(file.exists(path), info = where)
    if (!file.exists(path)) next
    nodes <- svg_text_nodes(path)
    V <- unname(value[m$panel %in% unlist(s$annotation_panels)])
    pm <- rd(file.path(repo("results", "reports", "manuscript_candidates", GEN, s$candidate), "panel_manifest.csv"))
    ticks <- strsplit(pm$axis_tick_labels[pm$panel == s$id], "|", fixed = TRUE)[[1]]
    tokens <- character(0)
    for (i in seq_len(nrow(nodes))) {
      t <- trimws(nodes$text[i])
      if (!nzchar(t)) next
      if (grepl(PURE_NUMBER, t, perl = TRUE) || identical(t, "\u2212")) {
        ok <- t %in% c(V, ticks) || (nodes$pt[i] < 5.9 && t %in% SUPERSCRIPT)
        expect_true(ok, info = sprintf("%s: bare number '%s' (%.2f pt) is neither a resolved value nor an axis break", where, t, nodes$pt[i]))
        tokens <- c(tokens, t)
        next
      }
      nums <- numbers_in(strip_labels(t))
      bad <- setdiff(nums, V)
      expect_equal(bad, character(0), info = sprintf("%s: '%s' prints numbers that are not resolved values", where, t))
      tokens <- c(tokens, nums)
    }
    # forward: each value the panel declares as printed is on it
    missing <- setdiff(V, tokens)
    expect_equal(missing, character(0), info = paste(where, "does not print", paste(missing, collapse = ", ")))
    sd <- rd(file.path(repo("results", "source_data", "manuscript_candidates", GEN, s$candidate),
                       sprintf("%s_%s_source_data.csv", s$candidate, s$id)))
    # the source data record each printed key as drawn: its src_printed is the resolved value and
    # occurs verbatim in the panel's SVG text
    pr <- sd[sd$src_block == "annotation key" & sd$src_role == "printed", , drop = FALSE]
    expect_setequal(pr$src_key, m$key[m$panel %in% unlist(s$annotation_panels)])
    expect_equal(pr$src_printed, unname(value[pr$src_key]), info = where)
    svg_txt <- paste(nodes$text, collapse = "\n")
    verbatim <- vapply(pr$src_printed, function(v) grepl(v, svg_txt, fixed = TRUE), logical(1))
    expect_true(all(verbatim), info = paste(where, "src_printed not in the SVG text:", paste(pr$src_printed[!verbatim], collapse = ", ")))
    # the numbers its draft legend quotes are in its source data, as resolved, and not on the panel
    lp <- unlist(s$annotation_panels_legend)
    if (length(lp)) {
      want <- m$key[m$panel %in% lp]
      got <- sd[sd$src_block == "annotation key" & sd$src_role == "legend", , drop = FALSE]
      expect_setequal(got$src_key, want)
      expect_equal(got$src_printed[match(want, got$src_key)], unname(value[want]), info = where)
    }
  }
})

test_that("the SVG text uses the manuscript terminology and typography", {
  skip_if_not(rendered, "candidates not rendered")
  skip_if_not_installed("xml2")
  for (s in all_panels()) {
    path <- panel_svg(s)
    if (!file.exists(path)) next
    nodes <- svg_text_nodes(path)
    txt <- paste(nodes$text, collapse = " | ")
    for (b in BANNED) expect_false(grepl(b, txt, ignore.case = TRUE), info = paste(s$candidate, s$id, "uses", b))
    expect_false(grepl("\\bNS\\b", txt, perl = TRUE), info = paste(s$candidate, s$id, "prints NS"))
    expect_false(grepl("*", txt, fixed = TRUE), info = paste(s$candidate, s$id, "prints a star"))
    # a sign is the typographic minus U+2212, never a hyphen-minus
    expect_false(grepl("(^|[\\s\\[(,=])-[0-9]", txt, perl = TRUE), info = paste(s$candidate, s$id, "prints a hyphen-minus sign"))
    # Arial; nothing below 6 pt except plotmath superscripts (the Nature layout: nothing below 5.0 pt, no exception)
    expect_true(all(nodes$family == "Arial"), info = paste(s$candidate, s$id))
    if (s$candidate %in% names(TEXT_FLOOR_PT)) {
      low <- nodes[nodes$pt < TEXT_FLOOR_PT[[s$candidate]] - 1e-9 & nzchar(trimws(nodes$text)), , drop = FALSE]
      expect_equal(nrow(low), 0L, info = paste(s$candidate, s$id, "text below 5.0 pt:", paste(low$text, collapse = " | ")))
      next
    }
    small <- nodes[nodes$pt < 5.95 & nzchar(trimws(nodes$text)), , drop = FALSE]
    expect_true(all(trimws(small$text) %in% SUPERSCRIPT), info = paste(s$candidate, s$id, "text below 6 pt:", paste(small$text, collapse = " | ")))
  }
  # every candidate title in the contract is free of banned wording too
  for (k in KEYS) for (b in BANNED) expect_false(grepl(b, CT$candidates[[k]]$title, ignore.case = TRUE), info = paste(k, b))
})

# Numbers in the candidate texts that are registered settings or design facts, not bundle cells:
# alpha; the inactivity axis start and threshold (0.975, 0.99 / 99%); clock times (16:00-18:30,
# 06:30); the 12-h window; the 40/60-s bouts, the 45/60-min cookie windows and the 60 of "delta 60";
# 1,000 permutations and 5,000 bootstrap samples; the 95% CI; the recording lag 2.3-8.5 h
# carried over from the canonical Figure 1 legend.
TEXT_SETTINGS <- c("0.05", "0.975", "0.99", "99", "00", "06", "12", "16", "17", "18", "30", "40", "45", "60",
                   "1,000", "5,000", "95", "2.3", "8.5")
text_tokens <- function(line) {
  s <- gsub("`[^`]*`", " ", line)                                           # code spans: ids, formulas, paths
  s <- gsub("(ebb|s30b)_v[0-9_a-z]+|v1\\.0(\\.[01])?(_be71e2f)?|\\bbe71e2f\\b", " ", s, perl = TRUE)  # bundle / config ids
  s <- gsub(paste0("\\b(CC[1-4]|B[1-6]|P25|EPM\\+[12]|S-TR-ORG|P-CC1|P-TR|F\\(3, df\\)|Fig\\. [0-9]+[a-z]?|",
                   "Figure [0-9]+|Stage (09|29|30)|\u00a7[0-9A-Z]+)"), " ", s, perl = TRUE)    # labels with digits
  regmatches(s, gregexpr("(?<![A-Za-z0-9.])\u2212?[0-9]+(?:[.,][0-9]+)?", s, perl = TRUE))[[1]]
}

test_that("the committed candidate texts use the manuscript terminology and quote only resolved values", {
  expect_true(all(file.exists(TEXTS)), info = paste(TEXTS, collapse = ", "))
  for (f in TEXTS[file.exists(TEXTS)]) {
    txt <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    # the one permitted use of the sleep vocabulary is the denial "... rather than EEG-defined sleep"
    scan <- gsub("EEG-defined sleep", " ", txt, fixed = TRUE)
    for (b in BANNED) expect_false(grepl(b, scan, ignore.case = TRUE), info = paste(basename(f), "uses", b))
    expect_false(grepl("\\bNS\\b", txt, perl = TRUE), info = paste(basename(f), "prints NS"))
    # figure identities are placeholders (ED X, ED Y, S-screen), never a numbered ED or Supplementary figure
    expect_false(grepl("(Extended Data|Supplementary) (Fig\\.|Figure|Table) [0-9]", txt), info = basename(f))
  }
  skip_if_not(have_pins, "bundles not pinned")
  m <- rd(MAP_PATH)
  value <- vapply(seq_len(nrow(m)), function(r) resolve(m[r, , drop = FALSE]), "")
  for (f in TEXTS[file.exists(TEXTS)]) {
    x <- readLines(f, warn = FALSE, encoding = "UTF-8")
    for (i in seq_along(x)) {
      bad <- setdiff(text_tokens(x[i]), c(value, TEXT_SETTINGS))
      expect_equal(bad, character(0), info = sprintf("%s line %d quotes numbers that are neither resolved map values nor registered settings", basename(f), i))
    }
  }
  # the quoted Movement-adjusted female - male bound resolves at full resolution, not as 0.0000
  expect_false(any(grepl("\\b0\\.0000\\b", unlist(lapply(TEXTS[file.exists(TEXTS)], readLines, warn = FALSE, encoding = "UTF-8")))))
})

test_that("the outputs exist, are authored at their boxes and equal the receipt", {
  skip_if_not(have_pins, "bundles not pinned")
  skip_if_not(rendered, "candidates not rendered")
  rc <- rd(RECEIPT)
  expect_identical(names(rc), c("generation", "candidate", "kind", "path", "bytes", "sha256", "ebb_bundle_id", "s30b_bundle_id"))
  expect_true(all(rc$generation == GEN))
  expect_true(all(rc$ebb_bundle_id == behaviour_bundle_pin()$bundle_id))
  expect_true(all(rc$s30b_bundle_id == stage30_bundle_pin()$bundle_id))
  expect_equal(anyDuplicated(rc$path), 0L)
  for (i in seq_len(nrow(rc))) {
    p <- repo(rc$path[i])
    expect_true(file.exists(p), info = rc$path[i])
    if (!file.exists(p)) next
    expect_equal(as.character(file.size(p)), rc$bytes[i], info = rc$path[i])
    expect_equal(sha(p), rc$sha256[i], info = rc$path[i])
  }
  # the receipt lists every file the generation wrote, and nothing else
  roots <- file.path("results", c("figures", "source_data", "reports", "logs"), "manuscript_candidates", GEN)
  on_disk <- unlist(lapply(roots, function(r) file.path(r, list.files(repo(r), recursive = TRUE))))
  expect_setequal(on_disk, c(rc$path, file.path("results", "reports", "manuscript_candidates", GEN, "RECEIPT.csv")))
  # per candidate: every contract panel, the assembled SVG/PNG/PDF and the manifests
  for (k in KEYS) {
    r <- rc[rc$candidate == k, , drop = FALSE]
    n <- length(CT$candidates[[k]]$panels)
    expect_equal(sum(r$kind == "panel_svg"), n, info = k)
    expect_equal(sum(r$kind == "source_data"), n, info = k)
    for (kind in c("assembled_svg", "assembled_png", "assembled_pdf", "panel_manifest", "input_manifest", "run_manifest"))
      expect_equal(sum(r$kind == kind), 1L, info = paste(k, kind))
    run <- yaml::read_yaml(repo(r$path[r$kind == "run_manifest"]))
    expect_equal(run$scientific_recomputation, "none", info = k)
    expect_false(isTRUE(run$is_numbered_manuscript_figure), info = k)
    expect_equal(run$status, "candidate_only_not_promoted", info = k)
    expect_equal(run$bundles$ebb$bundle_id, behaviour_bundle_pin()$bundle_id, info = k)
    expect_equal(run$bundles$s30b$bundle_id, stage30_bundle_pin()$bundle_id, info = k)
    expect_equal(run$bundles$s30b$manifest_sha256, stage30_bundle_pin()$manifest_sha256, info = k)
    expect_match(run$render_git_commit, "^[0-9a-f]{40}$", info = k)
    outs <- do.call(rbind, lapply(run$outputs, as.data.frame, stringsAsFactors = FALSE))
    expect_setequal(outs$sha256, r$sha256[r$kind != "run_manifest"])
    # panels authored at their boxes; the assembled SVG embeds exactly those panel files and draws the letters
    # (a vector page embeds nothing: its own test below checks the page)
    asm <- paste(readLines(repo(r$path[r$kind == "assembled_svg"]), warn = FALSE), collapse = "\n")
    embedded <- regmatches(asm, gregexpr("data:image/svg\\+xml;base64,[A-Za-z0-9+/=]+", asm))[[1]]
    expect_equal(length(embedded), if (vector_page(k)) 0L else n, info = k)
    if (!vector_page(k)) {
      got <- vapply(embedded, function(u) digest::digest(base64enc::base64decode(sub("^.*base64,", "", u)), algo = "sha256", serialize = FALSE), "")
      expect_setequal(unname(got), r$sha256[r$kind == "panel_svg"])
      drawn <- regmatches(asm, gregexpr('font-weight="bold">[a-z]*</text>', asm))[[1]]
      expect_setequal(sub('font-weight="bold">([a-z]*)</text>', "\\1", drawn),
                      vapply(CT$candidates[[k]]$panels, function(s) s$letter, ""))
    }
    for (s in CT$candidates[[k]]$panels) {
      head <- paste(readLines(panel_svg(c(s, list(candidate = k))), n = 3, warn = FALSE), collapse = " ")
      w_pt <- as.numeric(sub(".*width='([0-9.]+)pt'.*", "\\1", head)); h_pt <- as.numeric(sub(".*height='([0-9.]+)pt'.*", "\\1", head))
      expect_equal(c(w_pt, h_pt), as.numeric(c(s$w, s$h)) / 25.4 * 72, tolerance = 0.01, info = paste(k, s$id))
    }
    # the panel manifest records the declared inputs of every panel
    pm <- rd(repo(r$path[r$kind == "panel_manifest"]))
    for (s in CT$candidates[[k]]$panels)
      expect_setequal(strsplit(pm$inputs[pm$panel == s$id], ";", fixed = TRUE)[[1]], unlist(s$inputs))
  }
})

test_that("the Nature-layout candidate is one vector page with live text, embedded Arial and the option 2 inks", {
  cand <- CT$candidates[[NATURE]]
  expect_true(vector_page(NATURE))
  expect_equal(c(cand$width_mm, cand$height_mm), c(183, 160))
  expect_lte(cand$height_mm, 170)
  expect_equal(c(cand$page_margin_mm, cand$panel_gap_mm), c(4, 3))
  expect_equal(unname(vapply(cand$panels, function(s) s$letter, "")), letters[1:6])
  for (s in cand$panels) expect_identical(s$args$style, "nature", info = s$id)
  skip_if_not(have_pins, "bundles not pinned")
  skip_if_not(rendered, "candidates not rendered")
  skip_if_not_installed("xml2")
  rc <- rd(RECEIPT)
  r <- rc[rc$candidate == NATURE, , drop = FALSE]
  page_svg <- repo(r$path[r$kind == "assembled_svg"]); page_pdf <- repo(r$path[r$kind == "assembled_pdf"])
  # the SVG: no embedded image, live text only, nothing below 5.0 pt, Arial
  svg_src <- paste(readLines(page_svg, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("<image", svg_src, fixed = TRUE))
  expect_false(grepl("base64", svg_src, fixed = TRUE))
  page <- svg_text_nodes(page_svg)
  expect_true(all(page$family == "Arial"))
  expect_equal(sum(page$pt < 5.0 - 1e-9 & nzchar(trimws(page$text))), 0L)
  # the letters: 8 pt bold, at each box's x, baseline 0.92 letter heights below the box top
  doc <- xml2::xml_ns_strip(xml2::read_xml(page_svg))
  tn <- xml2::xml_find_all(doc, "//text")
  bold <- grepl("font-weight: bold", xml2::xml_attr(tn, "style"), fixed = TRUE)
  expect_setequal(xml2::xml_text(tn[bold]), letters[1:6])
  for (s in cand$panels) {
    node <- tn[bold & xml2::xml_text(tn) == s$letter]
    expect_equal(as.numeric(xml2::xml_attr(node, "x")), s$x / 25.4 * 72, tolerance = 0.02, info = s$letter)
    expect_equal(as.numeric(xml2::xml_attr(node, "y")), (s$y + 8 * 25.4 / 72 * 0.92) / 25.4 * 72, tolerance = 0.02, info = s$letter)
    expect_true(grepl("font-size: 8.00px", xml2::xml_attr(node, "style"), fixed = TRUE), info = s$letter)
  }
  # the page draws exactly the panels' text (so the value-for-value test on the panels holds for the page)
  panel_txt <- unlist(lapply(cand$panels, function(s) svg_text_nodes(panel_svg(c(s, list(candidate = NATURE))))$text))
  page_txt <- page$text[!(page$pt == 8 & page$text %in% letters[1:6])]
  expect_identical(sort(trimws(page_txt)), sort(trimws(panel_txt)))
  # the PDF: one page, vector (no image XObject), every font an embedded Arial
  pdf <- pdf_text(page_pdf)
  expect_true(grepl("^%PDF-", pdf, useBytes = TRUE))
  expect_length(pdf_hits(pdf, "/Subtype\\s*/Image"), 0L)
  expect_length(pdf_hits(pdf, "/Type\\s*/Page(?![a-z])", perl = TRUE), 1L)
  fonts <- unique(pdf_hits(pdf, "/BaseFont\\s*/[A-Za-z0-9+-]+"))
  expect_gt(length(fonts), 0L)
  expect_true(all(grepl("/[A-Z]{6}\\+Arial", fonts)), info = paste(fonts, collapse = ", "))
  expect_true(any(grepl("Arial-BoldMT", fonts)))
  # every font descriptor carries its embedded font program
  n_desc <- length(pdf_hits(pdf, "/Type\\s*/FontDescriptor"))
  expect_gt(n_desc, 0L)
  expect_equal(length(pdf_hits(pdf, "/FontFile[23]?\\s+[0-9]+\\s+[0-9]+\\s+R")), n_desc)
  # no new ink: every fill and stroke colour of the Nature-layout panels is one option 2 already draws
  o2 <- CT$candidates$figure_01_option2$panels
  o2_ink <- unique(unlist(lapply(o2, function(s) svg_colours(panel_svg(c(s, list(candidate = "figure_01_option2")))))))
  for (s in cand$panels) {
    ink <- svg_colours(panel_svg(c(s, list(candidate = NATURE))))
    expect_equal(setdiff(ink, o2_ink), character(0), info = paste(NATURE, s$id, "draws a colour option 2 does not"))
  }
  expect_equal(setdiff(svg_colours(page_svg), o2_ink), character(0))
})

test_that("the tracked receipt copy records every candidate output of the current generation", {
  expect_true(file.exists(TRACKED_RECEIPT))
  tr <- rd(TRACKED_RECEIPT)
  expect_identical(names(tr), c("generation", "candidate", "kind", "path", "bytes", "sha256", "ebb_bundle_id", "s30b_bundle_id"))
  expect_setequal(unique(tr$candidate), KEYS)
  for (k in KEYS) expect_equal(sum(tr$candidate == k & tr$kind == "panel_svg"), length(CT$candidates[[k]]$panels), info = k)
  skip_if_not(have_pins, "bundles not pinned")
  expect_true(all(tr$ebb_bundle_id == behaviour_bundle_pin()$bundle_id))
  expect_true(all(tr$s30b_bundle_id == stage30_bundle_pin()$bundle_id))
  skip_if_not(rendered, "candidates not rendered")
  # The deterministic outputs (panel and assembled SVGs, source data, panel and input manifests)
  # must equal the tracked copy. The PNG, the PDF and the run manifests that record their hashes
  # carry magick timestamps and differ between renders.
  DET <- c("panel_svg", "assembled_svg", "source_data", "panel_manifest", "input_manifest")
  rc <- rd(RECEIPT)
  a <- rc[rc$kind %in% DET, c("candidate", "kind", "path", "bytes", "sha256")]
  b <- tr[tr$kind %in% DET, c("candidate", "kind", "path", "bytes", "sha256")]
  a <- a[order(a$path), ]; b <- b[order(b$path), ]; rownames(a) <- rownames(b) <- NULL
  expect_identical(b, a)
})
