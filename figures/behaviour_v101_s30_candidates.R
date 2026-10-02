#!/usr/bin/env Rscript
# ================================================================
# Script: figures/behaviour_v101_s30_candidates.R
# Stage: manuscript_candidates (generation behaviour_v101_s30)
# Scope: candidate figures only; nothing is promoted
# Consumes: the pinned Stage 29 v1.0.1 behaviour bundle (config/behaviour_bundle.yml,
#           read through R/behaviour_bundle.R), the pinned Stage 30 figure bundle
#           (config/stage30_bundle.yml, read through R/stage30_bundle.R) and the pinned
#           figure-support bundle (config/figure_support_bundle.yml, read through
#           R/figure_support_bundle.R: descriptive CON cage means, CombZ components, and
#           from fsb_v2 the verbatim rows of the MMM post hoc CON/RES/SUS run 29b)
# Contract: figures/figure_behaviour_v101_s30_contract.yml
# Map:      figures/behaviour_v101_s30_annotation_map.csv
# Produces: results/figures/manuscript_candidates/behaviour_v101_s30/<key>/{panels,assembled}/
#           results/source_data/manuscript_candidates/behaviour_v101_s30/<key>/
#           results/reports/manuscript_candidates/behaviour_v101_s30/<key>/{panel,input}_manifest.csv
#           results/logs/manuscript_candidates/behaviour_v101_s30/<key>/run_manifest.yml
#           results/reports/manuscript_candidates/behaviour_v101_s30/RECEIPT.csv
# Notes: Rendering only. Fits no model, computes no statistic, prints no number of its own.
# ================================================================
#
# What this script does, per candidate of the contract:
#   1. builds every panel with the builder the contract names (R/panels/behaviour_*.R), at
#      the panel's box, with the contract's explicit jitter seeds; every printed number is
#      resolved by the builder through the annotation map to exactly one stored cell;
#   2. writes the panel SVG at its exact box (bh_save_svg), so the assembler places it at 1:1;
#   3. checks that the builder read exactly the bundle tables the contract declares and
#      resolved only keys of the annotation panels the contract declares;
#   4. writes the panel's source data: the rows and mapped columns of every plotted layer,
#      as passed to the plot (unjittered), the annotation keys it resolved with their stored
#      and printed values and their role (printed on the panel, plotted only, or quoted by the
#      draft legend: contract annotation_panels_legend, resolved here and not drawn), and the
#      stored bundle rows behind those keys (row filtering only);
#   5. assembles the candidate (manuscript_figure_assemble_svg, absolute layout, 8-pt letters)
#      and rasterises it (manuscript_figure_raster_companions: PNG and raster PDF at 300 dpi,
#      from a copy of the assembled page with the panels inlined, see inline_for_raster()); a
#      candidate with `assembly: vector_page` is instead drawn as ONE vector page (bh_save_page:
#      the same panel plots at their boxes on one device, letters as the assembler draws them;
#      SVG with live text and a cairo PDF with Arial embedded) with a 300-dpi PNG preview of the
#      page SVG (bh_page_png);
#   6. writes the panel manifest, the input manifest and the run manifest.
# Finally it writes the generation receipt (every output with bytes and sha256).
#
# Options
#   --copy-receipt   also copy RECEIPT.csv to the tracked provenance/candidates/ copy.
#
# Determinism. Jitter is seeded explicitly per panel (contract jitter_seed), so no panel depends
# on the global RNG stream or on build order (the reused Figure 1 builders still call set.seed(),
# which the explicit seeds make irrelevant), and nothing here reads the clock: two renders at one
# commit give byte-identical SVGs, source data and panel/input manifests. The assembled PNG and PDF carry magick's
# (and cairo's) timestamps, so their hashes (and the run manifests and receipt that record them) differ
# between renders.

suppressPackageStartupMessages({
  library(ggplot2)
})

paths_file <- if (file.exists(file.path("R", "paths.R"))) file.path("R", "paths.R") else file.path("..", "R", "paths.R")
source(paths_file)
source(repo_path("R", "behaviour_bundle.R"))
source(repo_path("R", "stage30_bundle.R"))
source(repo_path("R", "figure_support_bundle.R"))
source(repo_path("R", "manuscript_figure_utils.R"))

GENERATION <- "behaviour_v101_s30"
CONTRACT_REL <- "figures/figure_behaviour_v101_s30_contract.yml"
ENTRY_REL <- "figures/behaviour_v101_s30_candidates.R"
CONTRACT <- yaml::read_yaml(repo_path(CONTRACT_REL))
if (!identical(CONTRACT$generation, GENERATION) || !identical(CONTRACT$status, "candidate_only_not_promoted"))
  stop("The candidate contract is not the ", GENERATION, " candidate-only contract.", call. = FALSE)
if (any(vapply(CONTRACT$candidates, function(k) !isFALSE(k$is_numbered_manuscript_figure), logical(1))))
  stop("Every candidate must declare is_numbered_manuscript_figure: false.", call. = FALSE)
args <- commandArgs(trailingOnly = TRUE)
COPY_RECEIPT <- "--copy-receipt" %in% args

# ---------------------------------------------------------------- pinned bundles
PIN <- behaviour_bundle_pin()
behaviour_bundle_verify(PIN)
S30 <- stage30_bundle_pin()
stage30_bundle_verify(S30)
FSB <- fsb_bundle_pin()
fsb_bundle_verify(FSB)
PINS <- list(ebb = PIN, s30b = S30, fsb = FSB)
for (b in names(PINS)) {
  if (!identical(CONTRACT$bundles[[b]]$bundle_id, PINS[[b]]$bundle_id) ||
      !identical(CONTRACT$bundles[[b]]$manifest_sha256, PINS[[b]]$manifest_sha256))
    stop("The contract's ", b, " bundle is not the pinned one (", PINS[[b]]$bundle_id, ").", call. = FALSE)
}
BUNDLE_DIRS <- list(ebb = behaviour_bundle_dir(PIN), s30b = stage30_bundle_dir(S30), fsb = fsb_bundle_dir(FSB))

# Table getters, cached, that log every table a panel reads (builders and annotation keys alike).
READS <- new.env(parent = emptyenv())
READS$tables <- character(0)
logged_reader <- function(label, reader) {
  cache <- new.env(parent = emptyenv())
  function(name) {
    READS$tables <- union(READS$tables, paste0(label, "/", name))
    if (!exists(name, envir = cache, inherits = FALSE)) assign(name, reader(name), envir = cache)
    get(name, envir = cache, inherits = FALSE)
  }
}
TABS <- list(ebb = logged_reader("ebb", function(n) behaviour_bundle_table(n, PIN)),
             s30b = logged_reader("s30b", function(n) stage30_bundle_table(n, S30)),
             fsb = logged_reader("fsb", function(n) fsb_bundle_table(n, FSB)))

for (lib in CONTRACT$libraries) source(repo_path(lib))
MAP_PATH <- repo_path(CONTRACT$annotation_map)
MAP <- utils::read.csv(MAP_PATH, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0))

# ---------------------------------------------------------------- builders
# One entry per contract `builder`: the library call with the panel's box, seeds and layout
# arguments. Nothing here carries a value; every number comes from the annotation map.
arg <- function(s, name, default = NULL) if (is.null(s$args[[name]])) default else s$args[[name]]
seeds <- function(s, n) {
  v <- s$jitter_seed
  if (is.null(v) || length(v) != n || anyNA(v)) stop("panel ", s$id, ": the contract must give ", n, " explicit jitter seed(s).", call. = FALSE)
  as.integer(v)
}
prefixed <- function(an, s) edx_annotation_prefixed(an, arg(s, "key_prefix"))
typo <- function(s) arg(s, "typography", "figure1")
top_pt <- function(s) as.numeric(arg(s, "top_margin_pt", 11.5))
num_or_null <- function(s, name) if (is.null(s$args[[name]])) NULL else as.numeric(s$args[[name]])
# style: the builders' style profile ("figure1", the default, or "nature"); frame: the row's shared
# plot frame of a nature-style panel (args plot_top_mm and axis_mm), or NULL.
sty <- function(s) arg(s, "style", "figure1")
frame_of <- function(s) {
  if (is.null(s$args$plot_top_mm) != is.null(s$args$axis_mm)) stop("panel ", s$id, ": give plot_top_mm and axis_mm together.", call. = FALSE)
  if (is.null(s$args$plot_top_mm)) NULL else list(top = as.numeric(s$args$plot_top_mm), axis = as.numeric(s$args$axis_mm))
}
BUILDERS <- list(
  f1_panel_design = function(s, an) f1_panel_design(TABS$ebb, prefixed(an, s), s$w, s$h, typography = typo(s), style = sty(s)),
  f1_panel_combz = function(s, an) f1_panel_combz(TABS$ebb, prefixed(an, s), s$w, s$h, jitter_seed = seeds(s, 1L),
    subtitle = if (isTRUE(arg(s, "subtitle_line_break"))) F1OPT_COMBZ_SUBTITLE_NARROW else F1_COMBZ_SUBTITLE,
    minus_ticks = isTRUE(arg(s, "minus_ticks")), title_position = arg(s, "title_position", "panel"),
    style = sty(s), frame = frame_of(s)),
  f1_panel_cc1 = function(s, an) f1_panel_cc1(TABS$ebb, prefixed(an, s), s$w, s$h, jitter_seed = seeds(s, 2L),
    text_position = arg(s, "text_position", "panel"), typography = typo(s), compact_header = isTRUE(arg(s, "compact_header")),
    style = sty(s), frame = frame_of(s)),
  f1_panel_association = function(s, an) f1_panel_association(TABS$ebb, prefixed(an, s), s$w, s$h,
    minus_ticks = isTRUE(arg(s, "minus_ticks")), typography = typo(s),
    title = if (isTRUE(arg(s, "candidate_title"))) F1OPT_ASSOCIATION_TITLE else NULL,
    title_position = arg(s, "title_position", "panel"), style = sty(s), frame = frame_of(s)),
  f1_panel_prediction = function(s, an) f1_panel_prediction(TABS$ebb, prefixed(an, s), s$w, s$h,
    minus_ticks = isTRUE(arg(s, "minus_ticks")), typography = typo(s), compact_header = isTRUE(arg(s, "compact_header")),
    scatter_width = as.numeric(arg(s, "scatter_width", 0.6)), bottom_pad_pt = as.numeric(arg(s, "bottom_pad_pt", 0)),
    title_position = arg(s, "title_position", "panel"), style = sty(s), frame = frame_of(s)),
  edx_panel_primary_trajectory = function(s, an) edx_panel_primary_trajectory(TABS$ebb, an, arg(s, "construct"), s$w, s$h,
    show_con = isTRUE(arg(s, "show_con", TRUE))),
  edx_panel_secondary_trajectories = function(s, an) edx_panel_secondary_trajectories(TABS$ebb, an, s$w, s$h,
    show_con = isTRUE(arg(s, "show_con", TRUE))),
  s30_panel_light_measure = function(s, an) s30_panel_light_measure(arg(s, "measure"), TABS, an, s$w, s$h,
    jitter_seed = seeds(s, 1L), standalone = isTRUE(arg(s, "standalone", TRUE))),
  s30_panel_light_phase = function(s, an) s30_panel_light_phase(TABS, an, s$w, s$h, jitter_seed = seeds(s, 1L),
    top_margin_pt = top_pt(s), panel_top_mm = num_or_null(s, "panel_top_mm"), panel_h_mm = num_or_null(s, "panel_h_mm")),
  s30_panel_light_compact = function(s, an) s30_panel_light_compact(an, s$w, s$h, q_rows = as.character(unlist(arg(s, "q_rows"))),
    top_margin_pt = top_pt(s), style = sty(s), frame = frame_of(s)),
  s30_panel_light_dependence = function(s, an) s30_panel_light_dependence(TABS, an, s$w, s$h, top_margin_pt = top_pt(s),
    panel_top_mm = num_or_null(s, "panel_top_mm"), panel_h_mm = num_or_null(s, "panel_h_mm")),
  s30_panel_cookie_prepost = function(s, an) s30_panel_cookie_prepost(TABS$s30b, an, s$w, s$h),
  s30_panel_cookie_rs = function(s, an) s30_panel_cookie_rs(TABS$s30b, an, s$w, s$h, jitter_seed = seeds(s, 1L)),
  s30_panel_cookie_combz = function(s, an) s30_panel_cookie_combz(TABS$s30b, an, s$w, s$h),
  s30_panel_screen = function(s, an) s30_panel_screen(TABS$s30b, an, s$w, s$h, pq_style = arg(s, "pq_style", "stacked"),
    top_margin_pt = top_pt(s)),
  # Figure 1 option 3 (Nature layout): a-d read the behaviour bundle and the figure-support bundle
  f1o3n_panel_design = function(s, an) f1o3n_panel_design(TABS$ebb, TABS$fsb, prefixed(an, s), s$w, s$h),
  f1o3n_panel_combz = function(s, an) f1o3n_panel_combz(TABS$ebb, TABS$fsb, prefixed(an, s), s$w, s$h),
  f1o3n_panel_cc1 = function(s, an) f1o3n_panel_cc1(TABS$ebb, TABS$fsb, prefixed(an, s), s$w, s$h, jitter_seed = seeds(s, 2L),
    frame = frame_of(s)),
  f1o3n_panel_trajectory = function(s, an) f1o3n_panel_trajectory(TABS$ebb, TABS$fsb, prefixed(an, s), s$w, s$h, frame = frame_of(s),
    indent = nzchar(s$letter)),   # the letterless stand-alone d drops the header's letter indent
  # Figure 1 option 3b (Nature layout): c (post hoc CON / RES / SUS rows of fsb P1) and the simplified d
  f1o3b_panel_cc1 = function(s, an) f1o3b_panel_cc1(TABS$ebb, TABS$fsb, prefixed(an, s), s$w, s$h, jitter_seed = seeds(s, 2L),
    frame = frame_of(s)),
  f1o3b_panel_trajectory = function(s, an) f1o3b_panel_trajectory(TABS$ebb, TABS$fsb, prefixed(an, s), s$w, s$h, frame = frame_of(s)))

# ---------------------------------------------------------------- plot introspection
# The leaf ggplots of a panel (patchwork parts in order; a plain ggplot is its own leaf).
plot_leaves <- function(p) {
  if (inherits(p, "patchwork")) {
    parts <- patchwork:::get_patches(p)$plots
    return(unlist(lapply(parts, plot_leaves), recursive = FALSE))
  }
  if (inherits(p, "ggplot") && length(p$layers)) list(p) else list()
}

# Column names an aesthetic mapping refers to (symbols, .data[["x"]] and .data$x).
mapping_vars <- function(mapping) {
  out <- character(0)
  walk <- function(e) {
    if (is.name(e)) { out <<- c(out, as.character(e)); return(invisible()) }
    if (!is.call(e)) return(invisible())
    if (identical(e[[1]], as.name("[[")) && identical(e[[2]], as.name(".data")) && is.character(e[[3]])) {
      out <<- c(out, e[[3]]); return(invisible())
    }
    if (identical(e[[1]], as.name("$")) && identical(e[[2]], as.name(".data"))) {
      out <<- c(out, as.character(e[[3]])); return(invisible())
    }
    for (x in as.list(e)[-1]) walk(x)
  }
  for (q in mapping) walk(if (rlang::is_quosure(q)) rlang::quo_get_expr(q) else q)
  setdiff(unique(out), ".data")
}

# Identifier columns carried with the plotted columns wherever the layer data holds them.
ID_COLUMNS <- c("AnimalNum", "Sex", "sex", "Group", "Batch", "CC", "id", "model_id", "construct", "estimand",
                "measure", "scope", "window", "row", "label", "key", "question", "family", "row_order", "col_order")

as_text_frame <- function(d) {
  out <- lapply(as.list(d), as.character)          # factors, numbers and logicals as written text
  data.frame(out, check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL)
}
# Columns added by this script carry the prefix src_, so they never collide with a bundle column.
src_cols <- function(d) {
  if (any(startsWith(names(d), "src_"))) stop("a plotted or bundle column already starts with src_", call. = FALSE)
  d
}

# The displayed rows of every layer: the layer's (or the plot's) data, restricted to the columns
# its mappings and facets use plus identifier columns. Identical blocks are written once.
layer_blocks <- function(panel_plot) {
  blocks <- list(); seen <- character(0)
  leaves <- plot_leaves(panel_plot)
  for (i in seq_along(leaves)) {
    leaf <- leaves[[i]]
    title <- leaf$labels$title
    title <- if (is.character(title) && length(title) == 1L) title else ""
    facet_vars <- c(names(leaf$facet$params$facets), names(leaf$facet$params$rows), names(leaf$facet$params$cols))
    for (j in seq_along(leaf$layers)) {
      l <- leaf$layers[[j]]
      d <- if (is.data.frame(l$data)) l$data else if (is.data.frame(leaf$data)) leaf$data else NULL
      if (is.null(d) || !nrow(d)) next
      vars <- mapping_vars(l$mapping)
      if (isTRUE(l$inherit.aes)) vars <- c(vars, mapping_vars(leaf$mapping))
      keep <- intersect(names(d), unique(c(ID_COLUMNS, vars, facet_vars)))
      if (!length(keep)) next
      d <- src_cols(as_text_frame(d[, keep, drop = FALSE]))
      sig <- paste(c(keep, unlist(d)), collapse = "\r")
      if (sig %in% seen) next
      seen <- c(seen, sig)
      blocks[[length(blocks) + 1L]] <- cbind(
        data.frame(src_block = "plotted layer", src_part = i, src_part_title = title, src_layer = j,
                   src_geom = sub("^ggplot2::", "", class(l$geom)[1]), stringsAsFactors = FALSE), d)
    }
  }
  blocks
}

# Primary-axis tick labels as drawn (NA and blank axes dropped), plus any literal identity text
# drawn through geom_text/geom_label (e.g. a per-animal ID row): a free-x facet clones one shared
# scale per panel verbatim, so distinct facets cannot carry distinct tick labels at the same x
# position, and such a row's labels have to be a text layer instead. Recorded so the value-for-
# value test can tell these structural labels from printed values. Presentational layout only.
axis_labels <- function(panel_plot) {
  labs_of <- function(leaf) {
    th <- ggplot2::complete_theme(leaf$theme)
    blank <- function(el) inherits(ggplot2::calc_element(el, th), "element_blank")
    b <- ggplot2::ggplot_build(leaf)
    axis <- unlist(lapply(b$layout$panel_params, function(pp) c(
      if (!blank("axis.text.x.bottom")) pp$x$get_labels(),
      if (!blank("axis.text.y.left")) pp$y$get_labels())))
    is_text <- vapply(b$plot$layers, function(l) inherits(l$geom, "GeomText") || inherits(l$geom, "GeomLabel"), logical(1))
    literal <- unlist(lapply(b$data[is_text], function(d) if ("label" %in% names(d)) d$label else NULL))
    c(axis, literal)
  }
  out <- unlist(lapply(plot_leaves(panel_plot), labs_of))
  out <- unique(as.character(out[!is.na(out)]))
  out[nzchar(out)]
}

# The annotation rows a panel resolved, with the stored and the printed value. src_role says where
# the value appears: "printed" on the panel (its map panel is one the contract declares printed;
# the value-for-value test checks the SVG), "legend" quoted by the draft legend or Results text only, or
# "plotted_only" (a key used only to place a mark: src_printed is left empty, as nothing prints it).
# src_printed is the string as drawn: under the candidate typography the reused Figure 1 builders
# write an e-notation value (map format "%.<n>e") with U+2212 in the exponent and no leading
# exponent zeros (bh_sci_minus), so a printed key of such a panel is recorded in that form.
annotation_block <- function(an, keys, printed_panels, legend_panels, typography = "figure1") {
  if (!length(keys)) return(NULL)
  r <- MAP[match(keys, MAP$key), c("key", "panel", "bundle", "table", "column", "filters", "format")]
  names(r) <- paste0("src_", c("key", "annotation_panel", "bundle", "table", "column", "filters", "format"))
  r$src_role <- ifelse(r$src_annotation_panel %in% printed_panels, "printed",
                       ifelse(r$src_annotation_panel %in% legend_panels, "legend", "plotted_only"))
  r$src_stored_value <- vapply(keys, function(k) as.character(an$ann(k)), "")
  printed <- vapply(keys, an$fa, "")
  drawn <- r$src_role == "printed" & identical(typography, "candidate") & grepl("e$", r$src_format)
  printed[drawn] <- bh_sci_minus(printed[drawn])
  r$src_printed <- ifelse(r$src_role == "plotted_only", "", printed)
  cbind(data.frame(src_block = "annotation key", stringsAsFactors = FALSE), r, row.names = NULL)
}

# The stored bundle row behind each resolved key (the rows the filters select; bundle_cell per column).
bundle_row_blocks <- function(keys) {
  r <- MAP[match(keys, MAP$key), , drop = FALSE]
  r <- r[!duplicated(paste(r$bundle, r$table, r$filters)), , drop = FALSE]
  lapply(seq_len(nrow(r)), function(i) {
    f <- list()
    if (nzchar(r$filters[i])) {
      kv <- strsplit(strsplit(r$filters[i], ";", fixed = TRUE)[[1]], "=", fixed = TRUE)
      f <- stats::setNames(lapply(kv, function(x) if (identical(x[[2]], "NA")) NA else x[[2]]), vapply(kv, `[[`, "", 1L))
    }
    tab <- TABS[[r$bundle[i]]](r$table[i])
    row <- lapply(names(tab), function(cn) do.call(bundle_cell, c(list(tab, cn), f)))
    names(row) <- names(tab)
    cbind(data.frame(src_block = "stored bundle row", src_bundle = r$bundle[i], src_table = r$table[i], src_filters = r$filters[i],
                     stringsAsFactors = FALSE), src_cols(as_text_frame(row)))
  })
}

bind_fill <- function(blocks) {
  blocks <- Filter(Negate(is.null), blocks)
  cols <- unique(unlist(lapply(blocks, names)))
  do.call(rbind, lapply(blocks, function(b) {
    b <- as_text_frame(b)
    for (m in setdiff(cols, names(b))) b[[m]] <- NA_character_
    b[, cols, drop = FALSE]
  }))
}

# The rasterisation source of an assembled SVG. manuscript_figure_assemble_svg embeds each panel as
# an <image> data URI; librsvg (magick) rasterises such an image at the panel's intrinsic size and
# then scales it, so a 300-dpi raster of the assembled SVG is blurred. This writes the same page
# with each embedded panel inlined as a nested <svg> at the same box (x, y, width, height,
# preserveAspectRatio as the <image>), which librsvg draws as vectors at the target density.
# String handling only; the assembled SVG itself is the deliverable and is not changed.
inline_for_raster <- function(assembled_svg, target) {
  x <- readLines(assembled_svg, warn = FALSE, encoding = "UTF-8")
  for (i in grep("^<image ", x)) {
    a <- x[i]
    at <- function(n) sub(paste0('.*\\b', n, '="([^"]+)".*'), "\\1", a)
    inner <- rawToChar(base64enc::base64decode(sub('.*href="data:image/svg\\+xml;base64,([^"]+)".*', "\\1", a)))
    Encoding(inner) <- "UTF-8"
    inner <- sub("^<\\?xml[^>]*\\?>\\s*", "", inner)
    inner <- sub("(<svg [^>]*?) width='[^']*' height='[^']*'", "\\1", inner, perl = TRUE)
    x[i] <- sub("<svg ", sprintf('<svg x="%s" y="%s" width="%s" height="%s" preserveAspectRatio="%s" ',
                                 at("x"), at("y"), at("width"), at("height"), at("preserveAspectRatio")), inner, fixed = TRUE)
  }
  writeLines(enc2utf8(x), target, useBytes = TRUE)
  target
}

write_csv_utf8 <- function(d, path) {
  dir_create(dirname(path))
  utils::write.csv(d, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  path
}
rel <- function(p) relative_to(p)
sha <- function(p) unname(tools::sha256sum(p))

# ---------------------------------------------------------------- output roots
ROOTS <- lapply(CONTRACT$output_roots[c("figures", "source_data", "reports", "logs")], repo_path)
for (r in ROOTS) {
  if (!grepl("/results/[a-z_]+/manuscript_candidates/behaviour_v101_s30$", normalizePath(r, winslash = "/", mustWork = FALSE)))
    stop("Refusing an output root outside results/*/manuscript_candidates/behaviour_v101_s30: ", r, call. = FALSE)
  unlink(r, recursive = TRUE)   # the generation is regenerated whole, so no stale output survives
  dir_create(r)
}
RECEIPT_PATH <- repo_path(CONTRACT$output_roots$receipt)

code_files <- c(ENTRY_REL, CONTRACT_REL, CONTRACT$annotation_map, unlist(CONTRACT$libraries),
                "R/behaviour_bundle.R", "R/stage30_bundle.R", "R/figure_support_bundle.R", "R/panels/manuscript_figure_utils.R", "R/paths.R")
git_dirty <- tryCatch(system2("git", c("-C", repo_root(), "status", "--porcelain", "--untracked-files=all", "--", code_files),
                              stdout = TRUE, stderr = FALSE), error = function(e) NA_character_)
RENDER <- list(commit = git_commit_sha(), code_clean = identical(length(git_dirty), 0L), dirty = as.list(git_dirty))

receipt <- list()
note_output <- function(key, kind, path) {
  receipt[[length(receipt) + 1L]] <<- data.frame(generation = GENERATION, candidate = key, kind = kind, path = rel(path),
                                                bytes = file.size(path), sha256 = sha(path), stringsAsFactors = FALSE)
  path
}

# ---------------------------------------------------------------- render
for (key in names(CONTRACT$candidates)) {
  cand <- CONTRACT$candidates[[key]]
  dirs <- list(panels = file.path(ROOTS$figures, key, "panels"), assembled = file.path(ROOTS$figures, key, "assembled"),
               source_data = file.path(ROOTS$source_data, key), reports = file.path(ROOTS$reports, key),
               logs = file.path(ROOTS$logs, key))
  invisible(lapply(dirs, dir_create))
  panel_rows <- list(); input_use <- list(); asm <- list(); asm_paths <- character(0)
  vector_page <- identical(cand$assembly, "vector_page"); page_items <- list()

  for (s in cand$panels) {
    if (is.null(BUILDERS[[s$builder]])) stop(key, "/", s$id, ": unknown builder ", s$builder, call. = FALSE)
    READS$tables <- character(0)
    an <- bh_annotation(MAP_PATH, tables = TABS, cell = bundle_cell)
    panel <- BUILDERS[[s$builder]](s, an)
    if (!isTRUE(all.equal(c(panel$w_mm, panel$h_mm), c(as.numeric(s$w), as.numeric(s$h)))))
      stop(key, "/", s$id, ": the builder's box differs from the contract box.", call. = FALSE)
    svg <- bh_save_svg(panel, file.path(dirs$panels, sprintf("%s_%s.svg", key, s$id)))

    # the numbers the panel's draft legend quotes: resolved here (not drawn), so they reach the
    # source data with their stored cells, and their tables count as the panel's inputs
    printed_panels <- unlist(s$annotation_panels)
    legend_panels <- unlist(s$annotation_panels_legend)
    builder_keys <- an$resolved()
    for (k in MAP$key[MAP$panel %in% legend_panels]) an$fa(k)

    # the contract declares exactly what the panel read and printed
    read <- sort(READS$tables)
    if (!setequal(read, unlist(s$inputs)))
      stop(key, "/", s$id, ": the builder read ", paste(read, collapse = ", "), " but the contract declares ",
           paste(unlist(s$inputs), collapse = ", "), call. = FALSE)
    keys <- an$resolved()
    allowed <- MAP$key[MAP$panel %in% c(printed_panels, unlist(s$annotation_panels_plotted))]
    if (length(setdiff(builder_keys, allowed)))
      stop(key, "/", s$id, ": resolved keys outside its annotation panels: ", paste(setdiff(builder_keys, allowed), collapse = ", "), call. = FALSE)
    unresolved <- setdiff(MAP$key[MAP$panel %in% printed_panels], builder_keys)
    if (length(unresolved))
      stop(key, "/", s$id, ": declared keys the builder never resolved: ", paste(unresolved, collapse = ", "), call. = FALSE)

    sd_path <- write_csv_utf8(bind_fill(c(layer_blocks(panel$plot), list(annotation_block(an, keys, printed_panels, legend_panels, typo(s))),
                                          bundle_row_blocks(keys))),
                              file.path(dirs$source_data, sprintf("%s_%s_source_data.csv", key, s$id)))
    ticks <- axis_labels(panel$plot)
    note_output(key, "panel_svg", svg)
    note_output(key, "source_data", sd_path)
    for (t in read) input_use[[t]] <- c(input_use[[t]], s$id)

    asm_id <- if (nzchar(s$letter)) s$letter else "0"   # the assembler draws sub("^[0-9]+", "", id): "0" draws no letter
    asm[[length(asm) + 1L]] <- list(id = asm_id, x = s$x, y = s$y, w = s$w, h = s$h)
    asm_paths[[asm_id]] <- svg
    page_items[[length(page_items) + 1L]] <- list(panel = panel, x = s$x, y = s$y, letter = s$letter)
    panel_rows[[length(panel_rows) + 1L]] <- data.frame(
      generation = GENERATION, candidate = key, panel = s$id, letter = s$letter, builder = s$builder,
      description = gsub("[[:space:]]+", " ", s$description),
      x_mm = s$x, y_mm = s$y, w_mm = s$w, h_mm = s$h,
      jitter_seed = if (is.null(s$jitter_seed)) "" else paste(s$jitter_seed, collapse = ";"),
      args = if (length(s$args)) paste(sprintf("%s=%s", names(s$args), vapply(s$args, function(v) paste(v, collapse = "|"), "")), collapse = ";") else "",
      inputs = paste(read, collapse = ";"),
      annotation_panels = paste(printed_panels, collapse = ";"),
      annotation_panels_plotted = paste(unlist(s$annotation_panels_plotted), collapse = ";"),
      annotation_panels_legend = paste(legend_panels, collapse = ";"),
      annotation_keys = paste(keys, collapse = ";"),
      axis_tick_labels = paste(ticks, collapse = "|"),
      panel_svg = rel(svg), panel_svg_bytes = file.size(svg), panel_svg_sha256 = sha(svg),
      source_data = rel(sd_path), source_data_sha256 = sha(sd_path),
      status = "candidate_only_not_promoted", stringsAsFactors = FALSE)
  }

  # assemble (absolute layout; letters by the assembler) and rasterise at 300 dpi; or, for a
  # vector page, draw every panel plot at its box on one device (SVG and vector PDF) and
  # rasterise the page SVG to the PNG preview
  asm_svg <- file.path(dirs$assembled, paste0(key, ".svg"))
  if (vector_page) {
    page <- bh_save_page(page_items, cand$width_mm, cand$height_mm, asm_svg, file.path(dirs$assembled, paste0(key, ".pdf")),
                         letter_pt = CONTRACT$panel_letter_pt)
    raster <- list(bh_page_png(asm_svg, file.path(dirs$assembled, paste0(key, ".png"))), page[["pdf"]])
  } else {
    figure <- list(width_mm = cand$width_mm, height_mm = cand$height_mm, layout_mode = "absolute")
    manuscript_figure_assemble_svg(asm_paths, asm, figure, asm_svg)
    raster_src <- inline_for_raster(asm_svg, file.path(tempdir(), paste0(key, "_raster_source.svg")))
    raster <- manuscript_figure_raster_companions(raster_src, file.path(dirs$assembled, paste0(key, ".png")),
                                                  file.path(dirs$assembled, paste0(key, ".pdf")))
    unlink(raster_src)
  }
  if (length(raster) != 2L) stop(key, ": the PNG/PDF companions were not written.", call. = FALSE)
  note_output(key, "assembled_svg", asm_svg)
  note_output(key, "assembled_png", raster[[1]])
  note_output(key, "assembled_pdf", raster[[2]])

  # manifests
  pm <- write_csv_utf8(do.call(rbind, panel_rows), file.path(dirs$reports, "panel_manifest.csv"))
  inputs <- sort(names(input_use))
  im_rows <- lapply(inputs, function(t) {
    b <- sub("/.*$", "", t); tbl <- sub("^[^/]+/", "", t)
    f <- file.path(BUNDLE_DIRS[[b]], paste0(tbl, ".csv"))
    man <- utils::read.csv(file.path(BUNDLE_DIRS[[b]], "00_manifest.csv"), stringsAsFactors = FALSE)
    listed <- man$sha256[man$file == basename(f)]
    if (length(listed) != 1L || !identical(listed, sha(f))) stop(t, " does not match its bundle manifest.", call. = FALSE)
    data.frame(candidate = key, role = "bundle_table", bundle = b, bundle_id = PINS[[b]]$bundle_id, table = tbl,
               path = rel(f), bytes = file.size(f), sha256 = sha(f), matches_bundle_manifest = TRUE,
               panels = paste(unique(input_use[[t]]), collapse = ";"), stringsAsFactors = FALSE)
  })
  im_rows[[length(im_rows) + 1L]] <- data.frame(candidate = key, role = "annotation_map", bundle = "", bundle_id = "",
    table = "", path = CONTRACT$annotation_map, bytes = file.size(MAP_PATH), sha256 = sha(MAP_PATH),
    matches_bundle_manifest = NA, panels = "", stringsAsFactors = FALSE)
  im <- write_csv_utf8(do.call(rbind, im_rows), file.path(dirs$reports, "input_manifest.csv"))
  note_output(key, "panel_manifest", pm)
  note_output(key, "input_manifest", im)

  own <- Filter(function(r) identical(r$candidate, key), receipt)
  run <- list(
    generation = GENERATION, candidate = key, title = cand$title,
    status = CONTRACT$status, is_numbered_manuscript_figure = FALSE, promotion_status = cand$promotion_status,
    contract = list(path = CONTRACT_REL, sha256 = sha(repo_path(CONTRACT_REL))),
    entry_point = list(path = ENTRY_REL, sha256 = sha(repo_path(ENTRY_REL))),
    annotation_map = list(path = CONTRACT$annotation_map, sha256 = sha(MAP_PATH)),
    libraries = lapply(unlist(CONTRACT$libraries), function(p) list(path = p, sha256 = sha(repo_path(p)))),
    render_git_commit = RENDER$commit, render_code_clean = RENDER$code_clean, render_code_dirty = RENDER$dirty,
    bundles = lapply(PINS, function(p) list(bundle_id = p$bundle_id, manifest_sha256 = p$manifest_sha256)),
    scientific_recomputation = "none",
    rendering_repository_computes_statistics = FALSE,
    page = list(width_mm = cand$width_mm, height_mm = cand$height_mm, layout_mode = "absolute", panel_letter_pt = CONTRACT$panel_letter_pt),
    jitter_seeds = stats::setNames(lapply(cand$panels, function(s) if (is.null(s$jitter_seed)) "none" else as.integer(s$jitter_seed)),
                                   vapply(cand$panels, function(s) s$id, "")),
    panel_manifest = list(path = rel(pm), sha256 = sha(pm)),
    input_manifest = list(path = rel(im), sha256 = sha(im)),
    outputs = lapply(own, function(r) list(kind = r$kind, path = r$path, bytes = r$bytes, sha256 = r$sha256)),
    assembly = if (vector_page) "vector_page" else "embedded_panel_svgs",
    assembled_svg_contract = if (vector_page) "one_vector_page_svglite_live_text_no_embedded_images_absolute_layout"
                             else "self_contained_vector_svg_with_embedded_panel_svgs_absolute_layout",
    assembled_png_contract = if (vector_page) "preview_rasterized_via_magick_at_300_dpi_from_the_vector_page_svg"
                             else "rasterized_via_magick_at_300_dpi_from_the_assembled_SVG_with_its_panels_inlined_as_nested_svg",
    assembled_pdf_contract = if (vector_page) "vector_pdf_via_cairo_pdf_with_embedded_arial_no_raster_images"
                             else "raster_backed_PDF_via_magick_at_300_dpi_from_the_assembled_SVG_with_its_panels_inlined_as_nested_svg",
    software = list(r = paste(R.version$major, R.version$minor, sep = "."),
                    packages = lapply(c(ggplot2 = "ggplot2", patchwork = "patchwork", svglite = "svglite", magick = "magick"),
                                      function(p) as.character(utils::packageVersion(p)))))
  rm_path <- file.path(dirs$logs, "run_manifest.yml")
  writeLines(enc2utf8(yaml::as.yaml(run)), rm_path, useBytes = TRUE)
  note_output(key, "run_manifest", rm_path)
  cat(sprintf("%-34s %d panel(s)  %s x %s mm\n", key, length(cand$panels), cand$width_mm, cand$height_mm))
}

# ---------------------------------------------------------------- receipt
rc <- do.call(rbind, receipt)
rc$ebb_bundle_id <- PIN$bundle_id
rc$s30b_bundle_id <- S30$bundle_id
rc$fsb_bundle_id <- FSB$bundle_id
invisible(write_csv_utf8(rc, RECEIPT_PATH))
cat("receipt:", rel(RECEIPT_PATH), "(", nrow(rc), "outputs )\n")
if (COPY_RECEIPT) {
  tracked <- repo_path(CONTRACT$output_roots$tracked_receipt_copy)
  dir_create(dirname(tracked))
  if (!file.copy(RECEIPT_PATH, tracked, overwrite = TRUE)) stop("could not copy the receipt to ", tracked, call. = FALSE)
  cat("tracked receipt copy:", rel(tracked), "\n")
}
cat("bundles:", PIN$bundle_id, S30$bundle_id, FSB$bundle_id, " render commit:", RENDER$commit, if (!RENDER$code_clean) "(code not clean)" else "", "\n")
