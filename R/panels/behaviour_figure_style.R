# Shared house style for the behaviour figures (Figure 1 and its candidates).
#
# Extracted unchanged from figures/figure_01_panels.R so that the Figure 1
# renderer and any candidate or Extended Data behaviour panel draw with the same
# constants, theme, annotation resolver and SVG device. Nothing here computes a
# statistic: the annotation resolver returns exactly one stored bundle cell per
# printed number, formatted by the sprintf format the annotation map declares.
#
# Contents
#   GROUP_COL / GROUP_LEV / GROUP_SHAPE / SEX_SHAPE / CON_GREY   group and sex encoding
#   INK / MUTED / RULE / GREEN_* / NEUTRAL_BOX                    inks
#   BASE_PT / BODY_PT / NOTE_PT                                   type sizes (pt)
#   theme_f1()                                                    the Figure 1 theme
#   BH_FOREST, bh_forest_zero(), bh_forest_ci(), bh_forest_point(), bh_forest_int_point()
#                                                                 forest marks of the behaviour ED renderer
#                                                                 (interaction rows: white diamond)
#   bh_minus(), bh_sci_minus()                                    typographic minus (U+2212)
#   bh_annotation()                                               multi-bundle annotation-map resolver
#   bh_panel(), bh_save_svg()                                     panel box and SVG device

suppressPackageStartupMessages(library(ggplot2))

# ---------------------------------------------------------------- house style
# Group colours from R/plotting_nature.R (byte-identical to MMM_GROUP_COLOURS
# upstream). Shape carries group redundantly.
source(repo_path("R", "plotting_nature.R"))
GROUP_COL <- NATURE_SEMANTIC_PALETTES$group[c("CON", "RES", "SUS")]
GROUP_LEV <- c("CON", "RES", "SUS")
GROUP_SHAPE <- c(CON = 21L, RES = 24L, SUS = 22L)
SEX_SHAPE <- c(Female = 21L, Male = 24L)
CON_GREY <- "grey45"   # CON is descriptive: hollow grey marks and dashed lines (RES is the light group ink)

INK <- "#2B2B2B"; MUTED <- "#6E6E6E"; RULE <- "#B5B5B5"
GREEN_DARK <- "#2F6F62"; GREEN_MID <- "#6FA79B"; GREEN_LIGHT <- "#A8D5CF"; NEUTRAL_BOX <- "#EDEDED"
BASE_PT <- 7; BODY_PT <- 6.5; NOTE_PT <- 6

theme_f1 <- function(base = BASE_PT) {
  theme_classic(base_size = base, base_family = "sans") +
    theme(
      axis.text = element_text(size = BODY_PT, colour = INK),
      axis.title = element_text(size = base, colour = INK),
      axis.line = element_line(linewidth = 0.3, colour = "black"),
      axis.ticks = element_line(linewidth = 0.3, colour = "black"),
      axis.ticks.length = unit(2, "pt"),
      strip.background = element_blank(),
      strip.text = element_text(size = base, colour = INK, face = "italic", margin = margin(b = 1.5, t = 1)),
      legend.position = "top", legend.justification = "left",
      legend.key.height = unit(7, "pt"), legend.key.width = unit(9, "pt"),
      legend.text = element_text(size = NOTE_PT), legend.title = element_blank(),
      legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(0, 0, -3.5, 0),
      panel.grid = element_blank(),
      plot.title = element_text(size = base, colour = INK, hjust = 0, face = "plain", margin = margin(b = 0.5)),
      plot.subtitle = element_text(size = NOTE_PT, colour = "grey25", margin = margin(b = 1.5)),
      plot.caption = element_text(size = NOTE_PT, colour = INK, hjust = 0, lineheight = 1.05, margin = margin(t = 1.5)),
      # 4 mm of top margin: the assembler draws the panel letter over the top-left corner.
      plot.margin = margin(11.5, 3, 3, 3))
}

# ---------------------------------------------------------------- forest marks
# The estimate / interval / zero-line marks of the behaviour Extended Data
# renderer (figures/extended_data_behaviour_panels.R): point shape 21 size 1.9
# stroke 0.3; interval linewidth 0.7 in MUTED with round ends; zero reference
# dashed "22", linewidth 0.3, RULE.
# Interaction rows (the Female - male difference) take their own shape, a white
# diamond (23), so that no fill state reads as a significance code: Figures 2
# and 3 draw FDR-supported points filled and the others open (white).
BH_FOREST <- list(
  point_shape = 21L, point_size = 1.9, point_stroke = 0.3, point_colour = "grey20", point_fill = "white",
  ci_linewidth = 0.7, ci_colour = MUTED, ci_lineend = "round",
  zero_linetype = "22", zero_linewidth = 0.3, zero_colour = RULE,
  int_shape = 23L, int_size = 1.7, int_stroke = 0.3, int_fill = "white")

bh_forest_zero <- function(xintercept = 0, horizontal = FALSE, marks = BH_FOREST) {
  if (horizontal)
    return(geom_hline(yintercept = xintercept, linetype = marks$zero_linetype,
                      linewidth = marks$zero_linewidth, colour = marks$zero_colour))
  geom_vline(xintercept = xintercept, linetype = marks$zero_linetype,
             linewidth = marks$zero_linewidth, colour = marks$zero_colour)
}
bh_forest_ci <- function(mapping, ..., marks = BH_FOREST) {
  geom_segment(mapping, linewidth = marks$ci_linewidth, colour = marks$ci_colour,
               lineend = marks$ci_lineend, ...)
}
bh_forest_point <- function(mapping, fill = BH_FOREST$point_fill, ..., marks = BH_FOREST) {
  geom_point(mapping, shape = marks$point_shape, size = marks$point_size,
             stroke = marks$point_stroke, colour = marks$point_colour, fill = fill, ...)
}
#' The interaction-row mark (Female - male): a white diamond, never a fill-coded circle.
bh_forest_int_point <- function(mapping, ..., marks = BH_FOREST) {
  geom_point(mapping, shape = marks$int_shape, size = marks$int_size,
             stroke = marks$int_stroke, colour = marks$point_colour, fill = marks$int_fill, ...)
}

# ---------------------------------------------------------------- typography
#' Replace a leading hyphen-minus with the typographic minus sign U+2212.
bh_minus <- function(x) sub("^-", "−", x)
#' The typographic minus in the exponent of an e-notation string, without its leading
#' zeros ("2.61e-05" -> "2.61e−5"). String handling only; opt-in (candidate typography).
bh_sci_minus <- function(x) sub("e-0*([0-9])", "e−\\1", x)

# ---------------------------------------------------------------- annotation map
#' Resolver for an annotation map: every printed number is one stored cell.
#'
#' `map_csv` is the path of an annotation map with columns
#' key, panel, [bundle,] table, column, filters, format, meaning.
#' `tables` is a named list of table getters, one per bundle label, each
#' function(table_name) returning that bundle table as read from its pinned copy
#' (e.g. list(ebb = <behaviour bundle getter>, s30b = <Stage 30 bundle getter>)).
#' A map without a `bundle` column (figures/figure_01_annotation_map.csv) resolves
#' every row against `default_bundle`, the first getter. `filters` is
#' "name=value;name=value" (value NA matches a missing cell); an empty filter
#' selects the whole table, which bundle_cell() accepts only if it has one row.
#' `cell` is the one-cell extractor, bundle_cell() from R/behaviour_bundle.R.
#'
#' Returns list(ann, fa, ci, map, row, resolved):
#'   ann(key)  the stored value (unformatted);
#'   fa(key)   the value formatted with the map's sprintf format ("%d" rounds to
#'             integer first), leading minus as U+2212;
#'   ci(k)     "est [lo, hi]" from keys k, k_lo, k_hi;
#'   resolved() the keys resolved so far, in first-use order.
bh_annotation <- function(map_csv, tables, cell = bundle_cell, default_bundle = names(tables)[1]) {
  if (!is.list(tables) || !length(tables) || is.null(names(tables)) || any(!nzchar(names(tables))))
    stop("bh_annotation: tables must be a named list of table getters.", call. = FALSE)
  amap <- utils::read.csv(map_csv, stringsAsFactors = FALSE)
  if (anyDuplicated(amap$key)) stop(basename(map_csv), " has duplicated keys.", call. = FALSE)
  if (!"bundle" %in% names(amap)) amap$bundle <- default_bundle
  unknown <- setdiff(unique(amap$bundle), names(tables))
  if (length(unknown)) stop("bh_annotation: map names bundle(s) with no table getter: ", paste(unknown, collapse = ", "), call. = FALSE)
  seen <- character(0)
  map_row <- function(key) {
    r <- amap[amap$key == key, , drop = FALSE]
    if (nrow(r) != 1L) stop("annotation key not declared: ", key, call. = FALSE)
    r
  }
  ann <- function(key) {
    r <- map_row(key)
    filters <- list()
    if (!is.na(r$filters) && nzchar(r$filters)) {
      kv <- strsplit(strsplit(r$filters, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)
      filters <- stats::setNames(lapply(kv, function(x) if (identical(x[[2]], "NA")) NA else x[[2]]),
                                 vapply(kv, `[[`, "", 1L))
    }
    if (!key %in% seen) seen <<- c(seen, key)
    do.call(cell, c(list(tables[[r$bundle]](r$table), r$column), filters))
  }
  fa <- function(key) {
    r <- map_row(key); v <- ann(key)
    if (grepl("%d", r$format, fixed = TRUE)) v <- as.integer(round(v))
    bh_minus(sprintf(r$format, v))
  }
  ci <- function(k) sprintf("%s [%s, %s]", fa(k), fa(paste0(k, "_lo")), fa(paste0(k, "_hi")))
  list(ann = ann, fa = fa, ci = ci, map = amap, row = map_row, resolved = function() seen)
}

# ---------------------------------------------------------------- panel box and device
#' A panel plot together with the box (mm) it is authored for.
bh_panel <- function(plot, w_mm, h_mm) {
  if (!is.numeric(w_mm) || !is.numeric(h_mm) || length(w_mm) != 1L || length(h_mm) != 1L || !(w_mm > 0) || !(h_mm > 0))
    stop("bh_panel: box width and height must be single positive numbers (mm).", call. = FALSE)
  structure(list(plot = plot, w_mm = w_mm, h_mm = h_mm), class = "bh_panel")
}

#' Write a panel as SVG at its exact box, so the absolute-layout assembler places it at scale 1.
#' `p` is a ggplot/patchwork object, or a bh_panel whose box is used when w_mm/h_mm are not given.
bh_save_svg <- function(p, path, w_mm = NULL, h_mm = NULL) {
  if (inherits(p, "bh_panel")) {
    if (is.null(w_mm)) w_mm <- p$w_mm
    if (is.null(h_mm)) h_mm <- p$h_mm
    p <- p$plot
  }
  if (is.null(w_mm) || is.null(h_mm)) stop("bh_save_svg: the panel box (w_mm, h_mm) is required.", call. = FALSE)
  svglite::svglite(path, width = w_mm / 25.4, height = h_mm / 25.4, bg = "white", fix_text_size = FALSE)
  print(p); grDevices::dev.off()
  path
}
