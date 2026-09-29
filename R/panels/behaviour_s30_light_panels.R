# Light-phase panels of the behaviour candidate generation behaviour_v101_s30
# (CANDIDATE_SPEC B1-B4). Rendering only: fits no model, computes no statistic.
#
# Builders
#   s30_panel_light_measure(measure, tabs, an, w_mm, h_mm, jitter_seed, standalone)   B1
#   s30_panel_light_phase(tabs, an, w_mm, h_mm, jitter_seed, ...)                     B2 (light_phase_panel; ED X d)
#   s30_panel_light_compact(an, w_mm, h_mm, q_rows, ...)                              B3 (Figure 1 option 2, panel d)
#   s30_panel_light_dependence(tabs, an, w_mm, h_mm, ...)                             B4 (light_dependence_ed; ED X e)
#   s30_light_panel_data(panel, tabs, an, q_rows)    the displayed rows of each builder (source-data export)
# Arguments
#   tabs  named list of table getters, list(ebb = function(name), s30b = function(name)),
#         reading the pinned ebb_v101 bundle (R/behaviour_bundle.R) and the pinned
#         Stage 30 figure bundle (R/stage30_bundle.R); the same list bh_annotation() takes;
#   an    the resolver from bh_annotation() over a map holding the keys of
#         figures/behaviour_v101_s30_annotation_map.csv (panels light_*);
#   w_mm, h_mm  the box the panel is authored for (defaults: CANDIDATE_SPEC boxes);
#   top_margin_pt  (B2, B3, B4) the top margin: 11.5 pt leaves room for the panel letter
#         (Figure 1); a letterless single-panel page passes a small one;
#   panel_top_mm, panel_h_mm  (B2, B4) optional absolute geometry of the top plot panel: its
#         distance from the box top and its height (mm). Two boxes given the same values
#         share one panel frame, so B4 beside B2 (ED X) reads across on the same inactivity
#         axis. Presentational layout only (the header is measured on the SVG device).
# Every builder returns bh_panel(plot, w_mm, h_mm) as the Figure 1 builders do; $plot is
# the ggplot/patchwork object. There is no file I/O here: save with bh_save_svg().
#
# What is drawn
#   animals     ebb A1 (RES/SUS rows, light_phase_crossing_rate) and s30b S1
#               (posinact40_light): the bundles' stored animal values, row-filtered only;
#   estimates   the stored RES - SUS contrasts, read through the annotation map, so the
#               plotted point and interval and the printed text are the same cells: ebb C2
#               (source stage29_exposure; LIGHT_CC1_BY_SEX|Female/|Male, LIGHT_CC1_POOLED
#               Q1_light) and s30b S1b (RS_F, RS_M, INT_FM); the Female - male row is a white
#               diamond (BH_FOREST int_*), so no fill state reads as a significance code;
#   dependence  s30b S2 (444 light-phase animal-windows, group-blind) and its S2b light row.
# CON is not shown: these estimates are SIS-only, and every panel says so.
#
# Jitter is presentational only and always seeded explicitly (position_jitter(seed =)),
# so the output does not depend on the global RNG or on build/print order. The two
# animal layers are sorted by AnimalNum, so with one seed an animal sits at the same
# horizontal offset in the rate and inactivity panels.
#
# Requires: R/behaviour_bundle.R, R/stage30_bundle.R and R/panels/behaviour_figure_style.R
# sourced first; R/panels/behaviour_figure1_panels.R for F1_UNIT_PER_H.

# CANDIDATE_SPEC boxes, w x h in mm.
S30_LIGHT_BOX <- list(measure = c(60, 76), phase = c(122, 76), compact = c(35, 62), dependence = c(49, 76))

# Tier labels (CANDIDATE_SPEC A). The Stage 30 label names the local family size m (map key
# li_family_m / lc_family_m): its q values are local to the six registered inactivity tests.
S30_TIER_S29_LIGHT <- "Stage 29 secondary estimate; not tested"
s30_tier_s30 <- function(an, key = "li_family_m", sep = " ")
  sprintf("Stage 30 exploratory%s(local BH q, m = %s)", sep, an$fa(key))

# Figure 1c grammar at the Stage 30 marks: RES left, SUS right of each sex.
S30_GROUP_OFFSET <- c(RES = -0.14, SUS = 0.14)
S30_ANIMAL_MARK <- list(size = 0.7, stroke = 0.18, alpha = 0.55, jitter_width = 0.05)

# The truncated inactivity axis (CANDIDATE_SPEC B1): limits, breaks and their labels (constant precision).
S30_INACT_LIMITS <- c(0.975, 1)
S30_INACT_BREAKS <- c(0.975, 0.98, 0.99, 1)
S30_INACT_LABELS <- c("0.975", "0.980", "0.990", "1.000")

S30_ROW_LABEL <- c(F = "Female", M = "Male", INT = "Female − male")
S30_ROW_SHORT <- c(F = "F", M = "M", INT = "F − M")

# Forest marks of the compact panel B3: Figure 1c weight, so the exploratory panel does not
# outweigh the primary panel beside it.
S30_FOREST_COMPACT <- utils::modifyList(BH_FOREST, list(point_size = 1.4, point_stroke = 0.25, ci_linewidth = 0.5,
                                                        int_size = 1.25, int_stroke = 0.25))

# The per-hour unit as plotmath (Arial has no superscript-minus glyph), as in Figure 1.
S30_RS_RATE_X <- expression("RES − SUS (position changes h"^-1 * ")")
S30_RS_INACT_X <- "RES − SUS (fraction of light phase)"
# Two lines (one would be wider than the 49-mm ED X box), as F1_EARLY_RATE_X; the light phase
# is named in the subtitle.
S30_DEP_X <- expression(textstyle(atop(displaystyle("position-change rate"), displaystyle("(position changes h"^-1 * ")"))))
S30_DEP_Y <- "sustained positional inactivity\n(≥40 s), fraction"

# Height of the collected-legend row of B2 (mm), under the overall title.
S30_PHASE_LEGEND_MM <- 3.2
# Relative heights of the animal panel and the strip panel (B1 and B2 alike, so the two
# halves of B2 and a standalone B1 share one geometry), and their absolute form for B1:
# panel heights as fractions of the box height, so every B1 box of one height has one panel
# frame whatever its text heights.
S30_LIGHT_HEIGHTS <- c(1.55, 1)
S30_LIGHT_PANEL_FRAC <- c(top = 0.401, strip = 0.257)

#' Axis tick labels with the typographic minus (U+2212); presentational formatting of breaks only.
s30_tick_labels <- function(x) {
  out <- format(x, trim = TRUE, scientific = FALSE, drop0trailing = TRUE)
  out[is.na(x)] <- NA_character_
  bh_minus(out)
}

# ---------------------------------------------------------------- layout measurement
#' Distance (mm) from the top of a plot drawn in a w x h mm box to the top edge of its first
#' plot panel, on the SVG device the panels are written with (svglite, fix_text_size = FALSE),
#' so the text metrics are the ones drawn. Presentational layout only.
s30_panel_top_mm <- function(p, w_mm, h_mm) {
  svglite::svgstring(width = w_mm / 25.4, height = h_mm / 25.4, bg = "white", fix_text_size = FALSE)
  on.exit(grDevices::dev.off(), add = TRUE)
  g <- if (inherits(p, "patchwork")) patchwork::patchworkGrob(p) else ggplot2::ggplotGrob(p)
  lay <- g$layout
  t <- min(lay$t[grepl("^(free_)?panel(-[0-9-]+)?$", lay$name)])
  if (!is.finite(t)) stop("s30_panel_top_mm: the plot has no panel.", call. = FALSE)
  if (t == 1L) return(0)
  above <- g$heights[seq_len(t - 1L)]
  if (any(grid::unitType(above) == "null")) stop("s30_panel_top_mm: a relative row above the first panel.", call. = FALSE)
  sum(grid::convertHeight(above, "mm", valueOnly = TRUE))
}

#' The plot `build(pad_pt)` whose first panel starts `target_mm` below the box top: `build` adds
#' `pad_pt` of space to the header (where, is the builder's choice). A few fixed-point steps, since
#' the header height is measured, not predicted; stops if the header without padding is too tall.
s30_align_panel_top <- function(build, target_mm, w_mm, h_mm, what) {
  p <- build(0)
  natural <- s30_panel_top_mm(p, w_mm, h_mm)
  if (natural > target_mm + 0.02)
    stop(what, ": the header needs ", format(round(natural, 2)), " mm, more than panel_top_mm = ", target_mm, ".", call. = FALSE)
  pad <- 0; top <- natural
  for (i in 1:4) {
    if (abs(target_mm - top) < 0.005) break
    pad <- max(pad + (target_mm - top) * 72 / 25.4, 0)
    p <- build(pad)
    top <- s30_panel_top_mm(p, w_mm, h_mm)
  }
  p
}

# ---------------------------------------------------------------- stored contrast rows
#' The three stored RES - SUS rows (Female, Male, Female - male) of one light measure, read
#' through the annotation map: plotted values are an$ann() of the printed keys.
#' `measure` "rate" (keys lr_*) or "inactivity" (keys li_*); `q_keys` the q keys printed per
#' row (NULL: none); `row_labels` the row names.
s30_light_rows <- function(an, measure, q_keys = NULL, row_labels = S30_ROW_LABEL) {
  base <- switch(measure, rate = c(F = "lr_rs_f", M = "lr_rs_m", INT = "lr_int"),
                 inactivity = c(F = "li_rs_f", M = "li_rs_m", INT = "li_int"),
                 stop("s30_light_rows: measure must be 'rate' or 'inactivity'.", call. = FALSE))
  rows <- data.frame(row = names(base), y = c(3, 2, 1), label = unname(row_labels[names(base)]),
                     key = unname(base),
                     estimate = vapply(base, an$ann, numeric(1)),
                     lo = vapply(paste0(base, "_lo"), an$ann, numeric(1)),
                     hi = vapply(paste0(base, "_hi"), an$ann, numeric(1)),
                     ci_text = vapply(base, an$ci, character(1)),
                     q_key = NA_character_, q_text = "", stringsAsFactors = FALSE, row.names = NULL)
  if (!is.null(q_keys)) {
    have <- rows$row %in% names(q_keys)
    rows$q_key[have] <- unname(q_keys[rows$row[have]])
    rows$q_text[have] <- paste0("q = ", vapply(rows$q_key[have], an$fa, character(1)))
  }
  rows
}

#' A horizontal estimate strip (ED forest marks): zero line, CI segment, point (INK circle within
#' sex, white diamond for Female - male), row labels at left, `rows$text` printed at right as the
#' secondary axis labels (NULL/empty: nothing at right). `marks` the forest marks (BH_FOREST, or
#' S30_FOREST_COMPACT); `plot_margin` the plot margin.
s30_light_strip <- function(rows, x_title, right_text = rows$text, title = NULL, subtitle = NULL,
                            row_text_size = BODY_PT, right_text_size = NOTE_PT, x_breaks = waiver(),
                            marks = BH_FOREST, plot_margin = margin(2, 3, 2, 3)) {
  sex_rows <- rows[rows$row != "INT", , drop = FALSE]
  int_rows <- rows[rows$row == "INT", , drop = FALSE]
  has_right <- !is.null(right_text) && any(nzchar(right_text))
  y_scale <- if (has_right) {
    scale_y_continuous(breaks = rows$y, labels = rows$label, limits = c(0.45, 3.55), expand = c(0, 0),
                       sec.axis = sec_axis(~ ., breaks = rows$y, labels = right_text))
  } else {
    scale_y_continuous(breaks = rows$y, labels = rows$label, limits = c(0.45, 3.55), expand = c(0, 0))
  }
  ggplot(rows, aes(y = y)) +
    bh_forest_zero(marks = marks) +
    bh_forest_ci(aes(x = lo, xend = hi, yend = y), marks = marks) +
    bh_forest_point(aes(x = estimate), data = sex_rows, fill = INK, marks = marks) +
    bh_forest_int_point(aes(x = estimate), data = int_rows, marks = marks) +
    y_scale +
    scale_x_continuous(breaks = x_breaks, labels = s30_tick_labels, expand = expansion(mult = 0.06)) +
    expand_limits(x = 0) +
    labs(x = x_title, y = NULL, title = title, subtitle = subtitle) +
    theme_f1() +
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
          axis.text.y.left = element_text(size = row_text_size, colour = INK),
          axis.text.y.right = element_text(size = right_text_size, colour = INK, hjust = 0, lineheight = 1.0,
                                           margin = margin(l = 2)),
          plot.margin = plot_margin)
}

# ---------------------------------------------------------------- B1
#' The parts of B1 (list(top, strip, animals, estimates)): the SIS animals at CC1 and the stored
#' contrast strip. B1 stacks them; B2 lays out the parts of both measures in one grid
#' (patchwork's free() cannot be nested, and the strips are freed so their row labels do not
#' push the animal panels inward). Titles start at the plot edge, as the cookie and compact panels'.
s30_light_measure_parts <- function(measure, tabs, an, jitter_seed, standalone) {
  if (measure == "rate") {
    A1 <- tabs$ebb("A1_animal_cc1")
    pts <- A1[A1$Group %in% c("RES", "SUS"), c("AnimalNum", "Sex", "Group", "light_phase_crossing_rate")]
    title <- "Light-phase RFID position-change rate"; tier <- S30_TIER_S29_LIGHT
    y_title <- F1_UNIT_PER_H
    rows <- s30_light_rows(an, "rate")
    rows$text <- rows$ci_text
    x_title <- S30_RS_RATE_X
  } else {
    S1 <- tabs$s30b("S1_light_animals_cc1")
    pts <- S1[S1$Group %in% c("RES", "SUS"), c("AnimalNum", "Sex", "Group", "posinact40_light")]
    title <- "Sustained positional inactivity (≥40 s)"; tier <- s30_tier_s30(an)
    y_title <- "fraction of light phase"
    # q for every row (F, M and the interaction), so the display is not selective.
    rows <- s30_light_rows(an, "inactivity", q_keys = c(F = "li_rs_f_q", M = "li_rs_m_q", INT = "li_int_q"))
    rows$text <- paste0(rows$ci_text, "\n", rows$q_text)
    x_title <- S30_RS_INACT_X
  }
  names(pts)[4] <- "y"
  pts <- pts[is.finite(pts$y), , drop = FALSE]
  pts <- pts[order(as.character(pts$AnimalNum)), , drop = FALSE]
  pts$Sex <- factor(pts$Sex, levels = c("Female", "Male"))
  pts$Group <- factor(pts$Group, levels = c("RES", "SUS"))
  pts$x <- as.numeric(pts$Sex) + S30_GROUP_OFFSET[as.character(pts$Group)]
  subtitle <- if (standalone) paste0(tier, "\nSIS animals after CC1; CON not modelled") else tier

  y_scale <- if (measure == "rate") {
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))
  } else {
    scale_y_continuous(limits = S30_INACT_LIMITS, breaks = S30_INACT_BREAKS, labels = S30_INACT_LABELS,
                       expand = expansion(mult = c(0, 0.03)))
  }
  top <- ggplot(pts, aes(x, y)) +
    geom_point(aes(fill = Group, shape = Group),
               position = position_jitter(width = S30_ANIMAL_MARK$jitter_width, height = 0, seed = jitter_seed),
               size = S30_ANIMAL_MARK$size, stroke = S30_ANIMAL_MARK$stroke, alpha = S30_ANIMAL_MARK$alpha,
               colour = "grey20") +
    scale_fill_manual(values = GROUP_COL[c("RES", "SUS")], drop = FALSE) +
    scale_shape_manual(values = GROUP_SHAPE[c("RES", "SUS")], drop = FALSE) +
    guides(fill = guide_legend(override.aes = list(size = 1.3, alpha = 0.9, stroke = 0.25)),
           shape = guide_legend()) +
    scale_x_continuous(breaks = 1:2, labels = c("Female", "Male"), limits = c(0.62, 2.38), expand = c(0, 0)) +
    y_scale +
    labs(x = NULL, y = y_title, title = title, subtitle = subtitle) +
    theme_f1() +
    theme(plot.margin = margin(2, 3, 1, 3), legend.box.margin = margin(0, 0, -5, 0),
          plot.title.position = "plot")
  if (measure == "inactivity") {
    note <- sprintf("axis starts at 0.975;\n%s of %s animals ≥ 0.99", an$fa("li_n_ge099"), an$fa("li_n_animals"))
    top <- top + annotate("text", x = 2.36, y = S30_INACT_LIMITS[1] + 0.0006, label = note, hjust = 1, vjust = 0,
                          size = NOTE_PT / .pt, colour = "grey25", lineheight = 1.0)
  }
  list(top = top, strip = s30_light_strip(rows, x_title), animals = pts, estimates = rows)
}

#' B1. One light-phase measure: the SIS animals at CC1 (top) over the stored RES - SUS
#' contrast strip (bottom), sharing the width.
#'   measure     "rate" (ebb A1 light_phase_crossing_rate; ebb C2 contrasts; CIs only, no p)
#'               or "inactivity" (s30b S1 posinact40_light; s30b S1b contrasts; CI and q per row);
#'   jitter_seed the explicit seed of the presentational horizontal jitter;
#'   standalone  TRUE (default): complete on its own (group legend, "SIS animals after CC1; CON
#'               not modelled" line, 11.5-pt top margin for the panel letter); FALSE: the same
#'               panel without the SIS line and top margin, for a caller that states both.
#' The animal and strip panels have absolute heights (S30_LIGHT_PANEL_FRAC of the box height), so
#' two B1 boxes of one height share one panel frame whatever their axis-title heights.
s30_panel_light_measure <- function(measure = c("rate", "inactivity"), tabs, an,
                                    w_mm = S30_LIGHT_BOX$measure[1], h_mm = S30_LIGHT_BOX$measure[2],
                                    jitter_seed = 1L, standalone = TRUE) {
  measure <- match.arg(measure)
  if (length(jitter_seed) != 1L || is.na(jitter_seed)) stop("s30_panel_light_measure: pass one explicit jitter_seed.", call. = FALSE)
  parts <- s30_light_measure_parts(measure, tabs, an, jitter_seed, standalone)
  p <- patchwork::wrap_plots(parts$top, patchwork::free(parts$strip, side = "lr"), ncol = 1,
                             heights = grid::unit(h_mm * S30_LIGHT_PANEL_FRAC, "mm"))
  # 9.5 pt here + the 2-pt top margin of the animal plot = the Figure 1 top margin of 11.5 pt.
  if (standalone)
    p <- p + patchwork::plot_annotation(theme = theme(plot.margin = margin(9.5, 0, 0, 0)))
  bh_panel(p, w_mm, h_mm)
}

# ---------------------------------------------------------------- B2
#' B2 (candidate light_phase_panel; ED X d). B1(rate) and B1(inactivity) side by side, equal
#' widths, under the overall title and one collected group legend (its own row directly under
#' the title, so it reads as shared by both halves); two representations of one light-phase
#' phenotype, with no arrows and no claims.
#'   jitter_seed  one seed (used for both halves: an animal keeps its offset) or two (rate, inactivity);
#'   top_margin_pt, panel_top_mm, panel_h_mm  see the file header (panel = the animal panels).
s30_panel_light_phase <- function(tabs, an, w_mm = S30_LIGHT_BOX$phase[1], h_mm = S30_LIGHT_BOX$phase[2],
                                  jitter_seed = 1L, top_margin_pt = 11.5, panel_top_mm = NULL, panel_h_mm = NULL) {
  if (!length(jitter_seed) %in% 1:2 || anyNA(jitter_seed)) stop("s30_panel_light_phase: pass one or two explicit jitter seeds.", call. = FALSE)
  if (is.null(panel_top_mm) != is.null(panel_h_mm)) stop("s30_panel_light_phase: give panel_top_mm and panel_h_mm together.", call. = FALSE)
  rate <- s30_light_measure_parts("rate", tabs, an, jitter_seed[1], standalone = FALSE)
  inact <- s30_light_measure_parts("inactivity", tabs, an, jitter_seed[length(jitter_seed)], standalone = FALSE)
  build <- function(pad_pt) {
    legend_mm <- S30_PHASE_LEGEND_MM + pad_pt * 25.4 / 72
    heights <- if (is.null(panel_h_mm)) grid::unit(c(legend_mm, S30_LIGHT_HEIGHTS), c("mm", "null", "null"))
               else grid::unit(c(legend_mm, panel_h_mm, 1), c("mm", "mm", "null"))
    design <- c(patchwork::area(1, 1, 1, 2), patchwork::area(2, 1), patchwork::area(2, 2),
                patchwork::area(3, 1), patchwork::area(3, 2))
    patchwork::wrap_plots(patchwork::guide_area() + theme(plot.margin = margin(0, 0, 0, 0)), rate$top, inact$top,
                          patchwork::free(rate$strip, side = "lr"), patchwork::free(inact$strip, side = "lr"),
                          design = design, widths = c(1, 1), heights = heights, guides = "collect") +
      patchwork::plot_annotation(title = "Light phase after CC1 (SIS; CON not modelled)",
                                 theme = theme(plot.title = element_text(size = BASE_PT, colour = INK, margin = margin(b = 1)),
                                               plot.margin = margin(top_margin_pt, 0, 0, 0))) &
      theme(legend.position = "top", legend.justification = "left", legend.box.spacing = unit(0, "pt"),
            legend.box.margin = margin(0, 0, 0, 3))
  }
  # Aligned mode: the legend row absorbs the difference, so the title stays under the letter.
  p <- if (is.null(panel_top_mm)) build(0) else s30_align_panel_top(build, panel_top_mm, w_mm, h_mm, "s30_panel_light_phase")
  bh_panel(p, w_mm, h_mm)
}

# ---------------------------------------------------------------- B3
#' B3. Compact light-phase estimate panel (Figure 1 option 2, panel d): two stacked mini
#' strips, rows F / M / F - M, no animal points, at Figure 1c mark weight
#' (S30_FOREST_COMPACT). The rate strip prints no number (ebb C2 carries no p for these rows)
#' and has its unit as x title; the inactivity strip prints the local BH q of the rows in
#' `q_rows` (default all three, as in B1, so the display is not selective; c("F", "INT") gives
#' the CANDIDATE_SPEC B3 minimum) and names its unit in its subtitle, so that its axis (the
#' panel's last) has only tick labels below it, like the Figure 1b axis beside it.
s30_panel_light_compact <- function(an, w_mm = S30_LIGHT_BOX$compact[1], h_mm = S30_LIGHT_BOX$compact[2],
                                    q_rows = c("F", "M", "INT"), top_margin_pt = 11.5) {
  q_all <- c(F = "lc_ia_rs_f_q", M = "lc_ia_rs_m_q", INT = "lc_ia_int_q")
  if (!all(q_rows %in% names(q_all))) stop("s30_panel_light_compact: q_rows must be among F, M, INT.", call. = FALSE)
  rate <- s30_light_rows(an, "rate", row_labels = S30_ROW_SHORT)
  inact <- s30_light_rows(an, "inactivity", q_keys = q_all[q_rows], row_labels = S30_ROW_SHORT)
  strip_theme <- theme(plot.title = element_text(size = BODY_PT, colour = INK, margin = margin(b = 0.5)),
                       plot.subtitle = element_text(size = NOTE_PT, colour = "grey25", lineheight = 1.0, margin = margin(b = 1)),
                       plot.title.position = "plot")
  # About three breaks: at the 35-mm box ggplot's default five rate ticks (-3 to 1) touch; the
  # inactivity strip keeps 0 and 0.005 (its data run -0.0048 to 0.0090; the zero line marks 0).
  s_rate <- s30_light_strip(rate, F1_UNIT_PER_H, right_text = NULL, title = "position-change rate",
                            subtitle = "Stage 29 secondary estimate;\nnot tested",
                            x_breaks = scales::breaks_pretty(n = 3), marks = S30_FOREST_COMPACT) + strip_theme
  s_inact <- s30_light_strip(inact, NULL, right_text = inact$q_text,
                             title = "positional inactivity (≥40 s)",
                             subtitle = paste0("fraction of light phase;\n", s30_tier_s30(an, "lc_family_m", sep = "\n")),
                             x_breaks = c(0, 0.005), marks = S30_FOREST_COMPACT, plot_margin = margin(2, 3, 3, 3)) + strip_theme
  p <- patchwork::wrap_plots(s_rate, s_inact, ncol = 1) +
    patchwork::plot_annotation(title = "Light phase (exploratory)", subtitle = "RES − SUS after CC1; SIS only",
                               theme = theme(plot.title = element_text(size = BASE_PT, colour = INK, margin = margin(b = 0.5)),
                                             plot.subtitle = element_text(size = NOTE_PT, colour = "grey25", margin = margin(b = 1)),
                                             plot.margin = margin(top_margin_pt, 0, 0, 0)))
  bh_panel(p, w_mm, h_mm)
}

# ---------------------------------------------------------------- B4
#' B4. Inactivity versus the position-change rate over all 444 light-phase animal-windows
#' (s30b S2; all RFID animals, no group colour; the points are the stored window values, not
#' centred), annotated from the S2b light row. Descriptive: no line, no p.
#'   top_margin_pt, panel_top_mm, panel_h_mm  see the file header; in aligned mode the space
#'   below the subtitle absorbs the difference, so the title stays under the letter.
s30_panel_light_dependence <- function(tabs, an, w_mm = S30_LIGHT_BOX$dependence[1], h_mm = S30_LIGHT_BOX$dependence[2],
                                       top_margin_pt = 11.5, panel_top_mm = NULL, panel_h_mm = NULL) {
  if (is.null(panel_top_mm) != is.null(panel_h_mm)) stop("s30_panel_light_dependence: give panel_top_mm and panel_h_mm together.", call. = FALSE)
  S2 <- tabs$s30b("S2_rate_inactivity_windows")
  lab <- sprintf("ρ = %s (Spearman,\nafter Batch × cage-change centring)\n%s animal-windows, %s animals, CC1–CC4\nresidual reliability after the rate = %s",
                 an$fa("ld_rho"), an$fa("ld_n_windows"), an$fa("ld_n_animals"), an$fa("ld_icc"))
  build <- function(subtitle_pad_pt) {
    p <- ggplot(S2, aes(crossing_rate, frac)) +
      geom_point(shape = 16, size = 0.45, alpha = 0.4, colour = "grey55") +
      annotate("text", x = 0.15, y = S30_INACT_LIMITS[1] + 0.0006, label = "axis starts at 0.975", hjust = 0, vjust = 0,
               size = NOTE_PT / .pt, colour = "grey25") +
      scale_x_continuous(limits = c(0, NA), breaks = c(0, 2, 4, 6, 8), expand = expansion(mult = c(0.02, 0.04))) +
      scale_y_continuous(limits = S30_INACT_LIMITS, breaks = S30_INACT_BREAKS, labels = S30_INACT_LABELS,
                         expand = expansion(mult = c(0, 0.03))) +
      labs(x = S30_DEP_X, y = S30_DEP_Y, title = "Inactivity tracks the rate",
           subtitle = "light phase; descriptive; all animals;\npoints uncentred; not a test", caption = lab) +
      theme_f1() +
      theme(plot.title = element_text(size = BASE_PT, colour = INK, hjust = 0, lineheight = 1.0, margin = margin(b = 0.5)),
            plot.subtitle = element_text(size = NOTE_PT, colour = "grey25", lineheight = 1.0, margin = margin(b = 1.5 + subtitle_pad_pt)),
            plot.caption = element_text(size = NOTE_PT, colour = INK, hjust = 0, lineheight = 1.05, margin = margin(t = 1.5)),
            plot.caption.position = "plot", plot.title.position = "plot",
            plot.margin = margin(top_margin_pt, 3, 3, 3))
    if (is.null(panel_h_mm)) return(p)
    patchwork::wrap_plots(p, patchwork::plot_spacer(), ncol = 1, heights = grid::unit(c(panel_h_mm, 1), c("mm", "null"))) +
      patchwork::plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0)))
  }
  p <- if (is.null(panel_top_mm)) build(0) else s30_align_panel_top(build, panel_top_mm, w_mm, h_mm, "s30_panel_light_dependence")
  bh_panel(p, w_mm, h_mm)
}

# ---------------------------------------------------------------- displayed data
#' The rows each builder displays, for the candidate source-data export (no file I/O; the
#' caller writes them). Built by the builders' own code paths, so they equal what is drawn.
#'   panel "rate" / "inactivity" (B1), "phase" (B2: both), "compact" (B3), "dependence" (B4).
#' Returns a named list of data frames:
#'   animals_<measure>  AnimalNum, Sex, Group and the plotted stored value (unjittered);
#'   estimates_<measure> row, label, the annotation keys, estimate, ci_low, ci_high and the
#'                       printed strings (printed_q empty where nothing is printed);
#'   windows            (B4) AnimalNum, CC, Batch, crossing_rate, frac as stored in S2;
#'   annotation         (B4) key, stored value and printed string of the S2b light row.
s30_light_panel_data <- function(panel = c("rate", "inactivity", "phase", "compact", "dependence"), tabs, an,
                                 q_rows = c("F", "M", "INT")) {
  panel <- match.arg(panel)
  est_out <- function(rows, measure, printed_ci) {
    data.frame(measure = measure, row = rows$row, label = rows$label, key = rows$key,
               estimate = rows$estimate, ci_low = rows$lo, ci_high = rows$hi,
               printed_ci = if (printed_ci) rows$ci_text else "", q_key = rows$q_key, printed_q = rows$q_text,
               stringsAsFactors = FALSE, row.names = NULL)
  }
  animals_out <- function(parts, column) {
    a <- parts$animals[, c("AnimalNum", "Sex", "Group", "y")]
    names(a)[4] <- column
    a$Sex <- as.character(a$Sex); a$Group <- as.character(a$Group)
    rownames(a) <- NULL
    a
  }
  out <- list()
  if (panel %in% c("rate", "phase")) {
    parts <- s30_light_measure_parts("rate", tabs, an, jitter_seed = 1L, standalone = FALSE)
    out$animals_rate <- animals_out(parts, "light_phase_crossing_rate")
    out$estimates_rate <- est_out(parts$estimates, "light_phase_crossing_rate", printed_ci = TRUE)
  }
  if (panel %in% c("inactivity", "phase")) {
    parts <- s30_light_measure_parts("inactivity", tabs, an, jitter_seed = 1L, standalone = FALSE)
    out$animals_inactivity <- animals_out(parts, "posinact40_light")
    out$estimates_inactivity <- est_out(parts$estimates, "posinact40_light", printed_ci = TRUE)
  }
  if (panel == "compact") {
    q_all <- c(F = "lc_ia_rs_f_q", M = "lc_ia_rs_m_q", INT = "lc_ia_int_q")
    out$estimates_rate <- est_out(s30_light_rows(an, "rate", row_labels = S30_ROW_SHORT), "light_phase_crossing_rate", FALSE)
    out$estimates_inactivity <- est_out(s30_light_rows(an, "inactivity", q_keys = q_all[q_rows], row_labels = S30_ROW_SHORT),
                                        "posinact40_light", FALSE)
  }
  if (panel == "dependence") {
    S2 <- tabs$s30b("S2_rate_inactivity_windows")
    out$windows <- S2[, c("AnimalNum", "CC", "Batch", "crossing_rate", "frac")]
    keys <- c("ld_rho", "ld_n_windows", "ld_n_animals", "ld_icc")
    out$annotation <- data.frame(key = keys, value = vapply(keys, function(k) as.numeric(an$ann(k)), numeric(1)),
                                 printed = vapply(keys, an$fa, character(1)), stringsAsFactors = FALSE, row.names = NULL)
  }
  out
}
