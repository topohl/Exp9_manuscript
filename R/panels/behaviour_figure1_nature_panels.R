# Figure 1 option 2, Nature layout (candidate figure_01_option2_nature): the panel builders of the
# opt-in style profile "nature" (BH_NATURE, R/panels/behaviour_figure_style.R).
# Rendering only: fits no model, computes no statistic.
#
# The Figure 1 builders (R/panels/behaviour_figure1_panels.R) and the compact light-phase builder
# (R/panels/behaviour_s30_light_panels.R) take `style = "figure1"` by default and draw exactly as
# before; with style = "nature" they return the builders below. Same tables, same stored values,
# same inks and fills; group marks are circles throughout (colour = group); what changes is layout, type and weight (NATURE_REDESIGN_SPEC 2-3):
#   a  f1n_panel_design       boxes 6 mm tall, 5.5-pt labels, 0.49-pt brackets, no timeline rule
#                             and dots, the n footer quoted by the legend;
#   b  f1n_panel_combz        no title; upright 5.5-pt strips; 5-pt threshold labels; 0.3-pt CON
#                             mean rule; the one figure group key on row 2's second line (the
#                             baseline of c's Holm P line), left-aligned at the plot panel;
#   c  f1n_panel_cc1          one header over both constructs; b's grammar (facet by sex, x = CON /
#                             RES / SUS); the model mean and its capless 95% CI just right of each
#                             RES / SUS cloud; one line per construct, centred over both sexes, that
#                             names its contrast ("Female − male: Holm P = ..."); the contrast CIs
#                             are quoted by the legend;
#   d  s30n_panel_light_compact  two strips of one width, zero at one x, symmetric presentational
#                             limits, units in the x titles, one right-aligned BH q column;
#   e  f1n_panel_association  no title; one-line x title; two statistics lines, the q as
#                             "2.61 × 10^−5" (f1n_sci_parts: three text nodes, the exponent at 5 pt);
#   f  f1n_panel_prediction   group circles coloured by group (no sex shapes, no key); square scatter; direct labels
#                             by the observed line and the null bars; the resampling range and the
#                             permutation count quoted by the legend.
# Every printed number is an$fa() of one annotation key (the value-for-value test reads the panel
# SVGs). Numbers never go through ggtext: statistics lines are plotmath, so each number is its own
# text node (italic P, q, n, rho and R-squared symbols; Unicode superscript two, which Arial has),
# and only unit superscripts (h^-1 at 5 pt) are ggtext spans.
#
# Frame. `frame = list(top = <mm>, axis = <mm>)` (contract args plot_top_mm, axis_mm) puts the top
# edge of the first plot panel and the bottom edge (the x-axis line) of the last one at those
# distances below the box top, so every panel of a row shares one plot frame. The builders pad
# their headers and bottom margins to reach it (f1n_align, measured on the svglite device the
# panels are written with). Presentational layout only. NULL leaves the natural layout.
#
# Requires: R/behaviour_bundle.R, R/panels/behaviour_figure_style.R, R/panels/behaviour_figure1_panels.R
# and R/panels/behaviour_s30_light_panels.R sourced first; packages ggtext and patchwork.

F1N <- BH_NATURE
# Group marks are circles in every panel (user directive 2026-09-30): colour alone differentiates CON /
# RES / SUS, so a shape never means group in one panel and something else in another.
F1N_GROUP_SHAPE <- GROUP_CIRCLE
# The superscript -1 of the per-hour unit, at the 5-pt floor (a plain <sup> would draw 4.4 pt).
F1N_PER_H <- "h<sup><span style='font-size:5pt'>−1</span></sup>"
# Axis titles, sentence case "Quantity (unit)".
F1N_CONSTRUCT_Y <- c(crossing_rate = paste0("Position-change rate (", F1N_PER_H, ")"),
                     shared_zone_use = "Shared occupancy (fraction of dyadic time)")
F1N_EARLY_RATE_X <- paste0("Early position-change rate after CC1 (", F1N_PER_H, ")")
F1N_RS_RATE_X <- paste0("RES − SUS (", F1N_PER_H, ")")
F1N_RS_INACT_X <- "RES − SUS (fraction of light phase)"

# Header on the letter's line: its baseline sits at the letter baseline (0.92 letter heights, 2.6 mm,
# below the box top, as the assembler draws the 8-pt letter), indented past the letter.
F1N_LETTER_BASELINE_MM <- 8 * 25.4 / 72 * 0.92
F1N_HEADER_TOP_PT <- 3.35         # top margin that puts a 5.5-pt header's baseline there (measured)
# Row 2's second line: c's Holm P line has its baseline 7.0 pt below the letter baseline (measured on
# the page). b's 5-pt group key sits on it: 3.15 pt of top margin would put the key's baseline on the
# letter baseline (measured), so the key takes 3.15 + 7.0.
F1N_KEY_TOP_PT <- 3.15 + 7.0
F1N_INDENT_PT <- F1N$header_indent_mm * 72 / 25.4

# b and c: animal clouds, and in c the model mean just right of each RES / SUS cloud (group units).
F1N_CLOUD_OFFSET <- -0.07
F1N_MEAN_OFFSET <- 0.27
# d: symmetric presentational x limits and breaks, one pair per strip (zero at one x in both).
F1N_LIGHT_RATE_LIMITS <- c(-3.5, 3.5); F1N_LIGHT_RATE_BREAKS <- c(-3, 0, 3)
F1N_LIGHT_INACT_LIMITS <- c(-0.0105, 0.0105); F1N_LIGHT_INACT_BREAKS <- c(-0.01, 0, 0.01)
# d: the q column's right edge, this far right of the strip panel (mm); the strips keep that much
# right margin (both, so the two panels have one width).
F1N_Q_RIGHT_MM <- 5.6

# The page background of a composed (patchwork) panel: white, without the default 1.07-pt white border.
F1N_BACKGROUND <- element_rect(fill = "white", colour = NA)

f1n_text <- function(pt = F1N$text_pt) pt / .pt
f1n_markdown <- function(...) ggtext::element_markdown(...)

#' A plotmath label: `template` is a plotmath string whose %s slots take the values `...`, each
#' inserted as a quoted string, so a printed number is drawn verbatim, as its own text node.
#' Pieces are juxtaposed with *, and a space is the last character of a string, never the first
#' (SVG drops a leading blank, so the piece would shift; plotmath's ~ and == would draw the space
#' and the equals sign from the symbol font, which the PDF would then embed). Never
#' a blank-only string. Example:
#' f1n_expr("'Holm ' * italic('P ') * '= ' * %s", "0.52").
f1n_expr <- function(template, ...) {
  vals <- as.character(c(...))
  if (any(grepl("'", vals, fixed = TRUE))) stop("f1n_expr: a value contains a single quote.", call. = FALSE)
  txt <- if (length(vals)) do.call(sprintf, c(list(template), as.list(sprintf("'%s'", vals)))) else template
  parse(text = txt, keep.source = FALSE)
}
#' The mantissa and exponent of a stored value printed in e-notation ("2.61e−5", as bh_sci_minus
#' writes it), for the Nature form 2.61 × 10^−5: the mantissa, "× 10" and the exponent are drawn as
#' three text nodes (the exponent in textstyle, i.e. at the line's 5 pt, not the 3.5-pt script size),
#' and the value-for-value test reads them back as the one stored string. No number is computed.
f1n_sci_parts <- function(x) {
  m <- regmatches(x, regexec("^(−?[0-9]+(?:\\.[0-9]+)?)e([−+]?[0-9]+)$", x))[[1]]
  if (length(m) != 3L) stop("f1n_sci_parts: '", x, "' is not an e-notation value.", call. = FALSE)
  c(mantissa = m[2], exponent = sub("^\\+", "", m[3]))
}
#' A text grob of a plotmath label (f1n_expr) for annotation_custom (5 pt, INK).
f1n_math_grob <- function(label, x, y, hjust, vjust = 0, colour = INK) {
  grid::textGrob(label, x = x, y = y, hjust = hjust, vjust = vjust,
                 gp = grid::gpar(fontsize = F1N$text_pt, fontfamily = "sans", col = colour))
}

# ---------------------------------------------------------------- frame
#' Distances (mm) below the box top of the top edge of the first plot panel and of the bottom edge
#' of the last one, for a plot drawn in a w x h mm box on the svglite device. Layout measurement only.
f1n_frame_mm <- function(p, w_mm, h_mm) {
  svglite::svgstring(width = w_mm / 25.4, height = h_mm / 25.4, bg = "white", fix_text_size = FALSE)
  on.exit(grDevices::dev.off(), add = TRUE)
  g <- if (inherits(p, "patchwork")) patchwork::patchworkGrob(p) else ggplot2::ggplotGrob(p)
  # plot panels: "panel", "panel-1-2", and in a patchwork a faceted plot's "panel; panel-1-1, ..."
  pan <- g$layout[grepl("^(free_)?panel(-[0-9]+(-[0-9]+)*|;.*)?$", g$layout$name), , drop = FALSE]
  if (!nrow(pan)) stop("f1n_frame_mm: the plot has no panel.", call. = FALSE)
  t <- min(pan$t); b <- max(pan$b); n <- length(g$heights)
  above <- if (t > 1L) g$heights[seq_len(t - 1L)] else NULL
  below <- if (b < n) g$heights[seq.int(b + 1L, n)] else NULL
  for (u in list(above, below)) if (!is.null(u) && any(grid::unitType(u) == "null"))
    stop("f1n_frame_mm: a relative row outside the panel rows.", call. = FALSE)
  mm <- function(u) if (is.null(u)) 0 else sum(grid::convertHeight(u, "mm", valueOnly = TRUE))
  c(top = mm(above), axis = h_mm - mm(below))
}

#' The plot build(top_pad_pt, bottom_pad_pt) whose frame is `frame` (see the file header): the
#' builder adds the top pad under its header and the bottom pad under its last axis. A few
#' fixed-point steps (the header is measured, not predicted); stops if the natural layout needs
#' more room than the frame gives.
f1n_align <- function(build, frame, w_mm, h_mm, what) {
  p <- build(0, 0)
  if (is.null(frame)) return(p)
  target <- c(top = as.numeric(frame$top), axis = as.numeric(frame$axis))
  m <- f1n_frame_mm(p, w_mm, h_mm)
  if (m[["top"]] > target[["top"]] + 0.02 || m[["axis"]] < target[["axis"]] - 0.02)
    stop(what, ": the natural layout needs plot_top_mm >= ", format(round(m[["top"]], 2)), " and axis_mm <= ",
         format(round(m[["axis"]], 2)), ".", call. = FALSE)
  pads <- c(0, 0)
  for (i in 1:6) {
    d <- c(target[["top"]] - m[["top"]], m[["axis"]] - target[["axis"]])
    if (all(abs(d) < 0.005)) break
    pads <- pmax(pads + d * 72 / 25.4, 0)
    p <- build(pads[1], pads[2])
    m <- f1n_frame_mm(p, w_mm, h_mm)
  }
  p
}

# =============================================================== a
f1n_panel_design <- function(tab, an, w_mm, h_mm, typography = "candidate") {
  cand <- f1_candidate_typography(typography)
  tl <- tab("A0_design_timeline")
  tl <- tl[order(tl$step), , drop = FALSE]
  DISPLAY <- F1_TIMELINE_DISPLAY
  if (cand) DISPLAY[] <- gsub("CC2-CC4", "CC2–CC4", DISPLAY, fixed = TRUE)
  if (!identical(sort(names(DISPLAY)), sort(tl$event)))
    stop("The bundle timeline events differ from the display map.", call. = FALSE)
  n <- nrow(tl)
  is_rec <- tl$role %in% c("primary window (CC1)", "trajectory windows (CC1-CC4)")
  is_out <- tl$role %in% c("outcome", "classification")
  STAGE <- data.frame(x0 = seq_len(n) - 0.45, x1 = seq_len(n) + 0.45, label = unname(DISPLAY[tl$event]),
                      fill = ifelse(is_rec, GREEN_MID, ifelse(is_out, NEUTRAL_BOX, GREEN_LIGHT)),
                      ink = ifelse(is_rec, "white", INK), stringsAsFactors = FALSE)
  STAGE$fill[tl$role == "reference"] <- GREEN_DARK; STAGE$ink[tl$role == "reference"] <- "white"
  REC_I <- which(is_rec | tl$role %in% c("reference", "stressor")); OUT_I <- which(is_out)
  # Vertical positions in mm below the box top (the y axis runs 0 at the top to -h at the bottom):
  # bracket labels on the letter's baseline, their rules under them, then the 6-mm boxes.
  Y_LAB <- -F1N_LETTER_BASELINE_MM; Y_RULE <- -3.9; Y_BOX <- c(-10.7, -4.7)
  lab_pt <- F1N$title_pt
  pa <- ggplot() +
    geom_rect(data = STAGE, aes(xmin = x0, xmax = x1, ymin = Y_BOX[1], ymax = Y_BOX[2], fill = I(fill)), colour = NA) +
    geom_text(data = STAGE, aes(x = (x0 + x1) / 2, y = (Y_BOX[1] + Y_BOX[2]) / 2, label = label, colour = I(ink)),
              size = f1n_text(lab_pt), lineheight = 0.92) +
    annotate("segment", x = min(REC_I) - 0.45, xend = max(REC_I) + 0.45, y = Y_RULE, yend = Y_RULE, linewidth = F1N$bracket_lw, colour = GREEN_DARK) +
    annotate("text", x = (min(REC_I) + max(REC_I)) / 2, y = Y_LAB, label = "home-cage RFID behaviour recorded", vjust = 0,
             size = f1n_text(lab_pt), colour = GREEN_DARK) +
    annotate("segment", x = min(OUT_I) - 0.45, xend = max(OUT_I) + 0.45, y = Y_RULE, yend = Y_RULE, linewidth = F1N$bracket_lw, colour = MUTED) +
    annotate("text", x = (min(OUT_I) + max(OUT_I)) / 2, y = Y_LAB, label = "outcome, CombZ and group labels derived", vjust = 0,
             size = f1n_text(lab_pt), colour = MUTED) +
    scale_x_continuous(limits = c(0.55, n + 0.45), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-h_mm, 0), expand = c(0, 0)) +
    theme_void(base_size = lab_pt, base_family = "sans") +
    theme(plot.margin = margin(0, 0, 0, 0))
  bh_panel(pa, w_mm, h_mm)
}

# =============================================================== b
f1n_panel_combz <- function(tab, an, w_mm, h_mm, jitter_seed, frame = NULL) {
  fa <- an$fa
  cls <- tab("A4_combz_animals")
  thr <- tab("A2b_combz_thresholds")
  cls$Sex <- factor(cls$Sex, levels = c("Female", "Male")); cls$Group <- factor(cls$Group, levels = GROUP_LEV)
  thr$Sex <- factor(thr$Sex, levels = c("Female", "Male"))
  thr$label <- ifelse(thr$Sex == "Female", fa("b_thr_female"), fa("b_thr_male"))
  key_aes <- list(size = F1N$point_size, stroke = F1N$point_stroke, colour = F1N$point_colour)
  build <- function(top_pad, bottom_pad) {
    ggplot(cls, aes(Group, CombZ)) +
      geom_hline(data = thr, aes(yintercept = control_mean_combz), linewidth = F1N$rule_lw, colour = RULE) +
      geom_hline(data = thr, aes(yintercept = susceptibility_threshold), linewidth = 0.45, colour = INK, linetype = "22") +
      geom_point(aes(fill = Group, shape = Group), position = position_jitter(width = F1N$jitter_width, height = 0, seed = jitter_seed),
                 size = F1N$point_size, stroke = F1N$point_stroke, colour = F1N$point_colour) +
      geom_label(data = thr, aes(x = 3.45, y = susceptibility_threshold, label = label), inherit.aes = FALSE,
                 hjust = 1, vjust = -0.25, size = f1n_text(), colour = INK, fill = "white", linewidth = 0,
                 label.padding = unit(0.5, "pt")) +
      facet_wrap(~ Sex, nrow = 1) +
      scale_fill_manual(values = GROUP_COL, name = NULL) + scale_shape_manual(values = F1N_GROUP_SHAPE, name = NULL) +
      guides(fill = guide_legend(override.aes = key_aes), shape = guide_legend()) +
      scale_x_discrete(expand = expansion(add = c(0.45, 0.45))) +
      scale_y_continuous(labels = f1_minus_labels) +
      labs(x = NULL, y = "Later CombZ") +
      theme_f1(style = "nature") +
      theme(legend.position = "top", legend.justification = "left", legend.location = "panel",
            legend.margin = margin(F1N_KEY_TOP_PT, 0, 0, 0), legend.box.spacing = unit(top_pad, "pt"),
            legend.key.spacing.x = unit(3.5, "pt"), panel.spacing = unit(4, "pt"),
            plot.margin = margin(0, 1, 1 + bottom_pad, 1))
  }
  bh_panel(f1n_align(build, frame, w_mm, h_mm, "f1n_panel_combz"), w_mm, h_mm)
}

# =============================================================== c
f1n_panel_cc1 <- function(tab, an, w_mm, h_mm, jitter_seed = c(NA, NA), frame = NULL) {
  fa <- an$fa
  A1 <- tab("A1_animal_cc1")
  C2 <- tab("C2_estimates")
  one <- function(k, key, seed, top_pad, bottom_pad, right_pt) {
    pts <- A1[is.finite(A1[[k]]), c("AnimalNum", "Sex", "Group", k)]; names(pts)[4] <- "y"
    pts$Sex <- factor(pts$Sex, levels = c("Female", "Male")); pts$Group <- factor(pts$Group, levels = GROUP_LEV)
    pts$x <- match(as.character(pts$Group), GROUP_LEV) + F1N_CLOUD_OFFSET
    mm <- f1_model_means(C2, "^mean_(RES|SUS)_CC1$", k); mm <- mm[mm$sex %in% c("Female", "Male"), , drop = FALSE]
    if (nrow(mm) != 4L) stop("panel c: expected 4 model means for ", k, call. = FALSE)
    mm$Sex <- factor(mm$sex, levels = c("Female", "Male")); mm$Group <- factor(mm$Group, levels = GROUP_LEV)
    mm$x <- match(as.character(mm$Group), GROUP_LEV) + F1N_MEAN_OFFSET
    # the P-CC1 Holm P of Q1, the female - male difference in RES - SUS: named, over both sexes
    holm <- f1n_expr("'Female − male: Holm ' * italic('P ') * '= ' * %s", fa(paste0("c_", key, "_q1_holm")))
    ggplot(pts, aes(x, y)) +
      geom_point(aes(fill = Group, shape = Group, colour = Group == "CON"),
                 position = position_jitter(width = F1N$jitter_width, height = 0, seed = seed),
                 size = F1N$point_size, stroke = F1N$point_stroke) +
      geom_linerange(data = mm, aes(x = x, ymin = ci_low, ymax = ci_high), inherit.aes = FALSE,
                     linewidth = F1N$mean_lw, colour = "black") +
      geom_point(data = mm, aes(x = x, y = estimate, shape = Group), inherit.aes = FALSE,
                 size = F1N$mean_size, stroke = F1N$mean_stroke, colour = "black", fill = "black") +
      facet_wrap(~ Sex, nrow = 1) +
      scale_fill_manual(values = c(CON = "white", GROUP_COL[c("RES", "SUS")]), guide = "none") +
      scale_colour_manual(values = c(`TRUE` = CON_GREY, `FALSE` = F1N$point_colour), guide = "none") +
      scale_shape_manual(values = F1N_GROUP_SHAPE, guide = "none") +
      scale_x_continuous(breaks = seq_along(GROUP_LEV), labels = GROUP_LEV, limits = c(0.6, 3.48), expand = c(0, 0)) +
      labs(x = NULL, y = F1N_CONSTRUCT_Y[[k]], title = holm) +
      theme_f1(style = "nature") +
      theme(axis.title.y = f1n_markdown(), panel.spacing = unit(4, "pt"),
            plot.title = element_text(size = F1N$text_pt, colour = INK, hjust = 0.5, margin = margin(b = 1 + top_pad)),
            plot.title.position = "panel",
            plot.margin = margin(0, right_pt, 1 + bottom_pad, 1))
  }
  build <- function(top_pad, bottom_pad) {
    parts <- list(one("crossing_rate", "cr", jitter_seed[1], top_pad, bottom_pad, right_pt = 6),
                  one("shared_zone_use", "sz", jitter_seed[length(jitter_seed)], top_pad, bottom_pad, right_pt = 1))
    patchwork::wrap_plots(parts, nrow = 1) +
      patchwork::plot_annotation(title = "First active phase after CC1",
                                 theme = theme(plot.title = element_text(size = F1N$title_pt, colour = INK, hjust = 0,
                                                                         margin = margin(F1N_HEADER_TOP_PT, 0, 2.4, F1N_INDENT_PT)),
                                               plot.margin = margin(0, 0, 0, 0), plot.background = F1N_BACKGROUND))
  }
  bh_panel(f1n_align(build, frame, w_mm, h_mm, "f1n_panel_cc1"), w_mm, h_mm)
}

# =============================================================== d
#' One light-phase strip: rows Female / Male / Female - male (y 3, 2, 1), the stored estimates and
#' 95% CIs (forest marks lighter than c's), and at right, optionally, the printed q column with its
#' "BH q" header on the tier-note line.
f1n_light_strip <- function(rows, x_title, limits, breaks, title, tier, q_col = FALSE, bottom_pad = 0) {
  marks <- utils::modifyList(BH_FOREST, F1N$forest)
  sex_rows <- rows[rows$row != "INT", , drop = FALSE]
  int_rows <- rows[rows$row == "INT", , drop = FALSE]
  q_x <- grid::unit(F1N_Q_RIGHT_MM, "mm")
  q_layers <- NULL
  if (q_col) {
    q_rows <- rows[nzchar(rows$q_text), , drop = FALSE]
    q_layers <- c(
      lapply(seq_len(nrow(q_rows)), function(i) annotation_custom(
        grid::textGrob(q_rows$q_value[i], x = q_x, y = grid::unit(0.5, "npc"), hjust = 1, vjust = 0.5,
                       gp = grid::gpar(fontsize = F1N$text_pt, fontfamily = "sans", col = INK)),
        xmin = Inf, xmax = Inf, ymin = q_rows$y[i], ymax = q_rows$y[i])),
      list(annotation_custom(f1n_math_grob(f1n_expr("'BH ' * italic('q')"), x = q_x, y = grid::unit(0.59, "mm"), hjust = 1),
                             xmin = Inf, xmax = Inf, ymin = Inf, ymax = Inf)))
  }
  ggplot(rows, aes(y = y)) +
    bh_forest_zero(marks = marks) +
    bh_forest_ci(aes(x = lo, xend = hi, yend = y), marks = marks) +
    bh_forest_point(aes(x = estimate), data = sex_rows, fill = INK, marks = marks) +
    bh_forest_int_point(aes(x = estimate), data = int_rows, marks = marks) +
    q_layers +
    scale_y_continuous(breaks = rows$y, labels = rows$label, limits = c(0.45, 3.55), expand = c(0, 0)) +
    scale_x_continuous(limits = limits, breaks = breaks, labels = s30_tick_labels, expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    labs(x = x_title, y = NULL, title = title, subtitle = tier) +
    theme_f1(style = "nature") +
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
          axis.text.y.left = element_text(size = F1N$text_pt, colour = INK, margin = margin(r = 1.5)),
          axis.title.x = f1n_markdown(margin = margin(t = 1.2)),
          plot.title = element_text(size = F1N$title_pt, colour = INK, margin = margin(b = 0.6)),
          plot.subtitle = element_text(size = F1N$text_pt, colour = "grey25", margin = margin(b = 1.6)),
          plot.title.position = "plot",
          plot.margin = margin(0, F1N_Q_RIGHT_MM * 72 / 25.4 + 0.5, 1 + bottom_pad, 1))
}

s30n_panel_light_compact <- function(an, w_mm, h_mm, q_rows = c("F", "M", "INT"), frame = NULL) {
  q_all <- c(F = "lc_ia_rs_f_q", M = "lc_ia_rs_m_q", INT = "lc_ia_int_q")
  if (!all(q_rows %in% names(q_all))) stop("s30n_panel_light_compact: q_rows must be among F, M, INT.", call. = FALSE)
  rate <- s30_light_rows(an, "rate", row_labels = S30_ROW_LABEL)
  inact <- s30_light_rows(an, "inactivity", q_keys = q_all[q_rows], row_labels = S30_ROW_LABEL)
  inact$q_value <- ""
  have_q <- !is.na(inact$q_key)
  inact$q_value[have_q] <- vapply(inact$q_key[have_q], an$fa, "")
  tier_inact <- sprintf("local BH, m = %s", an$fa("lc_family_m"))
  build <- function(top_pad, bottom_pad) {
    s_rate <- f1n_light_strip(rate, F1N_RS_RATE_X, F1N_LIGHT_RATE_LIMITS, F1N_LIGHT_RATE_BREAKS,
                              title = "Position-change rate", tier = "not tested") +
      theme(plot.margin = margin(0, F1N_Q_RIGHT_MM * 72 / 25.4 + 0.5, 5, 1))
    s_inact <- f1n_light_strip(inact, F1N_RS_INACT_X, F1N_LIGHT_INACT_LIMITS, F1N_LIGHT_INACT_BREAKS,
                               title = "Positional inactivity (≥40 s)", tier = tier_inact, q_col = TRUE,
                               bottom_pad = bottom_pad)
    patchwork::wrap_plots(s_rate, s_inact, ncol = 1) +
      patchwork::plot_annotation(title = "Light phase (exploratory)",
                                 theme = theme(plot.title = element_text(size = F1N$title_pt, colour = INK, hjust = 0,
                                                                         margin = margin(F1N_HEADER_TOP_PT, 0, 1.2 + top_pad, F1N_INDENT_PT)),
                                               plot.margin = margin(0, 0, 0, 0), plot.background = F1N_BACKGROUND))
  }
  bh_panel(f1n_align(build, frame, w_mm, h_mm, "s30n_panel_light_compact"), w_mm, h_mm)
}

# =============================================================== e
f1n_panel_association <- function(tab, an, w_mm, h_mm, typography = "candidate", frame = NULL) {
  cand <- f1_candidate_typography(typography)
  fa <- an$fa
  A2 <- tab("A2_prediction_animals")
  l1 <- f1n_expr("'Spearman ' * italic('ρ ') * '= ' * %s * %s", paste0(fa("e_rho"), " "), sprintf("[%s, %s]", fa("e_rho_lo"), fa("e_rho_hi")))
  l2 <- if (cand) {
    q <- f1n_sci_parts(bh_sci_minus(fa("e_q")))
    f1n_expr("'BH ' * italic('q ') * '= ' * %s * '× 10'^textstyle(%s) * ', ' * italic('n ') * '= ' * %s",
             paste0(q[["mantissa"]], " "), q[["exponent"]], fa("e_n"))
  } else f1n_expr("'BH ' * italic('q ') * '= ' * %s * ', ' * italic('n ') * '= ' * %s", fa("e_q"), fa("e_n"))
  right <- grid::unit(1, "npc") - grid::unit(0.6, "mm")
  build <- function(top_pad, bottom_pad) {
    ggplot(A2, aes(crossing_rate_equiv_per_h, observed_CombZ)) +
      geom_point(size = F1N$point_size, stroke = F1N$point_stroke, shape = 21, colour = F1N$point_colour, fill = "#6E8B99") +
      annotation_custom(f1n_math_grob(l1, x = right, y = grid::unit(1, "npc") - grid::unit(2.3, "mm"), hjust = 1)) +
      annotation_custom(f1n_math_grob(l2, x = right, y = grid::unit(1, "npc") - grid::unit(4.9, "mm"), hjust = 1)) +  # 0.4 mm lower than f's second line: room for the 5-pt exponent
      scale_y_continuous(expand = expansion(mult = c(0.04, 0.16)), labels = f1_minus_labels) +
      labs(x = F1N_EARLY_RATE_X, y = "Later CombZ") +
      theme_f1(style = "nature") +
      theme(axis.title.x = f1n_markdown(margin = margin(t = 1.5)),
            plot.margin = margin(top_pad, 1, 1 + bottom_pad, 1))
  }
  bh_panel(f1n_align(build, frame, w_mm, h_mm, "f1n_panel_association"), w_mm, h_mm)
}

# =============================================================== f
#' With a frame the scatter's plot panel is square: its width is the frame's panel height.
f1n_panel_prediction <- function(tab, an, w_mm, h_mm, typography = "candidate", frame = NULL) {
  f1_candidate_typography(typography)
  fa <- an$fa
  A2 <- tab("A2_prediction_animals")
  A2$Group <- factor(A2$Group, levels = GROUP_LEV)
  lim <- range(c(A2$observed_CombZ, A2$heldout_prediction), na.rm = TRUE)
  A3 <- tab("A3_permutation_draws")
  nulls <- A3[A3$model_id == "movement_mean" & !A3$is_observed, , drop = FALSE]
  obs <- an$ann("f_loao")
  lab_loao <- f1n_expr("'LOAO ' * italic('R² ') * '= ' * %s", fa("f_loao"))
  lab_base <- f1n_expr("'Baseline ' * italic('R² ') * '= ' * %s", fa("f_base"))
  lab_p <- f1n_expr("'Holm ' * italic('P ') * '= ' * %s", fa("f_perm_p"))
  left <- grid::unit(0.8, "mm")
  txt <- function(label, x, y, hjust, vjust = 0)
    grid::textGrob(label, x = x, y = y, hjust = hjust, vjust = vjust, gp = grid::gpar(fontsize = F1N$text_pt, fontfamily = "sans", col = INK))
  build <- function(top_pad, bottom_pad) {
    pf_scatter <- ggplot(A2, aes(observed_CombZ, heldout_prediction)) +
      geom_abline(slope = 1, intercept = 0, linetype = "22", linewidth = F1N$identity_lw, colour = "grey62") +
      geom_point(aes(fill = Group, shape = Group), size = F1N$point_size, stroke = F1N$point_stroke, colour = F1N$point_colour) +
      scale_fill_manual(values = GROUP_COL, guide = "none") + scale_shape_manual(values = F1N_GROUP_SHAPE, guide = "none") +
      scale_x_continuous(labels = f1_minus_labels) + scale_y_continuous(labels = f1_minus_labels) +
      coord_cartesian(xlim = lim, ylim = lim) +
      annotation_custom(f1n_math_grob(lab_loao, x = left, y = grid::unit(1, "npc") - grid::unit(2.3, "mm"), hjust = 0)) +
      annotation_custom(f1n_math_grob(lab_base, x = left, y = grid::unit(1, "npc") - grid::unit(4.5, "mm"), hjust = 0)) +
      labs(x = "Observed CombZ", y = "Held-out predicted CombZ") +
      theme_f1(style = "nature") +
      theme(axis.title.x = f1n_markdown(margin = margin(t = 1.5)), plot.margin = margin(top_pad, 9, 1 + bottom_pad, 1))
    pf_null <- ggplot(nulls, aes(performance_value)) +
      geom_histogram(bins = 24, fill = "grey78", colour = "white", linewidth = 0.15) +
      geom_vline(xintercept = obs, linewidth = 0.45, colour = GROUP_COL[["SUS"]]) +
      annotation_custom(txt("Observed", x = grid::unit(-0.8, "mm"), y = grid::unit(1, "npc") - grid::unit(2.3, "mm"), hjust = 1),
                        xmin = obs, xmax = obs) +
      annotation_custom(f1n_math_grob(lab_p, x = grid::unit(-0.8, "mm"), y = grid::unit(1, "npc") - grid::unit(4.5, "mm"), hjust = 1),
                        xmin = obs, xmax = obs) +
      annotation_custom(txt("Null (permuted outcomes)", x = grid::unit(0.19, "npc"), y = grid::unit(0.62, "npc"), hjust = 0)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.04))) +
      scale_x_continuous(labels = f1_minus_labels) +
      labs(x = "Null *R*²", y = "Permutations") +
      theme_f1(style = "nature") +
      theme(axis.title.x = f1n_markdown(margin = margin(t = 1.5)), plot.margin = margin(top_pad, 1, 1 + bottom_pad, 1))
    widths <- if (is.null(frame)) c(1, 1) else grid::unit(c(as.numeric(frame$axis) - as.numeric(frame$top), 1), c("mm", "null"))
    patchwork::wrap_plots(pf_scatter, pf_null, nrow = 1, widths = widths) +
      patchwork::plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0), plot.background = F1N_BACKGROUND))
  }
  bh_panel(f1n_align(build, frame, w_mm, h_mm, "f1n_panel_prediction"), w_mm, h_mm)
}
