# Figure 1 panel builders (behaviour). Rendering only: fits no model, computes no statistic.
#
# Extracted unchanged from figures/figure_01_panels.R. Each builder takes
#   tab   a table getter, function(name) -> the pinned behaviour-bundle table;
#   an    the annotation resolver from bh_annotation() (R/panels/behaviour_figure_style.R);
#   w_mm, h_mm   the box the panel is authored for (defaults: the Figure 1 contract box);
# and returns bh_panel(plot, w_mm, h_mm). Every printed number goes through an$fa()
# / an$ci() / an$ann(), i.e. through the annotation map to exactly one bundle cell.
# Plotted group means and intervals are the bundle's model-based estimates from
# the sex-stratified fits; points are the bundle's animal values; CON is
# descriptive only.
#
# Jitter. ggplot2 4.0.2 draws a position_jitter(seed = NA) seed from the global
# RNG stream when the plot is built (PositionJitter$setup_params), i.e. at print
# time, not when the layer is constructed. The Figure 1 renderer therefore builds
# a-f in order (panel b sets seed 1, each half of panel c sets seed 2) and then
# prints a-f in order; that sequence is what makes its SVGs byte-reproducible. A
# caller that builds or prints panels in another order should pass an explicit
# `jitter_seed` (the default NA keeps the Figure 1 behaviour).
#
# Candidate reuse (generation behaviour_v101_s30). The builders also serve the
# Figure 1 option-1/2 candidates and Extended Data X, through optional arguments
# whose defaults reproduce the Figure 1 renderer byte for byte:
#   f1_panel_combz(subtitle, minus_ticks)            a subtitle with a line break for
#                                                    narrow boxes; U+2212 tick labels;
#   f1_panel_cc1(text_position)                      "plot" aligns titles, captions and the
#                                                    group legend to the plot edge;
#   f1_panel_trajectory(constructs, keys, family,    one or more constructs (incl. the
#                       title, caption, show_con)    secondary occupancy_dispersion and
#                                                    fragmentation), their annotation-key
#                                                    stems, the Holm family printed,
#                                                    the overall title/caption, CON on/off;
#   f1_panel_association(minus_ticks), f1_panel_prediction(minus_ticks);
#   typography = "candidate" (design, cc1, association, prediction)
#                                                    CANDIDATE_SPEC A typography: en-dash
#                                                    ranges (18:30–06:30, CC1–CC4, 0.125–0.193),
#                                                    U+2212 in the e-notation exponent,
#                                                    spaced "RES − SUS" and "Female − male" labels;
#   compact_header = TRUE (cc1, trajectory, prediction)
#                                                    no patchwork outer margin (the 5.5-pt
#                                                    default), so the title sits under the
#                                                    panel letter at the Figure 1 top margin
#                                                    (trajectory: 3 pt kept under the caption);
#   title_position = "plot" (combz, trajectory,      titles (and the trajectory and prediction
#                    association, prediction)        legends) start at the plot edge, under
#                                                    the panel letter;
#   f1_panel_prediction(scatter_width)               the scatter's relative width (Figure 1f 0.6);
#   f1_panel_prediction(bottom_pad_pt)               extra bottom margin (Figure 1f 0), so the axes
#                                                    sit level with a two-line-x-title row partner;
#   f1_panel_association(title)                      an optional panel title;
#   style = "nature", frame (design, combz, cc1,     the opt-in Nature profile (BH_NATURE): the
#                   association, prediction)         builders return the f1n_* variants of
#                                                    R/panels/behaviour_figure1_nature_panels.R
#                                                    (frame: the shared plot frame of a row).
# They change layout and typography only: every printed number still comes
# from an$fa() / an$ci() / an$ann().
#
# Requires: R/behaviour_bundle.R and R/panels/behaviour_figure_style.R sourced first.

# Figure 1 contract boxes (figures/figure_contract.yml, absolute layout), w x h in mm.
F1_BOX <- list(a = c(173, 36), b = c(58, 66), c = c(113, 66), d = c(173, 62), e = c(66, 58), f = c(105, 58))

# Display labels for the two Stage 29 primary constructs (legacy identifiers as names),
# in the v1.0.1 terminology of the bundle's configuration (metrics/*/display_name, unit).
# The inverse hour is plotmath (as the R-squared superscript of panel f): Arial has no
# superscript-minus glyph, so a Unicode "h^-1" would fall back to another font.
F1_UNIT_PER_H <- expression("position changes h"^-1)
F1_CONSTRUCT_TITLE <- c(crossing_rate = "RFID position-change rate", shared_zone_use = "Shared RFID-position occupancy",
                        # Stage 29 secondary constructs (ebb I_analysis_config.json metrics/*/label, unit):
                        occupancy_dispersion = "Occupancy dispersion", fragmentation = "Fragmentation")
# The occupancy unit is on two lines: on one line it is longer than the plot height of panels c and d.
# Secondary units: occupancy dispersion is the Shannon entropy (bits) of the share of window
# occupancy time across the RFID positions; fragmentation is the proportion of position-change
# bouts that contain exactly one position change.
F1_CONSTRUCT_Y <- list(crossing_rate = F1_UNIT_PER_H, shared_zone_use = "fraction of co-assigned\ndyadic time",
                       occupancy_dispersion = "entropy across RFID\npositions (bits)",
                       fragmentation = "proportion of bouts with\none position change")

# Figure 1d: the annotation-key stem of each construct (keys <stem>_q2b_{df1,df2,f,holm}),
# the overall title, and the panel b subtitle.
F1_TRAJECTORY_KEYS <- c(crossing_rate = "d_cr", shared_zone_use = "d_sz")
F1_TRAJECTORY_TITLE <- "Active phase after each cage change: sex-stratified model estimates ± 95% CI (CON descriptive means, dashed)"
F1_COMBZ_SUBTITLE <- "dashed threshold defines RES/SUS; not a test"

#' Tick labels exactly as ggplot2's default continuous formatter writes them, but with the
#' typographic minus U+2212 (bh_minus). Opt-in (minus_ticks = TRUE); Figure 1 keeps the default.
f1_minus_labels <- function(x) {
  out <- bh_minus(format(x, trim = TRUE, justify = "left"))
  out[is.na(x)] <- NA
  out
}
# Two lines (one would be wider than panel e). textstyle(atop(displaystyle(.), displaystyle(.)))
# keeps both lines full size with text-style (closer) line spacing.
F1_EARLY_RATE_X <- expression(textstyle(atop(displaystyle("Early RFID position-change rate after CC1"),
                                             displaystyle("(position changes h"^-1 * ")"))))

# Display names of the bundle's design-timeline events (A0_design_timeline$event).
F1_TIMELINE_DISPLAY <- c(
  "first cage change (CC1)" = "Cage change\nCC1 (P25)",
  "first active phase after CC1" = "First active\nphase after CC1",
  "cage changes CC2-CC4" = "Cage changes\nCC2-CC4",
  "active phase after each of CC2-CC4" = "Active phase\nafter each change",
  "later behavioural outcome tests" = "Behavioural\noutcome tests",
  "terminal physiology" = "Terminal\nphysiology",
  "CombZ computed" = "CombZ\ncomputed",
  "RES/SUS assigned" = "Resilient /\nsusceptible")

#' TRUE for the candidate typography, FALSE for Figure 1's own (the default everywhere).
f1_candidate_typography <- function(typography) {
  if (!is.character(typography) || length(typography) != 1L || !typography %in% c("figure1", "candidate"))
    stop("typography must be \"figure1\" or \"candidate\".", call. = FALSE)
  identical(typography, "candidate")
}

#' The stored Stage 29 per-group model means of one construct (row filter only).
f1_model_means <- function(C2, estimand_prefix, construct) {
  x <- C2[C2$source == "stage29" & C2$construct == construct & grepl(estimand_prefix, C2$estimand), , drop = FALSE]
  x$Group <- sub("^mean_(RES|SUS)_.*$", "\\1", x$estimand)
  x$CC <- suppressWarnings(as.integer(sub("^.*_CC([1-4])$", "\\1", x$estimand)))
  x
}

# =============================================================== panel a
f1_panel_design <- function(tab, an, w_mm = F1_BOX$a[1], h_mm = F1_BOX$a[2], typography = "figure1", style = "figure1") {
  if (bh_style_nature(style)) return(f1n_panel_design(tab, an, w_mm, h_mm, typography = typography))
  cand <- f1_candidate_typography(typography)
  fa <- an$fa
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
  a_cap <- sprintf(if (cand) "RFID: %s animals, %s active-phase animal-windows (18:30–06:30) across CC1–CC4; CombZ: %s animals"
                   else "RFID: %s animals, %s active-phase animal-windows (18:30-06:30) across CC1-CC4; CombZ: %s animals",
                   fa("a_n_cc1_animals"), fa("a_n_windows"), fa("a_n_combz"))
  pa <- ggplot() +
    geom_rect(data = STAGE, aes(xmin = x0, xmax = x1, ymin = 0.30, ymax = 1.15, fill = I(fill)), colour = NA) +
    geom_text(data = STAGE, aes(x = (x0 + x1) / 2, y = 0.725, label = label, colour = I(ink)), size = BODY_PT / .pt, lineheight = 0.95) +
    annotate("segment", x = 0.5, xend = n + 0.5, y = 0, yend = 0, linewidth = 0.55, colour = "black") +
    geom_point(data = data.frame(x = seq_len(n)), aes(x, 0), size = 1.4, colour = "black") +
    annotate("segment", x = min(REC_I) - 0.45, xend = max(REC_I) + 0.45, y = 1.34, yend = 1.34, linewidth = 0.45, colour = GREEN_DARK) +
    annotate("text", x = (min(REC_I) + max(REC_I)) / 2, y = 1.55, label = "home-cage RFID behaviour recorded", size = BODY_PT / .pt, colour = GREEN_DARK) +
    annotate("segment", x = min(OUT_I) - 0.45, xend = max(OUT_I) + 0.45, y = 1.34, yend = 1.34, linewidth = 0.45, colour = MUTED) +
    annotate("text", x = (min(OUT_I) + max(OUT_I)) / 2, y = 1.55, label = "outcome, CombZ and group labels derived", size = BODY_PT / .pt, colour = MUTED) +
    scale_x_continuous(limits = c(0.4, n + 0.6), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-0.25, 1.8), expand = c(0, 0)) +
    labs(caption = a_cap) +
    theme_void(base_size = BODY_PT, base_family = "sans") +
    theme(plot.caption = element_text(size = NOTE_PT, colour = MUTED, hjust = 0, margin = margin(t = 0.5)),
          plot.margin = margin(11.5, 3, 2, 3))
  bh_panel(pa, w_mm, h_mm)
}

# =============================================================== panel b
# The threshold defines the groups; the separation is by construction, so no brackets and no stars.
f1_panel_combz <- function(tab, an, w_mm = F1_BOX$b[1], h_mm = F1_BOX$b[2], jitter_seed = NA,
                           subtitle = F1_COMBZ_SUBTITLE, minus_ticks = FALSE, title_position = "panel",
                           style = "figure1", frame = NULL) {
  if (bh_style_nature(style)) return(f1n_panel_combz(tab, an, w_mm, h_mm, jitter_seed = jitter_seed, frame = frame))
  if (!identical(title_position, "panel") && !identical(title_position, "plot"))
    stop("f1_panel_combz: title_position must be \"panel\" or \"plot\".", call. = FALSE)
  fa <- an$fa
  cls <- tab("A4_combz_animals")
  thr <- tab("A2b_combz_thresholds")
  cls$Sex <- factor(cls$Sex, levels = c("Female", "Male")); cls$Group <- factor(cls$Group, levels = GROUP_LEV)
  thr$Sex <- factor(thr$Sex, levels = c("Female", "Male"))
  thr$label <- ifelse(thr$Sex == "Female", fa("b_thr_female"), fa("b_thr_male"))
  set.seed(1)  # jitter is presentational only
  pb <- ggplot(cls, aes(Group, CombZ)) +
    geom_hline(data = thr, aes(yintercept = control_mean_combz), linewidth = 0.25, colour = RULE) +
    geom_hline(data = thr, aes(yintercept = susceptibility_threshold), linewidth = 0.45, colour = INK, linetype = "22") +
    geom_point(aes(fill = Group, shape = Group), position = position_jitter(width = 0.2, height = 0, seed = jitter_seed),
               size = 0.95, stroke = 0.2, colour = "grey20", alpha = 0.85) +
    geom_label(data = thr, aes(x = 3.45, y = susceptibility_threshold, label = label), inherit.aes = FALSE,
               hjust = 1, vjust = -0.25, size = NOTE_PT / .pt, colour = INK, fill = "white", label.size = 0,
               label.padding = unit(0.6, "pt")) +
    facet_wrap(~ Sex, nrow = 1) +
    scale_fill_manual(values = GROUP_COL, guide = "none") + scale_shape_manual(values = GROUP_SHAPE, guide = "none") +
    scale_x_discrete(expand = expansion(add = c(0.45, 0.45))) +
    (if (isTRUE(minus_ticks)) scale_y_continuous(labels = f1_minus_labels) else NULL) +
    labs(x = NULL, y = "Later CombZ", title = "Later composite outcome",
         subtitle = subtitle) +
    theme_f1()
  if (identical(title_position, "plot")) pb <- pb + theme(plot.title.position = "plot")
  bh_panel(pb, w_mm, h_mm)
}

# =============================================================== panel c
# Groups sit at fixed offsets within each sex; the model mean and CI are drawn just right of their own group.
# text_position "plot" aligns the construct title and the contrast caption to the plot's left
# edge instead of the panel's (for narrow boxes, where the panel-aligned text overruns the half).
f1_panel_cc1_one <- function(A1, C2, an, k, key, jitter_seed = NA, text_position = "panel", typography = "figure1") {
  cand <- f1_candidate_typography(typography)
  fa <- an$fa; ci <- an$ci
  OFF <- c(CON = -0.28, RES = 0, SUS = 0.28)
  C_TITLE <- F1_CONSTRUCT_TITLE
  C_Y <- F1_CONSTRUCT_Y
  pts <- A1[is.finite(A1[[k]]), c("AnimalNum", "Sex", "Group", k)]; names(pts)[4] <- "y"
  pts$Sex <- factor(pts$Sex, levels = c("Female", "Male")); pts$Group <- factor(pts$Group, levels = GROUP_LEV)
  pts$x <- as.numeric(pts$Sex) + OFF[as.character(pts$Group)]
  mm <- f1_model_means(C2, "^mean_(RES|SUS)_CC1$", k); mm <- mm[mm$sex %in% c("Female", "Male"), , drop = FALSE]
  if (nrow(mm) != 4L) stop("panel c: expected 4 model means for ", k, call. = FALSE)
  mm$Sex <- factor(mm$sex, levels = c("Female", "Male")); mm$Group <- factor(mm$Group, levels = GROUP_LEV)
  mm$x <- as.numeric(mm$Sex) + OFF[as.character(mm$Group)] + 0.11
  cap <- sprintf(if (cand) "RES − SUS, F: %s\nRES − SUS, M: %s\nFemale − male: %s\nP-CC1 Holm p = %s"
                 else "RES−SUS, F: %s\nRES−SUS, M: %s\nsex difference: %s\nP-CC1 Holm p = %s",
                 ci(paste0("c_", key, "_rs_f")), ci(paste0("c_", key, "_rs_m")), ci(paste0("c_", key, "_q1")), fa(paste0("c_", key, "_q1_holm")))
  set.seed(2)  # jitter is presentational only
  p <- ggplot(pts, aes(x, y)) +
    geom_point(aes(fill = Group, shape = Group, colour = Group == "CON"), position = position_jitter(width = 0.06, height = 0, seed = jitter_seed),
               size = 0.8, stroke = 0.18, alpha = 0.8) +
    geom_errorbar(data = mm, aes(x = x, ymin = ci_low, ymax = ci_high), inherit.aes = FALSE, width = 0.07, linewidth = 0.4, colour = "black") +
    geom_point(data = mm, aes(x = x, y = estimate), inherit.aes = FALSE, size = 1.2, colour = "black") +
    scale_fill_manual(values = c(CON = "white", GROUP_COL[c("RES", "SUS")]), drop = FALSE) +
    scale_colour_manual(values = c(`TRUE` = CON_MARK, `FALSE` = "grey20"), guide = "none") +
    scale_shape_manual(values = GROUP_SHAPE, drop = FALSE) +
    scale_x_continuous(breaks = 1:2, labels = c("Female", "Male"), limits = c(0.6, 2.5)) +
    labs(x = NULL, y = C_Y[[k]], title = C_TITLE[[k]], caption = cap) +
    theme_f1()
  if (identical(text_position, "plot"))
    p <- p + theme(plot.title.position = "plot", plot.caption.position = "plot")
  else if (!identical(text_position, "panel"))
    stop("f1_panel_cc1: text_position must be \"panel\" or \"plot\".", call. = FALSE)
  p
}

#' compact_header = TRUE: the overall title takes the Figure 1 top margin under the panel letter
#' (the patchwork default outer margin of 5.5 pt would put it level with the letter) and the two
#' construct plots start just below it, with the collected legend left-justified.
#' text_position = "plot": the construct titles start at the plot edge, and so does the group
#' legend. patchwork attaches a collected legend over the panel columns only, so in this mode the
#' legend stays with the first construct plot, placed against its plot edge (legend.location), and
#' the second construct plot, with the same marks, draws none (the f1_panel_trajectory pattern).
f1_panel_cc1 <- function(tab, an, w_mm = F1_BOX$c[1], h_mm = F1_BOX$c[2], jitter_seed = c(NA, NA), text_position = "panel",
                         typography = "figure1", compact_header = FALSE, style = "figure1", frame = NULL) {
  if (bh_style_nature(style)) return(f1n_panel_cc1(tab, an, w_mm, h_mm, jitter_seed = jitter_seed, frame = frame))
  A1 <- tab("A1_animal_cc1")
  C2 <- tab("C2_estimates")
  parts <- list(f1_panel_cc1_one(A1, C2, an, "crossing_rate", "cr", jitter_seed[1], text_position, typography),
                f1_panel_cc1_one(A1, C2, an, "shared_zone_use", "sz", jitter_seed[length(jitter_seed)], text_position, typography))
  ann <- patchwork::plot_annotation(title = "First active phase after CC1 (SIS; CON hollow, not modelled)",
                                    theme = theme(plot.title = element_text(size = BASE_PT, colour = INK)))
  pc <- if (identical(text_position, "plot")) {
    parts[[1]] <- parts[[1]] + theme(legend.position = "top", legend.justification = "left", legend.location = "plot")
    parts[[2]] <- parts[[2]] + theme(legend.position = "none")
    patchwork::wrap_plots(parts, nrow = 1, guides = "keep") + ann
  } else {
    patchwork::wrap_plots(parts, nrow = 1, guides = "collect") + ann &
      theme(legend.position = "top")
  }
  if (isTRUE(compact_header))
    pc <- (pc & theme(plot.margin = margin(1, 3, 3, 3))) +
      patchwork::plot_annotation(theme = theme(plot.margin = margin(11.5, 0, 0, 0), legend.justification = "left"))
  bh_panel(pc, w_mm, h_mm)
}

# =============================================================== panel d
# One construct: the stored TR_BY_SEX per-group model means +/- 95% CI (C2), CON's stored
# descriptive means (B2; show_con = FALSE drops them), and the construct's Q2b joint test (C3)
# with its Holm p in `family` (E_multiplicity) as subtitle. `key` is the annotation-key stem:
# the keys <key>_q2b_df1, _q2b_df2, _q2b_f and _q2b_holm must be declared in the resolver's map.
f1_panel_trajectory_one <- function(C2, B2, an, k, key, family = "P-TR", show_con = TRUE, group_shape = GROUP_SHAPE) {
  fa <- an$fa
  C_TITLE <- F1_CONSTRUCT_TITLE
  C_Y <- F1_CONSTRUCT_Y
  if (!k %in% names(C_TITLE) || !k %in% names(C_Y)) stop("f1_panel_trajectory: no display labels for construct ", k, call. = FALSE)
  mm <- f1_model_means(C2, "^mean_(RES|SUS)_TR_CC[1-4]$", k)
  if (nrow(mm) != 16L) stop("panel d: expected 16 model means for ", k, call. = FALSE)
  if (!all(grepl("^TR_BY_SEX\\|", mm$model_id))) stop("panel d: model means for ", k, " are not all from TR_BY_SEX fits", call. = FALSE)
  mm$Sex <- factor(mm$sex, levels = c("Female", "Male")); mm$Group <- factor(mm$Group, levels = GROUP_LEV)
  con_layers <- NULL
  if (isTRUE(show_con)) {
    con <- B2[B2$construct == k & B2$Group == "CON", c("CC", "Sex", "mean")]
    if (nrow(con) != 8L) stop("panel d: expected 8 CON descriptive means for ", k, call. = FALSE)
    con$CC <- as.integer(sub("CC", "", con$CC)); con$Sex <- factor(con$Sex, levels = c("Female", "Male"))
    con_layers <- list(
      geom_line(data = con, aes(CC, mean, group = 1), inherit.aes = FALSE, colour = CON_MARK, linetype = "22", linewidth = 0.4),
      geom_point(data = con, aes(CC, mean), inherit.aes = FALSE, colour = CON_MARK, fill = "white", shape = 21, size = 1.1, stroke = 0.35))
  }
  sub_txt <- sprintf("Q2b F(%s, %s) = %s, %s Holm p = %s", fa(paste0(key, "_q2b_df1")), fa(paste0(key, "_q2b_df2")),
                     fa(paste0(key, "_q2b_f")), family, fa(paste0(key, "_q2b_holm")))
  pd <- position_dodge(width = 0.3)
  ggplot(mm, aes(CC, estimate, colour = Group, group = Group)) +
    con_layers +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15, linewidth = 0.35, position = pd) +
    geom_line(linewidth = 0.45, position = pd) +
    geom_point(aes(shape = Group, fill = Group), size = 1.2, stroke = 0.25, colour = "grey15", position = pd) +
    facet_wrap(~ Sex, nrow = 1) +
    scale_colour_manual(values = GROUP_COL[c("RES", "SUS")]) + scale_fill_manual(values = GROUP_COL[c("RES", "SUS")]) +
    scale_shape_manual(values = group_shape[c("RES", "SUS")]) +
    scale_x_continuous(breaks = 1:4, labels = paste0("CC", 1:4)) +
    labs(x = NULL, y = C_Y[[k]], title = C_TITLE[[k]], subtitle = sub_txt) +
    theme_f1()
}

#' CC1-CC4 trajectories, one sub-plot per construct, side by side with one collected legend.
#'
#' constructs  legacy construct identifiers, drawn left to right (any of names(F1_CONSTRUCT_TITLE));
#' keys        named character vector, construct -> annotation-key stem (see f1_panel_trajectory_one);
#'             defaults to the Figure 1d stems, so secondary constructs must be given explicitly;
#' family      the Holm family printed after the Q2b test: one value, or one per construct
#'             ("P-TR" for the primary constructs, "S-TR-ORG" for the secondary ones);
#' title       overall title (NULL: none); caption  overall caption (NULL: none);
#' show_con    draw CON's stored descriptive means, dashed in the CON colour (Figure 1d: TRUE);
#' compact_header  TRUE: no patchwork outer margin at the top and sides (the 5.5-pt default)
#'             and 1 pt between the collected legend and the plots, so the first title sits at
#'             the Figure 1 top margin under the panel letter; the bottom keeps 3 pt under the
#'             caption, as every other panel's plot margin (Figure 1d: FALSE).
#' title_position  "panel" (Figure 1d) or "plot": the construct titles and subtitles start at
#'             the plot edge, and the one group legend (drawn by the first construct plot) is
#'             placed against the plot, not the panel, so both sit under the panel letter.
#' The defaults draw Figure 1d exactly.
f1_panel_trajectory <- function(tab, an, w_mm = F1_BOX$d[1], h_mm = F1_BOX$d[2],
                                constructs = c("crossing_rate", "shared_zone_use"),
                                keys = F1_TRAJECTORY_KEYS[constructs], family = "P-TR",
                                title = F1_TRAJECTORY_TITLE, caption = NULL, show_con = TRUE, compact_header = FALSE,
                                title_position = "panel", group_shape = GROUP_SHAPE) {
  if (!identical(title_position, "panel") && !identical(title_position, "plot"))
    stop("f1_panel_trajectory: title_position must be \"panel\" or \"plot\".", call. = FALSE)
  C2 <- tab("C2_estimates")
  B2 <- tab("B2_descriptive_summaries")
  if (!length(constructs) || anyDuplicated(constructs)) stop("f1_panel_trajectory: give one or more distinct constructs.", call. = FALSE)
  keys <- keys[constructs]
  if (anyNA(keys) || any(!nzchar(keys))) stop("f1_panel_trajectory: no annotation-key stem for ", paste(constructs[is.na(keys) | !nzchar(keys)], collapse = ", "), call. = FALSE)
  if (!length(family) %in% c(1L, length(constructs))) stop("f1_panel_trajectory: family must have length 1 or one per construct.", call. = FALSE)
  family <- rep_len(family, length(constructs))
  parts <- lapply(seq_along(constructs), function(i)
    f1_panel_trajectory_one(C2, B2, an, constructs[[i]], keys[[i]], family = family[[i]], show_con = show_con, group_shape = group_shape))
  ann <- list(title = title, theme = theme(plot.title = element_text(size = BASE_PT, colour = INK)))
  if (!is.null(caption))
    ann <- list(title = title, caption = caption,
                theme = theme(plot.title = element_text(size = BASE_PT, colour = INK),
                              plot.caption = element_text(size = NOTE_PT, colour = INK, hjust = 0, lineheight = 1.05, margin = margin(t = 1.5))))
  pd <- if (identical(title_position, "panel")) {
    patchwork::wrap_plots(parts, nrow = 1, guides = "collect") +
      do.call(patchwork::plot_annotation, ann) &
      theme(legend.position = "top")
  } else {
    # patchwork attaches collected guides over the panel columns only, so the legend stays with
    # the first construct plot, where ggplot2 places it against the plot edge (legend.location)
    # under its plot-anchored title; the other construct plots, with the same marks, draw none.
    parts[[1]] <- parts[[1]] + theme(plot.title.position = "plot", legend.position = "top", legend.location = "plot")
    parts[-1] <- lapply(parts[-1], function(p) p + theme(plot.title.position = "plot", legend.position = "none"))
    patchwork::wrap_plots(parts, nrow = 1, guides = "keep") +
      do.call(patchwork::plot_annotation, ann)
  }
  if (isTRUE(compact_header))
    pd <- (pd & theme(legend.box.spacing = unit(1, "pt"))) +
      patchwork::plot_annotation(theme = theme(plot.margin = margin(0, 0, 3, 0), legend.justification = "left"))
  bh_panel(pd, w_mm, h_mm)
}

# =============================================================== panel e
#' title_position  "panel" (Figure 1e) or "plot": the title and subtitle start at the plot edge.
f1_panel_association <- function(tab, an, w_mm = F1_BOX$e[1], h_mm = F1_BOX$e[2], minus_ticks = FALSE,
                                 typography = "figure1", title = NULL, title_position = "panel",
                                 style = "figure1", frame = NULL) {
  if (bh_style_nature(style)) return(f1n_panel_association(tab, an, w_mm, h_mm, typography = typography, frame = frame))
  if (!identical(title_position, "panel") && !identical(title_position, "plot"))
    stop("f1_panel_association: title_position must be \"panel\" or \"plot\".", call. = FALSE)
  cand <- f1_candidate_typography(typography)
  fa <- an$fa
  A2 <- tab("A2_prediction_animals")
  e_q <- if (cand) bh_sci_minus(fa("e_q")) else fa("e_q")
  e_lab <- sprintf("Spearman ρ = %s\n95%% CI [%s, %s]\nBH q = %s, n = %s", fa("e_rho"), fa("e_rho_lo"), fa("e_rho_hi"), e_q, fa("e_n"))
  pe <- ggplot(A2, aes(crossing_rate_equiv_per_h, observed_CombZ)) +
    geom_point(size = 1.05, stroke = 0.2, alpha = 0.9, shape = 21, colour = "grey20", fill = POOLED_FILL) +
    annotate("text", x = Inf, y = Inf, label = e_lab, hjust = 1.04, vjust = 1.15, size = NOTE_PT / .pt, colour = INK, lineheight = 1.08) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.22)), labels = if (isTRUE(minus_ticks)) f1_minus_labels else waiver()) +
    labs(x = F1_EARLY_RATE_X, y = "Later CombZ",
         subtitle = "one point per animal; rank correlation; not split by sex") +
    (if (!is.null(title)) labs(title = title) else NULL) +
    theme_f1()
  if (identical(title_position, "plot")) pe <- pe + theme(plot.title.position = "plot")
  bh_panel(pe, w_mm, h_mm)
}

# =============================================================== panel f
#' scatter_width  relative width of the observed-vs-predicted scatter against the permutation
#'                histogram (1); Figure 1f: 0.6. At 1 the square scatter is as tall as the histogram,
#'                so their x axes line up.
#' bottom_pad_pt  extra bottom plot margin (pt) of both component plots (Figure 1f: 0). A row
#'                partner with a two-line x title (the association panel) has its x axis higher
#'                than these one-line titles; the pad lifts both axes to its level, and the
#'                height-limited square scatter then fills the lower plot height.
#' title_position "panel" (Figure 1f) or "plot": the title and subtitle start at the plot edge
#'                under the panel letter, and the scatter's legend is placed against the plot.
f1_panel_prediction <- function(tab, an, w_mm = F1_BOX$f[1], h_mm = F1_BOX$f[2], minus_ticks = FALSE,
                                typography = "figure1", compact_header = FALSE, scatter_width = 0.6,
                                bottom_pad_pt = 0, title_position = "panel", style = "figure1", frame = NULL) {
  if (bh_style_nature(style)) return(f1n_panel_prediction(tab, an, w_mm, h_mm, typography = typography, frame = frame))
  if (!is.numeric(scatter_width) || length(scatter_width) != 1L || !(scatter_width > 0))
    stop("f1_panel_prediction: scatter_width must be one positive number.", call. = FALSE)
  if (!is.numeric(bottom_pad_pt) || length(bottom_pad_pt) != 1L || !(bottom_pad_pt >= 0))
    stop("f1_panel_prediction: bottom_pad_pt must be one number >= 0.", call. = FALSE)
  if (!identical(title_position, "panel") && !identical(title_position, "plot"))
    stop("f1_panel_prediction: title_position must be \"panel\" or \"plot\".", call. = FALSE)
  cand <- f1_candidate_typography(typography)
  minus_ticks <- isTRUE(minus_ticks)
  fa <- an$fa
  A2 <- tab("A2_prediction_animals")
  A2$Group <- factor(A2$Group, levels = GROUP_LEV); A2$Sex <- factor(A2$Sex, levels = c("Female", "Male"))
  lim <- range(c(A2$observed_CombZ, A2$heldout_prediction), na.rm = TRUE)
  f_lab <- sprintf("LOAO R² = %s\nbaseline R² = %s", fa("f_loao"), fa("f_base"))
  pf_scatter <- ggplot(A2, aes(observed_CombZ, heldout_prediction)) +
    geom_abline(slope = 1, intercept = 0, linetype = "22", linewidth = 0.35, colour = "grey62") +
    geom_point(aes(fill = Group, shape = Sex), size = 0.95, stroke = 0.22, colour = "grey20", alpha = 0.86) +
    scale_fill_manual(values = GROUP_COL) + scale_shape_manual(values = SEX_SHAPE) +
    (if (minus_ticks) list(scale_x_continuous(labels = f1_minus_labels), scale_y_continuous(labels = f1_minus_labels)) else NULL) +
    coord_equal(xlim = lim, ylim = lim) +
    annotate("text", x = -Inf, y = Inf, label = f_lab, hjust = -0.06, vjust = 1.2, size = NOTE_PT / .pt, colour = INK, lineheight = 1.08) +
    guides(fill = guide_legend(order = 1, override.aes = list(shape = 21, size = 1.5, alpha = 1)),
           shape = guide_legend(order = 2, override.aes = list(fill = "grey70", size = 1.5))) +
    labs(x = "Observed CombZ", y = "Held-out predicted CombZ", title = "Held-out prediction and permutation null",
         subtitle = "movement-mean model") +
    theme_f1()
  A3 <- tab("A3_permutation_draws")
  nulls <- A3[A3$model_id == "movement_mean" & !A3$is_observed, , drop = FALSE]
  obs <- an$ann("f_loao")
  pf_null <- ggplot(nulls, aes(performance_value)) +
    geom_histogram(bins = 24, fill = "grey78", colour = "white", linewidth = 0.15) +
    geom_vline(xintercept = obs, linewidth = 0.45, colour = GROUP_COL[["SUS"]]) +
    annotate("text", x = obs, y = Inf, vjust = 1.15, hjust = 1.06,
             label = sprintf(if (cand) "observed R² = %s\nHolm p = %s\n\nrepeated grouped 5-fold\nR² = %s (%s–%s\nacross repeats)"
                             else "observed R² = %s\nHolm p = %s\n\nrepeated grouped 5-fold\nR² = %s (%s-%s\nacross repeats)",
                             fa("f_loao"), fa("f_perm_p"), fa("f_rcv"), fa("f_rcv_lo"), fa("f_rcv_hi")),
             size = NOTE_PT / .pt, colour = INK, lineheight = 1.08) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
    (if (minus_ticks) scale_x_continuous(labels = f1_minus_labels) else NULL) +
    labs(x = expression("null R"^2 ~ "(permuted outcomes)"), y = sprintf("Permutations (n = %s)", fa("f_nperm"))) +
    theme_f1()
  if (bottom_pad_pt > 0) {
    pf_scatter <- pf_scatter + theme(plot.margin = margin(11.5, 3, 3 + bottom_pad_pt, 3))
    pf_null <- pf_null + theme(plot.margin = margin(11.5, 3, 3 + bottom_pad_pt, 3))
  }
  if (identical(title_position, "plot"))
    pf_scatter <- pf_scatter + theme(plot.title.position = "plot", legend.location = "plot")
  pf <- patchwork::wrap_plots(pf_scatter, pf_null, nrow = 1, widths = c(scatter_width, 1))
  if (isTRUE(compact_header)) pf <- pf + patchwork::plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0)))
  bh_panel(pf, w_mm, h_mm)
}
