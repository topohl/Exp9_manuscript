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
# Requires: R/behaviour_bundle.R and R/panels/behaviour_figure_style.R sourced first.

# Figure 1 contract boxes (figures/figure_contract.yml, absolute layout), w x h in mm.
F1_BOX <- list(a = c(173, 36), b = c(58, 66), c = c(113, 66), d = c(173, 62), e = c(66, 58), f = c(105, 58))

# Display labels for the two Stage 29 primary constructs (legacy identifiers as names).
F1_CONSTRUCT_TITLE <- c(crossing_rate = "Antenna-crossing rate", shared_zone_use = "Shared antenna-zone use")
F1_CONSTRUCT_Y <- c(crossing_rate = "crossings per hour", shared_zone_use = "fraction of dyadic time")

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

#' The stored Stage 29 per-group model means of one construct (row filter only).
f1_model_means <- function(C2, estimand_prefix, construct) {
  x <- C2[C2$source == "stage29" & C2$construct == construct & grepl(estimand_prefix, C2$estimand), , drop = FALSE]
  x$Group <- sub("^mean_(RES|SUS)_.*$", "\\1", x$estimand)
  x$CC <- suppressWarnings(as.integer(sub("^.*_CC([1-4])$", "\\1", x$estimand)))
  x
}

# =============================================================== panel a
f1_panel_design <- function(tab, an, w_mm = F1_BOX$a[1], h_mm = F1_BOX$a[2]) {
  fa <- an$fa
  tl <- tab("A0_design_timeline")
  tl <- tl[order(tl$step), , drop = FALSE]
  DISPLAY <- F1_TIMELINE_DISPLAY
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
  a_cap <- sprintf("RFID: %s animals, %s active-phase animal-windows (18:30-06:30) across CC1-CC4; CombZ: %s animals",
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
f1_panel_combz <- function(tab, an, w_mm = F1_BOX$b[1], h_mm = F1_BOX$b[2], jitter_seed = NA) {
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
    labs(x = NULL, y = "Later CombZ", title = "Later composite outcome",
         subtitle = "dashed threshold defines RES/SUS; not a test") +
    theme_f1()
  bh_panel(pb, w_mm, h_mm)
}

# =============================================================== panel c
# Groups sit at fixed offsets within each sex; the model mean and CI are drawn just right of their own group.
f1_panel_cc1_one <- function(A1, C2, an, k, key, jitter_seed = NA) {
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
  cap <- sprintf("RES−SUS, F: %s\nRES−SUS, M: %s\nsex difference: %s\nP-CC1 Holm p = %s",
                 ci(paste0("c_", key, "_rs_f")), ci(paste0("c_", key, "_rs_m")), ci(paste0("c_", key, "_q1")), fa(paste0("c_", key, "_q1_holm")))
  set.seed(2)  # jitter is presentational only
  ggplot(pts, aes(x, y)) +
    geom_point(aes(fill = Group, shape = Group, colour = Group == "CON"), position = position_jitter(width = 0.06, height = 0, seed = jitter_seed),
               size = 0.8, stroke = 0.18, alpha = 0.8) +
    geom_errorbar(data = mm, aes(x = x, ymin = ci_low, ymax = ci_high), inherit.aes = FALSE, width = 0.07, linewidth = 0.4, colour = "black") +
    geom_point(data = mm, aes(x = x, y = estimate), inherit.aes = FALSE, size = 1.2, colour = "black") +
    scale_fill_manual(values = c(CON = "white", GROUP_COL[c("RES", "SUS")]), drop = FALSE) +
    scale_colour_manual(values = c(`TRUE` = CON_GREY, `FALSE` = "grey20"), guide = "none") +
    scale_shape_manual(values = GROUP_SHAPE, drop = FALSE) +
    scale_x_continuous(breaks = 1:2, labels = c("Female", "Male"), limits = c(0.6, 2.5)) +
    labs(x = NULL, y = C_Y[[k]], title = C_TITLE[[k]], caption = cap) +
    theme_f1()
}

f1_panel_cc1 <- function(tab, an, w_mm = F1_BOX$c[1], h_mm = F1_BOX$c[2], jitter_seed = c(NA, NA)) {
  A1 <- tab("A1_animal_cc1")
  C2 <- tab("C2_estimates")
  pc <- patchwork::wrap_plots(f1_panel_cc1_one(A1, C2, an, "crossing_rate", "cr", jitter_seed[1]),
                              f1_panel_cc1_one(A1, C2, an, "shared_zone_use", "sz", jitter_seed[length(jitter_seed)]),
                              nrow = 1, guides = "collect") +
    patchwork::plot_annotation(title = "First active phase after CC1 (SIS; CON hollow grey, not modelled)",
                               theme = theme(plot.title = element_text(size = BASE_PT, colour = INK))) &
    theme(legend.position = "top")
  bh_panel(pc, w_mm, h_mm)
}

# =============================================================== panel d
f1_panel_trajectory_one <- function(C2, B2, an, k, key) {
  fa <- an$fa
  C_TITLE <- F1_CONSTRUCT_TITLE
  C_Y <- F1_CONSTRUCT_Y
  mm <- f1_model_means(C2, "^mean_(RES|SUS)_TR_CC[1-4]$", k)
  if (nrow(mm) != 16L) stop("panel d: expected 16 model means for ", k, call. = FALSE)
  mm$Sex <- factor(mm$sex, levels = c("Female", "Male")); mm$Group <- factor(mm$Group, levels = GROUP_LEV)
  con <- B2[B2$construct == k & B2$Group == "CON", c("CC", "Sex", "mean")]
  con$CC <- as.integer(sub("CC", "", con$CC)); con$Sex <- factor(con$Sex, levels = c("Female", "Male"))
  sub_txt <- sprintf("Q2b F(%s, %s) = %s, P-TR Holm p = %s", fa(paste0("d_", key, "_q2b_df1")), fa(paste0("d_", key, "_q2b_df2")),
                     fa(paste0("d_", key, "_q2b_f")), fa(paste0("d_", key, "_q2b_holm")))
  pd <- position_dodge(width = 0.3)
  ggplot(mm, aes(CC, estimate, colour = Group, group = Group)) +
    geom_line(data = con, aes(CC, mean, group = 1), inherit.aes = FALSE, colour = CON_GREY, linetype = "22", linewidth = 0.4) +
    geom_point(data = con, aes(CC, mean), inherit.aes = FALSE, colour = CON_GREY, fill = "white", shape = 21, size = 1.1, stroke = 0.35) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15, linewidth = 0.35, position = pd) +
    geom_line(linewidth = 0.45, position = pd) +
    geom_point(aes(shape = Group, fill = Group), size = 1.2, stroke = 0.25, colour = "grey15", position = pd) +
    facet_wrap(~ Sex, nrow = 1) +
    scale_colour_manual(values = GROUP_COL[c("RES", "SUS")]) + scale_fill_manual(values = GROUP_COL[c("RES", "SUS")]) +
    scale_shape_manual(values = GROUP_SHAPE[c("RES", "SUS")]) +
    scale_x_continuous(breaks = 1:4, labels = paste0("CC", 1:4)) +
    labs(x = NULL, y = C_Y[[k]], title = C_TITLE[[k]], subtitle = sub_txt) +
    theme_f1()
}

f1_panel_trajectory <- function(tab, an, w_mm = F1_BOX$d[1], h_mm = F1_BOX$d[2]) {
  C2 <- tab("C2_estimates")
  B2 <- tab("B2_descriptive_summaries")
  pd <- patchwork::wrap_plots(f1_panel_trajectory_one(C2, B2, an, "crossing_rate", "cr"),
                              f1_panel_trajectory_one(C2, B2, an, "shared_zone_use", "sz"), nrow = 1, guides = "collect") +
    patchwork::plot_annotation(title = "Active phase after each cage change: sex-stratified model estimates ± 95% CI (CON descriptive means, grey)",
                               theme = theme(plot.title = element_text(size = BASE_PT, colour = INK))) &
    theme(legend.position = "top")
  bh_panel(pd, w_mm, h_mm)
}

# =============================================================== panel e
f1_panel_association <- function(tab, an, w_mm = F1_BOX$e[1], h_mm = F1_BOX$e[2]) {
  fa <- an$fa
  A2 <- tab("A2_prediction_animals")
  e_lab <- sprintf("Spearman ρ = %s\n95%% CI [%s, %s]\nBH q = %s, n = %s", fa("e_rho"), fa("e_rho_lo"), fa("e_rho_hi"), fa("e_q"), fa("e_n"))
  pe <- ggplot(A2, aes(crossing_rate_equiv_per_h, observed_CombZ)) +
    geom_point(size = 1.05, stroke = 0.2, alpha = 0.9, shape = 21, colour = "grey20", fill = "#6E8B99") +
    annotate("text", x = Inf, y = Inf, label = e_lab, hjust = 1.04, vjust = 1.15, size = NOTE_PT / .pt, colour = INK, lineheight = 1.08) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.22))) +
    labs(x = "Early crossing rate after CC1 (crossings/h)", y = "Later CombZ",
         subtitle = "one point per animal; rank correlation; not split by sex") +
    theme_f1()
  bh_panel(pe, w_mm, h_mm)
}

# =============================================================== panel f
f1_panel_prediction <- function(tab, an, w_mm = F1_BOX$f[1], h_mm = F1_BOX$f[2]) {
  fa <- an$fa
  A2 <- tab("A2_prediction_animals")
  A2$Group <- factor(A2$Group, levels = GROUP_LEV); A2$Sex <- factor(A2$Sex, levels = c("Female", "Male"))
  lim <- range(c(A2$observed_CombZ, A2$heldout_prediction), na.rm = TRUE)
  f_lab <- sprintf("LOAO R² = %s\nbaseline R² = %s", fa("f_loao"), fa("f_base"))
  pf_scatter <- ggplot(A2, aes(observed_CombZ, heldout_prediction)) +
    geom_abline(slope = 1, intercept = 0, linetype = "22", linewidth = 0.35, colour = "grey62") +
    geom_point(aes(fill = Group, shape = Sex), size = 0.95, stroke = 0.22, colour = "grey20", alpha = 0.86) +
    scale_fill_manual(values = GROUP_COL) + scale_shape_manual(values = SEX_SHAPE) +
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
             label = sprintf("observed R² = %s\nHolm p = %s\n\nrepeated grouped 5-fold\nR² = %s (%s-%s\nacross repeats)",
                             fa("f_loao"), fa("f_perm_p"), fa("f_rcv"), fa("f_rcv_lo"), fa("f_rcv_hi")),
             size = NOTE_PT / .pt, colour = INK, lineheight = 1.08) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
    labs(x = expression("null R"^2 ~ "(permuted outcomes)"), y = sprintf("Permutations (n = %s)", fa("f_nperm"))) +
    theme_f1()
  pf <- patchwork::wrap_plots(pf_scatter, pf_null, nrow = 1, widths = c(0.6, 1))
  bh_panel(pf, w_mm, h_mm)
}
