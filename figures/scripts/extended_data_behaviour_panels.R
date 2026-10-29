#!/usr/bin/env Rscript
# ================================================================
# Script: figures/scripts/extended_data_behaviour_panels.R
# Stage: manuscript_figures
# Scope: global
# Consumes: the pinned canonical behaviour bundle (config/behaviour_bundle.yml) and the pinned figure-support
#           bundle (config/figure_support_bundle.yml), source_data/MMMSociability/<bundle_id>/, through
#           R/behaviour_bundle.R and R/figure_support_bundle.R
# Produces: results/figures/manuscript/extended_data_behaviour_panels/ and its render record
# Notes: Rendering only. Reads stored bundle cells; computes no statistic.
# ================================================================
#
# Extended Data 4: early behaviour and later integrated resilience, beyond Figure 1. Figure 1 shows one model and one
# association; this page shows what a reader needs to judge them: the two secondary features as animals, how the early
# features relate to each other, the complete a-priori model set with its permutation tests, whether any other
# behavioural domain adds to mean movement, and everything the study can say about sex. Redesigned 2026-10-29 (user
# request: richer, in the house look of Figure 1 and Extended Data 2-3; the ladder's sex-adjusted permutations run).
#
# Every value comes from the bundles Figure 1 is drawn from:
#   ebb C2_estimates / E_multiplicity: the Stage 09 associations (PR1), the ladder (PR2) and the feature x sex terms (PR3);
#   fsb E4b: the per-sex Spearman intervals (post hoc run 29d);
#   fsb D1: the Stage 14 domain heatmap cells (Figure 1c's table), here its first-active-phase heatmap cc1_a1;
#   fsb E5-E8: the post hoc run 29e (registry POSTHOC_PREDICTION_LADDER v1.0): the ladder with the within-sex permutation
#       of the two sex-adjusted models, the exploratory first-night domain rungs, the early-feature correlations, the
#       per-animal values with the display lines of the two secondary features, and the interactions per SD.
# Nothing here is fitted, resampled or corrected; every printed value is a stored cell.
#
# Panels (house profile of Figure 1: 5 / 5.5-pt text, borderless circles coloured by group, square scatters, square
# heatmap tiles, explanatory text in the legend):
#   a  movement RMSSD and entropy ACF1 against later integrated resilience, per animal, with the display line
#   b  Spearman correlations among the early features and the first-night Figure 1c domains (lower triangle)
#   c  the five a-priori models: predictors as a matrix, held-out R2 with the repeated-CV range and the permutation null
#   d  beyond movement (exploratory): each first-night domain alone and added to mean movement
#   e  the per-sex Spearman rho of the three features with their bootstrap intervals (descriptive)
#   f  the feature x sex interactions per SD of the feature, with the stored BH q
#   g  Figure 1c's domain heatmap for the first active phase after CC1 (fsb D1, heatmap cc1_a1; added 2026-10-29)
#
# WORDING DISCIPLINE. The sex panels license a negative statement and block a positive one; the renderer refuses to
# draw if any interaction is rejected in its stored family (PR3). The ladder names no winner and is drawn in registry
# order. Point fill encodes the evidence status of a tested row: supported (P or BH q < 0.05) in the palette's
# "supported" ink, otherwise its "descriptive" grey.

suppressPackageStartupMessages({
  library(ggplot2)
})

paths_file <- if (file.exists(file.path("R", "paths.R"))) file.path("R", "paths.R") else file.path("..", "R", "paths.R")
source(paths_file)
source(repo_path("R", "behaviour_bundle.R"))
source(repo_path("R", "stage30_bundle.R"))
source(repo_path("R", "figure_support_bundle.R"))

PIN <- behaviour_bundle_pin()
behaviour_bundle_verify(PIN)
FSB <- fsb_bundle_pin()
fsb_bundle_verify(FSB)
OUT <- path_results("figures", "manuscript", "extended_data_behaviour_panels")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

for (lib in c("behaviour_figure_style.R", "behaviour_figure1_panels.R", "behaviour_s30_light_panels.R", "behaviour_figure1_house_panels.R",
              "behaviour_figure1_option3_panels.R", "behaviour_stage14_panels.R"))
  source(repo_path("R", "panels", lib))

C2 <- behaviour_bundle_table("C2_estimates", PIN)
EM <- behaviour_bundle_table("E_multiplicity", PIN)
fsb <- function(name) fsb_bundle_table(name, FSB)
E4b <- fsb("E4b_spearman_by_sex_ci")
E5 <- fsb("E5_ladder_registered"); E5b <- fsb("E5b_ladder_beyond_movement"); E5c <- fsb("E5c_permutation_draws")
E6 <- fsb("E6_feature_correlations"); E7 <- fsb("E7_animal_features"); E7b <- fsb("E7b_secondary_fit_lines")
E8 <- fsb("E8_interaction_per_sd")
cell <- function(tab, column, ...) bundle_cell(tab, column, ...)

FEATURES <- c("Movement_mean", "Movement_rmssd", "Entropy_acf1")
FEATURE_LABEL <- c(Movement_mean = "Mean movement", Movement_rmssd = "Movement RMSSD", Entropy_acf1 = "Entropy ACF1")
DOMAINS <- c("shared_occupancy", "occupancy_dispersion", "temporal_flexibility", "temporal_volatility", "rest_like_posinact40")
DOMAIN_LABEL <- c(psychomotor_rate = "Position-change rate", shared_occupancy = "Shared occupancy", occupancy_dispersion = "Occupancy dispersion",
                  temporal_flexibility = "Temporal flexibility", temporal_volatility = "Temporal volatility",
                  rest_like_posinact40 = "Rest-like (≥40 s)")
REGISTERED <- c("mean_only", "movement_mean", "primary_behavior_family", "movement_mean_sex", "primary_behavior_family_sex")
SEX_LABEL <- c(Female = "Females", Male = "Males")
SEX_SHAPE <- c(Female = 16L, Male = 17L)                       # as Figure 1e
PAL <- yaml::read_yaml(repo_path("config", "manuscript_palette.yml"))
SUPPORTED <- PAL$evidence$supported; DESCRIPTIVE <- PAL$evidence$descriptive
RHO_LIMIT <- as.numeric(PAL$diverging_limits$correlation)
TILE <- as.numeric(PAL$heatmap$tile)                              # square tiles, even gap (palette heatmap$tile)
NULL_FILL <- "grey88"; CV_COL <- "grey62"; BAND_ALPHA <- 0.22; LINE_COL <- "grey20"
MINUS <- "−"

# ------------------------------------------------------------------ contracts
# Each guards a sentence the manuscript may write only while it holds.
pr3 <- EM[EM$family_id == "PR3", , drop = FALSE]
if (nrow(pr3) != 3L || any(as.logical(pr3$reject_at_alpha_adjusted)))
  stop("A feature-by-sex interaction is rejected in its stored family (PR3) (or PR3 is incomplete). Panels e and f present ",
       "sex as a negative result, which is no longer defensible. Redesign the panels; do not re-render them.", call. = FALSE)
if (!identical(E5$model_id, REGISTERED))
  stop("fsb E5 does not hold the five registered models in registry order.", call. = FALSE)
for (m in REGISTERED) for (k in c("loao_r2", "repeated_cv_mean_r2")) {
  est <- if (k == "loao_r2") "LOAO_R2" else "repeated_CV_mean_R2"
  if (!isTRUE(all.equal(E5[[k]][E5$model_id == m], cell(C2, "estimate", model_id = paste0("stage09_", m), estimand = est), tolerance = 1e-12)))
    stop("fsb E5 ", m, " ", k, " differs from the pinned ebb C2: the two bundles do not belong together.", call. = FALSE)
}
pr2 <- EM[EM$family_id == "PR2", , drop = FALSE]
for (m in c("movement_mean", "primary_behavior_family"))
  if (!isTRUE(all.equal(E5$permutation_p[E5$model_id == m], cell(pr2, "p_raw", member = paste0("permutation|", m)), tolerance = 1e-12)))
    stop("fsb E5 permutation P of ", m, " differs from ebb PR2.", call. = FALSE)
if (!is.na(E5$permutation_p[1]) || anyNA(E5$permutation_p[-1]))
  stop("Every registered model except the intercept-only baseline must carry a permutation P.", call. = FALSE)
if (nrow(E7) != 111L || anyNA(E7[, c("CombZ", FEATURES)])) stop("fsb E7 must hold the 111 animals with every feature.", call. = FALSE)
N_ANIMALS <- nrow(E7)

# ------------------------------------------------------------------ typography (string handling only)
mnum <- function(x, d = 2) sub("^-", MINUS, formatC(x, format = "f", digits = d))
pq <- function(x) sub("^-", MINUS, formatC(x, format = "fg", digits = 2))     # a stored P or q, two significant digits
txt_pt <- F1H$text_pt; ttl_pt <- F1H$title_pt
ev_col <- function(p) ifelse(!is.na(p) & p < 0.05, SUPPORTED, DESCRIPTIVE)
theme_ed <- function(top_pt = 0) theme_f1(style = "house") + theme(plot.margin = margin(top_pt, 1, 1, 1))
forest_y <- function() theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

# ================================================================== a: secondary features, per animal
a_one <- function(feature, first) {
  an <- E7; an$Group <- factor(an$Group, levels = GROUP_LEV); an$x <- an[[feature]]
  fl <- E7b[E7b$feature == feature, , drop = FALSE]; fl <- fl[order(fl$grid_index), , drop = FALSE]
  rho <- cell(C2, "estimate", model_id = "stage09_association", construct = feature)
  q <- cell(EM, "p_adjusted", family_id = "PR1", member = paste0("spearman|", feature))
  right <- grid::unit(1, "npc") - grid::unit(0.6, "mm"); top <- function(mm) grid::unit(1, "npc") - grid::unit(mm, "mm")
  ggplot(an, aes(x, CombZ)) +
    geom_ribbon(data = fl, aes(x = x, ymin = ci_low, ymax = ci_high), inherit.aes = FALSE, fill = F1O3H_DIVERGING[["low"]], alpha = BAND_ALPHA) +
    geom_line(data = fl, aes(x = x, y = fit), inherit.aes = FALSE, linewidth = 0.4, colour = LINE_COL) +
    geom_point(aes(colour = Group, shape = Sex), size = F1H$point_size + 0.2) +
    annotation_custom(f1h_math_grob(f1h_expr("'Spearman ' * italic('ρ ') * '= ' * %s", mnum(rho)), x = right, y = top(0.6), hjust = 1, vjust = 1)) +
    annotation_custom(f1h_math_grob(f1h_expr("'BH ' * italic('q ') * '= ' * %s", pq(q)), x = right, y = top(2.9), hjust = 1, vjust = 1)) +
    scale_colour_manual(values = GROUP_COL, guide = "none") + scale_shape_manual(values = SEX_SHAPE, guide = "none") +
    scale_x_continuous(labels = f1_minus_labels) +
    scale_y_continuous(labels = f1_minus_labels, breaks = c(-1, 0, 1), expand = expansion(mult = c(0.04, 0.10))) +
    labs(x = FEATURE_LABEL[[feature]], y = if (first) "Integrated Resilience (*z*)" else NULL) +
    theme_ed() + theme(aspect.ratio = 1, axis.title.y = f1h_markdown(margin = margin(r = 1)), axis.title.x = element_text(margin = margin(t = 1.5)))
}
# One key line for both scatters, on the letter's line and indented past it: sex by shape, group by colour (as Figure 1e).
KEY_A <- data.frame(x = c(0, 9.5, 19.5, 25, 30.5), label = c("Female", "Male", "CON", "RES", "SUS"),
                    shape = c(SEX_SHAPE[["Female"]], SEX_SHAPE[["Male"]], NA, NA, NA),
                    col = c("grey35", "grey35", GROUP_COL[["CON"]], s14_darken(GROUP_COL[["RES"]], 0.25), GROUP_COL[["SUS"]]))
key_a <- ggplot(KEY_A) +
  geom_point(data = KEY_A[!is.na(KEY_A$shape), ], aes(x, 0, shape = I(shape)), size = F1H$point_size + 0.2, colour = "grey35") +
  geom_text(aes(x = x + ifelse(is.na(shape), 0, 1.1), y = 0, label = label, colour = I(col)), hjust = 0, size = f1h_text()) +
  scale_x_continuous(limits = c(-4.5, 90), expand = c(0, 0)) + scale_y_continuous(limits = c(-1, 1)) +
  theme_void() + theme(plot.margin = margin(0, 0, 0, 0))
pa <- patchwork::wrap_plots(key_a, patchwork::wrap_plots(a_one("Movement_rmssd", TRUE), a_one("Entropy_acf1", FALSE), nrow = 1),
                            ncol = 1, heights = c(0.045, 1)) +
  patchwork::plot_annotation(theme = theme(plot.margin = margin(1.2, 0, 0, 0), plot.background = F1H_BACKGROUND))
a_assoc <- do.call(rbind, lapply(c("Movement_rmssd", "Entropy_acf1"), function(f) data.frame(
  Feature = FEATURE_LABEL[[f]], n = cell(C2, "n", model_id = "stage09_association", construct = f),
  `Spearman rho` = cell(C2, "estimate", model_id = "stage09_association", construct = f),
  `CI low (95%, bootstrap)` = cell(C2, "ci_low", model_id = "stage09_association", construct = f),
  `CI high (95%, bootstrap)` = cell(C2, "ci_high", model_id = "stage09_association", construct = f),
  P = cell(EM, "p_raw", family_id = "PR1", member = paste0("spearman|", f)),
  `q (BH across the three features)` = cell(EM, "p_adjusted", family_id = "PR1", member = paste0("spearman|", f)),
  check.names = FALSE, stringsAsFactors = FALSE)))
PA <- bh_panel(pa, 1, 1, data = list(
  animals = data.frame(`Animal ID` = E7$AnimalNum, Group = E7$Group, Sex = E7$Sex, `Integrated resilience (CombZ)` = E7$CombZ,
                       `Movement RMSSD` = E7$Movement_rmssd, `Entropy ACF1` = E7$Entropy_acf1, check.names = FALSE, stringsAsFactors = FALSE),
  display_line = data.frame(Feature = unname(FEATURE_LABEL[E7b$feature]), x = E7b$x, `Least-squares fit` = E7b$fit,
                            `CI low (95%)` = E7b$ci_low, `CI high (95%)` = E7b$ci_high, check.names = FALSE, stringsAsFactors = FALSE),
  association = a_assoc))

# ================================================================== b: correlations among the early features
B_ORDER <- c("Movement_mean", "psychomotor_rate", "rest_like_posinact40", "Movement_rmssd", "temporal_volatility",
             "Entropy_acf1", "temporal_flexibility", "occupancy_dispersion", "shared_occupancy")
B_LABEL <- c(FEATURE_LABEL, DOMAIN_LABEL)
if (!setequal(unique(E6$variable_1), B_ORDER) || nrow(E6) != length(B_ORDER)^2) stop("fsb E6 is not the 9 x 9 matrix.", call. = FALSE)
k <- length(B_ORDER)
cm <- E6; cm$i <- match(cm$variable_1, B_ORDER); cm$j <- match(cm$variable_2, B_ORDER)
low <- cm[cm$i > cm$j, , drop = FALSE]; low$x <- low$j; low$y <- k + 1 - low$i
low$label <- sub("^-", MINUS, sub("^(-?)0[.]", "\\1.", formatC(low$spearman_rho, format = "f", digits = 2)))
diag <- data.frame(i = seq_len(k), label = unname(B_LABEL[B_ORDER])); diag$x <- diag$i - 0.42; diag$y <- k + 1 - diag$i
rule <- data.frame(y = k + 1 - 3.5)                                                # Stage 09 features above, the rest below
pb_heat <- ggplot(low, aes(x, y)) +
  geom_tile(aes(fill = spearman_rho), width = TILE, height = TILE) +
  geom_text(aes(label = label), size = f1h_text(), colour = INK) +
  geom_text(data = diag, aes(x, y, label = label), hjust = 0, size = f1h_text(ttl_pt), colour = INK) +
  s14_fill(RHO_LIMIT, fixed = TRUE) +
  scale_x_continuous(limits = c(0.5, k + 3.6), expand = c(0, 0)) + scale_y_continuous(limits = c(0.5, k + 0.5), expand = c(0, 0)) +
  coord_fixed() + theme_void() + theme(plot.margin = margin(9, 1, 0, 1))
bl <- s14_bar_labels(RHO_LIMIT, fixed = TRUE)
barv <- data.frame(value = seq(-RHO_LIMIT, RHO_LIMIT, length.out = 61))
pb_bar <- ggplot(barv, aes(value, 1, fill = value)) + geom_tile(width = 2 * RHO_LIMIT / 60, height = 1) + s14_fill(RHO_LIMIT, fixed = TRUE) +
  scale_x_continuous(breaks = bl$breaks, labels = bl$labels, expand = c(0, 0)) + scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
  labs(x = "Spearman *ρ*", y = NULL) + theme_f1(style = "house") +
  theme(axis.line = element_blank(), axis.title.x = f1h_markdown(margin = margin(t = 1)), plot.margin = margin(1, 1, 1, 1))
pb <- pb_heat + patchwork::inset_element(pb_bar, left = 0.57, bottom = 0.74, right = 0.97, top = 0.90, align_to = "panel") +
  patchwork::plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0), plot.background = F1H_BACKGROUND))
PB <- bh_panel(pb, 1, 1, data = data.frame(`Variable 1` = unname(B_LABEL[cm$variable_1]), `Variable 2` = unname(B_LABEL[cm$variable_2]),
                                           n = cm$n, `Spearman rho` = cm$spearman_rho, Window = cm$window, check.names = FALSE, stringsAsFactors = FALSE)[
  order(cm$i, cm$j), , drop = FALSE])

# Forest rows of c, d and f share one grammar: the 95% range of the permutation null as a pale box, the repeated-CV
# range as a grey bar (its mean a white tick), the held-out estimate as a circle whose fill is the evidence status, and
# the P or q as a column just right of the plot panel (x = Inf, clip off), so the columns align whatever the x scale.
ROW_H <- 0.2
stat_col <- function(label_col) geom_text(aes(x = Inf, label = .data[[label_col]]), hjust = -0.12, size = f1h_text(), colour = INK)
stat_margin <- function(top_pt, right_mm) theme(plot.margin = margin(top_pt, right_mm * 72 / 25.4, 1, 1))
# The key of c and d, drawn once above c's rows at fixed offsets (mm) from the plot panel's left edge.
key_grobs <- function(y_mm_from_top) {
  top <- grid::unit(1, "npc") + grid::unit(y_mm_from_top, "mm")
  x0 <- c(0, 20.5, 36.5); mm <- function(v) grid::unit(v, "mm")
  g <- grid::gList(
    grid::rectGrob(x = mm(x0[1]), y = top, width = mm(3), height = mm(1.2), just = c(0, 0.5), gp = grid::gpar(fill = NULL_FILL, col = NA)),
    grid::segmentsGrob(x0 = mm(x0[2]), x1 = mm(x0[2] + 3), y0 = top, y1 = top, gp = grid::gpar(col = CV_COL, lwd = 1.5 * .pt, lineend = "butt")),
    grid::pointsGrob(x = mm(x0[3] + 0.9), y = top, pch = 16, size = mm(1.6), gp = grid::gpar(col = SUPPORTED)),
    grid::textGrob(c("Permutation null", "Repeated CV", "LOAO"), x = mm(x0 + c(3.8, 3.8, 2.4)), y = top,
                   hjust = 0, gp = grid::gpar(fontsize = txt_pt, fontfamily = "sans", col = INK)))
  annotation_custom(grid::grobTree(g))
}

# ================================================================== c: the five a-priori models
C_PRED <- c(Movement_mean = "Movement", Movement_rmssd = "RMSSD", Entropy_acf1 = "ACF1", Sex = "Sex")
lad <- E5; lad$y <- rev(seq_len(nrow(lad)))
inc <- do.call(rbind, lapply(seq_len(nrow(lad)), function(r) {
  pr <- if (is.na(lad$predictors[r]) || !nzchar(lad$predictors[r])) character(0) else strsplit(lad$predictors[r], " + ", fixed = TRUE)[[1]]
  if (length(setdiff(pr, names(C_PRED)))) stop("E5 model ", lad$model_id[r], " has a predictor this panel cannot draw.", call. = FALSE)
  data.frame(y = lad$y[r], x = seq_along(C_PRED), included = names(C_PRED) %in% pr)
}))
seg <- do.call(rbind, lapply(split(inc[inc$included, ], inc$y[inc$included]), function(z) data.frame(y = z$y[1], x0 = min(z$x), x1 = max(z$x))))
ROLE <- data.frame(label = c("Baseline", "Behaviour", "Sex-adjusted"), y0 = c(5, 3, 1), y1 = c(5, 4, 2))
C_YLIM <- c(0.5, 5.5)
pc_mat <- ggplot() +
  geom_segment(data = seg, aes(x = x0, xend = x1, y = y, yend = y), linewidth = 0.35, colour = INK) +
  geom_point(data = inc[inc$included, ], aes(x, y), size = 1.3, colour = INK) +
  geom_point(data = inc[!inc$included, ], aes(x, y), size = 1.3, colour = "grey86") +
  geom_segment(data = ROLE[ROLE$y0 != ROLE$y1, ], aes(x = 0.1, xend = 0.1, y = y0 - 0.3, yend = y1 + 0.3), linewidth = 0.25, colour = INK) +
  geom_text(data = ROLE, aes(x = -0.25, y = (y0 + y1) / 2, label = label), hjust = 1, size = f1h_text(ttl_pt), colour = INK) +
  scale_x_continuous(breaks = seq_along(C_PRED), labels = unname(C_PRED), position = "top", limits = c(-5.6, length(C_PRED) + 0.5), expand = c(0, 0)) +
  scale_y_continuous(limits = C_YLIM, expand = c(0, 0)) + labs(x = NULL, y = NULL) +
  theme_ed(9) + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text.y = element_blank(),
                      axis.text.x.top = element_text(size = txt_pt, angle = 90, hjust = 0, vjust = 0.5, colour = INK))
lad$p_text <- ifelse(is.na(lad$permutation_p), "–", paste0("P = ", pq(lad$permutation_p)))
lad$fill <- ifelse(lad$model_id == "mean_only", DESCRIPTIVE, ev_col(lad$permutation_p))
pc_for <- ggplot(lad, aes(y = y)) +
  bh_forest_zero() +
  geom_rect(aes(xmin = null_q025, xmax = null_q975, ymin = y - ROW_H, ymax = y + ROW_H), fill = NULL_FILL, na.rm = TRUE) +
  geom_segment(aes(x = cv_r2_q025, xend = cv_r2_q975, yend = y), linewidth = 1.5, colour = CV_COL, lineend = "butt") +
  geom_point(aes(x = repeated_cv_mean_r2), shape = 124, size = 1.5, colour = "white") +
  geom_point(aes(x = loao_r2, colour = I(fill)), shape = 16, size = 1.9) +
  stat_col("p_text") + key_grobs(3.2) +
  scale_x_continuous(labels = f1_minus_labels, expand = expansion(mult = c(0.03, 0.04))) +
  scale_y_continuous(limits = C_YLIM, expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(x = "Cross-validated *R*²", y = NULL) +
  theme_ed() + forest_y() + stat_margin(9, 10) + theme(axis.text.y = element_blank(), axis.title.x = f1h_markdown(margin = margin(t = 1.5)))
pc <- patchwork::wrap_plots(pc_mat, pc_for, nrow = 1, widths = c(0.42, 1)) +
  patchwork::plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0), plot.background = F1H_BACKGROUND))
PC <- bh_panel(pc, 1, 1, data = list(
  models = data.frame(Model = lad$model_id, Role = lad$role, Predictors = lad$predictors, n = lad$n, `LOAO R2` = lad$loao_r2,
                      `Repeated-CV mean R2` = lad$repeated_cv_mean_r2, `Repeated-CV 2.5th percentile` = lad$cv_r2_q025,
                      `Repeated-CV 97.5th percentile` = lad$cv_r2_q975, `Permutation scheme` = lad$permutation_scheme,
                      `Permutation P (raw)` = lad$permutation_p, `P adjusted (Holm, family)` = lad$p_adjusted, Family = lad$family,
                      `Null 2.5th percentile` = lad$null_q025, `Null 97.5th percentile` = lad$null_q975, `Not tested` = lad$not_tested_reason,
                      check.names = FALSE, stringsAsFactors = FALSE)))

# ================================================================== d: beyond movement (exploratory)
bm <- E5b
if (!setequal(bm$model_id, c(paste0("alone_", DOMAINS), paste0("movement_plus_", DOMAINS), "movement_plus_all_domains")))
  stop("fsb E5b does not hold the eleven registered exploratory models.", call. = FALSE)
D_ROWS <- c(DOMAINS, "all")
D_LABEL <- c(DOMAIN_LABEL[DOMAINS], all = "All five")
bm$row <- ifelse(bm$kind == "added_all", "all", bm$domain)
bm$y <- length(D_ROWS) + 1 - match(bm$row, D_ROWS)
bm$lo <- ifelse(bm$kind == "alone", bm$cv_r2_q025, bm$delta_cv_q025); bm$hi <- ifelse(bm$kind == "alone", bm$cv_r2_q975, bm$delta_cv_q975)
bm$est <- bm$observed_statistic
bm$cv_mid <- ifelse(bm$kind == "alone", bm$repeated_cv_mean_r2, bm$delta_cv_mean)
bm$q_text <- paste0("q = ", pq(bm$q_bh)); bm$fill <- ev_col(bm$q_bh)
mov <- E5$loao_r2[E5$model_id == "movement_mean"]
D_YLIM <- c(0.5, length(D_ROWS) + 0.5)
d_one <- function(kind, title, x_title, labels) {
  z <- bm[if (kind == "alone") bm$kind == "alone" else bm$kind != "alone", , drop = FALSE]
  p <- ggplot(z, aes(y = y)) + bh_forest_zero()
  if (kind == "alone")
    p <- p + geom_vline(xintercept = mov, linetype = "22", linewidth = 0.3, colour = SUPPORTED) +
      annotate("text", x = mov, y = D_YLIM[2] - 0.04, label = "Mean movement", hjust = 1.06, vjust = 1, size = f1h_text(), colour = SUPPORTED)
  p + geom_rect(aes(xmin = null_q025, xmax = null_q975, ymin = y - ROW_H, ymax = y + ROW_H), fill = NULL_FILL) +
    geom_segment(aes(x = lo, xend = hi, yend = y), linewidth = 1.5, colour = CV_COL, lineend = "butt") +
    geom_point(aes(x = cv_mid), shape = 124, size = 1.5, colour = "white") +
    geom_point(aes(x = est, colour = I(fill)), shape = 16, size = 1.9) +
    stat_col("q_text") +
    scale_x_continuous(labels = f1_minus_labels, breaks = function(l) pretty(l, n = 3), expand = expansion(mult = c(0.04, 0.04))) +
    scale_y_continuous(breaks = rev(seq_along(D_ROWS)), labels = if (labels) unname(D_LABEL[D_ROWS]) else NULL, limits = D_YLIM, expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    labs(x = x_title, y = NULL, title = title) +
    theme_ed() + forest_y() + stat_margin(9, 8.5) +
    theme(axis.text.y = element_text(size = ttl_pt, colour = INK), axis.title.x = f1h_markdown(margin = margin(t = 1.5)),
          plot.title = element_text(size = ttl_pt, face = "bold", colour = INK, hjust = 0, margin = margin(b = 4)))
}
pd <- patchwork::wrap_plots(d_one("alone", "Alone", "Cross-validated *R*²", TRUE),
                            d_one("added", "Added to mean movement", "Change in *R*²", FALSE), nrow = 1) +
  patchwork::plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0), plot.background = F1H_BACKGROUND))
PD <- bh_panel(pd, 1, 1, data = data.frame(Model = bm$model_id, Kind = bm$kind, Domain = bm$domain, n = bm$n,
  `Animals with an imputed domain value` = bm$n_missing_imputed, `Domain Spearman rho with mean movement` = bm$domain_spearman_with_movement,
  `LOAO R2` = bm$loao_r2, `Repeated-CV mean R2` = bm$repeated_cv_mean_r2, `Repeated-CV 2.5th percentile` = bm$cv_r2_q025,
  `Repeated-CV 97.5th percentile` = bm$cv_r2_q975, `Change in LOAO R2 over mean movement` = bm$delta_loao_r2,
  `Paired repeated-CV change, mean` = bm$delta_cv_mean, `Paired repeated-CV change, 2.5th percentile` = bm$delta_cv_q025,
  `Paired repeated-CV change, 97.5th percentile` = bm$delta_cv_q975, `Permutation scheme` = bm$permutation_scheme,
  `Null 2.5th percentile` = bm$null_q025, `Null 97.5th percentile` = bm$null_q975, `Permutation P` = bm$permutation_p,
  `q (BH, family)` = bm$q_bh, Family = bm$family, check.names = FALSE, stringsAsFactors = FALSE))

# ================================================================== e: per-sex Spearman rho (descriptive)
es <- E4b[E4b$feature %in% FEATURES, , drop = FALSE]
if (nrow(es) != 6L) stop("fsb E4b must hold 3 features x 2 sexes.", call. = FALSE)
es$y <- length(FEATURES) + 1 - match(es$feature, FEATURES)
strip <- vapply(names(SEX_LABEL), function(s) paste0(SEX_LABEL[[s]], " (n = ", unique(es$n[es$Sex == s]), ")"), "")
es$facet <- factor(strip[es$Sex], levels = unname(strip))
pe <- ggplot(es, aes(y = y)) +
  bh_forest_zero() +
  geom_segment(aes(x = ci_low, xend = ci_high, yend = y), linewidth = 0.55, colour = INK, lineend = "butt") +
  geom_point(aes(x = rho), shape = 16, size = 1.9, colour = INK) +
  facet_wrap(~facet, nrow = 1) +
  scale_x_continuous(labels = f1_minus_labels, breaks = c(-0.6, -0.3, 0)) +
  scale_y_continuous(breaks = rev(seq_along(FEATURES)), labels = unname(FEATURE_LABEL[FEATURES]), limits = c(0.5, length(FEATURES) + 0.5)) +
  labs(x = "Spearman *ρ* with later integrated resilience", y = NULL) +
  theme_ed(9) + forest_y() +
  theme(axis.text.y = element_text(size = ttl_pt, colour = INK), axis.title.x = f1h_markdown(margin = margin(t = 1.5)),
        strip.text = element_text(size = ttl_pt, face = "bold", colour = INK, hjust = 0), panel.spacing.x = grid::unit(4, "mm"))
PE <- bh_panel(pe, 1, 1, data = data.frame(Feature = unname(FEATURE_LABEL[es$feature]), Sex = es$Sex, n = es$n, `Spearman rho` = es$rho,
                                           `CI low (95%)` = es$ci_low, `CI high (95%)` = es$ci_high, Interval = es$interval,
                                           check.names = FALSE, stringsAsFactors = FALSE))

# ================================================================== f: feature x sex interactions per SD
ix <- E8[match(FEATURES, E8$feature), , drop = FALSE]
ix$y <- length(FEATURES) + 1 - seq_len(nrow(ix))
ix$q_text <- paste0("q = ", pq(ix$q_bh_stored)); ix$fill <- ev_col(ix$q_bh_stored)
pf <- ggplot(ix, aes(y = y)) +
  bh_forest_zero() +
  geom_segment(aes(x = ci_low_per_sd, xend = ci_high_per_sd, yend = y, colour = I(fill)), linewidth = 0.55, lineend = "butt") +
  geom_point(aes(x = estimate_per_sd, colour = I(fill)), shape = 16, size = 1.9) +
  stat_col("q_text") +
  scale_x_continuous(labels = f1_minus_labels, breaks = c(-0.2, 0, 0.2, 0.4)) +
  scale_y_continuous(breaks = rev(seq_along(FEATURES)), labels = unname(FEATURE_LABEL[FEATURES]), limits = c(0.5, length(FEATURES) + 0.5)) +
  coord_cartesian(clip = "off") +
  labs(x = paste0("Male ", MINUS, " female slope (per SD)"), y = NULL, title = "Feature × sex interaction") +
  theme_ed() + forest_y() + stat_margin(9, 8.5) +
  theme(axis.text.y = element_text(size = ttl_pt, colour = INK),
        plot.title = element_text(size = ttl_pt, face = "bold", colour = INK, hjust = 0, margin = margin(b = 1.5)))
PF <- bh_panel(pf, 1, 1, data = data.frame(Feature = unname(FEATURE_LABEL[ix$feature]), n = ix$n, `Feature SD` = ix$feature_sd,
  `Male minus female slope per SD` = ix$estimate_per_sd, SE = ix$se_per_sd, t = ix$t, df = ix$df, `CI low (95%)` = ix$ci_low_per_sd,
  `CI high (95%)` = ix$ci_high_per_sd, `P (stored, PR3)` = ix$p_stored, `q (BH across the three features, stored)` = ix$q_bh_stored,
  check.names = FALSE, stringsAsFactors = FALSE))

# ================================================================== g: the Figure 1c domain heatmap, first active phase after CC1
# Figure 1c's grammar (f1v2_panel_domains: stored Hedges g in square tiles, the stored post hoc markers as dots, tier
# gaps, the fixed smd limit) for the Stage 14 heatmap "cc1_a1" (variant "picked"), the window of the Stage 09 features.
# The HMM row is not drawn: config v1.0.1 defines no first-night latent-state score (its cells are stored "not_shown").
G_CONTRAST <- c("RES-CON" = "RES\n−CON", "SUS-CON" = "SUS\n−CON", "RES-SUS" = "RES\n−SUS")
G_ROW_LABEL <- c(psychomotor_rate = "Movement")                                     # as Figure 1c
D1 <- fsb("D1_domain_heatmap_cells")
hg <- D1[D1$heatmap_id == "cc1_a1" & D1$variant == "picked", , drop = FALSE]
if (nrow(hg) != 42L || sum(hg$cell_status == "estimated") != 36L || any(hg$row_key[hg$cell_status != "estimated"] != "hmm_state_architecture"))
  stop("fsb D1 cc1_a1 / picked must hold 36 estimated cells and only the HMM row not shown.", call. = FALSE)
hg <- hg[hg$cell_status == "estimated", , drop = FALSE]
if (any(toupper(as.character(hg$marker)) == "TRUE" & hg$marker_class != "posthoc"))
  stop("A stored marker that is not post hoc has no glyph in panel g.", call. = FALSE)
grows <- unique(hg[order(hg$row_order), c("row_order", "row_key", "row_label", "display_tier")])
grows$display <- ifelse(grows$row_key %in% names(G_ROW_LABEL), G_ROW_LABEL[grows$row_key], grows$row_label)
tier_i <- match(grows$display_tier, unique(grows$display_tier))
y_tb <- seq_len(nrow(grows)) + (tier_i - 1L) * 0.5                                  # tiers half a row apart, as Figure 1c
grows$y <- max(y_tb) + 1 - y_tb
hg$y <- grows$y[match(hg$row_key, grows$row_key)]
hg$x <- match(hg$contrast, names(G_CONTRAST))
hg$column <- factor(paste("Dark phase", SEX_LABEL[hg$Sex], sep = "\n"), levels = paste("Dark phase", SEX_LABEL, sep = "\n"))
if (anyNA(hg$x) || anyNA(hg$column)) stop("panel g: an unknown sex or contrast in fsb D1.", call. = FALSE)
hg$label <- sub("^-", MINUS, formatC(hg$hedges_g, format = "f", digits = 2))
gdots <- hg[toupper(as.character(hg$marker)) == "TRUE", , drop = FALSE]
G_LIMIT <- S14_G_LIMIT
pg_heat <- ggplot(hg, aes(x, y)) +
  geom_tile(aes(fill = hedges_g), width = TILE, height = TILE) +
  geom_text(aes(label = label), size = f1h_text(), colour = INK) +
  geom_point(data = gdots, size = 0.55, colour = INK, position = position_nudge(x = 0.33, y = 0.28)) +
  facet_wrap(~column, nrow = 1) +
  s14_fill(G_LIMIT, fixed = TRUE) +
  scale_x_continuous(breaks = seq_along(G_CONTRAST), labels = unname(G_CONTRAST), limits = c(0.5, length(G_CONTRAST) + 0.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = grows$y, labels = grows$display, limits = c(0.5, max(grows$y) + 0.5), expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_f1(style = "house") +
  theme(axis.line = element_blank(), axis.ticks = element_blank(),
        axis.text.x = element_text(size = txt_pt, colour = INK, lineheight = 0.9),
        axis.text.y = element_text(size = ttl_pt, colour = INK),
        strip.text.x = element_text(size = ttl_pt, colour = INK, face = "bold", lineheight = 0.92, margin = margin(b = 1.5)),
        panel.spacing.x = grid::unit(1.6, "mm"), aspect.ratio = max(grows$y) / length(G_CONTRAST), plot.margin = margin(8, 1, 0, 1))
gbl <- s14_bar_labels(G_LIMIT, fixed = TRUE)
gbarv <- data.frame(value = seq(-G_LIMIT, G_LIMIT, length.out = 61))
pg_bar <- ggplot(gbarv, aes(value, 1, fill = value)) + geom_tile(width = 2 * G_LIMIT / 60, height = 1) + s14_fill(G_LIMIT, fixed = TRUE) +
  scale_x_continuous(breaks = gbl$breaks, labels = gbl$labels, expand = c(0, 0)) + scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
  labs(x = "Hedges *g*", y = NULL) + theme_f1(style = "house") +
  theme(axis.line = element_blank(), axis.title.x = f1h_markdown(margin = margin(t = 1)), plot.margin = margin(1, 5, 1, 1))
pg_key <- ggplot() +
  annotate("point", x = 0, y = 0, size = 0.9, colour = INK) +
  annotate("text", x = 0.35, y = 0, label = "post hoc q < 0.05", hjust = 0, size = f1h_text(), colour = INK) +
  scale_x_continuous(limits = c(-0.3, 4), expand = c(0, 0)) + scale_y_continuous(limits = c(-1, 1)) +
  theme_void() + theme(plot.margin = margin(0, 0, 0, 0))
pg <- pg_heat / (pg_key + pg_bar + patchwork::plot_layout(widths = c(0.8, 1))) + patchwork::plot_layout(heights = c(1, 0.11)) +
  patchwork::plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0), plot.background = F1H_BACKGROUND))
gcell <- hg[order(match(hg$Sex, names(SEX_LABEL)), -hg$y, hg$x), , drop = FALSE]
PG <- bh_panel(pg, 1, 1, data = data.frame(Window = gcell$window_label, Sex = gcell$Sex, Domain = grows$display[match(gcell$row_key, grows$row_key)],
  Tier = gcell$display_tier, Comparison = gcell$contrast, `n compared` = gcell$n_comp_animals, `n reference` = gcell$n_ref_animals,
  `Mean compared` = gcell$mean_comp, `Mean reference` = gcell$mean_ref, `Hedges g` = gcell$hedges_g, Estimate = gcell$estimate,
  `CI low (95%)` = gcell$ci_low, `CI high (95%)` = gcell$ci_high, Test = gcell$test_used, df = gcell$df, Statistic = gcell$statistic,
  P = gcell$p_raw, `q (BH, family)` = gcell$q_bh, `Correction family` = gcell$family_id,
  `Post hoc marker drawn` = toupper(as.character(gcell$marker)) == "TRUE", check.names = FALSE, stringsAsFactors = FALSE))

# ---------------------------------------------------------------- write out
# Authored at the exact placed size (the absolute boxes of figures/contracts/figure_contract.yml), so scale = 1.0 and
# native pt == printed pt.
BOX <- local({
  e <- yaml::read_yaml(repo_path("figures", "contracts", "figure_contract.yml"))$figures[["ED_behaviour_models"]]
  stats::setNames(lapply(e$panels, function(p) c(w = as.numeric(p$w), h = as.numeric(p$h))), vapply(e$panels, function(p) sub("^04", "ed04", p$id), ""))
})
PANELS <- list(ed04a = PA, ed04b = PB, ed04c = PC, ed04d = PD, ed04e = PE, ed04f = PF, ed04g = PG)
if (!setequal(names(BOX), names(PANELS))) stop("The contract's ED4 panels differ from the panels this script draws.", call. = FALSE)
for (id in names(PANELS)) { PANELS[[id]]$w_mm <- BOX[[id]][["w"]]; PANELS[[id]]$h_mm <- BOX[[id]][["h"]] }
written <- vapply(names(PANELS), function(id) bh_save_svg(PANELS[[id]], file.path(OUT, paste0("extended_data_behaviour_", id, ".svg"))),
                  character(1))
# The values each panel plots, one table per panel beside the SVG: the panel's source data
# (figures/contracts/figure_contract.yml source_data).
tables <- vapply(names(PANELS), function(id) {
  f <- file.path(OUT, paste0("extended_data_behaviour_", id, "_source_data.csv"))
  utils::write.csv(bh_source_table(PANELS[[id]]$data), f, row.names = FALSE, na = "")
  f
}, character(1))
stale <- setdiff(list.files(OUT, pattern = "^extended_data_behaviour_ed04[a-z]", full.names = TRUE), c(written, tables))
if (length(stale)) invisible(file.remove(stale))

cat("Behavioural Extended Data panels ->", OUT, "\n")
for (id in names(written))
  cat(sprintf("  extended_data_behaviour_%s.svg  %7d bytes  %.1f x %.0f mm\n", id, file.size(written[[id]]), PANELS[[id]]$w_mm, PANELS[[id]]$h_mm))
cat("  bundles", PIN$bundle_id, "and", FSB$bundle_id, "| animals:", N_ANIMALS, "\n")
cat("  every value read from stored bundle cells; nothing computed\n")

# ---------------------------------------------------------------- render record
# Which commit drew these panels, whether that code was clean, the bundles, and the hash of every panel, so that
# tools/promote_manuscript_render.R can refuse panels drawn from other or uncommitted code.
{
  code <- git_code_state()
  record_dir <- path_results("logs", "manuscript_figures", "extended_data_behaviour_panels")
  dir.create(record_dir, recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(list(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    producer = "figures/scripts/extended_data_behaviour_panels.R",
    behaviour_bundle_id = PIN$bundle_id, behaviour_bundle_manifest_sha256 = PIN$manifest_sha256,
    figure_support_bundle_id = FSB$bundle_id, figure_support_bundle_manifest_sha256 = FSB$manifest_sha256,
    render_git_commit = code$commit, render_code_clean = code$code_clean, render_code_dirty = code$dirty,
    files = lapply(unname(c(written, tables)), function(x) list(path = relative_to(x), sha256 = unname(tools::sha256sum(x))))),
    file.path(record_dir, "render_record.yml"))
  cat("  render record", relative_to(file.path(record_dir, "render_record.yml")), "at", substr(code$commit, 1, 7),
      if (isTRUE(code$code_clean)) "(clean code)\n" else "(UNCOMMITTED CODE)\n")
}
