#!/usr/bin/env Rscript
# ================================================================
# Script: figures/figure_01_panels.R
# Stage: manuscript_figures
# Scope: global
# Consumes: the pinned canonical behaviour bundle, source_data/MMMSociability/<bundle_id>/
#           (config/behaviour_bundle.yml), through R/behaviour_bundle.R
# Produces: results/figures/manuscript/figure_01_panels/
# Notes: Rendering only. Fits no model, computes no statistic.
# ================================================================
#
# Figure 1 panels. The behavioural analysis is owned and frozen by MMMSociability
# (Stage 29 characterisation, Stage 09 registered prediction, Stage 16b bundle).
# This script plots stored values. Every printed number is declared in
# figures/figure_01_annotation_map.csv and resolved to exactly one bundle cell by
# bundle_cell(); a number that is not in the bundle cannot reach the figure.
# Plotted group means and intervals are the bundle's model-based estimates from
# the sex-stratified fits (per-sex means come only from stratified fits); points
# are the bundle's animal values; CON is shown in grey as description only.
#
# Panels:
#   a  design and timeline
#   b  CombZ and the RES/SUS definition (definition, not validation)
#   c  first active phase after CC1: crossing rate and shared antenna-zone use,
#      per-sex RES/SUS estimates, the sex-difference contrast and its P-CC1 Holm p
#   d  CC1-CC4 active-phase trajectories, sex-stratified, with the Q2b P-TR Holm p
#   e  early crossing rate versus later CombZ (Stage 09)
#   f  held-out prediction and its permutation null (Stage 09)
# Panels c, d and e use overlapping animals and data (RES/SUS are defined from
# later CombZ): they are complementary views, not independent replication.
#
# VISUAL LANGUAGE. Descends from the original behaviour main figure (closest
# surviving ancestor MMMSociability Analysis/27_candidate_recompose_behavior_main_figure.R
# at 4b0f90f): the held-out prediction panel with its permutation null, the dashed
# identity line, the circle/triangle sex encoding and the point and histogram marks.

suppressPackageStartupMessages({
  library(ggplot2)
})

paths_file <- if (file.exists(file.path("R", "paths.R"))) file.path("R", "paths.R") else file.path("..", "R", "paths.R")
source(paths_file)
source(repo_path("R", "behaviour_bundle.R"))

PIN <- behaviour_bundle_pin()
behaviour_bundle_verify(PIN)
OUT <- path_results("figures", "manuscript", "figure_01_panels")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

TAB <- new.env(parent = emptyenv())
tab <- function(name) {
  if (!exists(name, envir = TAB, inherits = FALSE)) assign(name, behaviour_bundle_table(name, PIN), envir = TAB)
  get(name, envir = TAB, inherits = FALSE)
}

# ---------------------------------------------------------------- annotation map
AMAP <- utils::read.csv(repo_path("figures", "figure_01_annotation_map.csv"), stringsAsFactors = FALSE)
if (anyDuplicated(AMAP$key)) stop("figure_01_annotation_map.csv has duplicated keys.", call. = FALSE)
map_row <- function(key) {
  r <- AMAP[AMAP$key == key, , drop = FALSE]
  if (nrow(r) != 1L) stop("annotation key not declared: ", key, call. = FALSE)
  r
}
ann <- function(key) {
  r <- map_row(key)
  kv <- strsplit(strsplit(r$filters, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)
  filters <- stats::setNames(lapply(kv, function(x) if (identical(x[[2]], "NA")) NA else x[[2]]),
                             vapply(kv, `[[`, "", 1L))
  do.call(bundle_cell, c(list(tab(r$table), r$column), filters))
}
fa <- function(key) {
  r <- map_row(key); v <- ann(key)
  if (grepl("%d", r$format, fixed = TRUE)) v <- as.integer(round(v))
  out <- sprintf(r$format, v)
  sub("^-", "−", out)
}
ci <- function(k) sprintf("%s [%s, %s]", fa(k), fa(paste0(k, "_lo")), fa(paste0(k, "_hi")))

# ---------------------------------------------------------------- house style
# Group colours from R/plotting_nature.R, as Figures 2 and 3 render with (byte-identical
# to MMM_GROUP_COLOURS upstream). Shape carries group redundantly.
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

CONSTRUCT_LAB <- c(crossing_rate = "Antenna-crossing rate (crossings/h)",
                   shared_zone_use = "Shared antenna-zone use (fraction of dyadic time)")
C2 <- tab("C2_estimates")
model_means <- function(estimand_prefix, construct) {
  x <- C2[C2$source == "stage29" & C2$construct == construct & grepl(estimand_prefix, C2$estimand), , drop = FALSE]
  x$Group <- sub("^mean_(RES|SUS)_.*$", "\\1", x$estimand)
  x$CC <- suppressWarnings(as.integer(sub("^.*_CC([1-4])$", "\\1", x$estimand)))
  x
}

# =============================================================== panel a
tl <- tab("A0_design_timeline")
tl <- tl[order(tl$step), , drop = FALSE]
DISPLAY <- c(
  "first cage change (CC1)" = "Cage change\nCC1 (P25)",
  "first active phase after CC1" = "First active\nphase after CC1",
  "cage changes CC2-CC4" = "Cage changes\nCC2-CC4",
  "active phase after each of CC2-CC4" = "Active phase\nafter each change",
  "later behavioural outcome tests" = "Behavioural\noutcome tests",
  "terminal physiology" = "Terminal\nphysiology",
  "CombZ computed" = "CombZ\ncomputed",
  "RES/SUS assigned" = "Resilient /\nsusceptible")
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

# =============================================================== panel b
# The threshold defines the groups; the separation is by construction, so no brackets and no stars.
cls <- tab("A4_combz_animals")
thr <- tab("A2b_combz_thresholds")
cls$Sex <- factor(cls$Sex, levels = c("Female", "Male")); cls$Group <- factor(cls$Group, levels = GROUP_LEV)
thr$Sex <- factor(thr$Sex, levels = c("Female", "Male"))
thr$label <- ifelse(thr$Sex == "Female", fa("b_thr_female"), fa("b_thr_male"))
set.seed(1)  # jitter is presentational only
pb <- ggplot(cls, aes(Group, CombZ)) +
  geom_hline(data = thr, aes(yintercept = control_mean_combz), linewidth = 0.25, colour = RULE) +
  geom_hline(data = thr, aes(yintercept = susceptibility_threshold), linewidth = 0.45, colour = INK, linetype = "22") +
  geom_point(aes(fill = Group, shape = Group), position = position_jitter(width = 0.2, height = 0),
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

# =============================================================== panel c
# Groups sit at fixed offsets within each sex; the model mean and CI are drawn just right of their own group.
A1 <- tab("A1_animal_cc1")
OFF <- c(CON = -0.28, RES = 0, SUS = 0.28)
C_TITLE <- c(crossing_rate = "Antenna-crossing rate", shared_zone_use = "Shared antenna-zone use")
C_Y <- c(crossing_rate = "crossings per hour", shared_zone_use = "fraction of dyadic time")
c_one <- function(k, key) {
  pts <- A1[is.finite(A1[[k]]), c("AnimalNum", "Sex", "Group", k)]; names(pts)[4] <- "y"
  pts$Sex <- factor(pts$Sex, levels = c("Female", "Male")); pts$Group <- factor(pts$Group, levels = GROUP_LEV)
  pts$x <- as.numeric(pts$Sex) + OFF[as.character(pts$Group)]
  mm <- model_means("^mean_(RES|SUS)_CC1$", k); mm <- mm[mm$sex %in% c("Female", "Male"), , drop = FALSE]
  if (nrow(mm) != 4L) stop("panel c: expected 4 model means for ", k, call. = FALSE)
  mm$Sex <- factor(mm$sex, levels = c("Female", "Male")); mm$Group <- factor(mm$Group, levels = GROUP_LEV)
  mm$x <- as.numeric(mm$Sex) + OFF[as.character(mm$Group)] + 0.11
  cap <- sprintf("RES\u2212SUS, F: %s\nRES\u2212SUS, M: %s\nsex difference: %s\nP-CC1 Holm p = %s",
                 ci(paste0("c_", key, "_rs_f")), ci(paste0("c_", key, "_rs_m")), ci(paste0("c_", key, "_q1")), fa(paste0("c_", key, "_q1_holm")))
  set.seed(2)  # jitter is presentational only
  ggplot(pts, aes(x, y)) +
    geom_point(aes(fill = Group, shape = Group, colour = Group == "CON"), position = position_jitter(width = 0.06, height = 0),
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
pc <- patchwork::wrap_plots(c_one("crossing_rate", "cr"), c_one("shared_zone_use", "sz"), nrow = 1, guides = "collect") +
  patchwork::plot_annotation(title = "First active phase after CC1 (SIS; CON hollow grey, not modelled)",
                             theme = theme(plot.title = element_text(size = BASE_PT, colour = INK))) &
  theme(legend.position = "top")

# =============================================================== panel d
B2 <- tab("B2_descriptive_summaries")
d_one <- function(k, key) {
  mm <- model_means("^mean_(RES|SUS)_TR_CC[1-4]$", k)
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
pd <- patchwork::wrap_plots(d_one("crossing_rate", "cr"), d_one("shared_zone_use", "sz"), nrow = 1, guides = "collect") +
  patchwork::plot_annotation(title = "Active phase after each cage change: sex-stratified model estimates ± 95% CI (CON descriptive means, grey)",
                             theme = theme(plot.title = element_text(size = BASE_PT, colour = INK))) &
  theme(legend.position = "top")

# =============================================================== panel e
A2 <- tab("A2_prediction_animals")
e_lab <- sprintf("Spearman ρ = %s\n95%% CI [%s, %s]\nBH q = %s, n = %s", fa("e_rho"), fa("e_rho_lo"), fa("e_rho_hi"), fa("e_q"), fa("e_n"))
pe <- ggplot(A2, aes(crossing_rate_equiv_per_h, observed_CombZ)) +
  geom_point(size = 1.05, stroke = 0.2, alpha = 0.9, shape = 21, colour = "grey20", fill = "#6E8B99") +
  annotate("text", x = Inf, y = Inf, label = e_lab, hjust = 1.04, vjust = 1.15, size = NOTE_PT / .pt, colour = INK, lineheight = 1.08) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.22))) +
  labs(x = "Early crossing rate after CC1 (crossings/h)", y = "Later CombZ",
       subtitle = "one point per animal; rank correlation; not split by sex") +
  theme_f1()

# =============================================================== panel f
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
obs <- ann("f_loao")
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

# ---------------------------------------------------------------- write out
# Authored at the exact box the absolute-layout assembler places (figures/figure_contract.yml), so scale = 1.
DIMS <- list(a = c(173, 36), b = c(58, 66), c = c(113, 66), d = c(173, 62), e = c(66, 58), f = c(105, 58))
PLOTS <- list(a = pa, b = pb, c = pc, d = pd, e = pe, f = pf)
save_svg <- function(p, id) {
  d <- DIMS[[id]]
  f <- file.path(OUT, paste0("figure_01", id, ".svg"))
  svglite::svglite(f, width = d[1] / 25.4, height = d[2] / 25.4, bg = "white", fix_text_size = FALSE)
  print(p); grDevices::dev.off()
  f
}
written <- vapply(names(PLOTS), function(id) save_svg(PLOTS[[id]], id), character(1))
used <- unique(AMAP$key)
cat("Figure 1 panels ->", OUT, "\n  bundle", PIN$bundle_id, "config", PIN$config_version, PIN$config_sha256, "\n")
for (id in names(written)) cat(sprintf("  figure_01%s.svg  %7d bytes  %.0f x %.0f mm\n", id, file.size(written[[id]]), DIMS[[id]][1], DIMS[[id]][2]))
cat("  every printed number resolved through figure_01_annotation_map.csv (", length(used), "keys )\n")
