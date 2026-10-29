#!/usr/bin/env Rscript
# ================================================================
# Script: figures/scripts/extended_data_homecage_panels.R
# Stage: manuscript_figures
# Scope: global
# Consumes: the pinned canonical behaviour bundle (config/behaviour_bundle.yml) and the pinned figure-support
#           bundle (config/figure_support_bundle.yml), source_data/MMMSociability/<bundle_id>/, through
#           R/behaviour_bundle.R and R/figure_support_bundle.R
# Produces: results/figures/manuscript/extended_data_homecage_panels/ and its render record
# Notes: Rendering only. Reads stored bundle cells; computes no statistic.
# ================================================================
#
# Extended Data 10: the home-cage record (added 2026-10-30; user request after the figure review: the raw home-cage
# data, movement after every cage change, and whether the early association is specific to the first regrouping).
#
# Every value comes from the bundles Figure 1 is drawn from:
#   fsb S1-S6: the post hoc run 29f (registry POSTHOC_CAGE_CHANGE_SERIES v1.0): first-night movement after each cage
#       change against the later ISS (SIS and CON, bootstrap intervals, three paired contrasts), rank stability across
#       cage changes, the animal and group series, the actogram, one cage per sex minute by minute, the registered
#       Stage 29 tests quoted verbatim, and the cohort table;
#   ebb E_multiplicity: the Holm-adjusted P of the registered follow-up trajectory tests (FU-TR).
# Nothing here is fitted, resampled or corrected; every printed value is a stored cell. The only arithmetic is
# layout: the light-phase spans of the actogram from its stored hour labels.
#
# Panels (house profile of Figure 1 and Extended Data 4: 5 / 5.5-pt text, group colours, explanatory text in the legend):
#   a  animals per batch, sex and later group (RFID-tracked in brackets when fewer)
#   b  one cage per sex, first active phase after CC1, position changes per minute for each animal
#   c  group actogram over the four cage-change epochs, by sex (light phases shaded)
#   d  first-night movement after each cage change: every animal and the group means, by sex, with the registered
#      RES - SUS trajectory test of each sex
#   e  Spearman rho of first-night movement with the later ISS after each cage change, SIS and CON (all animals, and
#      within batch), with the front-loading contrast
#   f  rank stability of first-night movement across cage changes, SIS and CON
#
# WORDING DISCIPLINE. The page shows that no registered home-cage trajectory test is rejected; the renderer refuses to
# draw if one is. The 29f values are post hoc and descriptive (the registry discloses an unregistered look); panel e
# prints intervals and one contrast, no other P.

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
OUT <- path_results("figures", "manuscript", "extended_data_homecage_panels")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

for (lib in c("behaviour_figure_style.R", "behaviour_figure1_panels.R", "behaviour_s30_light_panels.R", "behaviour_figure1_house_panels.R",
              "behaviour_figure1_option3_panels.R", "behaviour_stage14_panels.R"))
  source(repo_path("R", "panels", lib))

EM <- behaviour_bundle_table("E_multiplicity", PIN)
fsb <- function(name) fsb_bundle_table(name, FSB)
S1 <- fsb("S1_association_by_cage_change"); S1b <- fsb("S1b_association_contrasts"); S1c <- fsb("S1c_rank_stability")
S2 <- fsb("S2_animal_series"); S2b <- fsb("S2b_group_series"); S3 <- fsb("S3_actogram"); S4 <- fsb("S4_cage_raster")
S5 <- fsb("S5_registered_tests"); S6 <- fsb("S6_cohort_table")
cell <- function(tab, column, ...) bundle_cell(tab, column, ...)

CC <- c("CC1", "CC2", "CC3", "CC4")
SEX_LABEL <- c(Female = "Females", Male = "Males")
PAL <- yaml::read_yaml(repo_path("config", "manuscript_palette.yml"))
COND_COL <- c(SIS = PAL$evidence$supported, CON = GROUP_COL[["CON"]])   # SIS (RES + SUS) in the supported ink, CON in its group colour
RHO_LIMIT <- as.numeric(PAL$diverging_limits$correlation)
TILE <- as.numeric(PAL$heatmap$tile)
RASTER_CAP <- 4                                   # colour saturates at 4 position changes per minute (99th percentile 5)
MINUS <- "−"
mnum <- function(x, d = 2) sub("^-", MINUS, formatC(x, format = "f", digits = d))
pq <- function(x) sub("^-", MINUS, formatC(x, format = "fg", digits = 2))
txt_pt <- F1H$text_pt; ttl_pt <- F1H$title_pt
theme_ed <- function(top_pt = 0) theme_f1(style = "house") + theme(plot.margin = margin(top_pt, 1, 1, 1))

# ------------------------------------------------------------------ contracts
# The page states that no registered home-cage test is rejected and that the 29f series belongs to these bundles.
reg <- EM[EM$family_id %in% c("P-CC1", "P-TR", "S-CC1-ORG", "S-TR-ORG") | grepl("^FU-TR[|]", EM$family_id), , drop = FALSE]
if (!nrow(reg) || any(as.logical(reg$reject_at_alpha_adjusted)))
  stop("A registered home-cage CC1 or trajectory test is rejected (or missing) in ebb E_multiplicity: panel d presents them as ",
       "null. Redesign the panel; do not re-render it.", call. = FALSE)
if (nrow(S1) != 32L || nrow(S1b) != 6L || nrow(S1c) != 12L || nrow(S2) != 444L || nrow(S4) != 5760L)
  stop("fsb S1-S4 do not have the declared sizes of the post hoc run 29f.", call. = FALSE)
s5m <- S5[S5$construct == "crossing_rate", , drop = FALSE]
for (i in seq_len(nrow(s5m)))
  if (!isTRUE(all.equal(as.numeric(s5m$p_raw[i]), cell(EM, "p_raw", member = s5m$member[i]), tolerance = 1e-12)))
    stop("fsb S5 quotes a P that differs from ebb E_multiplicity: ", s5m$member[i], call. = FALSE)
if (!setequal(unique(S2$CageChange), CC) || !all(S2$Group %in% GROUP_LEV)) stop("fsb S2: unexpected cage changes or groups.", call. = FALSE)

# ================================================================== a: animals per batch, sex and group
A_ORDER <- list(Female = c("B3", "B4", "B6"), Male = c("B1", "B2", "B5"))
ct <- S6; ct$cell <- ifelse(ct$n_rfid_tracked < ct$n_animals, sprintf("%d (%d)", ct$n_animals, ct$n_rfid_tracked), as.character(ct$n_animals))
rows <- do.call(rbind, lapply(names(A_ORDER), function(sx) data.frame(Sex = sx, Batch = c(NA, A_ORDER[[sx]]), stringsAsFactors = FALSE)))
rows$label <- ifelse(is.na(rows$Batch), SEX_LABEL[rows$Sex], paste("Batch", sub("^B", "", rows$Batch)))
rows$y <- rev(seq_len(nrow(rows)))
cols <- c(GROUP_LEV, "All")
body <- do.call(rbind, lapply(which(!is.na(rows$Batch)), function(i) {
  r <- ct[ct$Batch == rows$Batch[i] & ct$Sex == rows$Sex[i], , drop = FALSE]
  if (nrow(r) != 3L) stop("fsb S6: batch ", rows$Batch[i], " lacks a group.", call. = FALSE)
  n <- sum(r$n_animals); t <- sum(r$n_rfid_tracked)
  data.frame(y = rows$y[i], x = seq_along(cols), label = c(r$cell[match(GROUP_LEV, r$Group)], if (t < n) sprintf("%d (%d)", n, t) else as.character(n)),
             stringsAsFactors = FALSE)
}))
tot <- vapply(GROUP_LEV, function(g) { r <- ct[ct$Group == g, ]; n <- sum(r$n_animals); t <- sum(r$n_rfid_tracked)
  if (t < n) sprintf("%d (%d)", n, t) else as.character(n) }, "")
nall <- sum(ct$n_animals); tall <- sum(ct$n_rfid_tracked)
foot <- data.frame(y = 0, x = seq_along(cols), label = c(tot, sprintf("%d (%d)", nall, tall)), stringsAsFactors = FALSE)
head_df <- data.frame(x = seq_along(cols), y = max(rows$y) + 1.1, label = cols, colour = c(unname(GROUP_COL[GROUP_LEV]), INK), stringsAsFactors = FALSE)
pa <- ggplot() +
  geom_text(data = body, aes(x, y, label = label), size = f1h_text(), colour = INK) +
  geom_text(data = foot, aes(x, y, label = label), size = f1h_text(), colour = INK) +
  geom_text(data = head_df, aes(x, y, label = label), colour = head_df$colour, size = f1h_text(ttl_pt), fontface = "bold") +
  geom_text(data = rows, aes(-0.35, y, label = label), hjust = 0, size = f1h_text(ifelse(is.na(rows$Batch), ttl_pt, txt_pt)),
            fontface = ifelse(is.na(rows$Batch), "bold", "plain"), colour = INK) +
  annotate("text", x = -0.35, y = 0, label = "All", hjust = 0, size = f1h_text(ttl_pt), fontface = "bold", colour = INK) +
  annotate("segment", x = -0.35, xend = length(cols) + 0.45, y = max(rows$y) + 0.55, yend = max(rows$y) + 0.55, linewidth = F1H$rule_lw, colour = "grey45") +
  annotate("segment", x = -0.35, xend = length(cols) + 0.45, y = 0.5, yend = 0.5, linewidth = F1H$rule_lw, colour = "grey45") +
  scale_x_continuous(limits = c(-0.4, length(cols) + 0.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.6, max(rows$y) + 1.7), expand = c(0, 0)) +
  theme_void() + theme(plot.margin = margin(8, 2, 1, 6), plot.background = F1H_BACKGROUND)
PA <- bh_panel(pa, 1, 1, data = data.frame(Batch = S6$Batch, Sex = S6$Sex, Group = S6$Group, `Animals with ISS` = S6$n_animals,
  `RFID-tracked` = S6$n_rfid_tracked, check.names = FALSE, stringsAsFactors = FALSE))

# ================================================================== b: one cage per sex, minute by minute
rb <- S4
an <- unique(rb[, c("Sex", "Batch", "CageEpisodeID", "AnimalNum", "Group", "CombZ")])
an <- an[order(match(an$Sex, names(SEX_LABEL)), match(an$Group, GROUP_LEV), -an$CombZ), , drop = FALSE]
if (nrow(an) != 8L || any(table(an$CageEpisodeID) != 4L)) stop("fsb S4 must hold two cages of four animals.", call. = FALSE)
an$row <- rev(seq_len(nrow(an))) + ifelse(an$Sex == "Female", 0.6, 0)        # a half-row gap between the two cages
an$ylab <- sprintf("<span style='color:%s'>%s</span>  %s", GROUP_COL[an$Group], an$Group, mnum(an$CombZ))
rb$row <- an$row[match(rb$AnimalNum, an$AnimalNum)]
rb$fill <- pmin(rb$Movement, RASTER_CAP)
cage_lab <- do.call(rbind, lapply(split(an, an$CageEpisodeID), function(d) data.frame(
  y = max(d$row) + 0.62, label = sprintf("%s, batch %s, cage %s", SEX_LABEL[d$Sex[1]], sub("^B", "", d$Batch[1]),
                                         sub("^sys[.]", "", strsplit(d$CageEpisodeID[1], "|", fixed = TRUE)[[1]][2])), stringsAsFactors = FALSE)))
clock <- c(0, 180, 360, 540, 720); clock_lab <- c("18:30", "21:30", "00:30", "03:30", "06:30")
# minutes without a position change are left blank (the page is white): the same picture with far fewer SVG elements
pb_main <- ggplot(rb[rb$Movement > 0, , drop = FALSE], aes(minute + 0.5, row)) +
  geom_tile(aes(fill = fill), width = 1, height = 0.8) +
  geom_text(data = cage_lab, aes(0, y, label = label), hjust = 0, vjust = 0, size = f1h_text(), colour = INK, inherit.aes = FALSE) +
  scale_fill_gradient(low = "white", high = PAL$evidence$supported, limits = c(0, RASTER_CAP), guide = "none") +
  scale_x_continuous(breaks = clock, labels = clock_lab, limits = c(0, 720), expand = c(0, 0)) +
  scale_y_continuous(breaks = an$row, labels = an$ylab, expand = expansion(add = c(0.5, 1.1))) +
  labs(x = "Time (first active phase after CC1)", y = NULL) +
  theme_ed() + theme(axis.text.y = f1h_markdown(size = txt_pt, colour = INK), axis.line.y = element_blank(), axis.ticks.y = element_blank(),
                     plot.margin = margin(0, 9, 1, 1))
rbar <- data.frame(value = seq(0, RASTER_CAP, length.out = 41))
pb_bar <- ggplot(rbar, aes(value, 1, fill = value)) + geom_tile(width = RASTER_CAP / 40, height = 1) +
  scale_fill_gradient(low = "white", high = PAL$evidence$supported, limits = c(0, RASTER_CAP), guide = "none") +
  scale_x_continuous(breaks = 0:RASTER_CAP, labels = c(0:(RASTER_CAP - 1), paste0("≥", RASTER_CAP)), expand = c(0, 0)) +
  scale_y_continuous(breaks = NULL, expand = c(0, 0)) + labs(x = "Position changes per minute", y = NULL) +
  theme_f1(style = "house") + theme(axis.line = element_blank(), plot.margin = margin(1, 9, 1, 1))
# the bar under the right part of the panel, in its own nested row so the raster keeps its full width
pb <- patchwork::wrap_plots(pb_main, patchwork::wrap_plots(patchwork::plot_spacer(), pb_bar, nrow = 1, widths = c(1.5, 1)),
                            ncol = 1, heights = c(1, 0.06)) +
  patchwork::plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0), plot.background = F1H_BACKGROUND))
PB <- bh_panel(pb, 1, 1, data = data.frame(Cage = rb$CageEpisodeID, Animal = rb$AnimalNum, Group = rb$Group, ISS = rb$CombZ,
  `Minute from 18:30` = rb$minute, `Position changes` = rb$Movement, check.names = FALSE, stringsAsFactors = FALSE))

# ================================================================== c: group actogram over the four epochs
ac <- S3; ac$Group <- factor(ac$Group, levels = GROUP_LEV); ac$Sex <- factor(SEX_LABEL[ac$Sex], levels = SEX_LABEL)
ac$CageChange <- factor(ac$CageChange, levels = CC)
ph <- unique(ac[, c("CageChange", "hour", "phase")])
if (anyDuplicated(ph[, c("CageChange", "hour")])) stop("fsb S3: one hour carries two phases.", call. = FALSE)
light <- do.call(rbind, lapply(split(ph, ph$CageChange), function(d) {
  d <- d[order(d$hour), ]; r <- rle(d$phase == "Inactive"); e <- cumsum(r$lengths); s <- e - r$lengths
  if (!any(r$values)) return(NULL)
  data.frame(CageChange = d$CageChange[1], xmin = d$hour[s[r$values] + 1L], xmax = d$hour[e[r$values]] + 1, stringsAsFactors = FALSE) }))
light$CageChange <- factor(light$CageChange, levels = CC)
pc <- ggplot(ac, aes(hour + 0.5, mean, colour = Group, fill = Group)) +
  geom_rect(data = light, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "grey93", colour = NA) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), colour = NA, alpha = 0.18) +
  geom_line(linewidth = 0.3) +
  facet_grid(Sex ~ CageChange, scales = "free_x", space = "free_x") +
  scale_colour_manual(values = GROUP_COL, guide = "none") + scale_fill_manual(values = GROUP_COL, guide = "none") +
  scale_x_continuous(breaks = seq(0, 84, 12), expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.04))) +
  coord_cartesian(ylim = c(0, max(ac$ci_high, na.rm = TRUE)), clip = "on") +   # bands below zero are cut at the axis (display only)
  labs(x = "Hours after 18:30 on the day of the cage change", y = "Movement (position changes per h)") +
  theme_ed() + theme(strip.text.y = element_text(size = ttl_pt, colour = INK, angle = 0, hjust = 0),
                     strip.text.x = element_text(size = ttl_pt, colour = INK, face = "bold"), panel.spacing.x = grid::unit(1.2, "mm"),
                     panel.spacing.y = grid::unit(1.5, "mm"))
key_c <- data.frame(x = c(0.86, 0.91, 0.96), label = GROUP_LEV)
pc <- pc + patchwork::inset_element(ggplot(key_c) + geom_text(aes(x, 0, label = label), colour = GROUP_COL[key_c$label], size = f1h_text(),
  hjust = 1) + scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) + theme_void(), left = 0, bottom = 0.93, right = 0.99, top = 1)
PC <- bh_panel(pc, 1, 1, data = data.frame(Sex = S3$Sex, Group = S3$Group, `Cage change` = S3$CageChange, Hour = S3$hour, Phase = S3$phase,
  n = S3$n, `Mean (position changes per h)` = S3$mean, SD = S3$sd, `CI low (95%)` = S3$ci_low, `CI high (95%)` = S3$ci_high,
  check.names = FALSE, stringsAsFactors = FALSE))

# ================================================================== d: movement after each cage change
DODGE <- c(CON = -0.22, RES = 0, SUS = 0.22)
sd_ <- S2; sd_$x <- match(sd_$CageChange, CC) + DODGE[sd_$Group]; sd_$Group <- factor(sd_$Group, levels = GROUP_LEV)
sd_$Sex <- factor(SEX_LABEL[sd_$Sex], levels = SEX_LABEL)
gs <- S2b; gs$x <- match(gs$CageChange, CC) + DODGE[gs$Group]; gs$Group <- factor(gs$Group, levels = GROUP_LEV)
gs$Sex <- factor(SEX_LABEL[gs$Sex], levels = SEX_LABEL)
fu <- EM[grepl("^FU-TR[|]crossing_rate$", EM$family_id), , drop = FALSE]
fu$Sex <- factor(SEX_LABEL[sub("^.*[|]", "", fu$member)], levels = SEX_LABEL)
if (nrow(fu) != 2L || anyNA(fu$Sex)) stop("ebb E_multiplicity lacks the two FU-TR crossing_rate follow-ups.", call. = FALSE)
fu$label <- paste0("RES ", MINUS, " SUS across cage changes: P = ", pq(fu$p_adjusted))
pd <- ggplot(sd_, aes(x, movement, colour = Group)) +
  geom_line(aes(group = AnimalNum), linewidth = 0.16, alpha = 0.35) +
  geom_linerange(data = gs, aes(x = x, ymin = ci_low, ymax = ci_high), inherit.aes = FALSE, colour = GROUP_COL[as.character(gs$Group)], linewidth = F1H$mean_lw) +
  geom_line(data = gs, aes(x = x, y = mean, group = Group), linewidth = 0.4) +
  geom_point(data = gs, aes(x = x, y = mean), size = F1H$mean_size, shape = 16) +
  geom_text(data = fu, aes(x = 0.7, y = Inf, label = label), inherit.aes = FALSE, hjust = 0, vjust = 1.3, size = f1h_text(), colour = INK) +
  facet_wrap(~Sex, nrow = 1) +
  scale_colour_manual(values = GROUP_COL, guide = "none") +
  scale_x_continuous(breaks = seq_along(CC), labels = CC, limits = c(0.55, 4.45), expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0.03, 0.12))) +
  labs(x = NULL, y = "First-night movement (position changes per 10 min)") +
  theme_ed() + theme(strip.text = element_text(size = ttl_pt, colour = INK, face = "bold", hjust = 0))
PD <- bh_panel(pd, 1, 1, data = list(
  animals = data.frame(Animal = S2$AnimalNum, Sex = S2$Sex, Batch = S2$Batch, Group = S2$Group, `Cage change` = S2$CageChange,
                       `Movement (position changes per 10 min)` = S2$movement, ISS = S2$CombZ, check.names = FALSE, stringsAsFactors = FALSE),
  group_means = data.frame(Sex = S2b$Sex, Group = S2b$Group, `Cage change` = S2b$CageChange, n = S2b$n, Mean = S2b$mean, SD = S2b$sd,
                           `CI low (95%)` = S2b$ci_low, `CI high (95%)` = S2b$ci_high, check.names = FALSE, stringsAsFactors = FALSE),
  registered_tests = data.frame(Member = S5$member, Family = S5$family_registry, `P raw` = S5$p_raw, `P Holm (primary)` = S5$p_holm,
                                check.names = FALSE, stringsAsFactors = FALSE)))

# ================================================================== e: association with the later ISS after each cage change
ea <- S1[S1$kind == "rho" & S1$sex == "both", , drop = FALSE]
EOFF <- c(SIS = -0.14, CON = 0.14); LOFF <- c(all = 0, within_batch = 0.07)
ea$x <- match(ea$cage_change, CC) + EOFF[ea$condition] + LOFF[ea$level]
ea$condition <- factor(ea$condition, levels = names(EOFF)); ea$level <- factor(ea$level, levels = names(LOFF))
k1 <- S1b[S1b$contrast == "sis_cc1_minus_later" & S1b$level == "all", , drop = FALSE]
k2 <- S1b[S1b$contrast == "cc1_sis_minus_con" & S1b$level == "all", , drop = FALSE]
e_note <- data.frame(y = c(Inf, Inf), vjust = c(1.3, 2.6), label = c(
  sprintf("SIS, CC1 %s later: %s [%s, %s]", MINUS, mnum(k1$estimate), mnum(k1$ci_low), mnum(k1$ci_high)),
  sprintf("CC1, SIS %s CON: %s [%s, %s]", MINUS, mnum(k2$estimate), mnum(k2$ci_low), mnum(k2$ci_high))))
pe <- ggplot(ea, aes(x, estimate, colour = condition)) +
  geom_hline(yintercept = 0, linewidth = F1H$rule_lw, colour = "grey55", linetype = "22") +
  geom_linerange(aes(ymin = ci_low, ymax = ci_high), linewidth = 0.3) +
  geom_point(aes(shape = level), size = 1.15, stroke = 0.35, fill = "white") +
  geom_text(data = e_note, aes(x = 0.68, y = y, vjust = vjust, label = label), inherit.aes = FALSE, hjust = 0, size = f1h_text(), colour = INK) +
  annotate("text", x = 4.4, y = -0.82, label = "SIS", hjust = 1, size = f1h_text(), colour = COND_COL[["SIS"]]) +
  annotate("text", x = 4.4, y = -0.9, label = "CON", hjust = 1, size = f1h_text(), colour = COND_COL[["CON"]]) +
  annotate("point", x = c(2.75, 2.75), y = c(-0.82, -0.9), shape = c(16, 21), size = 1.15, stroke = 0.35, colour = INK, fill = "white") +
  annotate("text", x = c(2.88, 2.88), y = c(-0.82, -0.9), label = c("all animals", "within batch"), hjust = 0, size = f1h_text(), colour = INK) +
  scale_colour_manual(values = COND_COL, guide = "none") +
  scale_shape_manual(values = c(all = 16, within_batch = 21), guide = "none") +
  scale_x_continuous(breaks = seq_along(CC), labels = CC, limits = c(0.55, 4.45), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-1, 0.85), breaks = seq(-1, 0.5, 0.5), labels = bh_minus) +
  labs(x = NULL, y = "Spearman *ρ* with later ISS") +
  theme_ed() + theme(axis.title.y = f1h_markdown(size = ttl_pt, colour = INK))
PE <- bh_panel(pe, 1, 1, data = list(
  association = data.frame(Condition = ea$condition, `Cage change` = ea$cage_change, Level = ea$level, n = ea$n_animals,
                           `Spearman rho` = ea$estimate, `CI low (95%, bootstrap)` = ea$ci_low, `CI high (95%, bootstrap)` = ea$ci_high,
                           `Finite draws` = ea$n_finite_draws, check.names = FALSE, stringsAsFactors = FALSE),
  by_sex = data.frame(Condition = S1$condition[S1$kind == "rho_sex"], `Cage change` = S1$cage_change[S1$kind == "rho_sex"], Sex = S1$sex[S1$kind == "rho_sex"],
                      n = S1$n_animals[S1$kind == "rho_sex"], `Spearman rho` = S1$estimate[S1$kind == "rho_sex"], `CI low (95%, bootstrap)` = S1$ci_low[S1$kind == "rho_sex"],
                      `CI high (95%, bootstrap)` = S1$ci_high[S1$kind == "rho_sex"], check.names = FALSE, stringsAsFactors = FALSE),
  contrasts = data.frame(Contrast = S1b$definition, Level = S1b$level, Estimate = S1b$estimate, `CI low (95%, bootstrap)` = S1b$ci_low,
                         `CI high (95%, bootstrap)` = S1b$ci_high, `Bootstrap sign P` = S1b$p_boot_sign, Multiplicity = S1b$multiplicity,
                         check.names = FALSE, stringsAsFactors = FALSE)))

# ================================================================== f: rank stability across cage changes
st <- S1c; st$condition <- factor(paste0(st$condition, " (n = ", st$n_animals, ")"), levels = paste0(c("SIS", "CON"), " (n = ", c(87, 24), ")"))
if (anyNA(st$condition)) stop("fsb S1c: unexpected condition sizes.", call. = FALSE)
st$x <- match(st$cage_change_a, CC); st$y <- 5L - match(st$cage_change_b, CC)
st$label <- sub("^0[.]", ".", formatC(st$estimate, format = "f", digits = 2))
pf <- ggplot(st, aes(x, y)) +
  geom_tile(aes(fill = estimate), width = TILE, height = TILE) +
  geom_text(aes(label = label), size = f1h_text(), colour = INK) +
  facet_wrap(~condition, ncol = 1) +
  s14_fill(RHO_LIMIT, fixed = TRUE) +
  scale_x_continuous(breaks = 1:3, labels = CC[1:3], expand = c(0, 0), limits = c(0.5, 3.5)) +
  scale_y_continuous(breaks = 1:3, labels = rev(CC[2:4]), expand = c(0, 0), limits = c(0.5, 3.5)) +
  coord_equal() + labs(x = NULL, y = NULL) +
  theme_ed() + theme(axis.line = element_blank(), axis.ticks = element_blank(),
                     strip.text = element_text(size = ttl_pt, colour = INK, face = "bold", hjust = 0))
fbl <- s14_bar_labels(RHO_LIMIT, fixed = TRUE)
fbar <- data.frame(value = seq(-RHO_LIMIT, RHO_LIMIT, length.out = 61))
pf_bar <- ggplot(fbar, aes(value, 1, fill = value)) + geom_tile(width = 2 * RHO_LIMIT / 60, height = 1) + s14_fill(RHO_LIMIT, fixed = TRUE) +
  scale_x_continuous(breaks = fbl$breaks[c(1, 3, 5)], labels = fbl$labels[c(1, 3, 5)], expand = c(0, 0)) + scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
  labs(x = "Spearman *ρ*", y = NULL) + theme_f1(style = "house") +
  theme(axis.line = element_blank(), axis.title.x = f1h_markdown(size = txt_pt, margin = margin(t = 1)), plot.margin = margin(1, 9, 1, 6))
pf_all <- patchwork::wrap_plots(pf, pf_bar, ncol = 1, heights = c(1, 0.05)) +
  patchwork::plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0), plot.background = F1H_BACKGROUND))
PF <- bh_panel(pf_all, 1, 1, data = data.frame(Condition = S1c$condition, `Cage change A` = S1c$cage_change_a, `Cage change B` = S1c$cage_change_b,
  n = S1c$n_animals, `Spearman rho` = S1c$estimate, `CI low (95%, bootstrap)` = S1c$ci_low, `CI high (95%, bootstrap)` = S1c$ci_high,
  check.names = FALSE, stringsAsFactors = FALSE))

# ---------------------------------------------------------------- write out
BOX <- local({
  e <- yaml::read_yaml(repo_path("figures", "contracts", "figure_contract.yml"))$figures[["ED_homecage"]]
  stats::setNames(lapply(e$panels, function(p) c(w = as.numeric(p$w), h = as.numeric(p$h))), vapply(e$panels, function(p) sub("^10", "ed10", p$id), ""))
})
PANELS <- list(ed10a = PA, ed10b = PB, ed10c = PC, ed10d = PD, ed10e = PE, ed10f = PF)
if (!setequal(names(BOX), names(PANELS))) stop("The contract's ED10 panels differ from the panels this script draws.", call. = FALSE)
for (id in names(PANELS)) { PANELS[[id]]$w_mm <- BOX[[id]][["w"]]; PANELS[[id]]$h_mm <- BOX[[id]][["h"]] }
written <- vapply(names(PANELS), function(id) bh_save_svg(PANELS[[id]], file.path(OUT, paste0("extended_data_homecage_", id, ".svg"))),
                  character(1))
tables <- vapply(names(PANELS), function(id) {
  f <- file.path(OUT, paste0("extended_data_homecage_", id, "_source_data.csv"))
  utils::write.csv(bh_source_table(PANELS[[id]]$data), f, row.names = FALSE, na = "")
  f
}, character(1))
stale <- setdiff(list.files(OUT, pattern = "^extended_data_homecage_ed10[a-z]", full.names = TRUE), c(written, tables))
if (length(stale)) invisible(file.remove(stale))

cat("Home-cage Extended Data panels ->", OUT, "\n")
for (id in names(written))
  cat(sprintf("  extended_data_homecage_%s.svg  %7d bytes  %.1f x %.0f mm\n", id, file.size(written[[id]]), PANELS[[id]]$w_mm, PANELS[[id]]$h_mm))
cat("  bundles", PIN$bundle_id, "and", FSB$bundle_id, "\n")
cat("  every value read from stored bundle cells; nothing computed\n")

{
  code <- git_code_state()
  record_dir <- path_results("logs", "manuscript_figures", "extended_data_homecage_panels")
  dir.create(record_dir, recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(list(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    producer = "figures/scripts/extended_data_homecage_panels.R",
    behaviour_bundle_id = PIN$bundle_id, behaviour_bundle_manifest_sha256 = PIN$manifest_sha256,
    figure_support_bundle_id = FSB$bundle_id, figure_support_bundle_manifest_sha256 = FSB$manifest_sha256,
    render_git_commit = code$commit, render_code_clean = code$code_clean, render_code_dirty = code$dirty,
    files = lapply(unname(c(written, tables)), function(x) list(path = relative_to(x), sha256 = unname(tools::sha256sum(x))))),
    file.path(record_dir, "render_record.yml"))
  cat("  render record", relative_to(file.path(record_dir, "render_record.yml")), "at", substr(code$commit, 1, 7),
      if (isTRUE(code$code_clean)) "(clean code)\n" else "(UNCOMMITTED CODE)\n")
}
