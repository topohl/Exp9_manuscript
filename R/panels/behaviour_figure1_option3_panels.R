# Figure 1 option 3, Nature layout (candidate figure_01_option3_nature; OPTION3_SPEC 3): the new
# panel builders a-d of the opt-in Nature profile (BH_NATURE, R/panels/behaviour_figure_style.R).
# Rendering only: fits no model, computes no statistic. Panels e and f are the option-2 Nature
# builders (f1n_panel_association, f1n_panel_prediction) at option 3's boxes.
#
# Every builder takes
#   tab   the behaviour-bundle (ebb) table getter, function(name) -> pinned table;
#   fsb   the figure-support bundle (fsb) table getter (R/figure_support_bundle.R);
#   an    an annotation resolver (bh_annotation(), usually through edx_annotation_prefixed() with the
#         prefix f1o3n_), so every printed number is one stored cell of the annotation map;
#   w_mm, h_mm, and (c, d) `frame` as in R/panels/behaviour_figure1_nature_panels.R;
# and returns bh_panel(plot, w_mm, h_mm). Nothing here is used by the canonical Figure 1 or by any
# earlier candidate: the builders are new functions, and the shared constants they read are unchanged.
#
#   a  f1o3n_panel_design     the design timeline (CC1-CC4 as repeated cage-change episodes; SIS
#                             regrouped, CON intact; no reference or baseline wording) and the
#                             behavioural architecture: the RFID position stream, Movement and the
#                             three behavioural-organisation readouts with the stored tier of each
#                             metric (fsb F1, from the Stage 29 configuration), and the HMM layer as a
#                             downstream, exploratory box (not a fifth domain);
#   b  f1o3n_panel_combz      CombZ per animal at its stored rank within sex (fsb F2), circles coloured
#                             by group, the stored threshold and CON-mean lines (ebb A2b), the group
#                             strip, and the six components' signed z as they enter CombZ as a heatmap
#                             on the manuscript diverging palette (config/manuscript_palette.yml),
#                             symmetric limits capped at F1O3N_Z_CAP; the RES/SUS boundary is marked;
#   c  f1o3n_panel_cc1        first active phase after CC1: animals as circles by group (CON hollow),
#                             the RES/SUS model means and 95% CIs (ebb C2, CC1_BY_SEX), the CON
#                             descriptive mean (fsb F1b) and the three CON cage means per sex (fsb F1);
#                             beside each measure the stored RES - SUS contrasts (F, M) and the
#                             female - male difference (a compact sidecar) with the P-CC1 Holm P only;
#   d  f1o3n_panel_trajectory CC1 -> CC4: the RES/SUS TR_BY_SEX means and 95% CIs (ebb C2), the CON
#                             descriptive mean (dashed) and the three CON cage means per cage change
#                             (fsb F1b, F1), and one line per measure for the registered Q2b (P-TR Holm P).
# Group marks are circles, colour = group; every ink is one the earlier candidates draw, plus the
# manuscript's existing diverging palette for the heatmap.
#
# Requires: R/behaviour_bundle.R, R/panels/behaviour_figure_style.R, R/panels/behaviour_figure1_panels.R
# and R/panels/behaviour_figure1_nature_panels.R sourced first; packages ggtext and patchwork.

# The heatmap's symmetric presentational limits: signed z beyond +/-F1O3N_Z_CAP take the end colours
# (the legend declares the cap). Presentational only.
F1O3N_Z_CAP <- 3
# The manuscript's one diverging scale (config/manuscript_palette.yml: diverging low / mid / high,
# the colours nv_diverging() draws). Read, never redefined.
F1O3N_DIVERGING <- local({
  d <- yaml::read_yaml(repo_path("config", "manuscript_palette.yml"))$diverging
  c(low = d$low, mid = d$mid, high = d$high)
})
# One CON cage per batch and three batches per sex: the design the figure-support bundle's gates
# assert (exactly three CON cage epochs per sex and cage change, four animals each). The builders
# stop unless the stored rows show exactly that, so the "3 cages/sex" label is checked against the data.
F1O3N_CON_CAGES_PER_SEX <- 3L
F1O3N_CON_LABEL <- "CON descriptive reference; 3 cages/sex"

# Heatmap rows (fsb F2b component ids, in the stored component order), sentence case.
F1O3N_COMPONENT_LABEL <- c(NOR = "NOR discrimination", sucrose_pref = "Sucrose preference", weight_dev = "Body-weight change",
                           delta_cort = "Corticosterone rise", adrenal_weight = "Adrenal weight", spleen_weight = "Spleen weight")
# The architecture schematic (a): the four Stage 29 metrics (fsb F1 measure ids) with their domain and
# metric wording; the tier is read from the bundle.
F1O3N_ARCH <- data.frame(
  measure = c("crossing_rate", "shared_zone_use", "occupancy_dispersion", "fragmentation"),
  domain = c("Movement", "Social-spatial overlap", "Spatial organisation", "Temporal organisation"),
  metric = c("position-change rate", "shared occupancy", "occupancy dispersion", "fragmentation"),
  stringsAsFactors = FALSE)
F1O3N_Q2B_LABEL <- "'Female − male difference in RES − SUS change from CC1 (Q2b): Holm ' * italic('P ') * '= ' * %s"
F1O3N_RS_Y <- c(crossing_rate = paste0("RES − SUS (", F1N_PER_H, ")"), shared_zone_use = "RES − SUS (fraction)")
F1O3N_DOTS_Y <- c(crossing_rate = paste0("Position-change rate (", F1N_PER_H, ")"),
                  shared_zone_use = "Shared occupancy<br>(fraction of dyadic time)")
F1O3N_TRAJ_Y <- c(crossing_rate = paste0("Position-change<br>rate (", F1N_PER_H, ")"), shared_zone_use = "Shared<br>occupancy")
# c's sidecar columns (abbreviated; the legend spells them out) and d's group offsets within a cage change.
F1O3N_SIDE_LABEL <- c("F", "M", "F − M")
F1O3N_SIDE_X <- c(1, 2, 3.15)            # the wider F - M label takes a little more room
F1O3N_TRAJ_OFFSET <- c(CON = -0.25, RES = 0, SUS = 0.25)

# The stored tier of each Stage 29 metric (fsb F1 carries it from the Stage 29 configuration).
f1o3n_tiers <- function(F1) {
  t <- unique(F1[, c("measure", "tier")])
  if (anyDuplicated(t$measure) || !setequal(t$measure, F1O3N_ARCH$measure))
    stop("f1o3n: fsb F1 must give one tier for each of the four Stage 29 metrics.", call. = FALSE)
  stats::setNames(t$tier, t$measure)
}

# The CON rows of the figure-support bundle for one measure (row filters only), with the design guard:
# three CON cages per sex and cage change, four animals each, and one reference mean per cell.
f1o3n_con_rows <- function(F1, F1b, k, ccs) {
  cages <- F1[F1$measure == k & F1$CC %in% ccs & F1$Group == "CON", , drop = FALSE]
  ref <- F1b[F1b$measure == k & F1b$CC %in% ccs & F1b$Group == "CON", , drop = FALSE]
  cells <- paste(cages$Sex, cages$CC)
  if (!all(table(cells) == F1O3N_CON_CAGES_PER_SEX) || length(unique(cells)) != 2L * length(ccs) ||
      any(cages$n_animals != 4L) || anyDuplicated(cages$CageEpisodeID))
    stop("f1o3n: fsb F1 must hold exactly ", F1O3N_CON_CAGES_PER_SEX, " CON cages of 4 animals per sex and cage change for ", k, call. = FALSE)
  if (nrow(ref) != 2L * length(ccs) || anyDuplicated(paste(ref$Sex, ref$CC)))
    stop("f1o3n: fsb F1b must hold one CON reference mean per sex and cage change for ", k, call. = FALSE)
  list(cages = cages, ref = ref)
}

# =============================================================== a
f1o3n_panel_design <- function(tab, fsb, an, w_mm, h_mm) {
  tl <- tab("A0_design_timeline")
  tl <- tl[order(tl$step), , drop = FALSE]
  tiers <- f1o3n_tiers(fsb("F1_con_cage_means"))
  DISPLAY <- F1_TIMELINE_DISPLAY
  DISPLAY[] <- gsub("CC2-CC4", "CC2–CC4", DISPLAY, fixed = TRUE)
  if (!identical(sort(names(DISPLAY)), sort(tl$event)))
    stop("The bundle timeline events differ from the display map.", call. = FALSE)
  n <- nrow(tl)
  is_rec <- tl$role %in% c("primary window (CC1)", "trajectory windows (CC1-CC4)")
  is_out <- tl$role %in% c("outcome", "classification")
  # Timeline: n boxes across the width with 1.5-mm gaps (x in mm from the box's left edge; y in mm,
  # 0 at the top, negative downwards). The fills are the option-2 Nature fills, by stored role.
  gap <- 1.5; pitch <- (w_mm + gap) / n
  STAGE <- data.frame(x0 = (seq_len(n) - 1) * pitch, label = unname(DISPLAY[tl$event]),
                      fill = ifelse(is_rec, GREEN_MID, ifelse(is_out, NEUTRAL_BOX, GREEN_LIGHT)),
                      ink = ifelse(is_rec, "white", INK), stringsAsFactors = FALSE)
  STAGE$x1 <- STAGE$x0 + pitch - gap
  STAGE$fill[tl$role == "reference"] <- GREEN_DARK; STAGE$ink[tl$role == "reference"] <- "white"
  REC_I <- which(is_rec | tl$role %in% c("reference", "stressor")); OUT_I <- which(is_out)
  Y_LAB <- -F1N_LETTER_BASELINE_MM; Y_RULE <- -3.7; Y_BOX <- c(-10.2, -4.5)
  Y_NOTE <- -12.6                                      # the SIS / CON line under the cage-change boxes
  # Architecture schematic, one row under the timeline: group labels, their rules, then the boxes.
  Y_S_LAB <- -16.1; Y_S_RULE <- -16.9; Y_S_BOX <- c(-h_mm + 0.25, -17.6)
  arch <- F1O3N_ARCH
  arch$tier <- unname(tiers[arch$measure])
  src_w <- 19; arr <- 4.5; hmm_w <- 24; mgap <- 1.5
  box_w <- (w_mm - src_w - 2 * arr - hmm_w - 3 * mgap - 2) / 4
  arch$x0 <- src_w + arr + c(0, box_w + 2 + mgap * (0:2) + box_w * (0:2))
  arch$x1 <- arch$x0 + box_w
  arch$label <- sprintf("%s:\n%s\n%s", arch$domain, arch$metric, arch$tier)
  arch$label[arch$measure == "crossing_rate"] <- sprintf("%s\n%s", arch$metric[1], arch$tier[1])
  arch$label <- sub("^([a-z])", "\\U\\1", arch$label, perl = TRUE)
  hmm_x0 <- w_mm - hmm_w
  org <- arch$measure != "crossing_rate"
  y_mid <- (Y_S_BOX[1] + Y_S_BOX[2]) / 2
  lab_pt <- F1N$title_pt
  txt <- function(x, y, label, colour = INK, hjust = 0.5, vjust = 0.5, pt = F1N$text_pt, lineheight = 0.92)
    annotate("text", x = x, y = y, label = label, colour = colour, hjust = hjust, vjust = vjust,
             size = f1n_text(pt), lineheight = lineheight)
  arrow_to <- function(x0, x1) annotate("segment", x = x0, xend = x1, y = y_mid, yend = y_mid, linewidth = F1N$bracket_lw,
                                        colour = INK, arrow = grid::arrow(length = grid::unit(1.1, "mm"), type = "closed"),
                                        arrow.fill = INK)
  pa <- ggplot() +
    # timeline
    geom_rect(data = STAGE, aes(xmin = x0, xmax = x1, ymin = Y_BOX[1], ymax = Y_BOX[2], fill = I(fill)), colour = NA) +
    geom_text(data = STAGE, aes(x = (x0 + x1) / 2, y = (Y_BOX[1] + Y_BOX[2]) / 2, label = label, colour = I(ink)),
              size = f1n_text(lab_pt), lineheight = 0.92) +
    annotate("segment", x = STAGE$x0[min(REC_I)], xend = STAGE$x1[max(REC_I)], y = Y_RULE, yend = Y_RULE,
             linewidth = F1N$bracket_lw, colour = GREEN_DARK) +
    txt((STAGE$x0[min(REC_I)] + STAGE$x1[max(REC_I)]) / 2, Y_LAB, "CC1–CC4: repeated cage-change episodes (every 4 days)",
        colour = GREEN_DARK, vjust = 0, pt = lab_pt) +
    annotate("segment", x = STAGE$x0[min(OUT_I)], xend = STAGE$x1[max(OUT_I)], y = Y_RULE, yend = Y_RULE,
             linewidth = F1N$bracket_lw, colour = MUTED) +
    txt((STAGE$x0[min(OUT_I)] + STAGE$x1[max(OUT_I)]) / 2, Y_LAB, "outcome, CombZ and group labels derived",
        colour = MUTED, vjust = 0, pt = lab_pt) +
    txt(STAGE$x0[min(REC_I)], Y_NOTE, "SIS: regrouped", hjust = 0, vjust = 0) +
    txt(STAGE$x0[min(REC_I)] + 17, Y_NOTE, "CON: intact group (same platform-transfer episodes)", hjust = 0, vjust = 0) +
    # architecture: the RFID position stream -> Movement and behavioural organisation -> HMM layer
    annotate("rect", xmin = 0, xmax = src_w, ymin = Y_S_BOX[1], ymax = Y_S_BOX[2], fill = GREEN_DARK, colour = NA) +
    txt(src_w / 2, y_mid, "RFID position\nstream", colour = "white") +
    arrow_to(src_w + 0.4, arch$x0[1] - 0.5) +
    geom_rect(data = arch, aes(xmin = x0, xmax = x1, ymin = Y_S_BOX[1], ymax = Y_S_BOX[2]), fill = GREEN_LIGHT, colour = NA) +
    geom_text(data = arch, aes(x = (x0 + x1) / 2, y = y_mid, label = label), colour = INK, size = f1n_text(), lineheight = 0.92) +
    annotate("segment", x = arch$x0[1], xend = arch$x1[1], y = Y_S_RULE, yend = Y_S_RULE, linewidth = F1N$bracket_lw, colour = GREEN_DARK) +
    txt((arch$x0[1] + arch$x1[1]) / 2, Y_S_LAB, "Movement", colour = GREEN_DARK, vjust = 0) +
    annotate("segment", x = min(arch$x0[org]), xend = max(arch$x1[org]), y = Y_S_RULE, yend = Y_S_RULE,
             linewidth = F1N$bracket_lw, colour = GREEN_DARK) +
    txt((min(arch$x0[org]) + max(arch$x1[org])) / 2, Y_S_LAB, "Behavioural organisation", colour = GREEN_DARK, vjust = 0) +
    arrow_to(max(arch$x1) + 0.4, hmm_x0 - 0.5) +
    annotate("rect", xmin = hmm_x0, xmax = w_mm - 0.15, ymin = Y_S_BOX[1], ymax = Y_S_BOX[2], fill = "white", colour = MUTED,
             linewidth = F1N$bracket_lw, linetype = "22") +
    txt((hmm_x0 + w_mm) / 2, y_mid, "HMM behavioural states\nintegrative layer", colour = INK) +
    annotate("segment", x = hmm_x0, xend = w_mm - 0.15, y = Y_S_RULE, yend = Y_S_RULE, linewidth = F1N$bracket_lw, colour = MUTED) +
    txt((hmm_x0 + w_mm) / 2, Y_S_LAB, "Downstream, exploratory", colour = MUTED, vjust = 0) +
    scale_x_continuous(limits = c(0, w_mm), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-h_mm, 0), expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    theme_void(base_size = lab_pt, base_family = "sans") +
    theme(plot.margin = margin(0, 0, 0, 0))
  bh_panel(pa, w_mm, h_mm)
}

# =============================================================== b
f1o3n_panel_combz <- function(tab, fsb, an, w_mm, h_mm) {
  fa <- an$fa
  F2 <- fsb("F2_combz_components")
  F2b <- fsb("F2b_combz_definition")
  thr <- tab("A2b_combz_thresholds")
  F2b <- F2b[order(F2b$component_order), , drop = FALSE]
  if (!identical(F2b$component, names(F1O3N_COMPONENT_LABEL)))
    stop("f1o3n_panel_combz: the stored components differ from the heatmap rows.", call. = FALSE)
  sexes <- c("Female", "Male")
  ani <- F2[F2$component == F2b$component[1], c("AnimalNum", "Sex", "Group", "CombZ", "below_threshold", "heatmap_order_within_sex")]
  if (anyDuplicated(ani$AnimalNum) || nrow(F2) != nrow(ani) * nrow(F2b))
    stop("f1o3n_panel_combz: fsb F2 must hold every component once per animal.", call. = FALSE)
  names(ani)[names(ani) == "heatmap_order_within_sex"] <- "x"
  # the RES/SUS boundary: the stored order puts every below-threshold animal first (checked), so the
  # boundary sits half a column right of the last of them (layout only; nothing is printed from it)
  bnd <- do.call(rbind, lapply(sexes, function(s) {
    o <- ani$x[ani$Sex == s]; b <- ani$below_threshold[ani$Sex == s]
    if (!identical(sort(o), seq_along(o)) || !any(b) || max(o[b]) >= min(o[!b]))
      stop("f1o3n_panel_combz: the stored heatmap order does not put the below-threshold animals first (", s, ").", call. = FALSE)
    data.frame(Sex = s, x = max(o[b]) + 0.5, lo = 0.5, hi = length(o) + 0.5)
  }))
  hm <- F2[, c("AnimalNum", "Sex", "Group", "component", "signed_z", "present", "heatmap_order_within_sex")]
  names(hm)[names(hm) == "heatmap_order_within_sex"] <- "x"
  hm$row <- factor(unname(F1O3N_COMPONENT_LABEL[hm$component]), levels = rev(unname(F1O3N_COMPONENT_LABEL)))
  gone <- hm[!hm$present, , drop = FALSE]
  gone$yn <- as.integer(gone$row)
  ani$Sex <- factor(ani$Sex, levels = sexes); hm$Sex <- factor(hm$Sex, levels = sexes); bnd$Sex <- factor(bnd$Sex, levels = sexes)
  thr$Sex <- factor(thr$Sex, levels = sexes); gone$Sex <- factor(gone$Sex, levels = sexes)
  ani$Group <- factor(ani$Group, levels = GROUP_LEV); hm$Group <- factor(hm$Group, levels = GROUP_LEV)
  thr$label <- ifelse(thr$Sex == "Female", fa("b_thr_female"), fa("b_thr_male"))
  thr$x <- bnd$lo[match(thr$Sex, bnd$Sex)]   # the label sits at the left end, above the line: no animal there
  edges <- rbind(data.frame(Sex = bnd$Sex, x = bnd$lo), data.frame(Sex = bnd$Sex, x = bnd$hi))
  cols <- facet_grid(. ~ Sex, scales = "free_x", space = "free_x")
  xs <- scale_x_continuous(expand = c(0, 0))
  vline <- geom_vline(data = bnd, aes(xintercept = x), linewidth = F1N$axis_lw, colour = INK, linetype = "22")
  no_x <- theme(axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())
  key_aes <- list(size = F1N$point_size, stroke = F1N$point_stroke, colour = F1N$point_colour)
  fill_z <- scale_fill_gradient2(low = F1O3N_DIVERGING[["low"]], mid = F1O3N_DIVERGING[["mid"]], high = F1O3N_DIVERGING[["high"]],
                                 midpoint = 0, limits = c(-F1O3N_Z_CAP, F1O3N_Z_CAP), oob = scales::squish,
                                 na.value = "white", guide = "none")
  p_dots <- ggplot(ani, aes(x, CombZ)) +
    geom_blank(data = edges, aes(x = x), inherit.aes = FALSE) +
    geom_hline(data = thr, aes(yintercept = control_mean_combz), linewidth = F1N$rule_lw, colour = RULE) +
    geom_hline(data = thr, aes(yintercept = susceptibility_threshold), linewidth = 0.45, colour = INK, linetype = "22") +
    vline +
    geom_point(aes(fill = Group), shape = 21, size = F1N$point_size, stroke = F1N$point_stroke, colour = F1N$point_colour) +
    geom_label(data = thr, aes(x = x + 0.3, y = susceptibility_threshold, label = label), inherit.aes = FALSE,
               hjust = 0, vjust = -0.25, size = f1n_text(), colour = INK, fill = "white", linewidth = 0,
               label.padding = unit(0.5, "pt")) +
    cols + xs +
    scale_fill_manual(values = GROUP_COL, name = NULL) +
    guides(fill = guide_legend(override.aes = key_aes)) +
    scale_y_continuous(labels = f1_minus_labels, breaks = c(-1, 0, 1)) +
    labs(y = "CombZ") +
    theme_f1(style = "nature") + no_x +
    theme(legend.position = "inside", legend.position.inside = c(0.004, 1.02), legend.justification = c(0, 1),
          legend.direction = "horizontal", legend.background = element_blank(),
          legend.key.spacing.x = unit(3.5, "pt"), panel.spacing = unit(4, "pt"),
          plot.margin = margin(0, 1, 0.6, 1))
  p_strip <- ggplot(ani, aes(x, 1)) +
    geom_blank(data = edges, aes(x = x, y = 1), inherit.aes = FALSE) +
    geom_tile(aes(fill = Group), height = 1, colour = "white", linewidth = 0.12) +
    vline + cols + xs +
    scale_fill_manual(values = GROUP_COL, guide = "none") +
    scale_y_continuous(breaks = 1, labels = "Group", expand = c(0, 0)) +
    labs(y = NULL) +
    theme_f1(style = "nature") + no_x +
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), strip.text = element_blank(),
          panel.spacing = unit(4, "pt"), plot.margin = margin(0, 1, 0.6, 1))
  p_heat <- ggplot(hm, aes(x, row)) +
    geom_blank(data = edges, aes(x = x), inherit.aes = FALSE) +
    geom_tile(aes(fill = signed_z), colour = "white", linewidth = 0.12) +
    geom_segment(data = gone, aes(x = x - 0.38, xend = x + 0.38, y = yn - 0.38, yend = yn + 0.38),
                 inherit.aes = FALSE, linewidth = F1N$rule_lw, colour = MUTED) +
    vline + cols + xs + fill_z +
    scale_y_discrete(expand = c(0, 0)) +
    labs(y = NULL) +
    theme_f1(style = "nature") + no_x +
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), strip.text = element_blank(),
          panel.spacing = unit(4, "pt"), plot.margin = margin(0, 1, 1, 1))
  bar <- data.frame(z = seq(-F1O3N_Z_CAP, F1O3N_Z_CAP, length.out = 49))
  p_bar <- ggplot(bar, aes(1, z, fill = z)) +
    geom_tile(width = 1, height = 2 * F1O3N_Z_CAP / 48) +
    fill_z +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(breaks = c(-F1O3N_Z_CAP, 0, F1O3N_Z_CAP), labels = f1_minus_labels, position = "right", expand = c(0, 0)) +
    labs(x = NULL, y = "Signed *z*") +
    theme_f1(style = "nature") +
    theme(axis.line = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(),
          axis.title.y.right = f1n_markdown(angle = 90, margin = margin(l = 1.5)),
          plot.margin = margin(0, 1, 1, 3))
  design <- "A#\nB#\nCD"
  pb <- patchwork::wrap_plots(A = p_dots, B = p_strip, C = p_heat, D = p_bar, design = design,
                              heights = c(12.5, 1.9, 12.6), widths = grid::unit(c(1, 2.2), c("null", "mm"))) +
    patchwork::plot_annotation(title = "Later CombZ and its six components, animals ordered by CombZ within sex",
                               theme = theme(plot.title = element_text(size = F1N$title_pt, colour = INK, hjust = 0,
                                                                       margin = margin(F1N_HEADER_TOP_PT, 0, 1.2, F1N_INDENT_PT)),
                                             plot.margin = margin(0, 0, 0, 0), plot.background = F1N_BACKGROUND))
  bh_panel(pb, w_mm, h_mm)
}

# =============================================================== c
f1o3n_panel_cc1 <- function(tab, fsb, an, w_mm, h_mm, jitter_seed = c(NA, NA), frame = NULL) {
  fa <- an$fa
  A1 <- tab("A1_animal_cc1")
  C2 <- tab("C2_estimates")
  F1 <- fsb("F1_con_cage_means"); F1b <- fsb("F1b_con_reference_means")
  sexes <- c("Female", "Male")
  con_x <- match("CON", GROUP_LEV) + F1N_MEAN_OFFSET
  dots <- function(k, seed, title, top_pad, bottom_pad) {
    pts <- A1[is.finite(A1[[k]]), c("AnimalNum", "Sex", "Group", k)]; names(pts)[4] <- "y"
    pts$Sex <- factor(pts$Sex, levels = sexes); pts$Group <- factor(pts$Group, levels = GROUP_LEV)
    pts$x <- match(as.character(pts$Group), GROUP_LEV) + F1N_CLOUD_OFFSET
    mm <- f1_model_means(C2, "^mean_(RES|SUS)_CC1$", k); mm <- mm[mm$sex %in% sexes, , drop = FALSE]
    if (nrow(mm) != 4L || !all(grepl("^CC1_BY_SEX\\|", mm$model_id))) stop("panel c: expected 4 CC1_BY_SEX model means for ", k, call. = FALSE)
    mm$Sex <- factor(mm$sex, levels = sexes); mm$Group <- factor(mm$Group, levels = GROUP_LEV)
    mm$x <- match(as.character(mm$Group), GROUP_LEV) + F1N_MEAN_OFFSET
    con <- f1o3n_con_rows(F1, F1b, k, "CC1")
    cages <- con$cages; ref <- con$ref
    cages$Sex <- factor(cages$Sex, levels = sexes); ref$Sex <- factor(ref$Sex, levels = sexes)
    ggplot(pts, aes(x, y)) +
      geom_point(aes(fill = Group, colour = Group == "CON"), shape = 21,
                 position = position_jitter(width = F1N$jitter_width, height = 0, seed = seed),
                 size = F1N$point_size, stroke = F1N$point_stroke) +
      # CON: the descriptive mean (a short grey bar) and the three cage means (small grey dots)
      geom_segment(data = ref, aes(x = con_x - 0.13, xend = con_x + 0.13, y = mean, yend = mean), inherit.aes = FALSE,
                   linewidth = F1N$mean_lw, colour = CON_GREY) +
      geom_point(data = cages, aes(x = con_x, y = cage_mean), inherit.aes = FALSE, shape = 21, size = 0.65, stroke = 0.25,
                 colour = CON_GREY, fill = CON_GREY) +
      # RES / SUS: the model means and their capless 95% CIs
      geom_linerange(data = mm, aes(x = x, ymin = ci_low, ymax = ci_high), inherit.aes = FALSE,
                     linewidth = F1N$mean_lw, colour = "black") +
      geom_point(data = mm, aes(x = x, y = estimate), inherit.aes = FALSE, shape = 21,
                 size = F1N$mean_size, stroke = F1N$mean_stroke, colour = "black", fill = "black") +
      facet_wrap(~ Sex, nrow = 1) +
      scale_fill_manual(values = c(CON = "white", GROUP_COL[c("RES", "SUS")]), guide = "none") +
      scale_colour_manual(values = c(`TRUE` = CON_GREY, `FALSE` = F1N$point_colour), guide = "none") +
      scale_x_continuous(breaks = seq_along(GROUP_LEV), labels = GROUP_LEV, limits = c(0.6, 3.48), expand = c(0, 0)) +
      scale_y_continuous(labels = f1_minus_labels) +
      labs(x = NULL, y = F1O3N_DOTS_Y[[k]], title = title) +
      theme_f1(style = "nature") +
      theme(axis.title.y = f1n_markdown(lineheight = 1.05), panel.spacing = unit(4, "pt"),
            plot.title = element_text(size = F1N$text_pt, colour = CON_GREY, hjust = 0, margin = margin(b = 1 + top_pad, l = F1N_INDENT_PT - 1)),
            plot.title.position = "plot",
            plot.margin = margin(0, 1.5, 1 + bottom_pad, 1))
  }
  side <- function(k, key, top_pad, bottom_pad) {
    stem <- paste0("c_", key, c("_rs_f", "_rs_m", "_q1"))
    rows <- data.frame(x = F1O3N_SIDE_X, label = F1O3N_SIDE_LABEL, int = c(FALSE, FALSE, TRUE),
                       estimate = vapply(stem, an$ann, numeric(1)),
                       lo = vapply(paste0(stem, "_lo"), an$ann, numeric(1)),
                       hi = vapply(paste0(stem, "_hi"), an$ann, numeric(1)), row.names = NULL)
    marks <- utils::modifyList(BH_FOREST, F1N$forest)
    # the P-CC1 Holm P of Q1 (the female - male difference, the diamond), named on the line above it
    holm <- f1n_expr("'Holm ' * italic('P ') * '= ' * %s", fa(paste0("c_", key, "_q1_holm")))
    ggplot(rows, aes(x = x)) +
      bh_forest_zero(horizontal = TRUE, marks = marks) +
      geom_segment(aes(xend = x, y = lo, yend = hi), linewidth = marks$ci_linewidth, colour = marks$ci_colour, lineend = marks$ci_lineend) +
      bh_forest_point(aes(y = estimate), data = rows[!rows$int, , drop = FALSE], fill = INK, marks = marks) +
      bh_forest_int_point(aes(y = estimate), data = rows[rows$int, , drop = FALSE], marks = marks) +
      scale_x_continuous(breaks = rows$x, labels = rows$label, limits = c(0.4, 3.75), expand = c(0, 0)) +
      scale_y_continuous(labels = f1_minus_labels, position = "right") +
      labs(x = NULL, y = F1O3N_RS_Y[[k]], title = "F − M", subtitle = holm) +
      theme_f1(style = "nature") +
      theme(axis.title.y.right = f1n_markdown(angle = -90, margin = margin(l = 1.2)),
            axis.text.x = element_text(size = F1N$text_pt, colour = INK, margin = margin(t = 1)),
            plot.title = element_text(size = F1N$text_pt, colour = INK, hjust = 0.5, margin = margin(b = 0.4)),
            plot.subtitle = element_text(size = F1N$text_pt, colour = INK, hjust = 0.5, margin = margin(b = 1 + top_pad)),
            plot.title.position = "plot",
            plot.margin = margin(0, 1, 1 + bottom_pad, 1))
  }
  build <- function(top_pad, bottom_pad) {
    parts <- list(dots("crossing_rate", jitter_seed[1], F1O3N_CON_LABEL, top_pad, bottom_pad), side("crossing_rate", "cr", top_pad, bottom_pad),
                  dots("shared_zone_use", jitter_seed[length(jitter_seed)], NULL, top_pad, bottom_pad), side("shared_zone_use", "sz", top_pad, bottom_pad))
    patchwork::wrap_plots(parts, nrow = 1, widths = c(1, 0.46, 1, 0.46)) +
      patchwork::plot_annotation(title = "First active phase after CC1",
                                 theme = theme(plot.title = element_text(size = F1N$title_pt, colour = INK, hjust = 0,
                                                                         margin = margin(F1N_HEADER_TOP_PT, 0, 1.2, F1N_INDENT_PT)),
                                               plot.margin = margin(0, 0, 0, 0), plot.background = F1N_BACKGROUND))
  }
  bh_panel(f1n_align(build, frame, w_mm, h_mm, "f1o3n_panel_cc1"), w_mm, h_mm)
}

# =============================================================== d
f1o3n_panel_trajectory <- function(tab, fsb, an, w_mm, h_mm, frame = NULL) {
  fa <- an$fa
  C2 <- tab("C2_estimates")
  F1 <- fsb("F1_con_cage_means"); F1b <- fsb("F1b_con_reference_means")
  sexes <- c("Female", "Male"); ccs <- paste0("CC", 1:4)
  one <- function(k, key, top = TRUE, top_pad = 0, bottom_pad = 0) {
    mm <- f1_model_means(C2, "^mean_(RES|SUS)_TR_CC[1-4]$", k); mm <- mm[mm$sex %in% sexes, , drop = FALSE]
    if (nrow(mm) != 16L || !all(grepl("^TR_BY_SEX\\|", mm$model_id))) stop("panel d: expected 16 TR_BY_SEX model means for ", k, call. = FALSE)
    mm$Sex <- factor(mm$sex, levels = sexes); mm$Group <- factor(mm$Group, levels = c("RES", "SUS"))
    mm$x <- mm$CC + F1O3N_TRAJ_OFFSET[as.character(mm$Group)]
    con <- f1o3n_con_rows(F1, F1b, k, ccs)
    cages <- con$cages; ref <- con$ref
    cages$x <- match(cages$CC, ccs) + F1O3N_TRAJ_OFFSET[["CON"]]; ref$x <- match(ref$CC, ccs) + F1O3N_TRAJ_OFFSET[["CON"]]
    cages$Sex <- factor(cages$Sex, levels = sexes); ref$Sex <- factor(ref$Sex, levels = sexes)
    ref <- ref[order(ref$Sex, ref$x), , drop = FALSE]
    q2b <- f1n_expr(F1O3N_Q2B_LABEL, fa(paste0("d_", key, "_q2b_holm")))
    ggplot(mm, aes(x, estimate)) +
      geom_line(data = ref, aes(x, mean, group = Sex), inherit.aes = FALSE, colour = CON_GREY, linetype = "22", linewidth = F1N$axis_lw) +
      geom_segment(data = ref, aes(x = x - 0.1, xend = x + 0.1, y = mean, yend = mean), inherit.aes = FALSE,
                   linewidth = F1N$mean_lw, colour = CON_GREY) +
      geom_point(data = cages, aes(x, cage_mean), inherit.aes = FALSE, shape = 21, size = 0.65, stroke = 0.25,
                 colour = CON_GREY, fill = CON_GREY) +
      geom_line(aes(colour = Group, group = Group), linewidth = F1N$axis_lw) +
      geom_linerange(aes(ymin = ci_low, ymax = ci_high, colour = Group), linewidth = F1N$mean_lw) +
      geom_point(aes(fill = Group), shape = 21, size = F1N$point_size + 0.15, stroke = F1N$point_stroke, colour = F1N$point_colour) +
      facet_wrap(~ Sex, nrow = 1) +
      scale_colour_manual(values = GROUP_COL[c("RES", "SUS")], guide = "none") +
      scale_fill_manual(values = GROUP_COL[c("RES", "SUS")], guide = "none") +
      scale_x_continuous(breaks = 1:4, labels = ccs, limits = c(0.55, 4.45), expand = c(0, 0)) +
      scale_y_continuous(labels = f1_minus_labels, n.breaks = 4) +
      labs(x = NULL, y = F1O3N_TRAJ_Y[[k]], title = q2b) +
      theme_f1(style = "nature") +
      theme(axis.title.y = f1n_markdown(lineheight = 1.05), panel.spacing = unit(4, "pt"),
            plot.title = element_text(size = F1N$text_pt, colour = INK, hjust = 0, margin = margin(b = 1 + top_pad)),
            plot.title.position = "plot",
            strip.text = if (top) element_text(size = F1N$title_pt, colour = INK, margin = margin(b = 1, t = 0)) else element_blank(),
            axis.text.x = if (top) element_blank() else element_text(size = F1N$text_pt, colour = INK),
            plot.margin = margin(0, 1, if (top) 1.5 else 1 + bottom_pad, 1))
  }
  build <- function(top_pad, bottom_pad) {
    patchwork::wrap_plots(one("crossing_rate", "cr", TRUE, top_pad), one("shared_zone_use", "sz", FALSE, 0, bottom_pad), ncol = 1) +
      patchwork::plot_annotation(title = "CC1 → CC4, active phase after each cage change",
                                 theme = theme(plot.title = element_text(size = F1N$title_pt, colour = INK, hjust = 0,
                                                                         margin = margin(F1N_HEADER_TOP_PT, 0, 1.2, F1N_INDENT_PT)),
                                               plot.margin = margin(0, 0, 0, 0), plot.background = F1N_BACKGROUND))
  }
  bh_panel(f1n_align(build, frame, w_mm, h_mm, "f1o3n_panel_trajectory"), w_mm, h_mm)
}
