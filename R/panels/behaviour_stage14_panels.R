# Stage 14 behavioural domain heatmap and HMM figure candidates (the figure-support bundle from v3:
# tables D1 and M1-M4).
# Rendering only: fits no model, computes no statistic, adjusts no p value.
#
# The tables come from MMMSociability, which wrote them once from the Stage 14 run promoted on
# 2026-10-06 without re-running Stage 14 (MMMSociability docs/FIGURE_SUPPORT_BUNDLE_v3.md):
#   D1  the domain-heatmap cells (Hedges g by domain x phase x sex x contrast, as stored);
#   M1  the group mean transition probabilities and their differences, with the state display labels;
#   M2  the same means with the rank the state-flow figure draws by (the 8 largest per sex x group);
#   M3  the stored mean dwell times counted in bins doubling from one 10-minute HMM bin;
#   M4  the HMM-resolution sensitivity rows with the Wald interval (estimate -/+ 1.96 SE).
# The panels draw these values; the arithmetic here is layout only (tile, bar and row positions).
#
#   s14_panel_domain_heatmap(fsb, variant, w, h)   all phase blocks, CC1-CC4 pooled; one resolution variant;
#                                                  colours only (the stored markers are not drawn)
#   s14_panel_hmm_transition(fsb, w, h)            M1: difference in transition probability, From x To
#   s14_panel_hmm_flows(fsb, w, h)                 M2: the 8 largest transitions per sex x group
#   s14_panel_hmm_dwell(fsb, w, h)                 M3: share of records per dwell-time bin, by state and group
#   s14_panel_hmm_resolution(fsb, w, h)            M4: the state-architecture contrast at both HMM resolutions
#
# Colours: the manuscript palette (config/manuscript_palette.yml): its diverging scale, the fixed Hedges g
# limit (diverging_limits$smd: full colour beyond it, colourbar ends <= / >=), grey85 tile outlines so a
# tile at zero stays visible, the group colours, and for the contrasts the darkened RES, the SUS colour
# and grey20 that Figure 3k-m use. The HMM transition differences take their own symmetric limit, the
# largest stored difference (M1 colour_limit), since no other figure shares that measure.
# Colourbars are drawn as small tile strips whose labels are axis breaks (as Figure 1 option 3 b).
#
# Requires: R/panels/behaviour_figure_style.R sourced first; packages ggplot2, patchwork, scales.

S14_PALETTE <- yaml::read_yaml(repo_path("config", "manuscript_palette.yml"))
S14_DIVERGING <- unlist(S14_PALETTE$diverging)[c("low", "mid", "high")]
S14_G_LIMIT <- as.numeric(S14_PALETTE$diverging_limits$smd)
S14_TILE_BORDER <- "grey85"
S14_TILE_LW <- as.numeric(S14_PALETTE$line$tile_border_pt) * 0.75     # pt -> ggplot linewidth, as nv_lw()
S14_MISSING <- "grey90"                                                # a cell without a value (white means zero)
S14_MINUS <- intToUtf8(0x2212L); S14_LE <- intToUtf8(0x2264L); S14_GE <- intToUtf8(0x2265L)
S14_HEATMAP_ID <- "all_blocks"
S14_VARIANTS <- c("picked", "all5min", "all10min")
S14_PHASE_LABEL <- c(Active = "Dark phase", Inactive = "Light phase")
S14_SEX_LABEL <- c(Female = "Females", Male = "Males")
S14_HEATMAP_CONTRAST <- c("RES-CON" = paste0("RES\n", S14_MINUS, "CON"), "SUS-CON" = paste0("SUS\n", S14_MINUS, "CON"),
                          "RES-SUS" = paste0("RES\n", S14_MINUS, "SUS"))
S14_HMM_CONTRAST <- c("RES - CON" = paste("RES", S14_MINUS, "CON"), "SUS - CON" = paste("SUS", S14_MINUS, "CON"),
                      "SUS - RES" = paste("SUS", S14_MINUS, "RES"))
S14_RES_CONTRAST <- stats::setNames(unname(S14_HMM_CONTRAST), c("RES-CON", "SUS-CON", "SUS-RES"))
S14_RESOLUTION <- c("5min_based" = "5", "10min_based" = "10")         # the HMM bin in minutes (axis breaks)
s14_darken <- function(col, amount = 0.35) grDevices::colorRampPalette(c(col, "black"))(101)[round(amount * 100) + 1]
S14_CONTRAST_COL <- stats::setNames(c(s14_darken(GROUP_COL[["RES"]]), GROUP_COL[["SUS"]], "grey20"), unname(S14_HMM_CONTRAST))
S14_STATES <- as.character(1:4)

#' The state display label on two lines ("No position change,\nco-located"); string handling only.
s14_wrap_state <- function(x) sub(", ", ",\n", x, fixed = TRUE)
#' A stored text cell as a number (the bundle tables are read as text); conversion only.
s14_num <- function(x) as.numeric(as.character(x))
s14_true <- function(x) toupper(as.character(x)) == "TRUE"
s14_one <- function(x, what) {
  u <- unique(x)
  if (length(u) != 1L) stop("s14: expected one stored ", what, ", found ", length(u), ".", call. = FALSE)
  u
}
s14_minus_labels <- function(b) bh_minus(format(b, trim = TRUE, drop0trailing = TRUE))
#' Colourbar breaks and labels. Fixed limit: -limit, -limit/2, 0, limit/2, limit, the ends reading <= / >=.
#' Own limit: round breaks inside it.
s14_bar_labels <- function(limit, fixed) {
  if (!fixed) {
    b <- pretty(c(-limit, limit), n = 4); b <- b[abs(b) <= limit + 1e-12]
    return(list(breaks = b, labels = s14_minus_labels(b)))
  }
  num <- function(v) sub("[.]?0+$", "", formatC(v, format = "f", digits = 2))
  b <- c(-limit, -limit / 2, 0, limit / 2, limit)
  s <- bh_minus(num(b)); s[3] <- "0"
  s[1] <- paste0(S14_LE, S14_MINUS, num(limit)); s[5] <- paste0(S14_GE, num(limit))
  list(breaks = b, labels = s)
}

#' The diverging fill: the palette's ends, white at zero. `fixed`: the measure's fixed limit, full colour
#' beyond it (squish, as nv_diverging(measure =)); otherwise the figure's own symmetric limit (no value
#' lies beyond it, so nothing is squished).
s14_fill <- function(limit, fixed) {
  ggplot2::scale_fill_gradient2(low = S14_DIVERGING[["low"]], mid = S14_DIVERGING[["mid"]], high = S14_DIVERGING[["high"]],
                                midpoint = 0, limits = c(-limit, limit), oob = if (fixed) scales::squish else scales::censor,
                                na.value = S14_MISSING, guide = "none")
}
#' A colourbar as a tile strip (horizontal or vertical) with its labels as axis breaks.
s14_colourbar <- function(limit, fixed, title, horizontal = TRUE) {
  bar <- data.frame(value = seq(-limit, limit, length.out = 61))
  bl <- s14_bar_labels(limit, fixed)
  if (horizontal) {
    p <- ggplot2::ggplot(bar, ggplot2::aes(value, 1, fill = value)) +
      ggplot2::geom_tile(width = 2 * limit / 60, height = 1) +
      ggplot2::scale_x_continuous(breaks = bl$breaks, labels = bl$labels, expand = c(0, 0)) +
      ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
      ggplot2::labs(x = title, y = NULL)
  } else {
    p <- ggplot2::ggplot(bar, ggplot2::aes(1, value, fill = value)) +
      ggplot2::geom_tile(height = 2 * limit / 60, width = 1) +
      ggplot2::scale_y_continuous(breaks = bl$breaks, labels = bl$labels, expand = c(0, 0), position = "right") +
      ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
      ggplot2::labs(x = NULL, y = title)
  }
  p + s14_fill(limit, fixed) + theme_f1() +
    ggplot2::theme(plot.margin = ggplot2::margin(1, 1, 1, 1), axis.line = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = NOTE_PT, colour = INK), axis.title = ggplot2::element_text(size = NOTE_PT, colour = INK))
}
s14_theme <- function(top_margin_pt) {
  theme_f1() + ggplot2::theme(
    plot.margin = ggplot2::margin(top_margin_pt, 3, 2, 3),
    strip.text = ggplot2::element_text(size = BODY_PT, colour = INK, face = "plain", margin = ggplot2::margin(b = 1, t = 1, l = 1, r = 1)),
    legend.text = ggplot2::element_text(size = NOTE_PT, colour = INK))
}

# ---------------------------------------------------------------- domain heatmap (D1)
#' The all-blocks domain heatmap of one resolution variant: rows = domains grouped by display tier,
#' columns = phase x sex x contrast; fill = the stored Hedges g of every estimated cell.
s14_panel_domain_heatmap <- function(fsb, variant, w_mm, h_mm, top_margin_pt = 3) {
  if (!variant %in% S14_VARIANTS) stop("s14_panel_domain_heatmap: variant must be one of ", paste(S14_VARIANTS, collapse = ", "), call. = FALSE)
  d <- fsb("D1_domain_heatmap_cells")
  d <- d[d$heatmap_id == S14_HEATMAP_ID & d$variant == variant, , drop = FALSE]
  if (nrow(d) != 84L) stop("s14_panel_domain_heatmap: expected 84 stored cells for ", variant, ", found ", nrow(d), ".", call. = FALSE)
  rows <- unique(d[order(s14_num(d$row_order)), c("row_order", "row_label", "display_tier")])
  if (anyDuplicated(rows$row_label)) stop("s14_panel_domain_heatmap: a domain label occurs twice.", call. = FALSE)
  d$domain <- factor(d$row_label, levels = rev(rows$row_label))
  d$tier <- factor(d$display_tier, levels = unique(rows$display_tier))
  d$comparison <- factor(unname(S14_HEATMAP_CONTRAST[d$contrast]), levels = unname(S14_HEATMAP_CONTRAST))
  d$phase <- factor(unname(S14_PHASE_LABEL[d$PhaseClass]), levels = unname(S14_PHASE_LABEL))
  d$sex <- factor(unname(S14_SEX_LABEL[d$Sex]), levels = unname(S14_SEX_LABEL))
  if (anyNA(d$comparison) || anyNA(d$phase) || anyNA(d$sex)) stop("s14_panel_domain_heatmap: an unknown contrast, phase or sex.", call. = FALSE)
  d$hedges_g_drawn <- ifelse(d$cell_status == "estimated", s14_num(d$hedges_g), NA_real_)
  heat <- ggplot2::ggplot(d, ggplot2::aes(comparison, domain, fill = hedges_g_drawn)) +
    ggplot2::geom_tile(colour = S14_TILE_BORDER, linewidth = S14_TILE_LW) +
    ggplot2::facet_grid(tier ~ phase + sex, scales = "free_y", space = "free_y", switch = "y") +
    s14_fill(S14_G_LIMIT, fixed = TRUE) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) + ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL) +
    s14_theme(top_margin_pt) +
    ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(size = NOTE_PT, colour = INK, lineheight = 0.9),
                   strip.placement = "outside", strip.text.y.left = ggplot2::element_text(size = BODY_PT, angle = 90, colour = INK),
                   panel.spacing.x = grid::unit(1, "mm"), panel.spacing.y = grid::unit(0.8, "mm"))
  bar <- s14_colourbar(S14_G_LIMIT, fixed = TRUE, title = "Hedges g", horizontal = TRUE)
  bottom <- patchwork::plot_spacer() + bar + patchwork::plot_spacer() + patchwork::plot_layout(widths = c(1, 0.55, 1))
  p <- heat / bottom + patchwork::plot_layout(heights = c(1, 0.085))
  bh_panel(p, w_mm, h_mm)
}

# ---------------------------------------------------------------- HMM figures (M1-M4)
s14_state_levels <- function(d, from = "from_state", label = "from_label") {
  u <- unique(d[, c(from, label)]); u <- u[order(s14_num(u[[from]])), , drop = FALSE]
  if (!identical(as.character(u[[from]]), S14_STATES)) stop("s14: expected HMM states 1-4.", call. = FALSE)
  s14_wrap_state(u[[label]])
}

#' M1: difference in group mean transition probability, From (rows) x To (columns), by sex and contrast.
s14_panel_hmm_transition <- function(fsb, w_mm, h_mm, top_margin_pt = 11.5) {
  d <- fsb("M1_hmm_transition_difference")
  lim <- s14_num(s14_one(d$colour_limit, "colour limit"))
  lv <- s14_state_levels(d)
  d$from <- factor(s14_wrap_state(d$from_label), levels = rev(lv))
  d$to <- factor(s14_wrap_state(d$to_label), levels = lv)
  d$comparison <- factor(unname(S14_HMM_CONTRAST[d$contrast]), levels = unname(S14_HMM_CONTRAST))
  d$sex <- factor(unname(S14_SEX_LABEL[d$Sex]), levels = unname(S14_SEX_LABEL))
  d$delta_drawn <- s14_num(d$delta_probability)
  heat <- ggplot2::ggplot(d, ggplot2::aes(to, from, fill = delta_drawn)) +
    ggplot2::geom_tile(colour = S14_TILE_BORDER, linewidth = S14_TILE_LW) +
    ggplot2::facet_grid(sex ~ comparison) +
    s14_fill(lim, fixed = FALSE) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) + ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::labs(x = "To state", y = "From state") +
    s14_theme(top_margin_pt) +
    ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(size = NOTE_PT, colour = INK, lineheight = 0.9, angle = 90, hjust = 1, vjust = 0.5),
                   axis.text.y = ggplot2::element_text(size = NOTE_PT, colour = INK, lineheight = 0.9),
                   panel.spacing = grid::unit(1.2, "mm"))
  bar <- s14_colourbar(lim, fixed = FALSE, title = "Difference in transition probability", horizontal = FALSE)
  p <- (heat | (patchwork::plot_spacer() / bar / patchwork::plot_spacer() + patchwork::plot_layout(heights = c(0.25, 1, 0.25)))) +
    patchwork::plot_layout(widths = c(1, 0.07))
  bh_panel(p, w_mm, h_mm)
}

#' M2: the 8 largest group mean transition probabilities per sex x group, From -> To, as ribbons between
#' fixed state rows (state 1 at the top, the same rows in every facet, so the states are labelled once as
#' axis text: From on the left, To on the right). A ribbon's thickness is proportional to the stored mean;
#' the ribbons at a node are stacked in state order and centred on it. The arithmetic is layout only.
S14_FLOW_NODE_H <- 0.8      # node height (state rows are 1 apart)
S14_FLOW_K <- 0.6           # ribbon thickness per unit of the stored mean; the largest stored stack fits a node
S14_FLOW_NODE_W <- 0.08     # node width (From at x = 0, To at x = 1)
s14_panel_hmm_flows <- function(fsb, w_mm, h_mm, top_margin_pt = 11.5) {
  all <- fsb("M2_hmm_transition_flows")
  lv <- s14_state_levels(all, "from_state", "from_label")
  d <- all[s14_true(all$shown), , drop = FALSE]
  d$thickness <- s14_num(d$mean_probability) * S14_FLOW_K
  d$y_from <- length(S14_STATES) + 1 - match(d$from_state, S14_STATES)
  d$y_to <- length(S14_STATES) + 1 - match(d$to_state, S14_STATES)
  facet <- paste(d$Sex, d$Group)
  # the top edge of each ribbon at a node: the node's ribbons stacked in state order, centred on the node
  stack_top <- function(node, order_by, centre) {
    top <- numeric(nrow(d))
    for (k in unique(node)) {
      i <- which(node == k); i <- i[order(order_by[i])]
      th <- d$thickness[i]
      top[i] <- centre[i] + sum(th) / 2 - c(0, cumsum(th))[seq_along(th)]
    }
    top
  }
  d$top_from <- stack_top(paste(facet, d$from_state), match(d$to_state, S14_STATES), d$y_from)
  d$top_to <- stack_top(paste(facet, d$to_state), match(d$from_state, S14_STATES), d$y_to)
  if (any(d$top_from - d$y_from > S14_FLOW_NODE_H / 2 + 1e-9) || any(d$top_to - d$y_to > S14_FLOW_NODE_H / 2 + 1e-9))
    stop("s14_panel_hmm_flows: a stack of ribbons is taller than its node.", call. = FALSE)
  s <- seq(0, 1, length.out = 41)
  ease <- (1 - cos(pi * s)) / 2
  x <- S14_FLOW_NODE_W / 2 + s * (1 - S14_FLOW_NODE_W)
  ribbons <- do.call(rbind, lapply(seq_len(nrow(d)), function(r) {
    top <- d$top_from[r] + (d$top_to[r] - d$top_from[r]) * ease
    data.frame(ribbon = r, Sex = d$Sex[r], Group = d$Group[r], from_state = d$from_state[r], to_state = d$to_state[r],
               mean_probability = d$mean_probability[r], x_drawn = c(x, rev(x)), y_drawn = c(top, rev(top - d$thickness[r])),
               stringsAsFactors = FALSE)
  }))
  ribbons$group <- factor(ribbons$Group, levels = GROUP_LEV)
  ribbons$sex <- factor(unname(S14_SEX_LABEL[ribbons$Sex]), levels = unname(S14_SEX_LABEL))
  rows <- rev(seq_along(S14_STATES))
  nodes <- data.frame(x_drawn = rep(c(0, 1), each = length(rows)), y_drawn = rep(rows, 2))
  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = ribbons, ggplot2::aes(x_drawn, y_drawn, group = ribbon, fill = group), alpha = 0.85, colour = NA) +
    ggplot2::geom_rect(data = nodes, ggplot2::aes(xmin = x_drawn - S14_FLOW_NODE_W / 2, xmax = x_drawn + S14_FLOW_NODE_W / 2,
                                                  ymin = y_drawn - S14_FLOW_NODE_H / 2, ymax = y_drawn + S14_FLOW_NODE_H / 2),
                       fill = "grey96", colour = "grey35", linewidth = 0.12) +
    ggplot2::facet_grid(sex ~ group) +
    ggplot2::scale_fill_manual(values = GROUP_COL, drop = FALSE, guide = "none") +
    ggplot2::scale_x_continuous(breaks = c(0, 1), labels = c("From", "To"), expand = c(0.03, 0.03)) +
    ggplot2::scale_y_continuous(breaks = rows, labels = lv, sec.axis = ggplot2::dup_axis(name = NULL), expand = c(0.02, 0.02)) +
    ggplot2::labs(x = NULL, y = NULL) +
    s14_theme(top_margin_pt) +
    ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = NOTE_PT, colour = INK, lineheight = 0.9),
                   panel.spacing = grid::unit(2, "mm"))
  bh_panel(p, w_mm, h_mm)
}

#' M3: share of animal x cage-change records per mean dwell-time bin (bins doubling from the HMM bin),
#' one row per state (state 1 at the top, as in the transition panels), the three groups side by side in
#' every bin; dwell time on a log axis, every other bin edge labelled.
s14_panel_hmm_dwell <- function(fsb, w_mm, h_mm, top_margin_pt = 11.5) {
  d <- fsb("M3_hmm_dwell_distribution")
  d$state_row <- length(S14_STATES) + 1L - match(d$State, S14_STATES)
  if (anyNA(d$state_row)) stop("s14_panel_hmm_dwell: an unknown state.", call. = FALSE)
  states <- unique(d[order(d$state_row), c("state_row", "state_label")])
  edges <- sort(unique(c(s14_num(d$bin_lower_min), s14_num(d$bin_upper_min))))
  edge_labels <- ifelse(seq_along(edges) %% 2L == 1L, format(edges, trim = TRUE), "")
  d$lower_drawn <- s14_num(d$bin_lower_min)
  d$group_index <- match(d$Group, GROUP_LEV)
  d$fraction_drawn <- s14_num(d$fraction)
  d$group <- factor(d$Group, levels = GROUP_LEV)
  d$phase <- factor(unname(S14_PHASE_LABEL[d$Phase]), levels = unname(S14_PHASE_LABEL))
  d$sex <- factor(unname(S14_SEX_LABEL[d$Sex]), levels = unname(S14_SEX_LABEL))
  p <- ggplot2::ggplot(d) +
    ggplot2::geom_hline(yintercept = states$state_row, linewidth = 0.12, colour = RULE) +
    ggplot2::geom_rect(ggplot2::aes(xmin = lower_drawn * 2^((group_index - 1) / 3 + 0.03), xmax = lower_drawn * 2^(group_index / 3 - 0.03),
                                    ymin = state_row, ymax = state_row + fraction_drawn * 0.92, fill = group), colour = NA) +
    ggplot2::facet_grid(sex ~ phase) +
    ggplot2::scale_fill_manual(values = GROUP_COL, drop = FALSE, name = NULL) +
    ggplot2::scale_x_continuous(trans = "log2", breaks = edges, labels = edge_labels, expand = c(0.01, 0)) +
    ggplot2::scale_y_continuous(breaks = states$state_row + 0.4, labels = s14_wrap_state(states$state_label), expand = c(0, 0.08)) +
    ggplot2::labs(x = "Mean dwell time (min, log scale)", y = NULL) +
    s14_theme(top_margin_pt) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = NOTE_PT, colour = INK),
                   axis.text.y = ggplot2::element_text(size = NOTE_PT, colour = INK, lineheight = 0.9),
                   axis.ticks.y = ggplot2::element_blank(), panel.spacing = grid::unit(3, "mm"),
                   legend.position = "top", legend.justification = "left")
  bh_panel(p, w_mm, h_mm)
}

#' M4: the behavioural state-architecture contrast (mixed-model estimate, Wald 95% interval) at the
#' 5-min and 10-min HMM resolutions, by sex and phase.
s14_panel_hmm_resolution <- function(fsb, w_mm, h_mm, top_margin_pt = 11.5) {
  d <- fsb("M4_hmm_resolution_sensitivity")
  d$resolution_label <- factor(unname(S14_RESOLUTION[d$resolution]), levels = unname(S14_RESOLUTION))
  d$comparison <- factor(unname(S14_RES_CONTRAST[d$contrast]), levels = unname(S14_RES_CONTRAST))
  d$phase <- factor(unname(S14_PHASE_LABEL[d$Phase]), levels = unname(S14_PHASE_LABEL))
  d$sex <- factor(unname(S14_SEX_LABEL[d$Sex]), levels = unname(S14_SEX_LABEL))
  if (anyNA(d$resolution_label) || anyNA(d$comparison)) stop("s14_panel_hmm_resolution: an unknown resolution or contrast.", call. = FALSE)
  d$estimate_drawn <- s14_num(d$mixed_model_estimate)
  d$ci_low_drawn <- s14_num(d$wald_ci_low)
  d$ci_high_drawn <- s14_num(d$wald_ci_high)
  pd <- ggplot2::position_dodge(width = 0.45)
  p <- ggplot2::ggplot(d, ggplot2::aes(resolution_label, estimate_drawn, colour = comparison, group = comparison)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.25, colour = RULE, linetype = "22") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_low_drawn, ymax = ci_high_drawn), width = 0, linewidth = 0.35, position = pd) +
    ggplot2::geom_line(linewidth = 0.3, position = pd) +
    ggplot2::geom_point(size = 1.1, position = pd) +
    ggplot2::facet_grid(sex ~ phase) +
    ggplot2::scale_colour_manual(values = S14_CONTRAST_COL, drop = FALSE, name = NULL) +
    ggplot2::scale_y_continuous(labels = s14_minus_labels) +
    ggplot2::labs(x = "HMM bin (min)", y = "State-architecture contrast\n(mixed model, Wald 95% CI)") +
    s14_theme(top_margin_pt) +
    ggplot2::theme(panel.spacing = grid::unit(1.5, "mm"), legend.position = "top", legend.justification = "left")
  bh_panel(p, w_mm, h_mm)
}
