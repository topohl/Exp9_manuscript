# Figure 1, consolidated layout (Tobias, 2026-10-12): the panel builders a-e that draw the Figure 1 he had
# assembled by hand from the pipelines, from the pinned bundles. Rendering only: fits no model, computes
# no statistic. Opt-in house profile (BH_HOUSE), the manuscript palette, circles coloured by group.
#
#   a  f1v2_panel_timeline    the SIS design (RFID implant; social instability stress between shuffle start and
#                             shuffle stop; behaviour; samples) and the five behavioural tests, each with the
#                             postnatal age range of the 117 CombZ animals (fsb T1, date arithmetic upstream);
#   b  f1v2_panel_combz       per sex, female above male: later CombZ ("Integrated resilience") per animal at its
#                             stored rank within sex (fsb F2), the stored threshold and CON-mean lines (ebb A2b),
#                             the Animal ID row, the group strip and the six components' signed z as they enter
#                             CombZ (option 3 b's grammar and guards, R/panels/behaviour_figure1_option3_panels.R);
#   c  f1v2_panel_domains     the Stage 14 domain heatmap (fsb D1, all phase blocks, resolution variant "picked"):
#                             the stored Hedges g printed in every cell and the stored post hoc markers as dots;
#   d  f1v2_panel_movement    per sex: CombZ against early movement (ebb A2), the post hoc least-squares display
#                             line with its 95% band (fsb E2, run 29c, fitted upstream once) and Stage 09's
#                             Spearman rho with its BH q within sex (fsb E1);
#   e  f1v2_panel_prediction  held-out predicted against observed CombZ (ebb A2; LOAO and baseline R2, ebb C2;
#                             sex by shape, group by colour) with the permutation null as an inset (ebb A3; the
#                             Holm P, ebb E_multiplicity).
# Every printed number is an$fa() of one key of figures/figure_01_annotation_map.csv (bundle ebb or fsb); the
# value-for-value test reads the panel SVGs. The arithmetic here is layout only (box, icon and label positions).
#
# Requires: R/behaviour_bundle.R, R/figure_support_bundle.R, R/panels/behaviour_figure_style.R,
# R/panels/behaviour_figure1_panels.R, R/panels/behaviour_figure1_house_panels.R and
# R/panels/behaviour_figure1_option3_panels.R sourced first; packages ggtext and patchwork.

F1V2_OUTCOME <- "Integrated resilience"
# b's component rows (fsb F2b ids, stored order), named by what a high signed z means, as option 3 b; the
# author's wording for NOR.
F1V2_COMPONENT_LABEL <- c(NOR = "Novel object recognition", sucrose_pref = "Sucrose preference", weight_dev = "Body-weight change",
                          delta_cort = "Smaller corticosterone rise", adrenal_weight = "Lower adrenal weight",
                          spleen_weight = "Lower spleen weight")
# c: the stored D1 row labels, except the movement row, which Figure 1 calls Movement (as d's x axis)
F1V2_DOMAIN_LABEL <- c(psychomotor_rate = "Movement")
F1V2_PHASE_LABEL <- c(Active = "Dark phase", Inactive = "Light phase")
F1V2_SEX_LABEL <- c(Female = "Females", Male = "Males")
F1V2_CONTRAST_LABEL <- c("RES-CON" = "RES\n−CON", "SUS-CON" = "SUS\n−CON", "RES-SUS" = "RES\n−SUS")
# a: the design steps (fsb T1 event ids) and their display, and the schematic fills of the author's layout
F1V2_SIS_EVENTS <- c(shuffle_start = "Shuffle start", shuffle_stop = "Shuffle stop", samples = "Samples")
F1V2_TEST_EVENTS <- c(sucrose_preference = "Sucrose Preference", social_preference = "Social Preference",
                      open_field = "OFT", novel_object = "NOR", elevated_plus_maze = "EPM")
F1V2_FILL_RFID <- "#1F4A41"; F1V2_FILL_SIS <- GREEN_DARK; F1V2_FILL_BEHAVIOUR <- "#8FA8A2"
F1V2_ARENA <- "grey84"
# d: the display line and its band (the diverging scale's low end, light)
F1V2_LINE <- "grey20"; F1V2_BAND_ALPHA <- 0.22
F1V2_SEX_SHAPE <- c(Female = 21L, Male = 24L)
# e: the permutation-null inset, in fractions of the scatter's plot panel; the builder stops if an animal lies under it
# Corrected-outcome points reach 0.7403 of the panel height; retain clearance.
F1V2_INSET <- c(left = 0.50, bottom = 0.78, right = 1, top = 1)

f1v2_num <- function(x) as.numeric(as.character(x))
#' "P<min>-<max>" for a design step, from its two annotation keys (each a stored T1 cell).
f1v2_age <- function(an, event) paste0("P", an$fa(paste0("a_", event, "_min")), "–", an$fa(paste0("a_", event, "_max")))
#' Polygon of a circle (layout geometry, mm).
f1v2_circle <- function(cx, cy, r, n = 48L) {
  t <- seq(0, 2 * pi, length.out = n + 1L)[-1L]
  data.frame(x = cx + r * cos(t), y = cy + r * sin(t))
}
#' Polygon of a rectangle of width w and height h centred at (cx, cy), turned by `deg` degrees (layout geometry, mm).
f1v2_rect <- function(cx, cy, w, h, deg = 0) {
  x <- c(-w, w, w, -w) / 2; y <- c(-h, -h, h, h) / 2; a <- deg * pi / 180
  data.frame(x = cx + x * cos(a) - y * sin(a), y = cy + x * sin(a) + y * cos(a))
}

# =============================================================== a
f1v2_panel_timeline <- function(fsb, an, w_mm, h_mm) {
  T1 <- fsb("T1_design_ages")
  need <- c(names(F1V2_SIS_EVENTS), names(F1V2_TEST_EVENTS))
  if (!all(need %in% T1$event_id) || anyDuplicated(T1$event_id))
    stop("f1v2_panel_timeline: fsb T1 must hold each design step once.", call. = FALSE)
  txt <- function(x, y, label, pt = F1H$text_pt, colour = INK, hjust = 0.5, vjust = 0.5, angle = 0, fontface = "plain", lineheight = 0.92)
    annotate("text", x = x, y = y, label = label, size = f1h_text(pt), colour = colour, hjust = hjust, vjust = vjust,
             angle = angle, fontface = fontface, lineheight = lineheight)
  box <- function(x0, x1, y0, y1, fill) annotate("rect", xmin = x0, xmax = x1, ymin = y0, ymax = y1, fill = fill, colour = NA)
  poly <- function(d, fill) annotate("polygon", x = d$x, y = d$y, fill = fill, colour = NA)
  Y_BOX <- c(-11, -3.2); Y_MID <- (Y_BOX[1] + Y_BOX[2]) / 2; Y_LINE <- -13.8; Y_AGE <- -15.3
  # the SIS block: boxes and the three steps between them, each a dot on the timeline with its ages
  X_RFID <- c(4, 15.6); X_SIS <- c(19.6, 63.4); X_BEH <- c(67.4, 77.6)
  X_STEP <- c(shuffle_start = 17.6, shuffle_stop = 65.4, samples = 79.6)
  # the tests: five icons spaced over the right part of the box
  X_TEST <- stats::setNames(seq(94, w_mm - 6, length.out = length(F1V2_TEST_EVENTS)), names(F1V2_TEST_EVENTS))
  Y_ICON <- -5.6
  icon_dots <- function(cx, cy) list(poly(f1v2_circle(cx - 3.5, cy + 1.3, 1.8), GROUP_COL[["CON"]]),
                                     poly(f1v2_circle(cx + 3.5, cy - 1.3, 1.8), GROUP_COL[["SUS"]]))
  cs <- X_TEST[["sucrose_preference"]]; cp <- X_TEST[["social_preference"]]; co <- X_TEST[["open_field"]]
  cn <- X_TEST[["novel_object"]]; ce <- X_TEST[["elevated_plus_maze"]]
  shuffle <- function(x0, x1, y0, y1) list(
    annotate("segment", x = x0, xend = x1, y = y0, yend = y1, colour = "white", linewidth = 0.3,
             arrow = grid::arrow(length = grid::unit(0.9, "mm"), type = "closed"), arrow.fill = "white"),
    annotate("segment", x = x0, xend = x1, y = y1, yend = y0, colour = "white", linewidth = 0.3,
             arrow = grid::arrow(length = grid::unit(0.9, "mm"), type = "closed"), arrow.fill = "white"))
  pa <- ggplot() +
    box(X_RFID[1], X_RFID[2], Y_BOX[1], Y_BOX[2], F1V2_FILL_RFID) +
    txt(sum(X_RFID) / 2, Y_MID, "RFID\nimplant", pt = F1H$title_pt, colour = "white", fontface = "bold") +
    box(X_SIS[1], X_SIS[2], Y_BOX[1], Y_BOX[2], F1V2_FILL_SIS) +
    shuffle(X_SIS[1] + 1.4, X_SIS[1] + 6.2, Y_MID - 2.2, Y_MID + 2.2) +
    txt(X_SIS[1] + 7.8, Y_MID + 1.6, "Social Instability Stress", pt = F1H$title_pt, colour = "white", hjust = 0, fontface = "bold") +
    txt(X_SIS[1] + 7.8, Y_MID - 1.5, "Shuffling of social groups\ntwice per week for two weeks", colour = "white", hjust = 0) +
    box(X_BEH[1], X_BEH[2], Y_BOX[1], Y_BOX[2], F1V2_FILL_BEHAVIOUR) +
    txt(sum(X_BEH) / 2, Y_MID, "Behavior", pt = F1H$title_pt, colour = "white", fontface = "bold") +
    annotate("segment", x = X_RFID[1], xend = X_STEP[["samples"]] + 1.5, y = Y_LINE, yend = Y_LINE, linewidth = F1H$axis_lw, colour = INK) +
    annotate("point", x = X_STEP, y = Y_LINE, size = 1.1, colour = INK)
  for (e in names(F1V2_SIS_EVENTS))
    pa <- pa + txt(X_STEP[[e]], Y_MID, F1V2_SIS_EVENTS[[e]], angle = 90) + txt(X_STEP[[e]], Y_AGE, f1v2_age(an, e), vjust = 1, fontface = "bold")
  # icons (layout geometry only): two tilted bottles; arena with two mice; open field; arena with two objects; plus maze
  pa <- pa +
    poly(f1v2_rect(cs - 1.6, Y_ICON + 0.4, 2.2, 5.2, -24), "grey60") + poly(f1v2_rect(cs - 0.7, Y_ICON - 2.2, 0.8, 1.2, -24), "grey40") +
    poly(f1v2_rect(cs + 1.6, Y_ICON + 0.4, 2.2, 5.2, -24), "grey60") + poly(f1v2_rect(cs + 2.5, Y_ICON - 2.2, 0.8, 1.2, -24), "grey40") +
    box(cp - 6.6, cp + 6.6, Y_ICON - 3.4, Y_ICON + 3.4, F1V2_ARENA) + icon_dots(cp, Y_ICON) +
    box(co - 3.4, co + 3.4, Y_ICON - 3.4, Y_ICON + 3.4, F1V2_ARENA) +
    box(cn - 3.4, cn + 3.4, Y_ICON - 3.4, Y_ICON + 3.4, F1V2_ARENA) +
    poly(f1v2_circle(cn - 1.5, Y_ICON + 1.2, 1.05), GROUP_COL[["CON"]]) + box(cn + 0.6, cn + 2.6, Y_ICON - 2.3, Y_ICON - 0.3, GROUP_COL[["SUS"]]) +
    box(ce - 5.4, ce + 5.4, Y_ICON - 1.05, Y_ICON + 1.05, F1V2_ARENA) + box(ce - 1.05, ce + 1.05, Y_ICON - 3.6, Y_ICON + 3.6, F1V2_ARENA) +
    annotate("segment", x = min(X_TEST) - 6.6, xend = min(max(X_TEST) + 6.6, w_mm - 0.5), y = Y_LINE, yend = Y_LINE,
             linewidth = F1H$axis_lw, colour = INK) +
    annotate("point", x = X_TEST, y = Y_LINE, size = 1.1, colour = INK)
  for (e in names(F1V2_TEST_EVENTS))
    pa <- pa + txt(X_TEST[[e]], Y_LINE + 1.4, F1V2_TEST_EVENTS[[e]], pt = F1H$title_pt, vjust = 0, fontface = "bold") +
      txt(X_TEST[[e]], Y_AGE, f1v2_age(an, e), vjust = 1)
  pa <- pa +
    scale_x_continuous(limits = c(0, w_mm), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-h_mm, 0), expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    theme_void(base_size = F1H$title_pt, base_family = "sans") +
    theme(plot.margin = margin(0, 0, 0, 0))
  bh_panel(pa, w_mm, h_mm)
}

# =============================================================== b
#' One sex's block of b: dots (CombZ at its stored rank), Animal ID row, group strip, component heatmap.
f1v2_combz_block <- function(s, ani, hm, gone, bnd, thr, legend, colourbar, fill_z) {
  a <- ani[ani$Sex == s, , drop = FALSE]; h <- hm[hm$Sex == s, , drop = FALSE]; g <- gone[gone$Sex == s, , drop = FALSE]
  b <- bnd[bnd$Sex == s, , drop = FALSE]; t <- thr[thr$Sex == s, , drop = FALSE]
  xs <- scale_x_continuous(limits = c(b$lo, b$hi), expand = c(0, 0))
  vline <- geom_vline(xintercept = b$x, linewidth = F1H$axis_lw, colour = INK, linetype = "22")
  no_x <- theme(axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())
  row_theme <- theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0, 1, 0.5, 1))
  key_aes <- list(size = F1H$point_size, stroke = F1H$point_stroke, colour = F1H$point_colour)
  p_dots <- ggplot(a, aes(x, CombZ)) +
    geom_hline(yintercept = t$control_mean_combz, linewidth = F1H$rule_lw, colour = RULE) +
    geom_hline(yintercept = t$susceptibility_threshold, linewidth = 0.45, colour = INK, linetype = "22") +
    vline +
    geom_point(aes(fill = Group), shape = 21, size = F1H$point_size, stroke = F1H$point_stroke, colour = F1H$point_colour) +
    annotate("label", x = b$lo + 0.4, y = t$susceptibility_threshold, label = t$label, hjust = 0, vjust = -0.25, size = f1h_text(),
             colour = INK, fill = "white", linewidth = 0, label.padding = unit(0.5, "pt")) +
    annotate("text", x = b$lo + 0.4, y = Inf, label = s, hjust = 0, vjust = 1.3, size = f1h_text(F1H$title_pt), colour = INK) +
    xs + scale_fill_manual(values = GROUP_COL, name = NULL, drop = FALSE) +
    guides(fill = if (legend) guide_legend(override.aes = key_aes) else "none") +
    scale_y_continuous(labels = f1_minus_labels, breaks = c(-1, 0, 1), expand = expansion(mult = c(0.05, 0.12))) +
    labs(y = sub(" ", "\n", F1V2_OUTCOME, fixed = TRUE)) +
    theme_f1(style = "house") + no_x +
    theme(legend.position = "inside", legend.position.inside = c(0.995, 1.04), legend.justification = c(1, 1),
          legend.direction = "horizontal", legend.background = element_blank(), legend.key.spacing.x = unit(3.5, "pt"),
          axis.title.y = element_text(lineheight = 0.92), plot.margin = margin(0, 1, 0.5, 1))
  p_animal <- ggplot(a, aes(x, 1)) +
    geom_text(aes(label = AnimalNum), angle = 90, size = f1h_text(), colour = INK, hjust = 0.5, vjust = 0.5) +
    vline + xs + scale_y_continuous(breaks = 1, labels = "Animal ID", expand = c(0, 0)) + labs(y = NULL) +
    theme_f1(style = "house") + no_x + row_theme
  p_strip <- ggplot(a, aes(x, 1)) +
    geom_tile(aes(fill = Group), height = 1, colour = "white", linewidth = 0.12) +
    vline + xs + scale_fill_manual(values = GROUP_COL, guide = "none") +
    scale_y_continuous(breaks = 1, labels = "Group", expand = c(0, 0)) + labs(y = NULL) +
    theme_f1(style = "house") + no_x + row_theme
  p_heat <- ggplot(h, aes(x, row)) +
    geom_tile(aes(fill = signed_z), colour = NEUTRAL_BOX, linewidth = 0.12) +
    geom_segment(data = g, aes(x = x - 0.38, xend = x + 0.38, y = yn - 0.38, yend = yn + 0.38), inherit.aes = FALSE,
                 linewidth = F1H$rule_lw, colour = MUTED) +
    vline + xs + fill_z + scale_y_discrete(expand = c(0, 0)) + labs(y = NULL) +
    theme_f1(style = "house") + no_x + row_theme
  side <- if (colourbar) {
    bar <- data.frame(z = seq(-F1O3H_Z_CAP, F1O3H_Z_CAP, length.out = 49))
    ggplot(bar, aes(1, z, fill = z)) +
      geom_tile(width = 1, height = 2 * F1O3H_Z_CAP / 48) + fill_z +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(breaks = c(-F1O3H_Z_CAP, 0, F1O3H_Z_CAP),
                         labels = c(paste0("≤", f1_minus_labels(-F1O3H_Z_CAP)), "0", paste0("≥", f1_minus_labels(F1O3H_Z_CAP))),
                         position = "right", expand = expansion(mult = 0.06)) +
      labs(x = NULL, y = "Signed *z*") +
      theme_f1(style = "house") +
      theme(axis.line = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(),
            axis.title.y.right = f1h_markdown(angle = 90, margin = margin(l = 1.5)), plot.margin = margin(0, 1, 1, 3))
  } else patchwork::plot_spacer()
  patchwork::wrap_plots(A = p_dots, E = p_animal, B = p_strip, C = p_heat, D = side, design = "A#\nE#\nB#\nCD",
                        heights = c(13, 4.4, 1.9, 12.6), widths = grid::unit(c(1, 2.2), c("null", "mm")))
}

f1v2_panel_combz <- function(tab, fsb, an, w_mm, h_mm) {
  fa <- an$fa
  F2 <- fsb("F2_combz_components"); F2b <- fsb("F2b_combz_definition"); thr <- tab("A2b_combz_thresholds")
  F2b <- F2b[order(F2b$component_order), , drop = FALSE]
  if (!identical(F2b$component, names(F1V2_COMPONENT_LABEL)))
    stop("f1v2_panel_combz: the stored components differ from the heatmap rows.", call. = FALSE)
  inv <- toupper(as.character(F2b$sign_inverted)) == "TRUE"
  if (!setequal(F2b$component[inv], F1O3H_COMPONENT_INVERTED) || !all(F2b$direction[inv] == -1) || !all(F2b$direction[!inv] == 1))
    stop("f1v2_panel_combz: the stored sign inversions differ from the heatmap row names.", call. = FALSE)
  sexes <- c("Female", "Male")
  ani <- F2[F2$component == F2b$component[1], c("AnimalNum", "Sex", "Group", "CombZ", "below_threshold", "heatmap_order_within_sex")]
  if (anyDuplicated(ani$AnimalNum) || nrow(F2) != nrow(ani) * nrow(F2b))
    stop("f1v2_panel_combz: fsb F2 must hold every component once per animal.", call. = FALSE)
  names(ani)[names(ani) == "heatmap_order_within_sex"] <- "x"
  # the RES/SUS boundary half a column right of the last below-threshold animal (the stored order puts them first; checked)
  bnd <- do.call(rbind, lapply(sexes, function(s) {
    o <- ani$x[ani$Sex == s]; b <- ani$below_threshold[ani$Sex == s]
    if (!identical(sort(o), seq_along(o)) || !any(b) || max(o[b]) >= min(o[!b]))
      stop("f1v2_panel_combz: the stored heatmap order does not put the below-threshold animals first (", s, ").", call. = FALSE)
    data.frame(Sex = s, x = max(o[b]) + 0.5, lo = 0.5, hi = length(o) + 0.5)
  }))
  hm <- F2[, c("AnimalNum", "Sex", "Group", "component", "signed_z", "present", "heatmap_order_within_sex")]
  names(hm)[names(hm) == "heatmap_order_within_sex"] <- "x"
  hm$row <- factor(unname(F1V2_COMPONENT_LABEL[hm$component]), levels = rev(unname(F1V2_COMPONENT_LABEL)))
  gone <- hm[!hm$present, , drop = FALSE]; gone$yn <- as.integer(gone$row)
  ani$Group <- factor(ani$Group, levels = GROUP_LEV); hm$Group <- factor(hm$Group, levels = GROUP_LEV)
  thr$label <- ifelse(thr$Sex == "Female", fa("b_thr_female"), fa("b_thr_male"))
  fill_z <- scale_fill_gradient2(low = F1O3H_DIVERGING[["low"]], mid = F1O3H_DIVERGING[["mid"]], high = F1O3H_DIVERGING[["high"]],
                                 midpoint = 0, limits = c(-F1O3H_Z_CAP, F1O3H_Z_CAP), oob = scales::squish,
                                 na.value = NEUTRAL_BOX, guide = "none")
  blocks <- lapply(sexes, function(s) f1v2_combz_block(s, ani, hm, gone, bnd, thr, legend = s == "Female", colourbar = s == "Male", fill_z))
  pb <- patchwork::wrap_plots(blocks[[1]], patchwork::plot_spacer(), blocks[[2]], ncol = 1, heights = c(1, 0.035, 1)) +
    patchwork::plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0), plot.background = F1H_BACKGROUND))
  bh_panel(pb, w_mm, h_mm)
}

# =============================================================== c
f1v2_panel_domains <- function(fsb, an, w_mm, h_mm) {
  d <- fsb("D1_domain_heatmap_cells")
  d <- d[d$heatmap_id == "all_blocks" & d$variant == "picked", , drop = FALSE]
  if (nrow(d) != 84L || !all(d$cell_status == "estimated"))
    stop("f1v2_panel_domains: expected 84 estimated stored cells (all_blocks, picked).", call. = FALSE)
  marked <- toupper(as.character(d$marker)) == "TRUE"
  if (any(marked & d$marker_class != "posthoc"))
    stop("f1v2_panel_domains: a stored marker that is not post hoc has no glyph in this panel.", call. = FALSE)
  rows <- unique(d[order(f1v2_num(d$row_order)), c("row_order", "row_key", "row_label", "display_tier")])
  if (anyDuplicated(rows$row_key)) stop("f1v2_panel_domains: a domain occurs twice.", call. = FALSE)
  rows$display <- ifelse(rows$row_key %in% names(F1V2_DOMAIN_LABEL), F1V2_DOMAIN_LABEL[rows$row_key], rows$row_label)
  d$domain <- factor(rows$display[match(d$row_key, rows$row_key)], levels = rev(rows$display))
  d$tier <- factor(d$display_tier, levels = unique(rows$display_tier))
  d$column <- factor(paste(F1V2_PHASE_LABEL[d$PhaseClass], F1V2_SEX_LABEL[d$Sex], sep = "\n"),
                     levels = as.vector(outer(unname(F1V2_SEX_LABEL), unname(F1V2_PHASE_LABEL), function(s, p) paste(p, s, sep = "\n"))))
  d$comparison <- factor(unname(F1V2_CONTRAST_LABEL[d$contrast]), levels = unname(F1V2_CONTRAST_LABEL))
  if (anyNA(d$column) || anyNA(d$comparison)) stop("f1v2_panel_domains: an unknown phase, sex or contrast.", call. = FALSE)
  d$key <- sprintf("c_g_%s_%s_%s_%s", d$PhaseClass, d$Sex, sub("-", "", d$contrast, fixed = TRUE), d$row_key)
  d$label <- vapply(d$key, an$fa, "", USE.NAMES = FALSE)
  d$g <- f1v2_num(d$hedges_g)
  dots <- d[marked, , drop = FALSE]
  heat <- ggplot(d, aes(comparison, domain)) +
    geom_tile(aes(fill = g), colour = S14_TILE_BORDER, linewidth = S14_TILE_LW) +
    geom_text(aes(label = label), size = f1h_text(), colour = INK) +
    geom_point(data = dots, size = 0.55, colour = INK, position = position_nudge(x = 0.36, y = 0.3)) +
    facet_grid(tier ~ column, scales = "free_y", space = "free_y") +
    s14_fill(S14_G_LIMIT, fixed = TRUE) +
    scale_x_discrete(expand = c(0, 0), position = "bottom") + scale_y_discrete(expand = c(0, 0)) +
    labs(x = NULL, y = NULL) +
    theme_f1(style = "house") +
    theme(axis.line = element_blank(), axis.ticks = element_blank(),
          axis.text.x = element_text(size = F1H$text_pt, colour = INK, lineheight = 0.9),
          axis.text.y = element_text(size = F1H$title_pt, colour = INK),
          strip.text.x = element_text(size = F1H$title_pt, colour = INK, face = "bold", lineheight = 0.92, margin = margin(b = 1.5)),
          strip.text.y = element_blank(), panel.spacing.x = grid::unit(1.6, "mm"), panel.spacing.y = grid::unit(1.4, "mm"),
          plot.margin = margin(0, 1, 0, 1))
  bl <- s14_bar_labels(S14_G_LIMIT, fixed = TRUE)
  barv <- data.frame(value = seq(-S14_G_LIMIT, S14_G_LIMIT, length.out = 61))
  bar <- ggplot(barv, aes(value, 1, fill = value)) +
    geom_tile(width = 2 * S14_G_LIMIT / 60, height = 1) +
    s14_fill(S14_G_LIMIT, fixed = TRUE) +
    scale_x_continuous(breaks = bl$breaks, labels = bl$labels, expand = c(0, 0)) +
    scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
    labs(x = "Hedges *g*", y = NULL) +
    theme_f1(style = "house") +
    theme(axis.line = element_blank(), axis.title.x = f1h_markdown(margin = margin(t = 1)), plot.margin = margin(1, 1, 1, 1))
  key <- ggplot() +
    annotate("point", x = 0, y = 0, size = 0.9, colour = INK) +
    annotate("text", x = 0.35, y = 0, label = "post hoc q < 0.05", hjust = 0, size = f1h_text(), colour = INK) +
    scale_x_continuous(limits = c(-0.3, 4), expand = c(0, 0)) + scale_y_continuous(limits = c(-1, 1)) +
    theme_void() + theme(plot.margin = margin(0, 0, 0, 0))
  bottom <- patchwork::plot_spacer() + key + bar + patchwork::plot_spacer() + patchwork::plot_layout(widths = c(0.9, 0.55, 0.62, 0.22))
  pc <- heat / bottom + patchwork::plot_layout(heights = c(1, 0.075)) +
    patchwork::plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0), plot.background = F1H_BACKGROUND))
  bh_panel(pc, w_mm, h_mm)
}

# =============================================================== d
f1v2_panel_movement <- function(tab, fsb, an, w_mm, h_mm) {
  fa <- an$fa
  A2 <- tab("A2_prediction_animals"); A2$Group <- factor(A2$Group, levels = GROUP_LEV)
  E2 <- fsb("E2_movement_combz_fit_lines")
  for (k in c("x", "fit", "ci_low", "ci_high")) E2[[k]] <- f1v2_num(E2[[k]])
  if (!setequal(unique(E2$Sex), c("Female", "Male"))) stop("f1v2_panel_movement: fsb E2 must hold a line per sex.", call. = FALSE)
  band_fill <- F1O3H_DIVERGING[["low"]]
  one <- function(s, bottom) {
    a <- A2[A2$Sex == s, , drop = FALSE]; l <- E2[E2$Sex == s, , drop = FALSE]; l <- l[order(f1v2_num(l$grid_index)), , drop = FALSE]
    key <- tolower(s)
    stat <- list(f1h_expr("'Spearman ' * italic('ρ ') * '= ' * %s", fa(paste0("d_rho_", key))),
                 f1h_expr("'BH ' * italic('q ') * '= ' * %s", fa(paste0("d_q_", key))))
    right <- grid::unit(1, "npc") - grid::unit(0.8, "mm")
    gtxt <- function(label, dy, col, bold = FALSE) annotation_custom(grid::textGrob(label, x = right, y = grid::unit(1, "npc") - grid::unit(dy, "mm"),
      hjust = 1, vjust = 1, gp = grid::gpar(fontsize = if (bold) F1H$title_pt else F1H$text_pt, fontface = if (bold) "bold" else "plain",
                                              fontfamily = "sans", col = col)))
    ggplot(a, aes(Movement_mean, observed_CombZ)) +
      geom_ribbon(data = l, aes(x = x, ymin = ci_low, ymax = ci_high), inherit.aes = FALSE, fill = band_fill, alpha = F1V2_BAND_ALPHA) +
      geom_line(data = l, aes(x = x, y = fit), inherit.aes = FALSE, linewidth = 0.4, colour = F1V2_LINE) +
      geom_point(aes(fill = Group), shape = 21, size = F1H$point_size + 0.25, stroke = F1H$point_stroke, colour = F1H$point_colour) +
      gtxt(s, 0.8, INK, bold = TRUE) +
      annotation_custom(f1h_math_grob(stat[[1]], x = right, y = grid::unit(1, "npc") - grid::unit(4.9, "mm"), hjust = 1)) +
      annotation_custom(f1h_math_grob(stat[[2]], x = right, y = grid::unit(1, "npc") - grid::unit(7.1, "mm"), hjust = 1)) +
      gtxt("CON", 8.2, GROUP_COL[["CON"]]) + gtxt("RES", 10.4, s14_darken(GROUP_COL[["RES"]], 0.25)) + gtxt("SUS", 12.6, GROUP_COL[["SUS"]]) +
      scale_fill_manual(values = GROUP_COL, guide = "none") +
      scale_y_continuous(labels = f1_minus_labels, breaks = c(-1, 0, 1), expand = expansion(mult = c(0.04, 0.10))) +
      scale_x_continuous(labels = f1_minus_labels) +
      labs(x = "Movement", y = "Integrated Resilience (*z*)") +
      theme_f1(style = "house") +
      theme(axis.title.y = f1h_markdown(margin = margin(r = 1)), axis.title.x = element_text(margin = margin(t = 1.5)),
            plot.margin = margin(1, 1, if (bottom) 1 else 3, 1))
  }
  pd <- patchwork::wrap_plots(one("Female", FALSE), one("Male", TRUE), ncol = 1) +
    patchwork::plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0), plot.background = F1H_BACKGROUND))
  bh_panel(pd, w_mm, h_mm)
}

# =============================================================== e
f1v2_panel_prediction <- function(tab, an, w_mm, h_mm) {
  fa <- an$fa
  A2 <- tab("A2_prediction_animals")
  A2$Group <- factor(A2$Group, levels = GROUP_LEV); A2$Sex <- factor(A2$Sex, levels = names(F1V2_SEX_SHAPE))
  lim <- range(c(A2$observed_CombZ, A2$heldout_prediction), na.rm = TRUE)
  # layout guard: no animal under the inset (the plotted range is lim widened by ggplot's default 5% on each side)
  rng <- lim + c(-1, 1) * 0.05 * (lim[2] - lim[1])
  corner <- rng[1] + F1V2_INSET[c("left", "bottom")] * (rng[2] - rng[1])
  if (any(A2$observed_CombZ >= corner[["left"]] & A2$heldout_prediction >= corner[["bottom"]]))
    stop("f1v2_panel_prediction: an animal lies under the permutation inset; move it (F1V2_INSET).", call. = FALSE)
  A3 <- tab("A3_permutation_draws")
  nulls <- A3[A3$model_id == "movement_mean" & !A3$is_observed, , drop = FALSE]
  obs <- an$ann("e_loao")
  left <- grid::unit(0.9, "mm"); top <- function(mm) grid::unit(1, "npc") - grid::unit(mm, "mm")
  txt <- function(label, x, y, hjust = 0, vjust = 1, col = INK, pt = F1H$text_pt)
    grid::textGrob(label, x = x, y = y, hjust = hjust, vjust = vjust, gp = grid::gpar(fontsize = pt, fontfamily = "sans", col = col))
  pt_key <- function(pch, y) grid::pointsGrob(x = left + grid::unit(0.9, "mm"), y = y, pch = pch, size = grid::unit(1.9, "mm"),
                                              gp = grid::gpar(col = F1H$point_colour, fill = "grey35", lwd = 0.6))
  key_y <- c(Female = 7.4, Male = 9.6)
  scatter <- ggplot(A2, aes(observed_CombZ, heldout_prediction)) +
    geom_abline(slope = 1, intercept = 0, linetype = "22", linewidth = F1H$identity_lw, colour = "grey55") +
    geom_point(aes(fill = Group, shape = Sex), size = F1H$point_size + 0.35, stroke = F1H$point_stroke, colour = F1H$point_colour) +
    scale_fill_manual(values = GROUP_COL, guide = "none") + scale_shape_manual(values = F1V2_SEX_SHAPE, guide = "none") +
    scale_x_continuous(labels = f1_minus_labels) + scale_y_continuous(labels = f1_minus_labels) +
    coord_cartesian(xlim = lim, ylim = lim) +
    annotation_custom(f1h_math_grob(f1h_expr("'LOAO ' * italic('R² ') * '= ' * %s", fa("e_loao")), x = left, y = top(2.4), hjust = 0)) +
    annotation_custom(f1h_math_grob(f1h_expr("'Baseline ' * italic('R² ') * '= ' * %s", fa("e_base")), x = left, y = top(4.6), hjust = 0)) +
    annotation_custom(pt_key(21, top(key_y[["Female"]]))) + annotation_custom(txt("Female", left + grid::unit(2.2, "mm"), top(key_y[["Female"]] - 0.9))) +
    annotation_custom(pt_key(24, top(key_y[["Male"]]))) + annotation_custom(txt("Male", left + grid::unit(2.2, "mm"), top(key_y[["Male"]] - 0.9))) +
    annotation_custom(txt("CON", left, top(11.4), col = GROUP_COL[["CON"]])) +
    annotation_custom(txt("RES", left, top(13.5), col = s14_darken(GROUP_COL[["RES"]], 0.25))) +
    annotation_custom(txt("SUS", left, top(15.6), col = GROUP_COL[["SUS"]])) +
    labs(x = "Observed Integrated Resilience", y = "Held-out predicted Integrated Resilience") +
    theme_f1(style = "house") +
    theme(axis.title.x = element_text(margin = margin(t = 1.5)), plot.margin = margin(1, 1, 1, 1))
  inset <- ggplot(nulls, aes(performance_value)) +
    geom_histogram(bins = 24, fill = "grey78", colour = "white", linewidth = 0.12) +
    geom_vline(xintercept = obs, linewidth = 0.45, colour = GROUP_COL[["SUS"]]) +
    annotation_custom(txt("Observed", x = grid::unit(-0.5, "mm"), y = grid::unit(1, "npc") - grid::unit(0.3, "mm"), hjust = 1), xmin = obs, xmax = obs) +
    annotation_custom(f1h_math_grob(f1h_expr("'Holm ' * italic('P ') * '= ' * %s", fa("e_perm_p")), x = grid::unit(-0.5, "mm"),
                                    y = grid::unit(1, "npc") - grid::unit(4.0, "mm"), hjust = 1), xmin = obs, xmax = obs) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks = function(l) pretty(c(0, l[2]), n = 1)) +
    scale_x_continuous(labels = f1_minus_labels, breaks = function(l) pretty(l, n = 2)) +
    labs(x = NULL, y = NULL, title = "Permutations") +
    theme_f1(style = "house") +
    theme(plot.title = element_text(size = F1H$text_pt, colour = INK, hjust = 0.5, margin = margin(b = 0.5)),
          plot.background = element_rect(fill = "white", colour = NA), plot.margin = margin(1, 1, 1, 1))
  pe <- scatter + patchwork::inset_element(inset, left = F1V2_INSET[["left"]], bottom = F1V2_INSET[["bottom"]], right = F1V2_INSET[["right"]],
                                           top = F1V2_INSET[["top"]], align_to = "panel")
  bh_panel(pe, w_mm, h_mm)
}
