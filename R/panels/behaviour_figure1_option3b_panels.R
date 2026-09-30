# Figure 1 option 3b, Nature layout (candidate figure_01_option3b_nature; OPTION3B_SPEC): the revised
# panels c and d of option 3. Panels a, b, e and f are option 3's builders at option 3's boxes.
# Rendering only: fits no model, computes no statistic.
#
#   c  f1o3b_panel_cc1         first active phase after CC1, per measure (position-change rate; social-
#                              spatial overlap) and sex: every animal as a circle coloured by group (CON
#                              drawn like RES and SUS), the three post hoc batch-balanced group means with
#                              their 95% CIs (black, just right of each cloud), and under each sex a forest
#                              of RES - CON, SUS - CON and SUS - RES with the Holm P (m = 3) beside each
#                              row. The means and contrasts are the stored rows of the post hoc run 29b
#                              (registry POSTHOC_CON_CONTRASTS v1.0), copied verbatim into the figure-
#                              support bundle (fsb P1). No CON cage means, no female - male row.
#   d  f1o3b_panel_trajectory  CC1 -> CC4, per measure and sex: three lines, the CON descriptive mean
#                              (dashed grey, fsb F1b; no interval, no cage marks) and the registered
#                              RES / SUS TR_BY_SEX model means with 95% CIs (ebb C2), offset so their
#                              intervals do not overlap, and one line under the header for the
#                              registered P-TR test (Q2b, Holm P) of both measures.
#
# Arguments as in R/panels/behaviour_figure1_option3_panels.R (tab, fsb, an, w_mm, h_mm, frame, and
# for c the two jitter seeds). Every printed number is an$fa() of one annotation key; the plotted
# means, contrasts and intervals are the stored rows (row filters only).
#
# Display rule for the post hoc group means (OPTION3B_SPEC c; the CON means have Kenward-Roger df
# below 2): each measure has fixed presentational y limits (F1O3B_LIMITS) that hold every animal (the
# builder stops otherwise); an interval that extends beyond them is drawn to the axis limit and ends
# in an arrowhead (the legend says so; the full interval is in the source data). The forests are
# never truncated: their fixed limits (F1O3B_DIFF_LIMITS) must hold every interval.
#
# Requires: R/behaviour_bundle.R, R/panels/behaviour_figure_style.R, R/panels/behaviour_figure1_panels.R,
# R/panels/behaviour_figure1_nature_panels.R and R/panels/behaviour_figure1_option3_panels.R sourced
# first; packages ggtext and patchwork.

# The post hoc rows this figure may draw: the registry's primary model, frozen registry v1.0, its tier.
F1O3B_REGISTRY_SHA256 <- "a6435d8d84b164671b59486f7a3eb9af2bff29bd251c58866cd4b777f9fa0604"
F1O3B_TIER <- "POST HOC exploratory"
F1O3B_MODEL <- "primary_separate_cage_variances"
# The one on-panel label (the stored display_note of every P1 row; the builder checks it). "3 cages/sex"
# is the design (one CON cage per batch, three batches per sex), checked against P1 n_con_cages.
F1O3B_LABEL <- "Post hoc, cage-aware; CON = 3 cages/sex"
# Forest rows, top to bottom, with the key stem of their Holm P.
F1O3B_CONTRASTS <- c(RES_minus_CON = "RES − CON", SUS_minus_CON = "SUS − CON", SUS_minus_RES = "SUS − RES")
F1O3B_ROW_Y <- c(RES_minus_CON = 3, SUS_minus_CON = 2, SUS_minus_RES = 1)
F1O3B_KEY <- c(RES_minus_CON = "rc", SUS_minus_CON = "sc", SUS_minus_RES = "sr")
F1O3B_MEASURE_KEY <- c(crossing_rate = "cr", shared_zone_use = "sz")
F1O3B_SEX_KEY <- c(Female = "f", Male = "m")
# Presentational limits (see the header): the group-mean plots (y) and the forests (x).
F1O3B_LIMITS <- list(crossing_rate = c(0, 50), shared_zone_use = c(0.1, 0.45))
F1O3B_DIFF_LIMITS <- list(crossing_rate = c(-25, 25), shared_zone_use = c(-0.125, 0.125))
F1O3B_DIFF_BREAKS <- list(crossing_rate = c(-20, 0, 20), shared_zone_use = c(-0.1, 0, 0.1))
F1O3B_DOTS_Y <- c(crossing_rate = paste0("Position-change<br>rate (", F1N_PER_H, ")"),
                  shared_zone_use = "Social-spatial overlap<br>(fraction of dyadic time)")
F1O3B_DIFF_X <- c(crossing_rate = paste0("Difference (", F1N_PER_H, ")"), shared_zone_use = "Difference (fraction)")
F1O3B_TRAJ_Y <- c(crossing_rate = paste0("Position-change<br>rate (", F1N_PER_H, ")"),
                  shared_zone_use = "Social-spatial<br>overlap (fraction<br>of dyadic time)")
# d: the registered P-TR test of both measures on one line (two stored keys, two text nodes)
F1O3B_PTR_LABEL <- "'Group × cage change × sex (P-TR), Holm ' * italic('P') * ': rate ' * %s * ', overlap ' * %s"
# d: RES and SUS either side of the cage change; the CON mean at it (no interval, so no offset needed)
F1O3B_TRAJ_OFFSET <- c(CON = 0, RES = -0.14, SUS = 0.14)
F1O3B_ARROW <- grid::arrow(length = grid::unit(0.9, "mm"), angle = 28, type = "closed")

# The post hoc rows of one measure and sex (row filters only), with the guards: the frozen registry,
# the post hoc tier and its label, the primary model, every fit OK, three means and three contrasts,
# Holm over m = 3, and three CON cages.
f1o3b_posthoc_rows <- function(P1, k, sex) {
  r <- P1[P1$measure == k & P1$Sex == sex & P1$model == F1O3B_MODEL, , drop = FALSE]
  want <- c(paste0(GROUP_LEV, "_mean"), names(F1O3B_CONTRASTS))
  where <- paste0("f1o3b: fsb P1 ", k, " ", sex)
  if (nrow(r) != length(want) || !setequal(r$estimand, want) || anyDuplicated(r$estimand))
    stop(where, " must hold the three group means and the three contrasts once each.", call. = FALSE)
  if (!all(r$status == "OK")) stop(where, ": a post hoc fit is not OK; nothing is drawn for a failed fit.", call. = FALSE)
  if (!all(r$registry_sha256 == F1O3B_REGISTRY_SHA256) || !all(r$tier == F1O3B_TIER) || !all(r$display_note == F1O3B_LABEL))
    stop(where, " is not the frozen post hoc registry v1.0 row set (sha, tier or label).", call. = FALSE)
  con <- r[r$estimand_type == "contrast", , drop = FALSE]
  if (nrow(con) != 3L || !all(con$holm_m == 3) || !all(con$tested %in% c(TRUE, "TRUE")))
    stop(where, ": the contrasts must be tested with Holm over m = 3.", call. = FALSE)
  if (!all(r$n_con_cages == 3L)) stop(where, ": the design shows 3 CON cages per sex; the stored n_con_cages differs.", call. = FALSE)
  r
}

# =============================================================== c
f1o3b_panel_cc1 <- function(tab, fsb, an, w_mm, h_mm, jitter_seed = c(NA, NA), frame = NULL) {
  A1 <- tab("A1_animal_cc1")
  P1 <- fsb("P1_posthoc_con_contrasts")
  sexes <- c("Female", "Male"); measures <- names(F1O3B_MEASURE_KEY)
  marks <- utils::modifyList(BH_FOREST, F1N$forest)
  holm_head <- f1n_expr("'Holm ' * italic('P')")
  gutter_pt <- 4   # between the two measures
  dots <- function(k, sex, seed, first, right_pt) {
    rows <- f1o3b_posthoc_rows(P1, k, sex)
    lim <- F1O3B_LIMITS[[k]]
    pts <- A1[A1$Sex == sex & is.finite(A1[[k]]), c("AnimalNum", "Sex", "Group", k)]; names(pts)[4] <- "y"
    n_stored <- unlist(rows[1, paste0("n_", GROUP_LEV)])
    if (!identical(as.integer(table(factor(pts$Group, levels = GROUP_LEV))), as.integer(n_stored)))
      stop("f1o3b_panel_cc1: the animals drawn (ebb A1) differ from the post hoc model's n (fsb P1) for ", k, " ", sex, call. = FALSE)
    if (any(pts$y < lim[1] | pts$y > lim[2])) stop("f1o3b_panel_cc1: an animal lies outside the presentational limits of ", k, call. = FALSE)
    pts$Group <- factor(pts$Group, levels = GROUP_LEV)
    pts$x <- match(as.character(pts$Group), GROUP_LEV) + F1N_CLOUD_OFFSET
    mm <- rows[rows$estimand_type == "batch_balanced_mean", , drop = FALSE]
    mm$Group <- factor(sub("_mean$", "", mm$estimand), levels = GROUP_LEV)
    mm$x <- match(as.character(mm$Group), GROUP_LEV) + F1N_MEAN_OFFSET
    lo_cut <- mm$ci_low < lim[1]; hi_cut <- mm$ci_high > lim[2]
    bar <- function(d, end, arrow = NULL)
      geom_segment(data = d, aes(x = x, xend = x, y = estimate, yend = .data[[end]]), inherit.aes = FALSE,
                   linewidth = F1N$mean_lw, colour = "black", arrow = arrow, arrow.fill = "black", linejoin = "mitre")
    blank_y <- if (first) theme() else
      theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank())
    ggplot(pts, aes(x, y)) +
      geom_point(aes(fill = Group), shape = 21, position = position_jitter(width = F1N$jitter_width, height = 0, seed = seed),
                 size = F1N$point_size, stroke = F1N$point_stroke, colour = F1N$point_colour) +
      # the post hoc means and their capless 95% CIs; an interval beyond the axis limit ends in an arrowhead
      bar(mm[!lo_cut, , drop = FALSE], "ci_low") + bar(mm[lo_cut, , drop = FALSE], "ci_low", F1O3B_ARROW) +
      bar(mm[!hi_cut, , drop = FALSE], "ci_high") + bar(mm[hi_cut, , drop = FALSE], "ci_high", F1O3B_ARROW) +
      geom_point(data = mm, aes(x = x, y = estimate), inherit.aes = FALSE, shape = 21,
                 size = F1N$mean_size, stroke = F1N$mean_stroke, colour = "black", fill = "black") +
      facet_wrap(~ Sex) +
      scale_fill_manual(values = GROUP_COL, guide = "none") +
      scale_x_continuous(breaks = seq_along(GROUP_LEV), labels = GROUP_LEV, limits = c(0.6, 3.48), expand = c(0, 0)) +
      scale_y_continuous(limits = lim, oob = scales::squish, labels = f1_minus_labels) +
      labs(x = NULL, y = if (first) F1O3B_DOTS_Y[[k]] else NULL) +
      theme_f1(style = "nature") +
      theme(axis.title.y = f1n_markdown(lineheight = 1.05), plot.margin = margin(0, right_pt, 1, 1)) +
      blank_y
  }
  forest <- function(k, sex, first, right_pt, bottom_pad) {
    rows <- f1o3b_posthoc_rows(P1, k, sex)
    fr <- rows[rows$estimand_type == "contrast", , drop = FALSE]
    fr$row <- unname(F1O3B_ROW_Y[fr$estimand]); fr$label <- unname(F1O3B_CONTRASTS[fr$estimand])
    xl <- F1O3B_DIFF_LIMITS[[k]]
    if (any(fr$ci_low < xl[1] | fr$ci_high > xl[2])) stop("f1o3b_panel_cc1: a contrast interval exceeds the forest limits of ", k, call. = FALSE)
    # the Holm P of each row: one stored cell each, the same cell the plotted row carries
    keys <- paste0("c_", F1O3B_MEASURE_KEY[[k]], "_", F1O3B_SEX_KEY[[sex]], "_", F1O3B_KEY[names(F1O3B_ROW_Y)], "_holm")
    for (i in seq_along(keys)) {
      stored <- fr$p_holm[fr$estimand == names(F1O3B_ROW_Y)[i]]
      if (!identical(as.numeric(an$ann(keys[i])), as.numeric(stored)))
        stop("f1o3b_panel_cc1: annotation key ", keys[i], " is not the plotted row's Holm P.", call. = FALSE)
    }
    p_lab <- vapply(keys, an$fa, "", USE.NAMES = FALSE)
    left <- if (first) element_text(size = F1N$text_pt, colour = INK, margin = margin(r = 1.2)) else element_blank()
    ggplot(fr, aes(y = row)) +
      bh_forest_zero(horizontal = FALSE, marks = marks) +
      geom_segment(aes(x = ci_low, xend = ci_high, yend = row), linewidth = marks$ci_linewidth, colour = marks$ci_colour,
                   lineend = marks$ci_lineend) +
      bh_forest_point(aes(x = estimate), fill = INK, marks = marks) +
      scale_y_continuous(limits = c(0.5, 3.5), breaks = unname(F1O3B_ROW_Y), labels = unname(F1O3B_CONTRASTS), expand = c(0, 0),
                         sec.axis = sec_axis(transform = ~ ., breaks = unname(F1O3B_ROW_Y), labels = p_lab)) +
      scale_x_continuous(limits = xl, breaks = F1O3B_DIFF_BREAKS[[k]], labels = f1_minus_labels, expand = c(0, 0)) +
      labs(x = F1O3B_DIFF_X[[k]], y = NULL, title = holm_head) +
      theme_f1(style = "nature") +
      theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
            axis.text.y.left = left, axis.text.y.right = element_text(size = F1N$text_pt, colour = INK, hjust = 0, margin = margin(l = 1.2)),
            axis.title.x = f1n_markdown(margin = margin(t = 1)),
            plot.title = element_text(size = F1N$text_pt, colour = INK, hjust = 1, margin = margin(t = 2.5, b = 0.6)),
            plot.title.position = "plot",
            plot.margin = margin(0, right_pt, 1 + bottom_pad, 1))
  }
  build <- function(top_pad, bottom_pad) {
    parts <- list()
    for (k in measures) for (sex in sexes) {
      first <- identical(sex, "Female")
      right <- if (!first && identical(k, measures[1])) gutter_pt else 1
      seed <- jitter_seed[match(k, measures)]
      parts[[paste(k, sex, "dots")]] <- dots(k, sex, seed, first, right)
      parts[[paste(k, sex, "forest")]] <- forest(k, sex, first, right, bottom_pad)
    }
    top <- parts[grepl("dots$", names(parts))]; bottom <- parts[grepl("forest$", names(parts))]
    patchwork::wrap_plots(c(top, bottom), nrow = 2, byrow = TRUE, heights = c(2.45, 1)) +
      patchwork::plot_layout(axis_titles = "collect_x") +
      patchwork::plot_annotation(
        title = "First active phase after CC1", subtitle = F1O3B_LABEL,
        theme = theme(plot.title = element_text(size = F1N$title_pt, colour = INK, hjust = 0,
                                                margin = margin(F1N_HEADER_TOP_PT, 0, 1.2, F1N_INDENT_PT)),
                      plot.subtitle = element_text(size = F1N$text_pt, colour = MUTED, hjust = 0,
                                                   margin = margin(0, 0, 1 + top_pad, F1N_INDENT_PT)),
                      plot.margin = margin(0, 0, 0, 0), plot.background = F1N_BACKGROUND))
  }
  bh_panel(f1n_align(build, frame, w_mm, h_mm, "f1o3b_panel_cc1"), w_mm, h_mm)
}

# =============================================================== d
f1o3b_panel_trajectory <- function(tab, fsb, an, w_mm, h_mm, frame = NULL) {
  fa <- an$fa
  C2 <- tab("C2_estimates")
  F1b <- fsb("F1b_con_reference_means")
  sexes <- c("Female", "Male"); ccs <- paste0("CC", 1:4)
  ptr <- f1n_expr(F1O3B_PTR_LABEL, fa("d_cr_ptr_holm"), fa("d_sz_ptr_holm"))
  one <- function(k, top, bottom_pad = 0) {
    mm <- f1_model_means(C2, "^mean_(RES|SUS)_TR_CC[1-4]$", k); mm <- mm[mm$sex %in% sexes, , drop = FALSE]
    if (nrow(mm) != 16L || !all(grepl("^TR_BY_SEX\\|", mm$model_id))) stop("f1o3b panel d: expected 16 TR_BY_SEX model means for ", k, call. = FALSE)
    mm$Sex <- factor(mm$sex, levels = sexes); mm$Group <- factor(mm$Group, levels = c("RES", "SUS"))
    mm$x <- mm$CC + F1O3B_TRAJ_OFFSET[as.character(mm$Group)]
    ref <- F1b[F1b$measure == k & F1b$CC %in% ccs & F1b$Group == "CON" & F1b$Sex %in% sexes, , drop = FALSE]
    if (nrow(ref) != 2L * length(ccs) || anyDuplicated(paste(ref$Sex, ref$CC)))
      stop("f1o3b panel d: fsb F1b must hold one CON descriptive mean per sex and cage change for ", k, call. = FALSE)
    ref$Sex <- factor(ref$Sex, levels = sexes)
    ref$x <- match(ref$CC, ccs) + F1O3B_TRAJ_OFFSET[["CON"]]
    ref <- ref[order(ref$Sex, ref$x), , drop = FALSE]
    ggplot(mm, aes(x, estimate)) +
      # the CON descriptive mean: one dashed line, keyed once (top left of the top row's first facet)
      geom_line(data = ref, aes(x, mean, group = Sex, linetype = Group), inherit.aes = FALSE, colour = CON_GREY,
                linewidth = F1N$mean_lw, show.legend = top) +
      geom_line(aes(colour = Group, group = Group), linewidth = F1N$mean_lw) +
      geom_linerange(aes(ymin = ci_low, ymax = ci_high, colour = Group), linewidth = F1N$mean_lw) +
      geom_point(aes(fill = Group), shape = 21, size = F1N$point_size + 0.35, stroke = F1N$point_stroke, colour = F1N$point_colour) +
      facet_wrap(~ Sex, nrow = 1) +
      scale_colour_manual(values = GROUP_COL[c("RES", "SUS")], guide = "none") +
      scale_fill_manual(values = GROUP_COL[c("RES", "SUS")], guide = "none") +
      scale_linetype_manual(values = c(CON = "22"), name = NULL, guide = if (top) "legend" else "none") +
      scale_x_continuous(breaks = 1:4, labels = ccs, limits = c(0.6, 4.4), expand = c(0, 0)) +
      scale_y_continuous(labels = f1_minus_labels, n.breaks = 4) +
      labs(x = NULL, y = F1O3B_TRAJ_Y[[k]]) +
      theme_f1(style = "nature") +
      theme(axis.title.y = f1n_markdown(lineheight = 1.05), panel.spacing = unit(4, "pt"),
            legend.position = "inside", legend.position.inside = c(0.012, 0.995), legend.justification = c(0, 1),
            legend.background = element_blank(), legend.key = element_blank(), legend.key.width = unit(9, "pt"),
            legend.key.height = unit(5, "pt"), legend.margin = margin(0, 0, 0, 0),
            legend.text = element_text(size = F1N$text_pt, colour = INK, margin = margin(l = 1.5)),
            strip.text = if (top) element_text(size = F1N$title_pt, colour = INK, margin = margin(b = 1, t = 0)) else element_blank(),
            axis.text.x = if (top) element_blank() else element_text(size = F1N$text_pt, colour = INK),
            plot.margin = margin(if (top) 0 else 2.5, 1, if (top) 1.5 else 1 + bottom_pad, 1))
  }
  build <- function(top_pad, bottom_pad) {
    patchwork::wrap_plots(one("crossing_rate", TRUE), one("shared_zone_use", FALSE, bottom_pad), ncol = 1) +
      patchwork::plot_annotation(
        title = "CC1 → CC4, active phase after each cage change", subtitle = ptr,
        theme = theme(plot.title = element_text(size = F1N$title_pt, colour = INK, hjust = 0,
                                                margin = margin(F1N_HEADER_TOP_PT, 0, 1.2, F1N_INDENT_PT)),
                      plot.subtitle = element_text(size = F1N$text_pt, colour = INK, hjust = 0,
                                                   margin = margin(0, 0, 1 + top_pad, F1N_INDENT_PT)),
                      plot.margin = margin(0, 0, 0, 0), plot.background = F1N_BACKGROUND))
  }
  bh_panel(f1n_align(build, frame, w_mm, h_mm, "f1o3b_panel_trajectory"), w_mm, h_mm)
}
