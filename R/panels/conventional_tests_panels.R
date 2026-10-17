# Panels of the conventional-tests Extended Data candidates (figures/conventional_tests_candidate.R):
# Extended Data 10 (conventional behaviour tests, locomotion by sex), 11 (physiology, spread of CombZ) and 12 (automated
# against manual scoring).
#
# Everything drawn is read from the pinned SISanalyzer bundle (R/conventional_bundle.R): per-animal values, batch-adjusted
# group means with 95% intervals, d and adjusted p of the group comparisons, the heterogeneity, locomotion and validation
# tables. No statistic is computed here; positions, ranges and labels are layout only. Style: females above or left of males,
# never pooled; filled circles in the group colours, no outlines; dots ordered by batch within a group; dark mean and
# interval; brackets only where the stored adjusted p is below 0.05; square heatmap tiles with the palette's diverging scale
# at the fixed limit for standardized differences; short labels only (the full names are in the legends and the per-panel
# source data; n and the CombZ components are in the legends).

CT_GROUPS <- c("CON", "RES", "SUS")
CT_SEXES <- c("Female", "Male")
CT_SEXLAB <- c(Female = "Females", Male = "Males")
CT_CMP <- c("RES - CON", "SUS - CON", "RES - SUS", "SIS - CON")
CT_BPOS <- c(B1 = -1, B2 = 0, B5 = 1, B3 = -1, B4 = 0, B6 = 1)
CT_MINUS <- "−"
CT_MEAN <- "#3A3A3A"
CT_GREY <- "#4D4D4D"
CT_SEED <- 20261016L
CT_PT <- 2.0          # animal dots
CT_PT_MEAN <- 2.6     # group means
CT_ALPHA <- .9

ct_tx <- function(pt) pt / ggplot2::.pt
ct_cmp_label <- function(x) sub(" - ", CT_MINUS, x, fixed = TRUE)
ct_pnum <- function(p) ifelse(p < .001, "P < 0.001", sprintf("P = %.3f", p))   # the stored adjusted p above a bracket
#' Axis labels: no trailing zeros, a true minus sign.
ct_num <- function(x) { s <- format(x, trim = TRUE, drop0trailing = TRUE, scientific = FALSE); s[is.na(x)] <- NA; sub("^-", CT_MINUS, s) }

#' The pinned bundle and the palette, as one list.
ct_read <- function(pin = ctp_pin()) {
  P <- yaml::read_yaml(repo_path("config", "manuscript_palette.yml"))
  tb <- function(n) ctp_table(n, pin)
  X <- list(measures = tb("A_measures"), values = tb("B_values"), means = tb("C_group_means"), contrasts = tb("D_contrasts"),
            het = tb("E_heterogeneity_summary"), tails = tb("E_heterogeneity_tails"), loco_sessions = tb("F_locomotion_sessions"),
            loco_int = tb("F_locomotion_interaction"), val = tb("G_validation_pairs"), agree = tb("G_validation_agreement"), provenance = tb("H_provenance"), P = P, pin = pin)
  X$measures$main <- as.logical(X$measures$main)
  for (k in c("val", "agree")) for (f in c("same_quantity", "calibrated_on_these_animals")) X[[k]][[f]] <- as.logical(X[[k]][[f]])
  X
}

ct_theme <- function(P) {
  T <- P$typography; lw <- P$line$axis_pt * .75
  ggplot2::theme_bw(base_size = T$axis_text_pt, base_family = T$family) + ggplot2::theme(
    panel.border = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = "white", colour = NA), plot.background = ggplot2::element_rect(fill = "white", colour = NA),
    axis.line = ggplot2::element_line(linewidth = lw, colour = "black"), axis.ticks = ggplot2::element_line(linewidth = lw, colour = "black"),
    axis.ticks.length = grid::unit(.6, "mm"), axis.text = ggplot2::element_text(size = T$axis_text_pt, colour = "black"),
    axis.title = ggplot2::element_text(size = T$axis_title_pt, colour = CT_GREY), strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(size = T$axis_text_pt, colour = "black", hjust = 0), legend.position = "none",
    legend.title = ggplot2::element_text(size = T$legend_title_pt, colour = CT_GREY), legend.text = ggplot2::element_text(size = T$legend_text_pt),
    plot.title = ggplot2::element_text(face = "bold", size = T$axis_title_pt, colour = "black", hjust = 0, margin = ggplot2::margin(b = 1)),
    plot.margin = ggplot2::margin(1, 1, 1, 1, "mm"), panel.spacing = grid::unit(2, "mm"))
}

# Short display labels (layout only; the full measure names are in the bundle, the source data and the legends).
CT_SHORT <- c(
  epm_open_pct = "Open arms (%)", epm_closed_time = "Closed arms (s)", epm_distance = "Distance (cm)", epm_moving_pct = "Moving (%)", epm_stationary_time = "Stationary (s)",
  oft_centre_pct = "Centre (%)", oft_centre_entries = "Centre entries (n)", oft_distance = "Distance (cm)", oft_speed = "Speed (cm/s)", oft_highspeed_pct = "High speed (%)",
  oft_first_distance = "Early distance (cm)", oft_habituation = "Distance change (cm)", oft_habituation_slope = "Distance change (cm/min)",
  nor_d2 = "D2", nor_novel = "Contact, novel (s)", nor_prox_novel = "Near novel (s)",
  soc1_pref = "Preference D2", soc2_pref = "Novelty D2", soc2_total = "Contact, novelty (s)", soc2_speed = "Speed, novelty (cm/s)",
  bw_first = "First weighing (g)", bw_last = "Last weighing (g)", bw_change = "Weight gain (g)", bw_pct = "Weight gain (%)", bw_rate = "Growth (g/week)",
  adrenal_mg = "Adrenal (mg)", spleen_mg = "Spleen (mg)", adrenal_ratio = "Adrenal (mg/100 g)", spleen_ratio = "Spleen (mg/100 g)",
  cort_baseline = "Baseline (ng/mL)", cort_response = "Response (ng/mL)", cort_delta = "Rise (ng/mL)",
  suc_pref = "Preference (%)", suc_pref1 = "Preference, test 1 (%)", suc_pref2 = "Preference, test 2 (%)", suc_pref3 = "Preference, test 3 (%)",
  suc_total1 = "Intake, test 1 (g)", suc_total2 = "Intake, test 2 (g)", suc_total3 = "Intake, test 3 (g)")
# one-line titles of the dot panels
CT_TITLE <- c(epm_open_pct = "Plus maze, open arms (%)", oft_centre_pct = "Open field, centre (%)", nor_d2 = "Novel object D2", soc1_pref = "Social preference D2",
              soc2_pref = "Social novelty D2", bw_change = "Weight gain (g)", adrenal_ratio = "Adrenal (mg/100 g)", spleen_ratio = "Spleen (mg/100 g)",
              cort_delta = "Corticosterone rise (ng/mL)", suc_pref = "Sucrose preference (%)")
# one short name per test, the same on every page
CT_TESTSHORT <- c(`Elevated plus maze` = "Plus maze", `Open field` = "Open field", `Novel object recognition` = "Novel object", `Social preference` = "Social tests",
                  `Body weight` = "Body weight", `Organ weights` = "Organs", Corticosterone = "Corticosterone", `Sucrose preference` = "Sucrose")
CT_FOREST <- c(`Integrated Resilience (CombZ)` = "CombZ", `Elevated plus maze: Open-arm share of time (%)` = "Plus maze, open arms",
               `Open field: Time in centre (%)` = "Open field, centre", `Social preference: Social preference (D2)` = "Social preference D2",
               `Social preference: Social novelty preference (D2)` = "Social novelty D2",
               `Weight gain` = "Weight gain, weighings 2–6")   # the CombZ component, not the weight gain of the dot panel
ct_short <- function(X, id) { s <- unname(CT_SHORT[id]); ifelse(is.na(s), X$measures$label[match(id, X$measures$id)], s) }
ct_title <- function(X, id) { s <- unname(CT_TITLE[id]); ifelse(is.na(s), ct_short(X, id), s) }

# ------------------------------------------------------------------ dot panels
#' The data a dot block draws: every animal's value, the stored batch-adjusted means and intervals, and the brackets
#' (pairs whose stored adjusted p is below 0.05). Positions: group, plus a batch offset and a seeded jitter.
ct_dot_data <- function(X, ids, sex) {
  v <- X$values[X$values$id %in% ids & X$values$Sex == sex, , drop = FALSE]
  v <- v[order(match(v$id, ids), v$Batch, v$Animal), , drop = FALSE]
  set.seed(CT_SEED); v$gx <- match(v$Group, CT_GROUPS); v$x <- v$gx + CT_BPOS[v$Batch] * .26 + stats::runif(nrow(v), -.06, .06)
  m <- X$means[X$means$id %in% ids & X$means$Sex == sex, , drop = FALSE]; m$gx <- match(m$Group, CT_GROUPS)
  rng <- do.call(rbind, lapply(ids, function(i) { y <- v$value[v$id == i]; data.frame(id = i, ytop = max(y), span = diff(range(y)), stringsAsFactors = FALSE) }))
  b <- X$contrasts[X$contrasts$id %in% ids & X$contrasts$Sex == sex & X$contrasts$comparison %in% CT_CMP[1:3] & !is.na(X$contrasts$adj_p) & X$contrasts$adj_p < .05, , drop = FALSE]
  if (nrow(b)) { b$g1 <- match(sub(" - .*", "", b$comparison), CT_GROUPS); b$g2 <- match(sub(".* - ", "", b$comparison), CT_GROUPS)
    b <- merge(b, rng, by = "id"); b <- b[order(match(b$id, ids), b$comparison), ]; b$level <- stats::ave(seq_len(nrow(b)), b$id, FUN = seq_along)
    b$y <- b$ytop + b$span * (.08 + .15 * b$level) }
  list(points = v, means = m, brackets = b)
}

ct_dot_block <- function(X, ids, sex, titles, ncol = length(ids), show_titles = TRUE, show_groups = TRUE) {
  P <- X$P; T <- P$typography; lw <- P$line$axis_pt * .75; D <- ct_dot_data(X, ids, sex)
  lev <- titles[ids]; fac <- function(i) factor(titles[i], levels = lev)
  pts <- D$points; pts$panel <- fac(pts$id); mm <- D$means; mm$panel <- fac(mm$id)
  p <- ggplot2::ggplot(pts, ggplot2::aes(x, value)) +
    ggplot2::geom_point(ggplot2::aes(colour = Group), shape = 16, size = CT_PT, alpha = CT_ALPHA) +
    ggplot2::geom_linerange(data = mm, ggplot2::aes(x = gx, ymin = lower, ymax = upper), inherit.aes = FALSE, linewidth = lw * 1.8, colour = CT_MEAN) +
    ggplot2::geom_point(data = mm, ggplot2::aes(x = gx, y = mean), inherit.aes = FALSE, shape = 16, size = CT_PT_MEAN, colour = CT_MEAN) +
    ggplot2::scale_colour_manual(values = unlist(P$group)) + ggplot2::scale_x_continuous(breaks = 1:3, labels = CT_GROUPS, limits = c(.45, 3.55)) +
    ggplot2::scale_y_continuous(labels = ct_num, expand = ggplot2::expansion(mult = c(.06, .08))) +
    ggplot2::facet_wrap(~panel, ncol = ncol, scales = "free_y") +
    ggplot2::labs(x = NULL, y = NULL, title = CT_SEXLAB[[sex]]) + ct_theme(P)
  if (nrow(D$brackets)) { br <- D$brackets; br$panel <- fac(br$id); br$top <- br$y + br$span * .12
    p <- p + ggplot2::geom_segment(data = br, ggplot2::aes(x = g1 + .08, xend = g2 - .08, y = y, yend = y), inherit.aes = FALSE, linewidth = lw * 1.2, colour = "black") +
      ggplot2::geom_text(data = br, ggplot2::aes(x = (g1 + g2) / 2, y = y, label = ct_pnum(adj_p)), inherit.aes = FALSE, size = ct_tx(T$annotation_pt), family = T$family, vjust = -.35) +
      ggplot2::geom_blank(data = br, ggplot2::aes(x = g1, y = top), inherit.aes = FALSE) }
  if (!show_titles) p <- p + ggplot2::theme(strip.text = ggplot2::element_blank())
  if (!show_groups) p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank())
  p
}

#' Dot panels of the given measures: females above, males below. Panel titles above the females only, group names below
#' the males only (the columns line up).
ct_dots <- function(X, ids, ncol = length(ids)) {
  titles <- stats::setNames(ct_title(X, ids), ids)
  patchwork::wrap_elements(full = ct_dot_block(X, ids, "Female", titles, ncol, show_groups = FALSE) /
                                  ct_dot_block(X, ids, "Male", titles, ncol, show_titles = FALSE))
}

# ------------------------------------------------------------------ heatmap of d
#' The measures of a compact heatmap: the main readouts of the domain and every measure with a stored adjusted p below
#' 0.05 in any comparison of either sex.
ct_selected <- function(X, domain) {
  ms <- X$measures[X$measures$domain == domain & !X$measures$id %in% c("loco_index", "CombZ"), , drop = FALSE]
  sig <- unique(X$contrasts$id[!is.na(X$contrasts$adj_p) & X$contrasts$adj_p < .05])
  ms$id[ms$main | ms$id %in% sig]
}
ct_selected_behaviour <- function(X) ct_selected(X, "Behaviour")
ct_selected_physiology <- function(X) ct_selected(X, "Physiology")

#' Tile coordinates: measures as rows grouped by test (a header row per test), females in columns 1-4, males in 6-9.
ct_heatmap_data <- function(X, ids) {
  ms <- X$measures[match(ids, X$measures$id), , drop = FALSE]; ms <- ms[order(ms$order), , drop = FALSE]
  rows <- list(); y <- 0; heads <- list()
  for (t in unique(ms$test)) { y <- y - 1; ts <- unname(CT_TESTSHORT[t]); if (is.na(ts)) ts <- t
    heads[[length(heads) + 1]] <- data.frame(test = ts, y = y, stringsAsFactors = FALSE)
    for (i in ms$id[ms$test == t]) { y <- y - 1; rows[[length(rows) + 1]] <- data.frame(id = i, label = ct_short(X, i), y = y, stringsAsFactors = FALSE) }
    y <- y - .4 }
  rows <- do.call(rbind, rows); heads <- do.call(rbind, heads)
  ct <- X$contrasts[X$contrasts$id %in% ids, c("Sex", "id", "comparison", "d", "adj_p")]
  cells <- merge(ct, rows, by = "id"); cells$x <- match(cells$comparison, CT_CMP) + ifelse(cells$Sex == "Male", 5, 0)
  lim <- X$P$diverging_limits$smd; cells$fill <- pmax(pmin(cells$d, lim), -lim); cells$significant <- !is.na(cells$adj_p) & cells$adj_p < .05
  list(cells = cells[order(cells$y, cells$x), ], rows = rows, heads = heads)
}

ct_heat_scale <- function(P, legend = TRUE) { lim <- P$diverging_limits$smd; dv <- P$diverging
  ggplot2::scale_fill_gradient2(low = dv$low, mid = dv$mid, high = dv$high, midpoint = 0, limits = c(-lim, lim), breaks = c(-lim, 0, lim),
                                labels = c(paste0("≤", CT_MINUS, lim), "0", paste0("≥", lim)), name = "Effect size (d)", guide = if (legend) "colourbar" else "none") }
ct_heat_theme <- function(P) ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
  axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = .5), axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 1, unit = "mm")))

ct_heatmap <- function(X, ids, legend = TRUE) {
  P <- X$P; T <- P$typography; H <- ct_heatmap_data(X, ids)
  top <- max(H$heads$y) + 1.3
  sexlab <- data.frame(x = c(2.5, 7.5), y = top, label = CT_SEXLAB, stringsAsFactors = FALSE)
  sg <- H$cells[H$cells$significant, ]; sg$col <- ifelse(abs(sg$fill) > .6, "white", CT_MEAN)
  p <- ggplot2::ggplot(H$cells, ggplot2::aes(x, y, fill = fill)) +
    ggplot2::geom_tile(width = 1, height = 1, colour = "grey85", linewidth = P$line$tile_border_pt) +
    ggplot2::geom_point(data = sg, ggplot2::aes(x, y, colour = col), inherit.aes = FALSE, shape = 16, size = 1.1) + ggplot2::scale_colour_identity() +
    ggplot2::geom_text(data = H$heads, ggplot2::aes(x = .5, y = y, label = test), inherit.aes = FALSE, hjust = 0, fontface = "bold", size = ct_tx(T$axis_text_pt), family = T$family, colour = CT_GREY) +
    ggplot2::geom_text(data = sexlab, ggplot2::aes(x = x, y = y, label = label), inherit.aes = FALSE, fontface = "bold", size = ct_tx(T$axis_title_pt), family = T$family) +
    ct_heat_scale(P, legend) +
    ggplot2::scale_x_continuous(breaks = c(1:4, 6:9), labels = rep(ct_cmp_label(CT_CMP), 2), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = H$rows$y, labels = H$rows$label, expand = c(0, 0)) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(.5, 9.5), ylim = c(min(H$rows$y) - .5, top + .6), clip = "off") + ggplot2::labs(x = NULL, y = NULL) + ct_theme(P) + ct_heat_theme(P)
  if (legend) p <- p + ggplot2::guides(fill = ggplot2::guide_colourbar(title.position = "top", title.hjust = .5, ticks.colour = "white")) +
    ggplot2::theme(legend.position = "bottom", legend.key.width = grid::unit(5, "mm"), legend.key.height = grid::unit(2, "mm"))
  p
}

# ------------------------------------------------------------------ locomotion (exploratory)
ct_loco_index <- function(X) {
  P <- X$P; T <- P$typography; lw <- P$line$axis_pt * .75
  pts <- do.call(rbind, lapply(CT_SEXES, function(s) { d <- ct_dot_data(X, "loco_index", s); d$points$panel <- CT_SEXLAB[[s]]; d$points }))
  mm <- do.call(rbind, lapply(CT_SEXES, function(s) { d <- ct_dot_data(X, "loco_index", s); d$means$panel <- CT_SEXLAB[[s]]; d$means }))
  pts$panel <- factor(pts$panel, CT_SEXLAB); mm$panel <- factor(mm$panel, CT_SEXLAB)
  ggplot2::ggplot(pts, ggplot2::aes(x, value)) + ggplot2::geom_hline(yintercept = 0, linewidth = P$line$reference_pt * .75, colour = "grey75") +
    ggplot2::geom_point(ggplot2::aes(colour = Group), shape = 16, size = CT_PT, alpha = CT_ALPHA) +
    ggplot2::geom_linerange(data = mm, ggplot2::aes(x = gx, ymin = lower, ymax = upper), inherit.aes = FALSE, linewidth = lw * 1.8, colour = CT_MEAN) +
    ggplot2::geom_point(data = mm, ggplot2::aes(x = gx, y = mean), inherit.aes = FALSE, shape = 16, size = CT_PT_MEAN, colour = CT_MEAN) +
    ggplot2::scale_colour_manual(values = unlist(P$group)) + ggplot2::scale_x_continuous(breaks = 1:3, labels = CT_GROUPS, limits = c(.45, 3.55)) +
    ggplot2::scale_y_continuous(labels = ct_num, expand = ggplot2::expansion(mult = c(.06, .08))) + ggplot2::facet_wrap(~panel, nrow = 1) +
    ggplot2::labs(x = NULL, y = "Locomotion (z)") + ct_theme(P) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", size = T$axis_title_pt, colour = "black", hjust = 0))
}

#' Cohen's d of distance travelled in every session and of the index (stored values), the four comparisons per sex.
ct_loco_sessions_data <- function(X) {
  s <- X$loco_sessions; s$comparison <- sub(paste0(" ", CT_MINUS, " "), " - ", s$Comparison, fixed = TRUE)
  s <- s[s$comparison %in% CT_CMP, c("Session", "Sex", "comparison", "d")]
  ix <- X$contrasts[X$contrasts$id == "loco_index" & X$contrasts$comparison %in% CT_CMP, c("Sex", "comparison", "d")]; ix$Session <- "Locomotion index"
  d <- rbind(s, ix[names(s)]); ses <- c(unique(s$Session), "Locomotion index")
  d$y <- -match(d$Session, ses) - ifelse(d$Session == "Locomotion index", .25, 0); d$x <- match(d$comparison, CT_CMP) + ifelse(d$Sex == "Male", 5, 0)
  lim <- X$P$diverging_limits$smd; d$fill <- pmax(pmin(d$d, lim), -lim); d
}

ct_loco_sessions <- function(X) {
  P <- X$P; T <- P$typography; d <- ct_loco_sessions_data(X)
  rows <- unique(d[c("Session", "y")]); top <- max(d$y) + 1.2
  rows$Session <- c(`Elevated plus maze` = "Plus maze", `Open field` = "Open field", `Novel object` = "Novel object", `Social preference` = "Social preference",
                    `Social novelty` = "Social novelty", `Locomotion index` = "Locomotion")[rows$Session]
  ggplot2::ggplot(d, ggplot2::aes(x, y, fill = fill)) + ggplot2::geom_tile(width = 1, height = 1, colour = "grey85", linewidth = P$line$tile_border_pt) +
    ggplot2::annotate("text", x = c(2.5, 7.5), y = top, label = CT_SEXLAB, fontface = "bold", size = ct_tx(T$axis_title_pt), family = T$family) +
    ct_heat_scale(P, legend = FALSE) +
    ggplot2::scale_x_continuous(breaks = c(1:4, 6:9), labels = rep(ct_cmp_label(CT_CMP), 2), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = rows$y, labels = rows$Session, expand = c(0, 0)) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(.5, 9.5), ylim = c(min(d$y) - .5, top + .5), clip = "off") + ggplot2::labs(x = NULL, y = NULL) + ct_theme(P) + ct_heat_theme(P)
}

# ------------------------------------------------------------------ heterogeneity
ct_combz_spread <- function(X) {
  P <- X$P; T <- P$typography; lw <- P$line$axis_pt * .75
  v <- X$values[X$values$id == "CombZ", ]; v <- v[order(match(v$Sex, CT_SEXES), v$Batch, v$Animal), ]
  set.seed(CT_SEED); v$x <- ifelse(v$Condition == "CON", 1, 2) + CT_BPOS[v$Batch] * .17 + stats::runif(nrow(v), -.045, .045)
  v$panel <- factor(CT_SEXLAB[v$Sex], CT_SEXLAB)
  tl <- X$tails[X$tails$Condition == "CON", ]
  ref <- do.call(rbind, lapply(CT_SEXES, function(s) { r <- tl[tl$Sex == s, ]; mu <- r$`CON mean`; sd <- r$`CON SD (population)`
    data.frame(panel = CT_SEXLAB[[s]], y = c(mu, mu + sd, mu - sd), lt = c("solid", "22", "22"), stringsAsFactors = FALSE) }))
  ref$panel <- factor(ref$panel, CT_SEXLAB)
  ggplot2::ggplot(v, ggplot2::aes(x, value)) +
    ggplot2::geom_hline(data = ref, ggplot2::aes(yintercept = y, linetype = lt), linewidth = lw, colour = "grey55") +
    ggplot2::geom_point(ggplot2::aes(colour = Group), shape = 16, size = CT_PT, alpha = CT_ALPHA) +
    ggplot2::scale_linetype_identity() + ggplot2::scale_colour_manual(values = unlist(P$group)) +
    ggplot2::scale_x_continuous(breaks = 1:2, labels = c("CON", "SIS"), limits = c(.5, 2.5), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(labels = ct_num, expand = ggplot2::expansion(mult = c(.05, .05))) +
    ggplot2::facet_wrap(~panel, nrow = 1) + ggplot2::labs(x = NULL, y = "CombZ") + ct_theme(P) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", size = T$axis_title_pt, colour = "black", hjust = 0))
}

ct_ratio_forest <- function(X) {
  P <- X$P; T <- P$typography; lw <- P$line$axis_pt * .75; h <- X$het
  h$Sexl <- factor(CT_SEXLAB[h$Sex], CT_SEXLAB); h$Set <- factor(h$Set, c("Score", "Components", "Test readouts"))
  h$Short <- ifelse(h$Measure %in% names(CT_FOREST), unname(CT_FOREST[h$Measure]), h$Measure)
  h$row <- factor(h$Short, levels = rev(unique(h$Short)))
  lim <- c(.2, 6)   # every stored interval must fit: none is cut at the axis
  if (any(h$`95% CI low` < lim[1] | h$`95% CI high` > lim[2])) stop("An SD-ratio interval lies outside the forest axis; widen it.", call. = FALSE)
  ggplot2::ggplot(h, ggplot2::aes(`SD ratio SIS / CON`, row)) + ggplot2::geom_vline(xintercept = 1, linewidth = lw, colour = "grey55") +
    ggplot2::geom_linerange(ggplot2::aes(xmin = `95% CI low`, xmax = `95% CI high`), linewidth = lw * 1.8, colour = CT_MEAN) +
    ggplot2::geom_point(shape = 16, size = CT_PT, colour = CT_MEAN) +
    ggplot2::scale_x_log10(breaks = c(.25, .5, 1, 2, 4), labels = c("0.25", "0.5", "1", "2", "4"), limits = lim) +
    ggplot2::facet_grid(Set ~ Sexl, scales = "free_y", space = "free_y") +
    ggplot2::labs(x = "SD ratio, SIS / CON", y = NULL) + ct_theme(P) +
    ggplot2::theme(strip.text.y = ggplot2::element_blank(),
                   strip.text.x = ggplot2::element_text(face = "bold", hjust = 0, size = T$axis_title_pt, colour = "black"), panel.spacing.y = grid::unit(1.2, "mm"))
}

# ------------------------------------------------------------------ automated (SLEAP) versus manual (BORIS) scoring
# Batch 1 validation animals (males): the automated values the tests analyse against the manual scores (SISanalyzer
# Analysis/Behavior/Validation). Only pairs that measure the same quantity are drawn (the stored table marks the open-arm
# entries against BORIS bouts as a different quantity). Each panel has the same range on both axes, so the dashed
# identity line is the diagonal; r is the stored Pearson correlation. One neutral dot colour: group is not part of the
# agreement, and the palette never shows group by colour alone.
CT_VAL <- data.frame(
  metric = c("Open-arm time", "Closed-arm time", "Centre time", "Nose dips", "Novel object", "Familiar object", "Total interaction", "Discrimination D2",
             "S1 novel", "S1 familiar", "S1 preference", "S2 novel", "S2 familiar", "S2 preference"),
  title = c("Open arms (s)", "Closed arms (s)", "Centre (s)", "Nose dips (n)", "Novel object (s)", "Familiar object (s)", "Total contact (s)", "D2",
            "Mouse (s)", "Empty cup (s)", "Preference D2", "New mouse (s)", "Familiar mouse (s)", "Novelty D2"),
  block = rep(c("Plus maze", "Novel object", "Social tests"), c(4, 4, 6)), stringsAsFactors = FALSE)

ct_validation_data <- function(X) {
  v <- X$val[X$val$same_quantity & X$val$metric %in% CT_VAL$metric & is.finite(X$val$sleap) & is.finite(X$val$boris), , drop = FALSE]
  v$title <- CT_VAL$title[match(v$metric, CT_VAL$metric)]; v$block <- CT_VAL$block[match(v$metric, CT_VAL$metric)]
  a <- X$agree[match(CT_VAL$metric, X$agree$metric), c("metric", "n", "pearson_r", "ccc")]
  a$title <- CT_VAL$title; a$block <- CT_VAL$block
  list(points = v, agree = a)
}

ct_validation_block <- function(X, block, ncol, x_title = NULL, y_title = NULL) {
  P <- X$P; T <- P$typography; V <- ct_validation_data(X)
  lev <- CT_VAL$title[CT_VAL$block == block]
  pts <- V$points[V$points$block == block, ]; pts$panel <- factor(pts$title, lev)
  ann <- V$agree[V$agree$block == block, ]; ann$panel <- factor(ann$title, lev); ann$txt <- sprintf("r = %.2f", ann$pearson_r)
  mirror <- data.frame(panel = pts$panel, boris = pts$sleap, sleap = pts$boris)   # same range on both axes
  ggplot2::ggplot(pts, ggplot2::aes(boris, sleap)) +
    ggplot2::geom_blank(data = mirror) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "22", linewidth = P$line$reference_pt * .75, colour = "grey55") +
    ggplot2::geom_point(shape = 16, size = CT_PT, alpha = .75, colour = CT_MEAN) +
    ggplot2::geom_text(data = ann, ggplot2::aes(x = -Inf, y = Inf, label = txt), inherit.aes = FALSE, hjust = -.3, vjust = 1.5, size = ct_tx(T$annotation_pt), family = T$family) +
    ggplot2::scale_x_continuous(labels = ct_num, n.breaks = 4) + ggplot2::scale_y_continuous(labels = ct_num) +
    ggplot2::facet_wrap(~panel, ncol = ncol, scales = "free") + ggplot2::labs(x = x_title, y = y_title, title = block) + ct_theme(P) +
    ggplot2::theme(aspect.ratio = 1)
}

#' The validation page: plus maze and novel object as 2 x 2 blocks, the social tests as 3 x 2 (phase 1 above phase 2).
ct_validation <- function(X) {
  (ct_validation_block(X, "Plus maze", 2, y_title = "Automated") | ct_validation_block(X, "Novel object", 2, x_title = "Manual") |
     ct_validation_block(X, "Social tests", 3)) + patchwork::plot_layout(widths = c(2, 2, 3))
}
