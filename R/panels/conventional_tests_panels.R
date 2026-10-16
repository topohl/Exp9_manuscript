# Panels of the conventional-tests Extended Data candidates (figures/conventional_tests_candidate.R):
# Extended Data 10 (conventional behaviour tests, locomotion by sex) and Extended Data 11 (physiology, heterogeneity).
#
# Everything drawn is read from the pinned SISanalyzer bundle (R/conventional_bundle.R): per-animal values, batch-adjusted
# group means with 95% intervals, d and adjusted p of the group comparisons, the heterogeneity and locomotion tables. No
# statistic is computed here; positions, ranges and labels are layout only. The style is the one of the SISanalyzer result
# figures: females above or left of males, never pooled; filled circles in the group colours, no outlines; dots ordered by
# batch within a group; dark mean and interval; brackets only where the stored adjusted p is below 0.05; square heatmap
# tiles with the palette's diverging scale at the fixed limit for standardized differences.

CT_GROUPS <- c("CON", "RES", "SUS")
CT_SEXES <- c("Female", "Male")
CT_SEXLAB <- c(Female = "Females", Male = "Males")
CT_CMP <- c("RES - CON", "SUS - CON", "RES - SUS", "SIS - CON")
CT_BPOS <- c(B1 = -1, B2 = 0, B5 = 1, B3 = -1, B4 = 0, B6 = 1)
CT_MINUS <- "−"
CT_MEAN <- "#3A3A3A"
CT_GREY <- "#4D4D4D"
CT_SEED <- 20261016L

ct_tx <- function(pt) pt / ggplot2::.pt
ct_cmp_label <- function(x) sub(" - ", CT_MINUS, x, fixed = TRUE)
ct_fmt_r <- function(x) { s <- ifelse(is.na(x), "", ifelse(abs(x) < .005, ".00", sub("^(-?)0[.]", "\\1.", sprintf("%.2f", x)))); sub("-", CT_MINUS, s, fixed = TRUE) }
ct_stars <- function(p) ifelse(p < .001, "***", ifelse(p < .01, "**", "*"))
ct_pnice <- function(p) ifelse(p < .001, "< 0.001", ifelse(p < .01, sprintf("= %.3f", p), sprintf("= %.2f", p)))

#' The pinned bundle and the palette, as one list.
ct_read <- function(pin = ctp_pin()) {
  P <- yaml::read_yaml(repo_path("config", "manuscript_palette.yml"))
  tb <- function(n) ctp_table(n, pin)
  X <- list(measures = tb("A_measures"), values = tb("B_values"), means = tb("C_group_means"), contrasts = tb("D_contrasts"),
            het = tb("E_heterogeneity_summary"), tails = tb("E_heterogeneity_tails"), loco_sessions = tb("F_locomotion_sessions"),
            provenance = tb("H_provenance"), P = P, pin = pin)
  X$measures$main <- as.logical(X$measures$main)
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
    plot.title = ggplot2::element_text(face = "bold", size = T$axis_title_pt + 1, colour = "black", hjust = 0, margin = ggplot2::margin(b = 1)),
    plot.margin = ggplot2::margin(1, 1, 1, 1, "mm"), panel.spacing = grid::unit(2, "mm"))
}

ct_label <- function(X, id) X$measures$label[match(id, X$measures$id)]
ct_test <- function(X, id) X$measures$test[match(id, X$measures$id)]
ct_class_note <- function(X, id) { cl <- X$measures$class[match(id, X$measures$id)]
  ifelse(cl == "component", "CombZ component", ifelse(cl == "related", "related to a CombZ component", "")) }

# ------------------------------------------------------------------ dot panels
#' The data a dot block draws: every animal's value, the stored batch-adjusted means and intervals, and the brackets
#' (pairs whose stored adjusted p is below 0.05). Positions: group, plus a batch offset and a seeded jitter.
ct_dot_data <- function(X, ids, sex) {
  v <- X$values[X$values$id %in% ids & X$values$Sex == sex, , drop = FALSE]
  v <- v[order(match(v$id, ids), v$Batch, v$Animal), , drop = FALSE]
  set.seed(CT_SEED); v$gx <- match(v$Group, CT_GROUPS); v$x <- v$gx + CT_BPOS[v$Batch] * .24 + stats::runif(nrow(v), -.07, .07)
  m <- X$means[X$means$id %in% ids & X$means$Sex == sex, , drop = FALSE]; m$gx <- match(m$Group, CT_GROUPS)
  rng <- do.call(rbind, lapply(ids, function(i) { y <- v$value[v$id == i]; data.frame(id = i, ytop = max(y), span = diff(range(y)), stringsAsFactors = FALSE) }))
  b <- X$contrasts[X$contrasts$id %in% ids & X$contrasts$Sex == sex & X$contrasts$comparison %in% CT_CMP[1:3] & !is.na(X$contrasts$adj_p) & X$contrasts$adj_p < .05, , drop = FALSE]
  if (nrow(b)) { b$g1 <- match(sub(" - .*", "", b$comparison), CT_GROUPS); b$g2 <- match(sub(".* - ", "", b$comparison), CT_GROUPS)
    b <- merge(b, rng, by = "id"); b <- b[order(match(b$id, ids), b$comparison), ]; b$level <- stats::ave(seq_len(nrow(b)), b$id, FUN = seq_along); b$y <- b$ytop + b$span * (.06 + .11 * b$level) }
  n <- table(factor(X$values$Group[X$values$id == "CombZ" & X$values$Sex == sex], CT_GROUPS))
  list(points = v, means = m, brackets = b, n = n)
}

ct_dot_block <- function(X, ids, sex, titles, ncol = length(ids), pt = 1.2) {
  P <- X$P; T <- P$typography; lw <- P$line$axis_pt * .75; D <- ct_dot_data(X, ids, sex)
  lev <- titles[ids]; fac <- function(i) factor(titles[i], levels = lev)
  pts <- D$points; pts$panel <- fac(pts$id); mm <- D$means; mm$panel <- fac(mm$id)
  p <- ggplot2::ggplot(pts, ggplot2::aes(x, value)) +
    ggplot2::geom_point(ggplot2::aes(colour = Group), shape = 16, size = pt, alpha = .75) +
    ggplot2::geom_linerange(data = mm, ggplot2::aes(x = gx, ymin = lower, ymax = upper), inherit.aes = FALSE, linewidth = lw * 1.6, colour = CT_MEAN) +
    ggplot2::geom_point(data = mm, ggplot2::aes(x = gx, y = mean), inherit.aes = FALSE, shape = 16, size = pt + .7, colour = CT_MEAN) +
    ggplot2::scale_colour_manual(values = unlist(P$group)) + ggplot2::scale_x_continuous(breaks = 1:3, labels = CT_GROUPS, limits = c(.45, 3.55)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(.06, .3))) +
    ggplot2::facet_wrap(~panel, ncol = ncol, scales = "free_y") +
    ggplot2::labs(x = NULL, y = NULL, title = sprintf("%s    n = %s", CT_SEXLAB[[sex]], paste(as.integer(D$n), CT_GROUPS, collapse = ", "))) + ct_theme(P)
  if (nrow(D$brackets)) { br <- D$brackets; br$panel <- fac(br$id)
    p <- p + ggplot2::geom_segment(data = br, ggplot2::aes(x = g1, xend = g2, y = y, yend = y), inherit.aes = FALSE, linewidth = lw * 1.2, colour = "black") +
      ggplot2::geom_text(data = br, ggplot2::aes(x = (g1 + g2) / 2, y = y, label = ct_stars(adj_p)), inherit.aes = FALSE, size = ct_tx(T$axis_title_pt + 1), family = T$family, vjust = .1) }
  p
}

#' Dot panels of the given measures: females above, males below. Titles: test, measure and (physiology) the CombZ class.
ct_dots <- function(X, ids, ncol = length(ids), class_note = FALSE, wrap = 30) {
  lab <- vapply(ct_label(X, ids), function(s) paste(strwrap(s, width = wrap), collapse = "\n"), "")
  t <- paste0(ct_test(X, ids), "\n", lab); if (class_note) { cn <- ct_class_note(X, ids); t <- ifelse(nzchar(cn), paste0(t, "\n", cn), t) }
  titles <- stats::setNames(t, ids)
  patchwork::wrap_elements(full = ct_dot_block(X, ids, "Female", titles, ncol) / ct_dot_block(X, ids, "Male", titles, ncol))
}

# ------------------------------------------------------------------ heatmap of d
#' Behaviour measures for the compact heatmap: the main readouts and every measure with a stored adjusted p below 0.05
#' in any comparison of either sex.
ct_selected_behaviour <- function(X) {
  ms <- X$measures[X$measures$domain == "Behaviour" & X$measures$id != "loco_index", , drop = FALSE]
  sig <- unique(X$contrasts$id[!is.na(X$contrasts$adj_p) & X$contrasts$adj_p < .05])
  ms$id[ms$main | ms$id %in% sig]
}

#' Tile coordinates: measures as rows grouped by test (a header row per test), females in columns 1-4, males in 6-9.
ct_heatmap_data <- function(X, ids) {
  ms <- X$measures[match(ids, X$measures$id), , drop = FALSE]; ms <- ms[order(ms$order), , drop = FALSE]
  rows <- list(); y <- 0; heads <- list()
  for (t in unique(ms$test)) { y <- y - 1; heads[[length(heads) + 1]] <- data.frame(test = t, y = y, stringsAsFactors = FALSE)
    for (i in ms$id[ms$test == t]) { y <- y - 1; rows[[length(rows) + 1]] <- data.frame(id = i, label = ms$label[ms$id == i], y = y, stringsAsFactors = FALSE) }
    y <- y - .4 }
  rows <- do.call(rbind, rows); heads <- do.call(rbind, heads)
  ct <- X$contrasts[X$contrasts$id %in% ids, c("Sex", "id", "comparison", "d", "adj_p")]
  cells <- merge(ct, rows, by = "id"); cells$x <- match(cells$comparison, CT_CMP) + ifelse(cells$Sex == "Male", 5, 0)
  lim <- X$P$diverging_limits$smd; cells$fill <- pmax(pmin(cells$d, lim), -lim); cells$significant <- !is.na(cells$adj_p) & cells$adj_p < .05
  list(cells = cells[order(cells$y, cells$x), ], rows = rows, heads = heads)
}

ct_heatmap <- function(X, ids, legend = TRUE) {
  P <- X$P; T <- P$typography; H <- ct_heatmap_data(X, ids); lim <- P$diverging_limits$smd; dv <- P$diverging
  top <- max(H$heads$y) + 1.3
  sexlab <- data.frame(x = c(2.5, 7.5), y = top, label = CT_SEXLAB, stringsAsFactors = FALSE)
  p <- ggplot2::ggplot(H$cells, ggplot2::aes(x, y, fill = fill)) +
    ggplot2::geom_tile(width = 1, height = 1, colour = "grey85", linewidth = P$line$tile_border_pt) +
    ggplot2::geom_point(data = H$cells[H$cells$significant, ], ggplot2::aes(x + .3, y + .28), inherit.aes = FALSE, shape = 16, size = .6, colour = "black") +
    ggplot2::geom_text(data = H$heads, ggplot2::aes(x = .5, y = y, label = test), inherit.aes = FALSE, hjust = 0, fontface = "bold", size = ct_tx(T$axis_text_pt), family = T$family, colour = CT_GREY) +
    ggplot2::geom_text(data = sexlab, ggplot2::aes(x = x, y = y, label = label), inherit.aes = FALSE, fontface = "bold", size = ct_tx(T$axis_title_pt), family = T$family) +
    ggplot2::scale_fill_gradient2(low = dv$low, mid = dv$mid, high = dv$high, midpoint = 0, limits = c(-lim, lim), breaks = c(-lim, -lim / 2, 0, lim / 2, lim),
                                  labels = c(paste0("≤", CT_MINUS, lim), paste0(CT_MINUS, lim / 2), "0", lim / 2, paste0("≥", lim)), name = "Cohen's d") +
    ggplot2::scale_x_continuous(breaks = c(1:4, 6:9), labels = rep(ct_cmp_label(CT_CMP), 2), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = H$rows$y, labels = H$rows$label, expand = c(0, 0)) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(.5, 9.5), ylim = c(min(H$rows$y) - .5, top + .6), clip = "off") + ggplot2::labs(x = NULL, y = NULL) + ct_theme(P) +
    ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = .5))
  if (legend) p <- p + ggplot2::guides(fill = ggplot2::guide_colourbar(title.position = "top", title.hjust = .5, ticks.colour = "white")) +
    ggplot2::theme(legend.position = "bottom", legend.key.width = grid::unit(5, "mm"), legend.key.height = grid::unit(2, "mm"))
  p
}

# ------------------------------------------------------------------ locomotion (exploratory)
ct_loco_index <- function(X) {
  P <- X$P; T <- P$typography; lw <- P$line$axis_pt * .75
  pts <- do.call(rbind, lapply(CT_SEXES, function(s) { d <- ct_dot_data(X, "loco_index", s); d$points$panel <- CT_SEXLAB[[s]]; d$points }))
  mm <- do.call(rbind, lapply(CT_SEXES, function(s) { d <- ct_dot_data(X, "loco_index", s); d$means$panel <- CT_SEXLAB[[s]]; d$means }))
  br <- do.call(rbind, lapply(CT_SEXES, function(s) {
    b <- X$contrasts[X$contrasts$id == "loco_index" & X$contrasts$Sex == s & X$contrasts$comparison %in% CT_CMP[1:3] & X$contrasts$p < .05, , drop = FALSE]
    if (!nrow(b)) return(NULL); y <- X$values$value[X$values$id == "loco_index" & X$values$Sex == s]
    b$g1 <- match(sub(" - .*", "", b$comparison), CT_GROUPS); b$g2 <- match(sub(".* - ", "", b$comparison), CT_GROUPS); b$y <- max(y) + diff(range(y)) * (.06 + .11 * seq_len(nrow(b))); b$panel <- CT_SEXLAB[[s]]; b }))
  pts$panel <- factor(pts$panel, CT_SEXLAB); mm$panel <- factor(mm$panel, CT_SEXLAB)
  p <- ggplot2::ggplot(pts, ggplot2::aes(x, value)) + ggplot2::geom_hline(yintercept = 0, linewidth = P$line$reference_pt * .75, colour = "grey75") +
    ggplot2::geom_point(ggplot2::aes(colour = Group), shape = 16, size = 1.1, alpha = .75) +
    ggplot2::geom_linerange(data = mm, ggplot2::aes(x = gx, ymin = lower, ymax = upper), inherit.aes = FALSE, linewidth = lw * 1.6, colour = CT_MEAN) +
    ggplot2::geom_point(data = mm, ggplot2::aes(x = gx, y = mean), inherit.aes = FALSE, shape = 16, size = 1.8, colour = CT_MEAN) +
    ggplot2::scale_colour_manual(values = unlist(P$group)) + ggplot2::scale_x_continuous(breaks = 1:3, labels = CT_GROUPS, limits = c(.45, 3.55)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(.06, .22))) + ggplot2::facet_wrap(~panel, nrow = 1) +
    ggplot2::labs(x = NULL, y = "Locomotion index (z, CON = 0)") + ct_theme(P) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", size = T$axis_title_pt, colour = "black", hjust = 0))
  if (!is.null(br) && nrow(br)) { br$panel <- factor(br$panel, CT_SEXLAB)
    p <- p + ggplot2::geom_segment(data = br, ggplot2::aes(x = g1, xend = g2, y = y, yend = y), inherit.aes = FALSE, linewidth = lw * 1.2) +
      ggplot2::geom_text(data = br, ggplot2::aes(x = (g1 + g2) / 2, y = y, label = ct_stars(p)), inherit.aes = FALSE, size = ct_tx(T$axis_title_pt + 1), family = T$family, vjust = .1) }
  p
}

#' Cohen's d of distance travelled in every session and of the index (stored values), RES - CON, SUS - CON, RES - SUS per sex.
ct_loco_sessions_data <- function(X) {
  s <- X$loco_sessions; s$comparison <- sub(paste0(" ", CT_MINUS, " "), " - ", s$Comparison, fixed = TRUE)
  s <- s[s$comparison %in% CT_CMP[1:3], c("Session", "Sex", "comparison", "d")]
  ix <- X$contrasts[X$contrasts$id == "loco_index" & X$contrasts$comparison %in% CT_CMP[1:3], c("Sex", "comparison", "d")]; ix$Session <- "Locomotion index"
  d <- rbind(s, ix[names(s)]); ses <- c(unique(s$Session), "Locomotion index")
  d$y <- -match(d$Session, ses) - ifelse(d$Session == "Locomotion index", .25, 0); d$x <- match(d$comparison, CT_CMP) + ifelse(d$Sex == "Male", 4, 0)
  lim <- X$P$diverging_limits$smd; d$fill <- pmax(pmin(d$d, lim), -lim); d
}

ct_loco_sessions <- function(X) {
  P <- X$P; T <- P$typography; d <- ct_loco_sessions_data(X); dv <- P$diverging; lim <- P$diverging_limits$smd
  rows <- unique(d[c("Session", "y")]); top <- max(d$y) + 1.2
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, fill = fill)) + ggplot2::geom_tile(width = 1, height = 1, colour = "grey85", linewidth = P$line$tile_border_pt) +
    ggplot2::geom_text(ggplot2::aes(label = ct_fmt_r(d)), size = ct_tx(T$annotation_pt), family = T$family) +
    ggplot2::annotate("text", x = c(2, 6), y = top, label = CT_SEXLAB, fontface = "bold", size = ct_tx(T$axis_title_pt), family = T$family) +
    ggplot2::scale_fill_gradient2(low = dv$low, mid = dv$mid, high = dv$high, midpoint = 0, limits = c(-lim, lim), guide = "none") +
    ggplot2::scale_x_continuous(breaks = c(1:3, 5:7), labels = rep(ct_cmp_label(CT_CMP[1:3]), 2), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = rows$y, labels = rows$Session, expand = c(0, 0)) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(.5, 7.5), ylim = c(min(d$y) - .5, top + .5), clip = "off") + ggplot2::labs(x = NULL, y = NULL) + ct_theme(P) +
    ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = .5))
  p
}

# ------------------------------------------------------------------ heterogeneity
ct_combz_spread <- function(X) {
  P <- X$P; T <- P$typography; lw <- P$line$axis_pt * .75
  v <- X$values[X$values$id == "CombZ", ]; v <- v[order(match(v$Sex, CT_SEXES), v$Batch, v$Animal), ]
  set.seed(CT_SEED); v$x <- ifelse(v$Condition == "CON", 1, 2) + CT_BPOS[v$Batch] * .17 + stats::runif(nrow(v), -.045, .045)
  lab <- stats::setNames(vapply(CT_SEXES, function(s) sprintf("%s    n = %d CON, %d SIS", CT_SEXLAB[[s]], sum(v$Sex == s & v$Condition == "CON"), sum(v$Sex == s & v$Condition == "SIS")), ""), CT_SEXES)
  v$panel <- factor(lab[v$Sex], lab)
  tl <- X$tails[X$tails$Condition == "CON", ]
  ref <- do.call(rbind, lapply(CT_SEXES, function(s) { r <- tl[tl$Sex == s, ]; mu <- r$`CON mean`; sd <- r$`CON SD (population)`
    data.frame(panel = lab[[s]], y = c(mu, mu + sd, mu - sd), lt = c("solid", "22", "22"), lab = c("CON mean", "+1 SD", paste0(CT_MINUS, "1 SD")), stringsAsFactors = FALSE) }))
  ref$panel <- factor(ref$panel, lab)
  h <- X$het[X$het$Set == "Score", ]
  ann <- data.frame(panel = factor(lab[h$Sex], lab), txt = sprintf("SD ratio %.2f [%.2f, %.2f]\np %s", h$`SD ratio SIS / CON`, h$`95% CI low`, h$`95% CI high`, ct_pnice(h$`p (Brown-Forsythe)`)), stringsAsFactors = FALSE)
  ggplot2::ggplot(v, ggplot2::aes(x, value)) +
    ggplot2::geom_segment(data = ref, ggplot2::aes(x = .5, xend = 2.34, y = y, yend = y, linetype = lt), inherit.aes = FALSE, linewidth = lw, colour = "grey55") +
    ggplot2::geom_text(data = ref, ggplot2::aes(x = 2.38, y = y, label = lab), inherit.aes = FALSE, hjust = 0, size = ct_tx(T$annotation_pt), family = T$family, colour = CT_GREY) +
    ggplot2::geom_point(ggplot2::aes(colour = Group), shape = 16, size = 1.1, alpha = .8) +
    ggplot2::geom_text(data = ann, ggplot2::aes(x = .55, y = Inf, label = txt), inherit.aes = FALSE, hjust = 0, vjust = 1.15, size = ct_tx(T$annotation_pt), family = T$family, lineheight = .95) +
    ggplot2::scale_linetype_identity() + ggplot2::scale_colour_manual(values = unlist(P$group)) +
    ggplot2::scale_x_continuous(breaks = 1:2, labels = c("CON", "SIS"), limits = c(.5, 3.05), expand = c(0, 0)) + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(.05, .24))) +
    ggplot2::facet_wrap(~panel, nrow = 1) + ggplot2::coord_cartesian(clip = "off") + ggplot2::labs(x = NULL, y = "Integrated Resilience (CombZ)") + ct_theme(P) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", size = T$axis_title_pt, colour = "black", hjust = 0))
}

ct_ratio_forest <- function(X) {
  P <- X$P; T <- P$typography; lw <- P$line$axis_pt * .75; h <- X$het
  h$Sexl <- factor(CT_SEXLAB[h$Sex], CT_SEXLAB); h$Set <- factor(h$Set, c("Score", "Components", "Test readouts"))
  h$row <- factor(h$Measure, levels = rev(unique(h$Measure))); h$filled <- !is.na(h$`adj. p`) & h$`adj. p` < .05
  h$lo <- pmax(h$`95% CI low`, .3); h$hi <- pmin(h$`95% CI high`, 6)   # intervals beyond the axis are drawn to its end
  ggplot2::ggplot(h, ggplot2::aes(`SD ratio SIS / CON`, row)) + ggplot2::geom_vline(xintercept = 1, linewidth = lw, colour = "grey55") +
    ggplot2::geom_linerange(ggplot2::aes(xmin = lo, xmax = hi, colour = filled), linewidth = lw * 1.6) + ggplot2::geom_point(ggplot2::aes(colour = filled), shape = 16, size = 1.4) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "black", `FALSE` = "grey60"), guide = "none") +
    ggplot2::scale_x_log10(breaks = c(.5, 1, 2, 4), labels = c("0.5", "1", "2", "4"), limits = c(.3, 6)) +
    ggplot2::facet_grid(Set ~ Sexl, scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::labs(x = "SD ratio, SIS / CON (log scale)", y = NULL) + ct_theme(P) +
    ggplot2::theme(strip.placement = "outside", strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1, face = "bold", colour = CT_GREY),
                   strip.text.x = ggplot2::element_text(face = "bold", hjust = 0, size = T$axis_title_pt, colour = "black"), panel.spacing.y = grid::unit(1.2, "mm"))
}
