# Stage 30 exploratory screen summary (CANDIDATE_SPEC B6, s30_panel_screen).
# Rendering only: fits no model, computes no statistic, adjusts no p value.
#
# A decision matrix of the 48 registered Stage 30 discovery tests, drawn from the
# pinned Stage 30 figure bundle (s30b):
#   - three column facets, Female | Male | Female - male (S4 col_order / sex_col);
#   - three row blocks, stacked as separate patchwork rows so that each block has
#     its own x-scale, shared by its three columns:
#       CONT (S4 rows 1-6)   standardized slope, CombZ per SD of the metric;
#       CAT  (S4 rows 7-9)   standardized RES - SUS difference, in SD of the metric;
#       L    (S4 rows 10-16) joint F of CombZ x cage change, on a 0-to-max scale
#                            with a thin solid reference line at F = 1 (labelled once,
#                            unlike the dashed zero lines of the forests above it);
#   - CONT/CAT cells: the stored standardized estimate and standardized CI
#     (S4 standardized_estimate / standardized_ci_low / standardized_ci_high, the
#     bundle's rescaling of the frozen CI with the stored rule: the SD of the metric's
#     within-batch residuals, in each sex, and over both sexes for Female - male), zero line;
#   - the Female - male column draws the white diamond of BH_FOREST (int_*), never a
#     fill-coded circle;
#   - in every cell the stored raw p and local BH q (S0_master_hypotheses p_raw /
#     q_local_bh, one annotation key per test id and quantity), 6 pt;
#   - the registered class letter A-D (S4 classification) as small grey text at
#     the far right of each cell: no fill, colour or highlighting by class.
# Row labels are the S4 metric_label (short form "sustained positional inactivity
# (>=40 s)", CANDIDATE_SPEC A) over the S4 window_label, with the family (S4
# block_label) and its size m as a small grey suffix. The registered family ids are
# not printed: their SLEEP- prefix is a banned display word.
#
# Every printed number goes through the annotation map (bh_annotation(), keys from
# s30_screen_key() / s30_screen_family_key() / S30_SCREEN_*_KEY) to exactly one
# stored cell. The only other numbers on the panel are axis limits and breaks
# (the L reference F = 1 is always a labelled break) and the words of the
# registered labels (CC1, CC1-CC4, >=40 s, EPM+1).
#
# Layout. Each block is a header strip (block name; column names) over a content
# row of seven plots: row labels | (forest, p/q text) x 3, with an empty 2.5-mm gap
# column after the Female and Male cells. Horizontal positions
# inside the label, text and header plots are in mm (their x-scale is 0..width mm
# with no expansion), so padding is exact at any box width.
#
# Requires: R/behaviour_bundle.R, R/stage30_bundle.R and
# R/panels/behaviour_figure_style.R sourced first; packages ggplot2, patchwork, scales.

# Default panel box (w x h, mm): the Supplementary Figure body, a 183-mm page less 5-mm margins.
S30_SCREEN_BOX <- c(w = 173, h = 128)
S30_SCREEN_TABLE <- "S4_screen_matrix"
S30_SCREEN_BLOCKS <- c("CONT", "CAT", "L")
# Annotation keys that are not per test id.
S30_SCREEN_N_KEY <- "scr_n_tests"      # the number of registered tests in the title
S30_SCREEN_DF1_KEY <- "scr_l_df1"      # the numerator df of every L joint F (subtitle)
# Width (mm) of the p/q text column of one cell, by print style; the class letter sits at its right.
S30_SCREEN_TEXT_MM <- c(stacked = 16.5, inline = 27, compact = 21)
S30_SCREEN_PAD_MM <- c(text_left = 1.2, class_right = 2.2, label_right = 2.2, axis_title_down = 1.2)
# Gap (mm) after the Female and Male cells, so each class letter sits nearer its own p/q text than
# the next column's forest.
S30_SCREEN_COL_GAP_MM <- 2.5
# Header strips: height of the first and of the later block headers (mm), and the space between
# the header baseline and the strip bottom (mm), which keeps the header off the block's first row.
S30_SCREEN_HEADER_MM <- c(first = 4.3, later = 6.3)
S30_SCREEN_HEADER_BASELINE_MM <- c(first = 1.06, later = 1.26)
# Axis title of each block, drawn in the label column beside the block's x-axes (7 pt, right-aligned).
# The SD is each column's own standardizer (S0 standardization_rule): the within-batch residual SD.
S30_SCREEN_AXIS_TITLE <- c(CONT = "CombZ per within-batch SD", CAT = "RES \u2212 SUS, within-batch SD", L = "joint F")
# The L reference line: thin and solid (the forests' zero lines are dashed), labelled once.
S30_SCREEN_L_REF <- list(linewidth = 0.3, colour = RULE, label = "F = 1")

#' Annotation key of one test's printed p ("p") or local BH q ("q").
s30_screen_key <- function(id, what = c("p", "q")) {
  what <- match.arg(what)
  paste0("scr_", tolower(gsub("-", "_", id, fixed = TRUE)), "_", what)
}

#' Annotation key of one family's printed size m.
s30_screen_family_key <- function(family) paste0("scr_m_", tolower(gsub("-", "_", family, fixed = TRUE)))

#' Display rows for the S4 labels: line 1 the metric, line 2 the window (string handling only).
#' The inactivity metric takes the panel short form; a metric whose window is empty and
#' whose label ends in a parenthesis (the cookie response) moves that parenthesis to line 2.
s30_screen_row_label <- function(metric_label, window_label) {
  metric <- sub("^RFID-defined ", "", metric_label)
  window <- ifelse(is.na(window_label), "", gsub("CC1-CC4", "CC1\u2013CC4", window_label, fixed = TRUE))
  paren <- !nzchar(window) & grepl(" \\(([^()]*)\\)$", metric)
  window[paren] <- sub("^.* \\(([^()]*)\\)$", "\\1", metric[paren])
  metric[paren] <- sub(" \\(([^()]*)\\)$", "", metric[paren])
  data.frame(metric = metric, window = window, stringsAsFactors = FALSE)
}

#' Axis tick labels with the typographic minus.
s30_screen_ticks <- function(x) bh_minus(as.character(x))

#' A void plot whose x-scale is 0..width_mm and y-scale ylim, both without expansion.
s30_screen_void <- function(data, width_mm, ylim) {
  ggplot(data) +
    scale_x_continuous(limits = c(0, width_mm), expand = c(0, 0)) +
    scale_y_continuous(limits = ylim, expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    theme_void(base_family = "sans") + theme(plot.margin = margin(0, 0, 0, 0))
}

#' A header strip `height_mm` high: `left` text at x = 0 and optional grey 6-pt heads, all on one
#' baseline `baseline_mm` above the strip's bottom edge.
s30_screen_strip <- function(width_mm, left, face = "plain", heads = list(), height_mm = 1, baseline_mm = 0.1) {
  p <- s30_screen_void(NULL, width_mm, c(0, height_mm)) +
    annotate("text", x = 0, y = baseline_mm, label = left, hjust = 0, vjust = 0, size = BASE_PT / .pt, colour = INK, fontface = face)
  for (h in heads)
    p <- p + annotate("text", x = h$x, y = baseline_mm, label = h$label, hjust = h$hjust, vjust = 0, size = NOTE_PT / .pt, colour = MUTED)
  p
}

#' Stage 30 screen summary, CANDIDATE_SPEC B6.
#'
#' `s30`       table getter, function(name) -> the pinned Stage 30 bundle table
#'             (e.g. function(n) stage30_bundle_table(n));
#' `an`        the resolver from bh_annotation() over a map holding the keys of
#'             figures/behaviour_v101_s30_annotation_map.csv (panel screen_matrix, bundle label "s30b");
#' `w_mm, h_mm` the box the panel is authored for (default 173 x 128);
#' `pq_style`  how each cell prints its p and local q (all resolve to the same keys):
#'             "stacked" "p = ..." over "q = ..." (default: two lines, like the row labels);
#'             "inline"  "p = ...; q = ..." on one line (needs a wider box);
#'             "compact" "... | ..." under a "p | q" column head;
#' `label_mm`  width of the row-label column (mm);
#' `top_margin_pt`  the top margin: 11.5 pt leaves room for a panel letter; the letterless
#'             Supplementary page passes a small one.
#' Returns bh_panel(patchwork, w_mm, h_mm).
s30_panel_screen <- function(s30, an, w_mm = S30_SCREEN_BOX[["w"]], h_mm = S30_SCREEN_BOX[["h"]],
                             pq_style = c("stacked", "inline", "compact"), label_mm = 45, top_margin_pt = 11.5) {
  pq_style <- match.arg(pq_style)
  # Legibility floor of the layout: the two-line row labels need a row pitch of about 5.2 mm
  # (h >= 123 mm with the letter margin; about 37 mm of the height is titles, headers and axes),
  # and the two-line subtitle is about 150 mm wide at 6 pt.
  if (h_mm < 123 - (11.5 - top_margin_pt) * 25.4 / 72)
    stop("s30_panel_screen: the box is too low for legible two-line rows.", call. = FALSE)
  if (w_mm < 160) stop("s30_panel_screen: the box must be at least 160 mm wide (subtitle width).", call. = FALSE)
  fa <- an$fa
  S4 <- s30(S30_SCREEN_TABLE)

  # ---- layout checks (the table is used as stored; nothing is derived from it)
  need <- c("id", "question", "question_label", "family", "family_m", "block_label", "metric_label", "window_label",
            "sex_col", "row_order", "col_order", "standardized_estimate", "standardized_ci_low", "standardized_ci_high",
            "F", "df1", "classification")
  miss <- setdiff(need, names(S4))
  if (length(miss)) stop("s30_panel_screen: ", S30_SCREEN_TABLE, " lacks column(s) ", paste(miss, collapse = ", "), call. = FALSE)
  if (anyDuplicated(S4$id) || anyDuplicated(paste(S4$row_order, S4$col_order)))
    stop("s30_panel_screen: ids or (row_order, col_order) cells are not unique.", call. = FALSE)
  if (!setequal(S4$question, S30_SCREEN_BLOCKS) || !setequal(S4$col_order, 1:3))
    stop("s30_panel_screen: expected blocks CONT/CAT/L and three columns.", call. = FALSE)
  if (nrow(S4) != length(unique(S4$row_order)) * 3L)
    stop("s30_panel_screen: the matrix is not complete (every row needs all three columns).", call. = FALSE)
  if (!all(S4$classification %in% c("A", "B", "C", "D")))
    stop("s30_panel_screen: a class is not one of A-D.", call. = FALSE)
  is_l <- S4$question == "L"
  if (any(!is.finite(S4$F[is_l])) ||
      any(!is.finite(c(S4$standardized_estimate[!is_l], S4$standardized_ci_low[!is_l], S4$standardized_ci_high[!is_l]))))
    stop("s30_panel_screen: a plotted estimate, interval or F is missing.", call. = FALSE)
  # A printed value may stand for several stored cells only where every one of them equals it.
  if (!isTRUE(all.equal(an$ann(S30_SCREEN_N_KEY), nrow(S4))))
    stop("s30_panel_screen: the printed number of tests differs from the rows of ", S30_SCREEN_TABLE, ".", call. = FALSE)
  if (any(S4$df1[is_l] != an$ann(S30_SCREEN_DF1_KEY)))
    stop("s30_panel_screen: the L tests do not share the printed numerator df.", call. = FALSE)
  for (fm in unique(S4$family))
    if (any(S4$family_m[S4$family == fm] != an$ann(s30_screen_family_key(fm))))
      stop("s30_panel_screen: family ", fm, " rows do not share the printed m.", call. = FALSE)

  COL_LAB <- S4$sex_col[match(1:3, S4$col_order)]
  S4 <- S4[order(S4$row_order, S4$col_order), , drop = FALSE]
  p_txt <- vapply(S4$id, function(i) fa(s30_screen_key(i, "p")), "")
  q_txt <- vapply(S4$id, function(i) fa(s30_screen_key(i, "q")), "")
  S4$pq <- switch(pq_style,
                  stacked = paste0("p = ", p_txt, "\nq = ", q_txt),
                  inline = paste0("p = ", p_txt, "; q = ", q_txt),
                  compact = paste0(p_txt, " | ", q_txt))

  # ---- widths (mm): row labels | forest, p/q text, gap | forest, p/q text, gap | forest, p/q text
  text_mm <- S30_SCREEN_TEXT_MM[[pq_style]]
  PAD <- S30_SCREEN_PAD_MM
  gap_mm <- S30_SCREEN_COL_GAP_MM
  inner_w <- w_mm - 2 * 3 * 25.4 / 72                       # plot.margin 3 pt left and right
  forest_mm <- (inner_w - label_mm - 3 * text_mm - 2 * gap_mm) / 3
  if (forest_mm < 16) stop("s30_panel_screen: the box is too narrow for ", pq_style, " cells (forest ",
                           format(round(forest_mm, 1)), " mm).", call. = FALSE)
  widths <- c(label_mm, forest_mm, text_mm, gap_mm, forest_mm, text_mm, gap_mm, forest_mm, text_mm)
  col_forest <- function(s) 3L * s - 1L                      # grid columns of cell s: forest, then text
  col_text <- function(s) 3L * s
  cell_mm <- forest_mm + text_mm

  EXPAND <- 0.42                                             # row units above the first and below the last row
  plots <- list(); areas <- list(); heights <- numeric(0); height_units <- character(0)
  add <- function(p, t, l, r = l) {
    plots[[length(plots) + 1L]] <<- p
    areas[[length(areas) + 1L]] <<- patchwork::area(t = t, l = l, b = t, r = r)
  }
  grid_row <- 0L
  for (b in S30_SCREEN_BLOCKS) {
    d <- S4[S4$question == b, , drop = FALSE]
    rows <- sort(unique(d$row_order)); n <- length(rows)
    d$y <- -match(d$row_order, rows)
    ylim <- c(-n - EXPAND, -1 + EXPAND)

    # header strip: block name over the labels; column name over each cell, "class" over the
    # last column's class letters only (and "p | q" over each cell in the compact style)
    grid_row <- grid_row + 1L
    hk <- if (grid_row == 1L) "first" else "later"
    head_mm <- S30_SCREEN_HEADER_MM[[hk]]; base_mm <- S30_SCREEN_HEADER_BASELINE_MM[[hk]]
    heights <- c(heights, head_mm); height_units <- c(height_units, "mm")
    add(s30_screen_strip(label_mm, paste0(b, " \u00b7 ", d$question_label[1]), height_mm = head_mm, baseline_mm = base_mm),
        grid_row, 1L)
    for (s in 1:3) {
      heads <- if (s == 3L) list(list(x = cell_mm - PAD[["class_right"]], label = "class", hjust = 1)) else list()
      if (pq_style == "compact") heads <- c(heads, list(list(x = forest_mm + PAD[["text_left"]], label = "p | q", hjust = 0)))
      add(s30_screen_strip(cell_mm, COL_LAB[s], face = "italic", heads, height_mm = head_mm, baseline_mm = base_mm),
          grid_row, col_forest(s), col_text(s))
    }

    # content row
    grid_row <- grid_row + 1L
    heights <- c(heights, n + 2 * EXPAND - 1); height_units <- c(height_units, "null")
    lab <- d[d$col_order == 1L, , drop = FALSE]
    lab <- cbind(lab, s30_screen_row_label(lab$metric_label, lab$window_label))
    # The grey suffix is a two-line string with an empty first line, so it sits on the window line.
    lab$suffix <- paste0(" \n", tolower(lab$block_label), ", m = ",
                         vapply(lab$family, function(f) fa(s30_screen_family_key(f)), ""))
    p_lab <- s30_screen_void(lab, label_mm, ylim) +
      geom_text(aes(x = 0, y = y, label = paste0(metric, "\n", window)), hjust = 0, vjust = 0.5, lineheight = 1,
                size = NOTE_PT / .pt, colour = INK) +
      geom_text(aes(x = label_mm - PAD[["label_right"]], y = y, label = suffix), hjust = 1, vjust = 0.5, lineheight = 1,
                size = NOTE_PT / .pt, colour = MUTED) +
      annotation_custom(grid::textGrob(S30_SCREEN_AXIS_TITLE[[b]],
                                       x = grid::unit(1, "npc") - grid::unit(PAD[["label_right"]], "mm"),
                                       y = grid::unit(0, "npc") - grid::unit(PAD[["axis_title_down"]], "mm"),
                                       hjust = 1, vjust = 1, gp = grid::gpar(fontsize = BASE_PT, col = INK, fontfamily = "sans")),
                        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
    add(p_lab, grid_row, 1L)

    if (b == "L") {
      xlim <- range(c(0, d$F))                               # 0-to-max scale (presentational limits)
      x_expand <- expansion(mult = c(0.03, 0.06))
      x_breaks <- function(l) seq(0, floor(l[2]), by = 1)    # integer breaks: the F = 1 reference is a labelled break
    } else {
      xlim <- range(c(0, d$standardized_ci_low, d$standardized_ci_high))
      x_expand <- expansion(mult = 0.06)
      x_breaks <- scales::breaks_pretty(n = 3)
    }
    for (s in 1:3) {
      ds <- d[d$col_order == s, , drop = FALSE]
      # within-sex columns: INK circles; the Female - male column: the white diamond
      mark <- function(mapping) if (s == 3L) bh_forest_int_point(mapping) else bh_forest_point(mapping, fill = INK)
      pf <- ggplot(ds, aes(y = y))
      if (b == "L") {
        pf <- pf + geom_vline(xintercept = 1, linewidth = S30_SCREEN_L_REF$linewidth, colour = S30_SCREEN_L_REF$colour) +
          mark(aes(x = .data[["F"]]))                                                  # the column, never FALSE
        # labelled once, at the foot of the line (clear of the column header and the first row)
        if (s == 1L)
          pf <- pf + annotate("text", x = 1, y = ylim[1], label = S30_SCREEN_L_REF$label, hjust = -0.15, vjust = -0.35,
                              size = NOTE_PT / .pt, colour = MUTED)
      } else {
        pf <- pf + bh_forest_zero(0) +
          bh_forest_ci(aes(x = standardized_ci_low, xend = standardized_ci_high, yend = y)) +
          mark(aes(x = standardized_estimate))
      }
      pf <- pf +
        scale_x_continuous(limits = xlim, breaks = x_breaks, labels = s30_screen_ticks, expand = x_expand) +
        scale_y_continuous(limits = ylim, expand = c(0, 0)) +
        coord_cartesian(clip = "off") +
        theme_f1() +
        theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),
              axis.title = element_blank(), plot.margin = margin(0, 0, 0, 0))
      add(pf, grid_row, col_forest(s))

      pt <- s30_screen_void(ds, text_mm, ylim) +
        geom_text(aes(x = PAD[["text_left"]], y = y, label = pq), hjust = 0, vjust = 0.5, lineheight = 1,
                  size = NOTE_PT / .pt, colour = INK) +
        geom_text(aes(x = text_mm - PAD[["class_right"]], y = y, label = classification), hjust = 1, vjust = 0.5,
                  size = NOTE_PT / .pt, colour = MUTED)
      add(pt, grid_row, col_text(s))
    }
  }

  # Two lines: the tier, population and class; the standardizer and the L statistic. The rest
  # (the families and their m, what p and q are) is in the row labels and the legend.
  sub_txt <- paste0(
    "Stage 30 exploratory: raw p and local BH q per cell (family and m after each row label); ",
    "SIS animals, CON not modelled; grey letter: registered class A\u2013D\n",
    "SD: within-batch residual SD of the metric in each sex (Female \u2212 male: both sexes); ",
    sprintf("L: joint F(%s, df) of CombZ \u00d7 cage change (Female \u2212 male: its sex difference)", fa(S30_SCREEN_DF1_KEY)))
  pw <- patchwork::wrap_plots(plots, design = do.call(c, areas)) +
    patchwork::plot_layout(widths = grid::unit(widths, "null"), heights = grid::unit(heights, height_units)) +
    patchwork::plot_annotation(
      title = sprintf("Stage 30 exploratory screen: %s registered tests", fa(S30_SCREEN_N_KEY)),
      subtitle = sub_txt,
      theme = theme(plot.title = element_text(size = BASE_PT, colour = INK, hjust = 0, face = "plain", margin = margin(b = 0.5)),
                    plot.subtitle = element_text(size = NOTE_PT, colour = "grey25", lineheight = 1.05, margin = margin(b = 2)),
                    plot.margin = margin(top_margin_pt, 3, 3, 3)))
  bh_panel(pw, w_mm, h_mm)
}
