# Stage 30 home-cage cookie response panels (candidate generation behaviour_v101_s30;
# CANDIDATE_SPEC B5 a/b/c). Rendering only: fits no model, computes no statistic.
#
# The home-cage cookie response (second presentation, EPM+1) is the RFID
# position-change rate in POST60 = EPM+1 [17:00, 18:00) minus the rate in
# PRE60 = [16:00, 17:00), on the logger clock (Stage 30 registry v1.0). SIS
# animals only (B2-B6): CON is not shown and was not modelled.
#
#   s30_panel_cookie_prepost  a  PRE60 -> POST60 per animal, by sex, with the stored
#                                median and IQR per window (S3, S3b)            58 x 62 mm
#   s30_panel_cookie_rs       b  Δ60 by sex x later group, and the stored RES - SUS
#                                contrasts F / M / F - M of COOKIE-CAT (S3, S3c)  60 x 62 mm
#   s30_panel_cookie_combz    c  Δ60 vs later CombZ by sex, with the stored line of the
#                                frozen COOKIE-CONT slope through the centroid (S3, S3c, S3d)
#                                                                                59 x 62 mm
#
# Every builder takes
#   tab   the Stage 30 figure-bundle table getter, function(name) -> the pinned table
#         (stage30_bundle_table() on the pinned copy);
#   an    the annotation resolver from bh_annotation() over a map that declares the
#         cookie keys (figures/behaviour_v101_s30_annotation_map.csv, panels cookie_*, bundle s30b);
#   w_mm, h_mm   the box the panel is authored for (defaults: the CANDIDATE_SPEC box);
# and returns bh_panel(plot, w_mm, h_mm), as the Figure 1 builders do ($plot is the
# ggplot / patchwork object). No file I/O.
#
# Every printed number goes through an$fa() / an$ann(), i.e. through the annotation
# map to exactly one stored cell. Plotted animals are stored S3 rows; plotted medians
# and quartiles are stored S3b rows; the forest estimates are the resolved annotation
# cells themselves (so the drawn point and the printed number are the same cell);
# the fitted line is the stored S3d segment. The window times printed on panel a are
# the registered windows, checked against the bundle's S5 display labels.
#
# Requires: R/behaviour_bundle.R, R/stage30_bundle.R and R/panels/behaviour_figure_style.R
# sourced first. All helpers here are prefixed s30ck_ so that the other Stage 30 panel
# libraries can be sourced into the same environment.

# CANDIDATE_SPEC B5 boxes, w x h in mm.
S30_COOKIE_BOX <- list(prepost = c(58, 62), rs = c(60, 62), combz = c(59, 62))
# Panel b: relative heights of the animal panel and the contrast strip, and relative widths of
# the strip's forest and its text column (the text column holds "-5.62 [-15.7, 4.49]" at three
# significant digits with 1 mm to spare).
S30_COOKIE_RS_HEIGHTS <- c(2.4, 1)
S30_COOKIE_RS_WIDTHS <- c(1, 0.82)

S30CK_SEX <- c("Female", "Male")
S30CK_OFF <- c(RES = -0.14, SUS = 0.14)          # Figure 1c grammar: group offsets within sex
S30CK_NEUTRAL_FILL <- "#6E8B99"                   # the Figure 1e fill, for non-group scatters
# The registered windows (Stage 30 registry v1.0, measures.cookie_response_60) as displayed;
# each must occur in the bundle's S5 display label of its window.
S30CK_WINDOW <- c(PRE60 = "16:00–17:00", POST60 = "17:00–18:00")
S30CK_WINDOW_S5 <- c(PRE60 = "[16:00, 17:00)", POST60 = "[17:00, 18:00)")

# Units. The inverse hour is plotmath, as in Figure 1 (Arial has no superscript minus).
S30CK_RATE_Y <- expression(textstyle(atop(displaystyle("RFID position-change rate"),
                                          displaystyle("(position changes h"^-1 * ")"))))
# Two lines, quantity then unit, for the short y axis of panel b's animals.
S30CK_DELTA_2 <- expression(textstyle(atop(displaystyle("Δ rate (POST − PRE)"),
                                           displaystyle("(position changes h"^-1 * ")"))))
S30CK_DELTA_1 <- expression("Δ position changes h"^-1 ~ "(POST − PRE)")
# The contrast strip of panel b is in RES - SUS units of the response, labelled as the light strips.
S30CK_RS_X <- expression("RES − SUS (Δ position changes h"^-1 * ")")
# Panel a and c share one footer: a one-line x title and two 6-pt caption lines, so their facet
# strips and x axes line up in the row. The slope unit (CombZ per position change per hour, the
# y unit per x unit) is stated in the legend: a plotmath caption would draw its superscript at 4.2 pt.
S30CK_COMBZ_CAPTION <- "line: frozen slope through the centroid\nFemale − male difference in slope: p = %s"
S30CK_PREPOST_CAPTION <- "one grey line per SIS animal;\nopen point and bar: median and IQR (descriptive)"

# ---------------------------------------------------------------- helpers
s30ck_stop <- function(...) stop(..., call. = FALSE)

#' Axis tick labels with the typographic minus (presentational; the breaks are ggplot's).
s30ck_ticks <- function(b) bh_minus(as.character(b))

#' The rows of a table whose `col` equals each of `values`, in that order, exactly one each.
s30ck_rows <- function(tab, col, values, what) {
  i <- match(values, tab[[col]])
  if (anyNA(i) || sum(tab[[col]] %in% values) != length(values))
    s30ck_stop(what, ": expected exactly one row for each of ", paste(values, collapse = ", "))
  tab[i, , drop = FALSE]
}

#' theme_f1 with the title, subtitle and caption placed from the plot edge (the long
#' Stage 30 subtitles would otherwise start after the y axis and run out of the box).
s30ck_theme <- function() {
  theme_f1() + theme(plot.title.position = "plot", plot.caption.position = "plot")
}

#' The stored estimate strip: rows Female / Male / Female - male (top to bottom), the
#' ED forest marks (Female - male: the white diamond), and at the right of each row the
#' printed text of that row.
#' `keys` are the annotation keys of the three estimates (each with _lo/_hi);
#' `row_text` is the printed text per row (already resolved through the map).
s30ck_strip <- function(an, keys, row_text, xlab) {
  est <- data.frame(y = 3:1, label = c("Female", "Male", "Female − male"),
                    estimate = vapply(keys, an$ann, numeric(1)),
                    lo = vapply(paste0(keys, "_lo"), an$ann, numeric(1)),
                    hi = vapply(paste0(keys, "_hi"), an$ann, numeric(1)),
                    text = row_text, within = c(TRUE, TRUE, FALSE), stringsAsFactors = FALSE)
  forest <- ggplot(est) +
    bh_forest_zero() +
    bh_forest_ci(aes(x = lo, xend = hi, y = y, yend = y)) +
    bh_forest_point(aes(x = estimate, y = y), data = est[est$within, , drop = FALSE], fill = INK) +
    bh_forest_int_point(aes(x = estimate, y = y), data = est[!est$within, , drop = FALSE]) +
    scale_y_continuous(limits = c(0.5, 3.5), expand = c(0, 0), breaks = est$y, labels = est$label) +
    scale_x_continuous(labels = s30ck_ticks, expand = expansion(mult = 0.06)) +
    labs(x = xlab, y = NULL) +
    theme_f1() +
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
          plot.margin = margin(1, 0, 3, 3))
  txt <- ggplot(est) +
    # tight leading within a row's two lines, so each pair reads as one row
    geom_text(aes(x = 0, y = y, label = text), hjust = 0, vjust = 0.5, size = NOTE_PT / .pt,
              colour = INK, lineheight = 0.9) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.5, 3.5), expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    theme_void(base_size = BASE_PT, base_family = "sans") +
    theme(plot.margin = margin(1, 3, 3, 2))
  list(forest = forest, text = txt, rows = est)
}

# =============================================================== a: PRE60 -> POST60
#' Home-cage cookie response: PRE60 -> POST60 per animal, by sex.
#' Lines: S3 PRE60/POST60, one per animal (group-blind). Open point and bar: the stored
#' S3b median and quartiles (q25-q75) of each window within sex, drawn just outside the
#' animals' line ends; a white point on a bar without whiskers, so the descriptive summary
#' does not take the mark of a model estimate +/- CI (Figure 1c: black point, whiskered bar).
#' Subtitle: S3b n_increased / n of the pooled 77 SIS animals.
#' Keys: ckpp_n_increased, ckpp_n.
s30_panel_cookie_prepost <- function(tab, an, w_mm = S30_COOKIE_BOX$prepost[1], h_mm = S30_COOKIE_BOX$prepost[2]) {
  fa <- an$fa
  S3 <- tab("S3_cookie_animals")
  S3b <- tab("S3b_cookie_descriptives")
  S5 <- tab("S5_display_labels")
  lab5 <- s30ck_rows(S5, "measure_col", names(S30CK_WINDOW_S5), "S5_display_labels")$display_label
  if (!all(mapply(grepl, S30CK_WINDOW_S5, lab5, MoreArgs = list(fixed = TRUE))))
    s30ck_stop("panel a: the displayed windows differ from the bundle's S5 display labels.")

  win <- names(S30CK_WINDOW)
  long <- data.frame(AnimalNum = rep(S3$AnimalNum, 2), Sex = rep(S3$Sex, 2),
                     window = rep(win, each = nrow(S3)), rate = c(S3$PRE60, S3$POST60),
                     stringsAsFactors = FALSE)
  long <- long[is.finite(long$rate), , drop = FALSE]
  long$x <- match(long$window, win)
  long$Sex <- factor(long$Sex, levels = S30CK_SEX)

  md <- S3b[S3b$scope == "sex" & S3b$Sex %in% S30CK_SEX & S3b$measure %in% win, , drop = FALSE]
  if (nrow(md) != 4L || anyDuplicated(md[, c("Sex", "measure")]))
    s30ck_stop("panel a: expected one S3b median row per sex x window.")
  md$Sex <- factor(md$Sex, levels = S30CK_SEX)
  md$x <- match(md$measure, win) + c(-0.16, 0.16)[match(md$measure, win)]

  # Presented at the start of POST60 (the registered window checked against S5 above).
  sub_txt <- sprintf("second presentation, %s on EPM+1; %s/%s increased", sub("–.*$", "", S30CK_WINDOW[["POST60"]]),
                     fa("ckpp_n_increased"), fa("ckpp_n"))
  p <- ggplot(long, aes(x, rate)) +
    geom_line(aes(group = AnimalNum), linewidth = 0.2, alpha = 0.35, colour = "grey30") +
    geom_linerange(data = md, aes(x = x, ymin = q25, ymax = q75), inherit.aes = FALSE,
                   linewidth = 0.4, colour = "black") +
    geom_point(data = md, aes(x = x, y = median), inherit.aes = FALSE, shape = 21, size = 1.3, stroke = 0.4,
               colour = "black", fill = "white") +
    facet_wrap(~ Sex, nrow = 1) +
    scale_x_continuous(breaks = 1:2, labels = c("PRE", "POST"), limits = c(0.72, 2.28), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.02, 0.04)), labels = s30ck_ticks) +
    # phantom("(h^-1)"): the vertical text extent of panel c's plotmath x title, so the x axes line up
    labs(x = bquote(.(sprintf("PRE %s, POST %s", S30CK_WINDOW[["PRE60"]], S30CK_WINDOW[["POST60"]])) * phantom("(h"^-1 * ")")),
         y = S30CK_RATE_Y, title = "Home-cage cookie response", subtitle = sub_txt,
         caption = S30CK_PREPOST_CAPTION) +
    s30ck_theme()
  bh_panel(p, w_mm, h_mm)
}

# =============================================================== b: response by later group
#' Response by later group: Δ60 animals by sex x group (Figure 1c grammar: RES -0.14 /
#' SUS +0.14, seeded jitter of width 0.05) above the stored COOKIE-CAT RES - SUS strip
#' (F, M, F - M) with "estimate [lo, hi]" and "p = ...; q = ..." (local BH) per row.
#' `jitter_seed` is the seed of the presentational jitter (explicit, so the SVG does
#' not depend on the global RNG stream or the build order).
#' Keys: ckrs_{f,m,int}{,_lo,_hi,_p,_q}, ckrs_family_m (the local family size in the subtitle).
s30_panel_cookie_rs <- function(tab, an, w_mm = S30_COOKIE_BOX$rs[1], h_mm = S30_COOKIE_BOX$rs[2], jitter_seed = 30L) {
  fa <- an$fa
  S3 <- tab("S3_cookie_animals")
  pts <- S3[is.finite(S3$dcookie60) & S3$Group %in% names(S30CK_OFF), c("AnimalNum", "Sex", "Group", "dcookie60")]
  pts$Sex <- factor(pts$Sex, levels = S30CK_SEX)
  pts$Group <- factor(pts$Group, levels = names(S30CK_OFF))
  pts$x <- as.numeric(pts$Sex) + S30CK_OFF[as.character(pts$Group)]

  top <- ggplot(pts, aes(x, dcookie60)) +
    geom_point(aes(fill = Group, shape = Group), position = position_jitter(width = 0.05, height = 0, seed = jitter_seed),
               size = 0.7, stroke = 0.18, alpha = 0.55, colour = "grey20") +
    scale_fill_manual(values = GROUP_COL[c("RES", "SUS")]) +
    scale_shape_manual(values = GROUP_SHAPE[c("RES", "SUS")]) +
    guides(fill = guide_legend(override.aes = list(size = 1.3, alpha = 0.9, stroke = 0.25))) +
    scale_x_continuous(breaks = 1:2, labels = S30CK_SEX, limits = c(0.6, 2.4), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.02, 0.05)), labels = s30ck_ticks) +
    # The q printed per row is local to the registered cookie-group family (its m from the map),
    # as the light panels say of theirs; the subtitle names it, so it is not read as the 48-test q.
    # One line, so the animal panel keeps its height (longer than its two-line y title).
    labs(x = NULL, y = S30CK_DELTA_2, title = "Response by later group",
         subtitle = sprintf("registered exploratory test (local BH q, m = %s); SIS only", fa("ckrs_family_m"))) +
    s30ck_theme() +
    # The house legend (Figure 1, B1/B2): above the plot, left-justified, no title; placed
    # against the plot edge (legend.location), so the key sits under the flush-left title.
    theme(legend.position = "top", legend.justification = "left", legend.location = "plot",
          legend.box.spacing = unit(1, "pt"), plot.margin = margin(11.5, 3, 1.5, 3))

  keys <- c("ckrs_f", "ckrs_m", "ckrs_int")
  # The one printed m stands for the family of all three printed q values: check that it does.
  q_ids <- sub("^id=", "", vapply(paste0(keys, "_q"), function(k) an$row(k)$filters, ""))
  S3c <- tab("S3c_cookie_estimates")
  if (anyNA(match(q_ids, S3c$id)) || any(S3c$family_m[match(q_ids, S3c$id)] != an$ann("ckrs_family_m")))
    s30ck_stop("panel b: the printed q values do not share the printed family size m.")
  row_text <- vapply(keys, function(k) sprintf("%s\np = %s; q = %s", an$ci(k), fa(paste0(k, "_p")), fa(paste0(k, "_q"))), "")
  st <- s30ck_strip(an, keys, row_text, S30CK_RS_X)

  # free(): the animals' y axis and the strip's row labels do not share one left gutter.
  design <- c(patchwork::area(1, 1, 1, 2), patchwork::area(2, 1), patchwork::area(2, 2))
  # heights: the animal panel is longer than its two-line y title (S30_COOKIE_RS_HEIGHTS)
  p <- patchwork::wrap_plots(patchwork::free(top), st$forest, st$text, design = design,
                             heights = S30_COOKIE_RS_HEIGHTS, widths = S30_COOKIE_RS_WIDTHS) +
    patchwork::plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0)))
  bh_panel(p, w_mm, h_mm)
}

# =============================================================== c: response vs later CombZ
#' Response vs later CombZ: S3 animals (x = dcookie60, y = CombZ; neutral fill, no group
#' colour), by sex, with the stored S3d segment (x_min, y_at_x_min) -> (x_max, y_at_x_max):
#' the frozen COOKIE-CONT slope through the sex's centroid. Per facet the printed slope and
#' CI (S3c); caption what the line is and the Female - male slope-difference p
#' (COOKIE-CONT-DC60-INT). Header and footer as panel a (one-line subtitle, one-line x title,
#' two caption lines), so the two facetted panels line up.
#' Keys: ckcz_{f,m}{,_lo,_hi}, ckcz_int_p.
s30_panel_cookie_combz <- function(tab, an, w_mm = S30_COOKIE_BOX$combz[1], h_mm = S30_COOKIE_BOX$combz[2]) {
  fa <- an$fa
  S3 <- tab("S3_cookie_animals")
  S3d <- s30ck_rows(tab("S3d_cookie_cont_lines"), "Sex", S30CK_SEX, "S3d_cookie_cont_lines")
  # The drawn line and the printed slope must be the same frozen estimate.
  for (i in seq_along(S30CK_SEX)) {
    k <- c("ckcz_f", "ckcz_m")[i]
    if (!identical(S3d$slope[i], an$ann(k)) || !identical(S3d$slope_ci_low[i], an$ann(paste0(k, "_lo"))) ||
        !identical(S3d$slope_ci_high[i], an$ann(paste0(k, "_hi"))))
      s30ck_stop("panel c: the S3d line of ", S30CK_SEX[i], " is not the printed frozen slope.")
  }
  pts <- S3[is.finite(S3$dcookie60) & is.finite(S3$CombZ), c("AnimalNum", "Sex", "dcookie60", "CombZ")]
  pts$Sex <- factor(pts$Sex, levels = S30CK_SEX)
  S3d$Sex <- factor(S3d$Sex, levels = S30CK_SEX)
  S3d$label <- c(sprintf("slope = %s\n[%s, %s]", fa("ckcz_f"), fa("ckcz_f_lo"), fa("ckcz_f_hi")),
                 sprintf("slope = %s\n[%s, %s]", fa("ckcz_m"), fa("ckcz_m_lo"), fa("ckcz_m_hi")))

  p <- ggplot(pts, aes(dcookie60, CombZ)) +
    geom_point(size = 0.9, stroke = 0.2, alpha = 0.9, shape = 21, colour = "grey20", fill = S30CK_NEUTRAL_FILL) +
    geom_segment(data = S3d, aes(x = x_min, y = y_at_x_min, xend = x_max, yend = y_at_x_max), inherit.aes = FALSE,
                 colour = INK, linewidth = 0.45) +
    geom_text(data = S3d, aes(x = Inf, y = Inf, label = label), inherit.aes = FALSE, hjust = 1.04, vjust = 1.12,
              size = NOTE_PT / .pt, colour = INK, lineheight = 1.0) +
    facet_wrap(~ Sex, nrow = 1) +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0.03, 0.05)), labels = s30ck_ticks) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.30)), labels = s30ck_ticks) +
    labs(x = S30CK_DELTA_1, y = "Later CombZ", title = "Response vs later CombZ",
         subtitle = "registered exploratory linear model",
         caption = sprintf(S30CK_COMBZ_CAPTION, fa("ckcz_int_p"))) +
    s30ck_theme()
  bh_panel(p, w_mm, h_mm)
}
