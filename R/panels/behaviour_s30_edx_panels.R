# Candidate generation behaviour_v101_s30: Extended Data X (ed_behaviour_longitudinal_light)
# panels a/b/c, and the Figure 1 option-1/2 panel sets (figure_01_option1, figure_01_option2).
# Rendering only: fits no model, computes no statistic, reads nothing itself.
#
# Every builder reuses a Figure 1 builder (R/panels/behaviour_figure1_panels.R) and takes
#   tab   the behaviour-bundle table getter, function(name) -> pinned ebb table;
#   an    an annotation resolver from bh_annotation() (R/panels/behaviour_figure_style.R)
#         on a map that declares the keys the builder prints;
#   w_mm, h_mm   the CANDIDATE_SPEC box (defaults below);
# and returns bh_panel(plot, w_mm, h_mm) (or a named list of them), ready for bh_save_svg().
#
# Annotation keys (figures/behaviour_v101_s30_annotation_map.csv, bundle ebb):
#   ED X    edx_<a|b|c>_<cr|sz|od|fr>_q2b_{df1,df2,f,holm}: the C3 Q2b joint test (TR_POOLED)
#           and its Holm p in E_multiplicity (P-TR for cr/sz, S-TR-ORG for od/fr).
#   Fig. 1 options  f1o1_<canonical key> and f1o2_<canonical key>: one row per number the
#           option panels print, copied from figures/figure_01_annotation_map.csv (same
#           table, column, filters and format) with the candidate panel id. The Figure 1
#           builders ask for the canonical key names; edx_annotation_prefixed() maps them onto
#           the candidate keys, so the option panels resolve through the candidate map and
#           figure_01_annotation_map.csv is left untouched.
# Map panel ids: trajectory_rate / trajectory_occupancy / trajectory_secondary (ED X a/b/c) and
# f1o<1|2>_<design|combz|cc1|association|prediction> (option 1 panels a-e; option 2 panels
# a, b, c, e, f, its d being the compact light-phase panel).
#
# Plotted values: the stored TR_BY_SEX per-group model means +/- 95% CI (ebb C2), the stored
# CON descriptive means (ebb B2), and for the option panels exactly the Figure 1 marks.
#
# Requires: R/behaviour_bundle.R, R/panels/behaviour_figure_style.R and
# R/panels/behaviour_figure1_panels.R sourced first.

# ---------------------------------------------------------------- boxes (w x h, mm)
# ED X (CANDIDATE_SPEC C7): a/b either as two 85.5 x 53 panels (a = position-change rate,
# b = shared occupancy; recommended) or as one 173 x 53 panel ("ab"); c is 173 x 53. The compact
# header (no patchwork outer margin) gives them the plot height the spec's 58 mm gave before.
EDX_BOX <- list(a = c(85.5, 53), b = c(85.5, 53), ab = c(173, 53), c = c(173, 53))
# Figure 1 options (figures/figure_behaviour_v101_s30_contract.yml). Option 2 letters its panels
# in reading order: its compact light-phase panel d (35 x 62, s30_panel_light_compact in
# R/panels/behaviour_s30_light_panels.R) sits in row 2, so association and prediction are e and f.
F1OPT_BOX <- list(
  figure_01_option1 = list(a = c(173, 29), b = c(58, 62), c = c(113, 62), d = c(66, 65), e = c(105, 65)),
  figure_01_option2 = list(a = c(173, 29), b = c(50, 62), c = c(84, 62), e = c(66, 65), f = c(105, 65)))
# Option panel letter -> the Figure 1 panel whose builder draws it.
F1OPT_SOURCE_PANEL <- list(figure_01_option1 = c(a = "1a", b = "1b", c = "1c", d = "1e", e = "1f"),
                           figure_01_option2 = c(a = "1a", b = "1b", c = "1c", e = "1e", f = "1f"))
F1OPT_KEY_PREFIX <- c(figure_01_option1 = "f1o1_", figure_01_option2 = "f1o2_")
# Candidate-only panel title of the association panel (Figure 1e has none).
F1OPT_ASSOCIATION_TITLE <- "Early position-change rate vs later CombZ"
# The prediction scatter and its permutation histogram take equal widths in the options (Figure 1f:
# 0.6 to 1), so the scatter is as tall as the histogram and the two x axes line up.
F1OPT_SCATTER_WIDTH <- 1
# Extra bottom margin (pt) of the prediction plots in the options (Figure 1f: 0): their one-line x
# titles end above the association panel's two-line one, so all three row-3 axes sit on one line.
F1OPT_PREDICTION_BOTTOM_PAD_PT <- 8.7
# One title anchor on the option pages: every title starts at the plot edge, under the letter.
F1OPT_TITLE_POSITION <- "plot"

# ---------------------------------------------------------------- annotation keys and labels
EDX_TRAJECTORY_KEYS <- c(crossing_rate = "edx_a_cr", shared_zone_use = "edx_b_sz",
                         occupancy_dispersion = "edx_c_od", fragmentation = "edx_c_fr")
EDX_PRIMARY <- c("crossing_rate", "shared_zone_use")
EDX_SECONDARY <- c("occupancy_dispersion", "fragmentation")
# Header structure shared by a, b and c: construct title, Q2b subtitle, collected legend, and a
# one-line caption that says what the marks are (the split a/b panels have no room for the
# Figure 1d overall title, and c follows them so the ED page reads as one grammar).
# Each caption has a with-CON and a without-CON form; the builders pick by show_con.
EDX_CAPTION_PRIMARY <- c(con = "Active phase after each cage change: sex-stratified model estimates ± 95% CI; CON descriptive means (grey, dashed)",
                         no_con = "Active phase after each cage change: sex-stratified model estimates ± 95% CI (SIS; CON not shown)")
# The split a/b captions name their tier, as c does (85.5-mm boxes: the shortest full wording).
EDX_CAPTION_SPLIT <- c(con = "Stage 29 primary: sex-stratified estimates ± 95% CI; CON descriptive (grey, dashed)",
                       no_con = "Stage 29 primary: sex-stratified estimates ± 95% CI (SIS; CON not shown)")
# c in the a/b wording (the window, the active phase after each cage change, is stated once in
# the ED X legend for a-c).
EDX_CAPTION_SECONDARY <- c(con = "Stage 29 secondary: sex-stratified estimates ± 95% CI; CON descriptive (grey, dashed)",
                           no_con = "Stage 29 secondary: sex-stratified estimates ± 95% CI (SIS; CON not shown)")
edx_caption <- function(captions, show_con) unname(captions[[if (isTRUE(show_con)) "con" else "no_con"]])
# One title anchor on the ED X page: a-c start their titles and legend at the plot edge, under the
# panel letter, as d (B2) and e (B4) do.
EDX_TITLE_POSITION <- "plot"
# The Figure 1d-style overall titles, for callers that prefer a title over the caption.
EDX_TITLE_PRIMARY <- "Primary constructs, active phase after each cage change: sex-stratified model estimates ± 95% CI (CON descriptive means, grey)"
EDX_TITLE_SECONDARY <- "Secondary constructs, active phase after each cage change: sex-stratified model estimates ± 95% CI (CON descriptive means, grey)"
# The Figure 1b subtitle broken after the semicolon, for a narrow panel-anchored box (contract arg
# subtitle_line_break); at the plot edge the one-line subtitle fits option 2's 50-mm b.
F1OPT_COMBZ_SUBTITLE_NARROW <- "dashed threshold defines RES/SUS;\nnot a test"

#' A resolver that answers the Figure 1 builders' key names from prefixed candidate keys.
#'
#' `an` is a bh_annotation() resolver on the candidate map; builder key k resolves as
#' paste0(prefix, k). Values, formats and the U+2212 minus are exactly an's.
edx_annotation_prefixed <- function(an, prefix) {
  if (!is.character(prefix) || length(prefix) != 1L) stop("edx_annotation_prefixed: prefix must be one string.", call. = FALSE)
  fa <- function(key) an$fa(paste0(prefix, key))
  list(ann = function(key) an$ann(paste0(prefix, key)),
       fa = fa,
       ci = function(k) sprintf("%s [%s, %s]", fa(k), fa(paste0(k, "_lo")), fa(paste0(k, "_hi"))),
       map = an$map,
       row = function(key) an$row(paste0(prefix, key)),
       resolved = an$resolved,
       prefix = prefix)
}

# ---------------------------------------------------------------- Figure 1 options 1 and 2
#' The five Figure 1 panels of a Figure 1 option (letters a-e), at the option's boxes.
#'
#' option       "figure_01_option1" or "figure_01_option2".
#' an           resolver on the candidate map (keys f1o1_* / f1o2_*). With key_prefix = ""
#'              it may instead be the Figure 1 resolver on figure_01_annotation_map.csv.
#' jitter_seed  explicit seeds, list(b = <one>, c = <two, one per construct>), so the output
#'              does not depend on build or print order.
#' minus_ticks  U+2212 in negative tick labels (CANDIDATE_SPEC A); Figure 1 itself keeps "-".
#' typography   "candidate" (default): CANDIDATE_SPEC A typography (en-dash ranges, U+2212 in
#'              the e-notation exponent, "RES − SUS" / "Female − male" labels); "figure1" keeps
#'              Figure 1's own strings.
#' compact_header  TRUE (default): c, the prediction panel and nothing else lose the patchwork
#'              outer margin, so their titles sit under the letter as b's and d's do.
#' Returns the five Figure 1 panels as bh_panel objects, named by the option's letters:
#' option 1 list(a, b, c, d, e); option 2 list(a, b, c, e, f), its compact light-phase panel d
#' coming from s30_panel_light_compact (not built here).
#' Both options put every title (and c its contrast captions and group legend) at the plot edge
#' (title_position / text_position = "plot"), as the compact panel d of option 2 does, and pad the
#' prediction plots at the bottom so the row-3 axes sit on one line (the figure contract's args).
#' Option 2 differs from option 1 only in the narrower b and c boxes.
f1opt_panels <- function(tab, an, option = c("figure_01_option1", "figure_01_option2"),
                         jitter_seed = list(b = 1L, c = c(2L, 2L)), minus_ticks = TRUE,
                         typography = "candidate", compact_header = TRUE,
                         key_prefix = F1OPT_KEY_PREFIX[[option]], box = F1OPT_BOX[[option]]) {
  option <- match.arg(option)
  force(key_prefix); force(box)
  if (is.null(jitter_seed$b) || length(jitter_seed$c) < 1L || anyNA(c(jitter_seed$b, jitter_seed$c)))
    stop("f1opt_panels: pass explicit jitter seeds list(b = , c = c(, )).", call. = FALSE)
  pre <- edx_annotation_prefixed(an, key_prefix)
  let <- names(F1OPT_SOURCE_PANEL[[option]])
  out <- list(
    f1_panel_design(tab, pre, box$a[1], box$a[2], typography = typography),
    f1_panel_combz(tab, pre, box$b[1], box$b[2], jitter_seed = jitter_seed$b,
                   subtitle = F1_COMBZ_SUBTITLE, minus_ticks = minus_ticks, title_position = F1OPT_TITLE_POSITION),
    f1_panel_cc1(tab, pre, box$c[1], box$c[2], jitter_seed = jitter_seed$c,
                 text_position = F1OPT_TITLE_POSITION, typography = typography, compact_header = compact_header),
    f1_panel_association(tab, pre, box[[let[4]]][1], box[[let[4]]][2], minus_ticks = minus_ticks,
                         typography = typography, title = F1OPT_ASSOCIATION_TITLE, title_position = F1OPT_TITLE_POSITION),
    f1_panel_prediction(tab, pre, box[[let[5]]][1], box[[let[5]]][2], minus_ticks = minus_ticks,
                        typography = typography, compact_header = compact_header, scatter_width = F1OPT_SCATTER_WIDTH,
                        bottom_pad_pt = F1OPT_PREDICTION_BOTTOM_PAD_PT, title_position = F1OPT_TITLE_POSITION))
  stats::setNames(out, let)
}

# ---------------------------------------------------------------- ED X a/b: primary constructs
#' ED X a or b: one primary construct's CC1-CC4 trajectories (85.5 x 58), Figure 1d grammar.
#'
#' construct  "crossing_rate" (panel a) or "shared_zone_use" (panel b).
#' Prints the construct's Q2b F(df1, df2) and its P-TR Holm p (keys edx_a_cr_* / edx_b_sz_*).
edx_panel_primary_trajectory <- function(tab, an, construct = c("crossing_rate", "shared_zone_use"),
                                         w_mm = EDX_BOX$a[1], h_mm = EDX_BOX$a[2], show_con = TRUE,
                                         caption = edx_caption(EDX_CAPTION_SPLIT, show_con)) {
  construct <- match.arg(construct)
  f1_panel_trajectory(tab, an, w_mm, h_mm, constructs = construct, keys = EDX_TRAJECTORY_KEYS[construct],
                      family = "P-TR", title = NULL, caption = caption, show_con = show_con, compact_header = TRUE,
                      title_position = EDX_TITLE_POSITION, group_shape = GROUP_CIRCLE)
}

#' ED X a/b as one panel (173 x 58): both primary constructs side by side (Figure 1d at the ED
#' height). Same keys as the split panels. Default header: the caption, as a/b/c; pass
#' title = EDX_TITLE_PRIMARY, caption = NULL for the Figure 1d-style overall title.
edx_panel_primary_trajectories <- function(tab, an, w_mm = EDX_BOX$ab[1], h_mm = EDX_BOX$ab[2], show_con = TRUE,
                                           title = NULL, caption = edx_caption(EDX_CAPTION_PRIMARY, show_con)) {
  f1_panel_trajectory(tab, an, w_mm, h_mm, constructs = EDX_PRIMARY, keys = EDX_TRAJECTORY_KEYS[EDX_PRIMARY],
                      family = "P-TR", title = title, caption = caption, show_con = show_con, compact_header = TRUE,
                      title_position = EDX_TITLE_POSITION, group_shape = GROUP_CIRCLE)
}

# ---------------------------------------------------------------- ED X c: secondary constructs
#' ED X c (173 x 58): occupancy dispersion and fragmentation, CC1-CC4, Stage 29 secondary.
#' Stored TR_BY_SEX means +/- 95% CI (C2), CON descriptive means (B2), and per construct the
#' Q2b F(df1, df2) (C3) with the S-TR-ORG Holm p (E_multiplicity); keys edx_c_od_* / edx_c_fr_*.
#' Default header: the caption (as a/b); title = EDX_TITLE_SECONDARY, caption = NULL gives
#' the Figure 1d-style overall title instead.
edx_panel_secondary_trajectories <- function(tab, an, w_mm = EDX_BOX$c[1], h_mm = EDX_BOX$c[2], show_con = TRUE,
                                             title = NULL, caption = edx_caption(EDX_CAPTION_SECONDARY, show_con)) {
  f1_panel_trajectory(tab, an, w_mm, h_mm, constructs = EDX_SECONDARY, keys = EDX_TRAJECTORY_KEYS[EDX_SECONDARY],
                      family = "S-TR-ORG", title = title, caption = caption, show_con = show_con, compact_header = TRUE,
                      title_position = EDX_TITLE_POSITION, group_shape = GROUP_CIRCLE)
}
