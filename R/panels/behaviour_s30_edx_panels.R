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
# f1o<1|2>_<design|combz|cc1|association|prediction> (option panels a-e).
#
# Plotted values: the stored TR_BY_SEX per-group model means +/- 95% CI (ebb C2), the stored
# CON descriptive means (ebb B2), and for the option panels exactly the Figure 1 marks.
#
# Requires: R/behaviour_bundle.R, R/panels/behaviour_figure_style.R and
# R/panels/behaviour_figure1_panels.R sourced first.

# ---------------------------------------------------------------- boxes (w x h, mm)
# ED X (CANDIDATE_SPEC C7): a/b either as two 85.5 x 58 panels (a = position-change rate,
# b = shared occupancy; recommended) or as one 173 x 58 panel ("ab"); c is 173 x 58.
EDX_BOX <- list(a = c(85.5, 58), b = c(85.5, 58), ab = c(173, 58), c = c(173, 58))
# Figure 1 options (CANDIDATE_SPEC C1/C2). Option 2's panel f (36 x 62) is the compact
# light-phase panel s30_panel_light_compact (R/panels/behaviour_s30_light_panels.R).
F1OPT_BOX <- list(
  figure_01_option1 = list(a = c(173, 32), b = c(58, 62), c = c(113, 62), d = c(66, 62), e = c(105, 62)),
  figure_01_option2 = list(a = c(173, 32), b = c(50, 62), c = c(88, 62), d = c(66, 62), e = c(105, 62)))
# Option panel letter -> the Figure 1 panel whose builder draws it.
F1OPT_SOURCE_PANEL <- c(a = "1a", b = "1b", c = "1c", d = "1e", e = "1f")
F1OPT_KEY_PREFIX <- c(figure_01_option1 = "f1o1_", figure_01_option2 = "f1o2_")

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
EDX_CAPTION_SPLIT <- c(con = "sex-stratified model estimates ± 95% CI; CON descriptive means (grey, dashed)",
                       no_con = "sex-stratified model estimates ± 95% CI (SIS; CON not shown)")
EDX_CAPTION_SECONDARY <- c(con = "Stage 29 secondary constructs, active phase after each cage change: sex-stratified model estimates ± 95% CI; CON descriptive means (grey, dashed)",
                           no_con = "Stage 29 secondary constructs, active phase after each cage change: sex-stratified model estimates ± 95% CI (SIS; CON not shown)")
edx_caption <- function(captions, show_con) unname(captions[[if (isTRUE(show_con)) "con" else "no_con"]])
# The Figure 1d-style overall titles, for callers that prefer a title over the caption.
EDX_TITLE_PRIMARY <- "Primary constructs, active phase after each cage change: sex-stratified model estimates ± 95% CI (CON descriptive means, grey)"
EDX_TITLE_SECONDARY <- "Secondary constructs, active phase after each cage change: sex-stratified model estimates ± 95% CI (CON descriptive means, grey)"
# Panel b of option 2 is 50 mm wide: the Figure 1b subtitle breaks after the semicolon.
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
#' Returns list(a, b, c, d, e) of bh_panel objects. Option 2 adds panel f from
#' s30_panel_light_compact (not built here).
#' Option 2 differs from option 1 only in the narrower b and c boxes, and in two typographic
#' adjustments those boxes need: the b subtitle breaks onto two lines, and c aligns its
#' construct titles and contrast captions to the plot edge (text_position = "plot").
f1opt_panels <- function(tab, an, option = c("figure_01_option1", "figure_01_option2"),
                         jitter_seed = list(b = 1L, c = c(2L, 2L)), minus_ticks = TRUE,
                         key_prefix = F1OPT_KEY_PREFIX[[option]], box = F1OPT_BOX[[option]]) {
  option <- match.arg(option)
  force(key_prefix); force(box)
  if (is.null(jitter_seed$b) || length(jitter_seed$c) < 1L || anyNA(c(jitter_seed$b, jitter_seed$c)))
    stop("f1opt_panels: pass explicit jitter seeds list(b = , c = c(, )).", call. = FALSE)
  pre <- edx_annotation_prefixed(an, key_prefix)
  narrow <- identical(option, "figure_01_option2")
  list(
    a = f1_panel_design(tab, pre, box$a[1], box$a[2]),
    b = f1_panel_combz(tab, pre, box$b[1], box$b[2], jitter_seed = jitter_seed$b,
                       subtitle = if (narrow) F1OPT_COMBZ_SUBTITLE_NARROW else F1_COMBZ_SUBTITLE, minus_ticks = minus_ticks),
    c = f1_panel_cc1(tab, pre, box$c[1], box$c[2], jitter_seed = jitter_seed$c,
                     text_position = if (narrow) "plot" else "panel"),
    d = f1_panel_association(tab, pre, box$d[1], box$d[2], minus_ticks = minus_ticks),
    e = f1_panel_prediction(tab, pre, box$e[1], box$e[2], minus_ticks = minus_ticks))
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
                      family = "P-TR", title = NULL, caption = caption, show_con = show_con)
}

#' ED X a/b as one panel (173 x 58): both primary constructs side by side (Figure 1d at the ED
#' height). Same keys as the split panels. Default header: the caption, as a/b/c; pass
#' title = EDX_TITLE_PRIMARY, caption = NULL for the Figure 1d-style overall title.
edx_panel_primary_trajectories <- function(tab, an, w_mm = EDX_BOX$ab[1], h_mm = EDX_BOX$ab[2], show_con = TRUE,
                                           title = NULL, caption = edx_caption(EDX_CAPTION_PRIMARY, show_con)) {
  f1_panel_trajectory(tab, an, w_mm, h_mm, constructs = EDX_PRIMARY, keys = EDX_TRAJECTORY_KEYS[EDX_PRIMARY],
                      family = "P-TR", title = title, caption = caption, show_con = show_con)
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
                      family = "S-TR-ORG", title = title, caption = caption, show_con = show_con)
}
