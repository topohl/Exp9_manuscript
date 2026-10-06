#!/usr/bin/env Rscript

# Part-28: does every colour-encoded matrix in this layer represent its numbers
# faithfully?
#
# VISUAL-ENCODING AUDIT ONLY. No statistic is recomputed and no scale is
# changed here; this script measures what the released panels actually do.
#
# THE HARD RULE (Part-28 section 21). No heatmap may map two different numbers
# to the same endpoint colour without saying so. A panel fails if any plotted
# value lies outside its colour limits UNLESS the saturation is intentional,
# disclosed (on the colourbar, or for a panel without one by the value printed
# in every tile), and the uncapped values are still in the source data. This
# script hard-stops on a failure.
#
# WHAT "OUTSIDE THE LIMITS" MEANS HERE. Two kinds of colour limit exist since
# palette v3.2 (config/manuscript_palette.yml diverging_limits):
#  * a FIXED limit by measure, drawn by nv_diverging(measure = ...): values
#    beyond it are squished to full colour on purpose, the colourbar ends read
#    <= / >=, and the sidecar keeps the uncapped value. Such a panel is audited
#    against the fixed limit it actually draws, as intentional saturation.
#  * a panel's own limit (a declared display cap, or the data maximum):
#    nv_diverging(limits = ...) keeps the ggplot2 default oob = censor, so an
#    out-of-range value is NOT squished, it becomes NA and is painted na.value
#    (grey50). A silent clip there would show up as a grey tile, and the audit
#    checks both the numbers and the rendered SVG.

source(file.path("R", "paths.R"))
source(repo_path("R", "null_coalescing.R"))
source(repo_path("R", "integration_utils.R"))
source(repo_path("R", "nature_v2_figure_utils.R"))
source(repo_path("R", "final_truth_v9_figure_utils.R"))
Sys.setenv(PROTEOMICS_SCRIPT_ID = "figures/final_truth_v9_heatmap_scale_audit.R")

args <- commandArgs(trailingOnly = TRUE)
if ("--dry-run" %in% args || is_dry_run()) {
  message("[DRY-RUN] final_truth_v9 heatmap scale integrity audit")
  quit(save = "no", status = 0L)
}

OUT <- path_results("tables", "manuscript_candidates", "final_truth_v9", "audit")
REP <- path_results("reports", "manuscript_candidates", "final_truth_v9")
FIG <- path_results("figures", "manuscript_candidates", "final_truth_v9")
SD <- path_results("source_data", "manuscript_candidates", "final_truth_v9")
dir_create(OUT); dir_create(REP)

ct <- s9f_contract()
fig_of <- do.call(rbind, lapply(ct$figures, function(f) do.call(rbind, lapply(
  f$layout, function(l) data.frame(figure = as.character(f$name),
    figure_key = as.character(f$figure_key), panel_label = as.character(l$label),
    panel_id = as.character(l$panel), stringsAsFactors = FALSE)))))

# ------------------------------------------------- S20 the code search, first
#
# Every token the brief names, over the renderer files this layer actually uses,
# recorded whether or not it turns out to matter. A token that reaches no v9
# panel is still listed, so the search is auditable rather than asserted.
TOKENS <- c("geom_tile", "geom_raster", "Heatmap\\(", "scale_fill_gradient2",
            "scale_fill_gradient", "scale_fill_distiller", "colorRamp2",
            "nv_diverging", "limits *=", "oob *=", "squish", "scales::squish",
            "pmin\\(", "pmax\\(", "cut\\(", "winsor", "truncate", "\\bcap\\b",
            "clamp", "censor")
# recursive: the panel libraries live under R/panels
SRC <- list.files(repo_path("R"), pattern = "[.]R$", full.names = TRUE,
                  recursive = TRUE)
SRC <- c(SRC[grepl("final_truth_v9|nature_v2_figure_utils|nature_final_v7|figure3_adaptation",
                   basename(SRC))],
         list.files(repo_path("figures"), pattern = "^final_truth_v9.*[.]R$",
                    full.names = TRUE),
         # Part-29 section 31: the analysis directories can also draw a
         # publication NES matrix, and two of them did so on a 98th-percentile
         # colour limit that disagreed with the frozen atlas. The search must
         # reach them or the certification is scoped too narrowly to mean what
         # it says.
         list.files(repo_path("04_differential_expression_enrichment"),
                    pattern = "[.][Rr]$", full.names = TRUE),
         list.files(repo_path("10_biological_integration"),
                    pattern = "[.][Rr]$", full.names = TRUE))
code <- do.call(rbind, lapply(sort(unique(SRC)), function(f) {
  ln <- readLines(f, warn = FALSE)
  do.call(rbind, lapply(TOKENS, function(tk) {
    i <- grep(tk, ln, perl = TRUE)
    if (!length(i)) return(NULL)
    data.frame(file = sub(".*proteomics[/\\]", "", f), line = i, token = tk,
               code = trimws(substr(ln[i], 1, 160)),
               is_v9_renderer = grepl("final_truth_v9|figure3_adaptation",
                                      basename(f)),
               stringsAsFactors = FALSE)
  }))
}))
code$alters_displayed_value <- code$token %in%
  c("squish", "scales::squish", "pmin\\(", "pmax\\(", "winsor", "clamp",
    "truncate", "\\bcap\\b")
code <- code[order(code$file, code$line), , drop = FALSE]
write_csv_safe(code, file.path(OUT, "heatmap_scale_code_search.csv"))

# ------------------------------------------------- S20 the per-panel integrity
#
# Declared once, from the renderers, because a colourbar's name and the identity
# of a shared-scale group are editorial facts about the design and cannot be
# read out of a CSV.
#
# `measure` names a panel drawn on a FIXED limit by measure (palette v3.2,
# config/manuscript_palette.yml diverging_limits): its limit is read from the
# palette, saturation beyond it is intentional and disclosed by the colourbar's
# <= / >= ends, and the plotted column itself is the uncapped value (the scale
# squishes the colour, never the number). NA means the panel's own limit.
# `disclosure` says how saturation is disclosed: on the colourbar (its <= / >=
# ends), or by the value printed in every tile (the Figure 3 cards, which carry
# no colourbar).
H <- function(panel_id, quantity, value_col, limit_fun, shared_group,
              zero_meaningful, intentional_saturation, uncapped_col = NA,
              colourbar = NA, measure = NA, disclosure = "colourbar")
  data.frame(panel_id = panel_id, quantity = quantity, value_col = value_col,
             limit_rule = limit_fun, shared_scale_group = shared_group,
             zero_meaningful = zero_meaningful,
             intentional_saturation = intentional_saturation,
             uncapped_col = uncapped_col, colourbar_name = colourbar,
             measure = measure, disclosure = disclosure, stringsAsFactors = FALSE)

SPEC <- rbind(
  H("v9_fingerprint", "baseline abundance, CON z-score", "con_z",
    "fixed z limit (palette diverging_limits$z)", "none", TRUE, TRUE, "con_z",
    "Baseline abundance (CON z-score)", "z"),
  H("v9_compartment", "marker abundance, median centred log2",
    "displayed_value", "declared display cap from the source table", "none",
    TRUE, TRUE, "true_value", "unnamed"),
  H("v9_atlas", "median NES across a programme's GO terms", "median_NES",
    "fixed NES limit (palette diverging_limits$nes)", "atlas_NES", TRUE, TRUE,
    "median_NES", "Median NES (SUS - RES)", "nes"),
  H("v9_card_syn", "constituent-term NES in three contrasts", "NES",
    "fixed NES limit (palette diverging_limits$nes)", "card_NES", TRUE, TRUE,
    "NES", "none (every tile prints its NES)", "nes", "printed"),
  H("v9_card_rna", "constituent-term NES in three contrasts", "NES",
    "fixed NES limit (palette diverging_limits$nes)", "card_NES", TRUE, TRUE,
    "NES", "none (every tile prints its NES)", "nes", "printed"),
  H("v9_card_ox", "constituent-term NES in three contrasts", "NES",
    "fixed NES limit (palette diverging_limits$nes)", "card_NES", TRUE, TRUE,
    "NES", "none (every tile prints its NES)", "nes", "printed"),
  H("v9_ed_atlas_rescon", "median NES across a theme's GO terms", "median_NES",
    "fixed NES limit (palette diverging_limits$nes)", "atlas_NES", TRUE, TRUE,
    "median_NES", "Median normalised enrichment score", "nes"),
  H("v9_ed_atlas_suscon", "median NES across a theme's GO terms", "median_NES",
    "fixed NES limit (palette diverging_limits$nes)", "atlas_NES", TRUE, TRUE,
    "median_NES", "Median normalised enrichment score", "nes"),
  H("v9_ed_fingerprint_full", "signature score, mean of member CON z-scores",
    "score", "fixed set-mean z limit (palette diverging_limits$z_set_mean)",
    "none", TRUE, TRUE, "score", "Signature score (CON z-score)", "z_set_mean"),
  H("v9_ed_module_fingerprint", "mean module-member CON z-score", "mean_con_z",
    "max(abs(plotted)) symmetric", "none", TRUE, FALSE, NA,
    "Mean module-member abundance (CON z-score)"),
  H("v9_ed_wgcna_phenotype", "module eigengene group difference", "estimate",
    "max(abs(plotted)) symmetric", "none", TRUE, FALSE, NA,
    "Module eigengene difference"),
  H("v9_ed_similarity", "median CON profile correlation between two units",
    "median_similarity",
    "fixed correlation limit (palette diverging_limits$correlation), one global scale",
    "none", TRUE, TRUE, "median_similarity", "Median profile correlation (CON)",
    "correlation"))

num <- function(x) suppressWarnings(as.numeric(x))
sidecar <- function(pid) {
  k <- fig_of$figure_key[match(pid, fig_of$panel_id)]
  p <- file.path(SD, k, paste0(pid, "_source_data.csv"))
  if (file.exists(p)) nv_read_csv(p) else NULL
}
svg_path <- function(pid) {
  k <- fig_of$figure_key[match(pid, fig_of$panel_id)]
  file.path(FIG, k, "panels", paste0(pid, ".svg"))
}

rows <- do.call(rbind, lapply(seq_len(nrow(SPEC)), function(i) {
  pid <- SPEC$panel_id[i]
  d <- sidecar(pid)
  vc <- SPEC$value_col[i]
  if (is.null(d) || !vc %in% names(d))
    stop("heatmap audit: source data or value column missing for ", pid,
         call. = FALSE)
  v <- num(d[[vc]]); v <- v[is.finite(v)]
  uc <- SPEC$uncapped_col[i]
  tv <- if (!is.na(uc) && uc %in% names(d)) num(d[[uc]]) else v
  tv <- tv[is.finite(tv)]
  fixed <- !is.na(SPEC$measure[i])
  # The audit measures what a panel DRAWS. A fixed-limit panel draws the
  # palette's limit for its measure - unless its sidecar records another limit:
  # then it was drawn before palette v3.2 and not re-rendered since (the ED6
  # atlases and strips need upstream inputs this repository does not import), so
  # it still carries its old data-maximum limit and ggplot's censor.
  pal_lim <- if (fixed) nv_diverging_limit(SPEC$measure[i]) else NA_real_
  rec <- intersect(c("colour_limit", "shared_NES_scale_limit",
                     "shared_NES_strip_limit"), names(d))[1]
  rec_lim <- if (!is.na(rec)) num(d[[rec]][1]) else NA_real_
  stale <- fixed && is.finite(rec_lim) && abs(rec_lim - pal_lim) > 1e-9
  drawn_fixed <- fixed && !stale
  lim <- if (drawn_fixed) pal_lim else if (stale) rec_lim else max(abs(v))
  # a shared-scale group of own-limit panels takes the group's limit, not this
  # panel's own (a fixed-limit group already shares the palette's limit)
  if (!fixed && SPEC$shared_scale_group[i] != "none") {
    grp <- SPEC$panel_id[SPEC$shared_scale_group ==
                           SPEC$shared_scale_group[i]]
    lim <- max(vapply(grp, function(q) {
      dd <- sidecar(q)
      if (is.null(dd) || !vc %in% names(dd)) return(0)
      max(abs(num(dd[[vc]])), na.rm = TRUE)
    }, numeric(1)))
  }
  sv <- svg_path(pid)
  txt <- if (file.exists(sv))
    paste(readLines(sv, warn = FALSE, encoding = "UTF-8"), collapse = " ") else ""
  labs <- sub(".*>([^<]*)</text>", "\\1",
              unlist(regmatches(txt, gregexpr("<text[^>]*>[^<]*</text>", txt))))
  # a panel drawn at a fixed limit must show exactly that limit's end labels;
  # an own-limit panel discloses with any <= / >= mark (Figure 2e)
  on_bar <- if (drawn_fixed)
    all(nv_diverging_labels(pal_lim)[c(1, 5)] %in% labs) else
    any(grepl("≥|≤", labs))
  printed <- identical(SPEC$disclosure[i], "printed") &&
    all(sprintf("%.1f", v) %in% labs)
  data.frame(
    figure = fig_of$figure[match(pid, fig_of$panel_id)],
    panel = fig_of$panel_label[match(pid, fig_of$panel_id)],
    panel_id = pid, quantity = SPEC$quantity[i],
    raw_min = min(tv), raw_max = max(tv),
    colour_min = -lim, colour_max = lim,
    n_below_colour_min = sum(tv < -lim - 1e-9),
    n_above_colour_max = sum(tv > lim + 1e-9),
    oob_handling = if (drawn_fixed)
      "scales::squish at the fixed limit by measure: values beyond take full colour, the colourbar ends read <= / >=, the sidecar keeps the uncapped value" else
      "scales::censor (ggplot2 default); an out-of-range value would be painted grey50",
    shared_scale_group = SPEC$shared_scale_group[i],
    scale_symmetric = TRUE, zero_meaningful = SPEC$zero_meaningful[i],
    intentional_saturation = if (fixed) drawn_fixed else SPEC$intentional_saturation[i],
    disclosed_on_colourbar = on_bar,
    disclosed_by_printed_values = printed,
    disclosed_in_legend = any(grepl("saturat", labs, ignore.case = TRUE)),
    uncapped_source_values_present = !is.na(uc) && uc %in% names(d),
    grey50_pixels_in_svg = grepl("grey50|#7F7F7F|#808080", txt,
                                 ignore.case = TRUE),
    colourbar_name = SPEC$colourbar_name[i],
    limit_rule = SPEC$limit_rule[i],
    colour_limit_measure = SPEC$measure[i],
    colour_limit_source = if (drawn_fixed) "palette diverging_limits" else
      if (stale) paste0("sidecar ", rec, " (drawn before palette v3.2)") else
      "the panel's own limit",
    colour_limit_recorded = is.finite(rec_lim),
    drawn_before_palette_v3_2 = stale,
    stringsAsFactors = FALSE)
}))

# disclosed = on the colourbar, or by the value printed in every tile
rows$status <- with(rows, ifelse(
  n_below_colour_min + n_above_colour_max == 0L, "FAITHFUL",
  ifelse(intentional_saturation &
           (disclosed_on_colourbar | disclosed_by_printed_values) &
           uncapped_source_values_present, "DISCLOSED_SATURATION",
         "SILENT_CLIP")))
rows <- rows[order(rows$figure, rows$panel), , drop = FALSE]
write_csv_safe(rows, file.path(OUT, "heatmap_scale_integrity_audit.csv"))

cat("\n===== PART-28 HEATMAP SCALE INTEGRITY =====\n")
print(rows[, c("figure", "panel", "panel_id", "raw_min", "raw_max",
               "colour_min", "colour_max", "n_above_colour_max",
               "intentional_saturation", "disclosed_on_colourbar", "status")],
      row.names = FALSE)
cat("\ncode-search hits:", nrow(code), "| tokens that could alter a displayed value:",
    sum(code$alters_displayed_value), "| of those inside a v9 renderer:",
    sum(code$alters_displayed_value & code$is_v9_renderer), "\n")
cat("panels with a grey50 tile in the rendered SVG:",
    sum(rows$grey50_pixels_in_svg), "\n")

bad <- rows[rows$status == "SILENT_CLIP", , drop = FALSE]
if (nrow(bad)) {
  print(bad)
  stop("section 21: ", nrow(bad), " heatmap(s) clip values without disclosure",
       call. = FALSE)
}
cat("section 21 PASSES: no heatmap clips a value without disclosure\n")
cat("\nwritten to:", OUT, "\n")

# ------------------------------------------------------- S27 decisions, prose
fx <- rows[!is.na(rows$colour_limit_measure) & !rows$drawn_before_palette_v3_2, ,
           drop = FALSE]
old <- rows[rows$drawn_before_palette_v3_2, , drop = FALSE]
cap <- rows[rows$panel_id == "v9_compartment", , drop = FALSE]
md <- c(
"# Heatmap scale decisions (Part 28)",
"",
sprintf("%d colour-encoded matrix panels were audited. %d represent every value",
        nrow(rows), sum(rows$status == "FAITHFUL")),
sprintf("faithfully; %d saturate deliberately and disclose it. None clips silently.",
        sum(rows$status == "DISCLOSED_SATURATION")),
"",
"## Why a clip here would be visible rather than silent",
"",
"A panel on its own limit (a declared display cap or its data maximum) draws",
"through nv_diverging(limits = ...), which keeps the ggplot2 default",
"oob = scales::censor. An out-of-range value is therefore NOT squished to the",
"endpoint colour - it becomes NA and is painted grey50. The audit checks the",
sprintf("rendered SVGs for that colour as well as the numbers: %d panels contain",
        sum(rows$grey50_pixels_in_svg)),
"a grey50 tile.",
"",
"## Fixed colour limits by measure (palette v3.2)",
"",
"Since palette v3.2 (config/manuscript_palette.yml diverging_limits) every panel",
"of a measure draws one fixed limit, so a colour means the same value in every",
"figure: full colour at |z| 2 for a single-protein z-score, at 1 for a mean of",
"z-scores over a protein set, at |NES| 2 and at |r| 0.6. A value beyond the",
"limit is squished to full colour on purpose: the colourbar ends read <= / >=,",
"and the sidecar keeps the uncapped value. For the GO NES and z-score panels the",
"data maxima these limits replace left most cells pale; the correlation limit is",
"close to that panel's data maximum. The Figure 3 cards carry no colourbar: every",
"tile prints its NES, which is their disclosure.",
"",
"| panel | measure | limit | cells beyond the limit |",
"|---|---|---|---|")
for (i in seq_len(nrow(fx)))
  md <- c(md, sprintf("| %s %s (%s) | %s | +/-%g | %d of the cells (raw %.3f to %.3f) |",
                      fx$figure[i], fx$panel[i], fx$panel_id[i],
                      fx$colour_limit_measure[i], fx$colour_max[i],
                      fx$n_above_colour_max[i] + fx$n_below_colour_min[i],
                      fx$raw_min[i], fx$raw_max[i]))
if (nrow(old)) md <- c(md, "",
  "Drawn before palette v3.2 and not re-rendered since, so audited at the limit",
  "their sidecars record (the renderers now draw the fixed limit; these panels",
  "need upstream inputs that this repository records as provenance only):",
  "",
  sprintf("- %s %s (%s): drawn at +/-%.3f, %d of the cells beyond it", old$figure,
          old$panel, old$panel_id, old$colour_max,
          old$n_above_colour_max + old$n_below_colour_min))
md <- c(md, "",
"## The declared display cap (Figure 2e)",
"")
if (nrow(cap)) md <- c(md,
  sprintf("%s panel %s (%s) maps colour to +/-%.1f while the true values run",
          cap$figure[1], cap$panel[1], cap$panel_id[1], cap$colour_max[1]),
  sprintf("%.3f to %.3f. %d cells exceed the cap.", cap$raw_min[1], cap$raw_max[1],
          cap$n_above_colour_max[1]),
  "",
  "Retained, because three extreme cells would otherwise compress the colour",
  "discrimination of the whole remaining matrix. The saturation is disclosed",
  "four ways: the uncapped value is printed inside each saturated tile, the",
  "caption states the cap and the number of affected cells, the colourbar top",
  "tick reads >=3, and the sidecar carries true_value and colour_scale_censored",
  "alongside displayed_value.",
  "",
  "Only the upper tail saturates - the displayed minimum is above the lower",
  "limit - so only the top tick carries the >= mark. Marking both ends would",
  "imply an overflow bin at the bottom that does not exist.",
  "",
  "The colour limit is now read from the declared centered_log2_display_cap",
  "column rather than re-derived from the already-capped data, so the",
  "disclosure cannot quietly disappear if a future refresh leaves no cell",
  "sitting exactly at the cap.",
  "")
md <- c(md,
"## Shared scales",
"",
"| group | limit drawn by each panel | shared now | why shared |",
"|---|---|---|---|")
for (g in setdiff(unique(rows$shared_scale_group), "none")) {
  z <- rows[rows$shared_scale_group == g, ]
  md <- c(md, sprintf("| %s | %s | %s | same quantity, intended for direct comparison |",
                      g, paste(sprintf("%s +/-%.3f", z$panel_id, z$colour_max),
                               collapse = "; "),
                      if (length(unique(round(z$colour_max, 9))) == 1L) "yes" else
                        "NO - a member was drawn before palette v3.2"))
}
md <- c(md, "",
"Quantities that are NOT forced onto a common scale, correctly: NES against",
"log2FC, CON z-score against module eigengene difference, and profile",
"correlation against any of them. Each carries its own named colourbar.",
"",
"## The NES strip residual",
"",
"The three-cell NES strips in ED6 c-e and the Figure 3 e-g cards carry no colour",
"key; they print their NES in every tile. Until palette v3.2 they used a",
"different limit from the atlas on the same page (theme-summary +/-2.479, later",
"2.713, against single-term 2.761 and 3.0). The renderers now give strips, cards",
"and atlases the one fixed NES limit, and the atlas colourbar ends at that limit",
"(ticks -2, -1, 0, 1, 2, ends marked <= / >=), so no labelled tick understates",
"the endpoint.",
"")
md <- c(md, if (nrow(old))
  c("NOT YET RESOLVED in these outputs: the panels listed above as drawn before",
    "palette v3.2 keep their old limit until they are re-rendered, so Figure 3 and",
    "those panels do not yet share one NES scale.") else
  "Resolved in these outputs: every NES panel audited here draws the fixed limit.")
writeLines(md, file.path(REP, "heatmap_scale_decisions.md"))
cat("heatmap decisions doc written
")
