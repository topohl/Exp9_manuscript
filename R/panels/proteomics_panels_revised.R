# Editorial-v8 fidelity pass.
#
# This is NOT a redesign. Every renderer here fixes a factual, quantitative or
# print-size defect found by the Part-25 encoding audit, or re-encodes data the
# panel already carries. No analysis is reopened, no new statistic is computed,
# and no panel changes membership.
#
# The defects fixed here, with the measurement that established each:
#
#  1. NES STRIP COLOUR LIMIT. f9_gsea_curve and s4_gsea_curve each computed
#     lim <- max(abs(NES)) INSIDE one panel, so F3 d/e/f and ED6 c/d/e carried
#     five different NES-to-colour mappings - on pages whose atlases state
#     "colour scale is shared by all three contrast atlases". Measured: -1.716
#     rendered at 99.8% of the ramp in one panel while the larger -1.775
#     rendered at 64.3% in another, so the weaker effect was drawn darker.
#     f9_nes_strip_limit() computes ONE limit across all three programs
#     (palette v3.2: the manuscript's fixed NES limit, shared with the atlases).
#
#  2. ED8c TICK OVERPRINT. With a 31.6 mm plot width and a linear 0-1.05 axis,
#     the "0" tick sits at 0.0 mm and "0.05" at 1.5 mm while their combined
#     half-widths are 2.2 mm, so the two labels overprinted. The attainable
#     floor sat at 0.34% of the axis and was indistinguishable from the axis
#     line - the panel's own stated reading was the one fact it could not show.
#
#  3. ED7a PERCENTAGE AXIS. A proportion was drawn on an axis running to 122%,
#     widened only to fit the k/n labels, and all six values lay in 88.9-100%
#     so ~89% of every bar was ink common to all six rows.
#
#  4. F2e SILENT CENSORING. The plotted column is capped at 3: Ptbp2 (5.100),
#     Npm1 (4.171) and Anp32a (3.105) were all drawn as the identical darkest
#     red, and the sidecar carried only the capped 3.0, so the true values were
#     unrecoverable from the released source data.
#
#  5. F2b RANK SAWTOOTH. The jitter offset was (rank %% 5 - 2) * 0.07 applied
#     after sorting by value, so horizontal position was a deterministic
#     function of rank. Extracting all 323 circle positions from the rendered
#     SVG confirmed exactly 5 discrete lanes and a period-5 staircase in both
#     tails - visible diagonal structure with no data behind it.

# ==========================================================================
# One NES colour limit for every three-contrast strip in the family
# ==========================================================================
#
# F3 d/e/f and ED6 c/d/e draw the same nine numbers: three programs x three
# contrasts. They must therefore share one mapping. Palette v3.2: the mapping is
# the manuscript's fixed NES limit (config/manuscript_palette.yml
# diverging_limits$nes), the same one the atlases and the Figure 3 cards use,
# so one colour means one NES in every NES panel. A value beyond it takes full
# colour; every strip prints its NES, so nothing is hidden. `th` is unused and
# kept for the callers.
f9_nes_strip_limit <- function(th = NULL) hv_diverging_limit("nes")

# ==========================================================================
# ED6 c/d/e. The detailed GSEA curve, on the shared strip limit
# ==========================================================================
#
# Derived from the frozen s4_gsea_curve. Two changes only: the NES strip uses
# the shared fixed NES limit (f9_nes_strip_limit()) instead of a per-panel
# maximum, and the theme is hf_theme so every label clears the 5 pt floor.
#
# The curve itself is read from the frozen Figure 3 enrichment-curve export
# (source_data/pRoteomics/figure_03_adaptation), which carries the same three
# exemplars Figure 3h-j draw. It used to be rebuilt by s4_gsea_scores() from the
# analysis repository's clusterProfiler inputs (data/processed/...), which this
# repository does not hold and no contract declared. Before the switch
# (2026-10-07) the two agreed exactly for all three programs: running ES to
# 5e-16, member ranks, peak, ES, NES, FDR, set size and leading-edge count. The
# checks below are the ones s4_gsea_scores() made of its own reconstruction.
f9_frozen_curve <- function(prog, deps) {
  pick <- function(name) {
    p <- deps[basename(deps) == name]
    if (length(p) != 1L)
      stop("f9_ed_gsea_curve: the panel must declare ", name,
           " as an input dependency", call. = FALSE)
    hv_read_csv(repo_path(p))
  }
  sel <- pick("selection.csv")
  cur <- pick("running_enrichment_curves.csv")
  s <- sel[sel$GO_ID == prog$term & sel$dataset == prog$dataset &
             sel$spatial_unit == prog$unit, , drop = FALSE]
  z <- cur[cur$GO_ID == prog$term & cur$dataset == prog$dataset &
             cur$spatial_unit == prog$unit, , drop = FALSE]
  z <- z[order(z$rank), , drop = FALSE]
  if (nrow(s) != 1L || nrow(z) != s$n_ranked ||
      !all(z$rank == seq_len(nrow(z))) || sum(z$peak) != 1L ||
      which(z$peak) != s$peak_rank || sum(z$hit) != s$setSize)
    stop("f9_ed_gsea_curve: the frozen curve does not match its selection row for ",
         prog$term, call. = FALSE)
  if (!isTRUE(all.equal(z$running_ES[s$peak_rank], s$enrichmentScore,
                        tolerance = 1e-10)))
    stop("f9_ed_gsea_curve: the frozen curve does not peak at the stored ES for ",
         prog$term, call. = FALSE)
  N <- s$n_ranked
  hits <- as.logical(z$hit)
  lead <- if (s$enrichmentScore < 0) s$peak_rank:N else seq_len(s$peak_rank)
  list(runes = z$running_ES, hits = hits, peak = s$peak_rank,
       ES = s$enrichmentScore, NES = s$NES, FDR = s$BH_FDR, setSize = s$setSize,
       N = N, n_leading = sum(hits[lead]))
}

#' The curve header's statistics: NES and the FDR exponent with a true minus,
#' and the leading-edge count. E-notation, because a plotmath exponent falls
#' below the 5-pt floor and Arial has no superscript minus.
f9_curve_stat_label <- function(nes, fdr, n_leading = NULL, n_set = NULL) {
  e <- floor(log10(fdr)); m <- fdr / 10^e
  le <- if (is.null(n_leading)) "" else sprintf(", leading edge %d/%d", n_leading, n_set)
  gsub("-", "−", sprintf("NES %.2f, FDR %.1fe%d%s", nes, m, e, le), fixed = TRUE)
}

f9_ed_gsea_curve <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  pr <- s4_programs()
  prog <- pr[pr$key == as.character(panel$program_key), , drop = FALSE]
  if (!nrow(prog)) stop("f9_ed_gsea_curve: unknown program key", call. = FALSE)
  deps <- as.character(unlist(panel$input_dependencies))
  ev <- f9_frozen_curve(prog, deps)
  acc <- prog$accent[1]
  N <- ev$N
  curve <- data.frame(rank = seq_len(N), es = as.numeric(ev$runes))
  curve <- curve[unique(c(seq(1, N, by = 3), ev$peak, N)), ]
  ticks <- data.frame(rank = which(ev$hits))

  top <- ggplot2::ggplot(curve, ggplot2::aes(rank, es)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = hv_lw("reference_pt"),
                        colour = "grey75") +
    ggplot2::geom_line(colour = acc, linewidth = 0.45) +
    ggplot2::annotate("segment", x = ev$peak, xend = ev$peak, y = 0,
                      yend = ev$ES, linewidth = hv_lw("reference_pt"),
                      colour = "grey55", linetype = "22") +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(1, N),
                                breaks = c(1, N), labels = c("SUS", "RES")) +
    ggplot2::scale_y_continuous(labels = hv_minus_labels) +
    ggplot2::labs(x = NULL, y = "Enrichment score") +
    hf_theme() +
    ggplot2::theme(plot.margin = ggplot2::margin(1, 1, 0, 1, "mm"),
                   axis.title.y = ggplot2::element_text(size = hf_pt(5.4)))
  mid <- ggplot2::ggplot(ticks, ggplot2::aes(rank)) +
    ggplot2::geom_segment(ggplot2::aes(x = rank, xend = rank, y = 0, yend = 1),
                          colour = acc, linewidth = 0.12) +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(1, N)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL) +
    hf_theme() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(0, 1, 0, 1, "mm"))

  th <- hv_read_csv(repo_path(deps[1]))
  tr <- th[th$dataset == prog$dataset[1] & th$spatial_unit == prog$unit[1] &
             th$GO_ID == prog$term[1], , drop = FALSE]
  tr <- tr[match(c("RES - CON", "SUS - CON", "SUS - RES"), tr$contrast), ]
  tr <- tr[!is.na(tr$contrast), ]
  tr$short <- c("R−C", "S−C", "S−R")[seq_len(nrow(tr))]
  tr$short <- factor(tr$short, levels = tr$short)
  tr$sig <- is.finite(tr$GSEA_FDR) & tr$GSEA_FDR < 0.05
  lim <- f9_nes_strip_limit(th)

  # square tiles (house style)
  bot <- ggplot2::ggplot(tr, ggplot2::aes(short, 1, fill = NES)) +
    ggplot2::geom_tile(width = hv_tile(), height = hv_tile()) +
    ggplot2::geom_text(ggplot2::aes(label = hv_minus(sprintf("%.1f", NES))), family = fam,
                       size = hf_sz(5.0)) +
    ggplot2::geom_point(data = tr[tr$sig, , drop = FALSE],
                        ggplot2::aes(short, 1.44), size = 0.4, colour = "black",
                        inherit.aes = FALSE) +
    hv_diverging(measure = "nes", name = NULL, guide = "none") +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::coord_fixed(ratio = 1, ylim = c(0.5, 1.62)) +
    ggplot2::labs(x = NULL, y = NULL) +
    hf_theme_tile() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(size = HF_MIN_PT),
                   plot.margin = ggplot2::margin(0, 1, 0.5, 1, "mm"))

  hdr <- ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 1, hjust = 0, vjust = 1, family = fam,
                      size = hf_sz(5.4), fontface = "bold", colour = "black",   # ink, as Figure 3's curves; the accent stays in the marks
                      label = prog$label[1]) +
    ggplot2::annotate("text", x = 0, y = 0.25, hjust = 0, vjust = 1,
                      family = fam, size = hf_sz(5.0), colour = "grey40",
                      label = f9_curve_stat_label(ev$NES, ev$FDR, ev$n_leading,
                                                  sum(ev$hits))) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(-0.2, 1.05),
                             expand = FALSE) +
    ggplot2::theme_void(base_family = fam) +
    ggplot2::theme(plot.margin = ggplot2::margin(0.5, 1, 0, 1, "mm"))

  p <- patchwork::wrap_plots(hdr, top, mid, bot, ncol = 1,
                             heights = c(0.30, 1, 0.08, 0.22))
  out <- data.frame(
    program = prog$label[1], term_id = prog$term[1], dataset = prog$dataset[1],
    NES = ev$NES, FDR = ev$FDR, set_size = ev$setSize,
    leading_edge_n = ev$n_leading,
    curve_source = deps[basename(deps) == "running_enrichment_curves.csv"],
    RES_CON_NES = tr$NES[1], SUS_CON_NES = tr$NES[2], SUS_RES_NES = tr$NES[3],
    shared_NES_strip_limit = lim,
    shared_scale_note = paste0(
      "the three-contrast NES strip uses the manuscript's fixed NES colour ",
      "limit +/-", lim, " (config/manuscript_palette.yml diverging_limits), shared ",
      "by every GO-term NES panel, so the same colour means the same NES in every ",
      "strip; a value beyond it takes full colour and the tile prints its NES"),
    stringsAsFactors = FALSE)
  write_csv_safe(out, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# ==========================================================================
# ED8 c. The whole-network null, on a log axis
# ==========================================================================
#
# Defect 2. Chart type, marks and data are unchanged; only the scale changes.
# On the linear axis the attainable floor (0.0036) and the 0.05 rule were both
# pinned to the y axis and their tick labels overprinted. A log axis separates
# them and makes the panel's stated reading - that the enumeration had
# resolution far below 0.05 and still found nothing - directly readable.
f9_ed_nulls <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  n <- hv_read_csv(repo_path(panel$primary_source))
  z <- data.frame(dataset = n$dataset, p = n$exact_p,
                  floor = n$min_attainable_p, n_edges = n$n_edges,
                  stringsAsFactors = FALSE)
  z$lab <- hv_dataset_label(z$dataset)
  z$lab <- factor(z$lab, levels = hv_dataset_label(
    c("neuron_neuropil", "neuron_soma", "microglia")))
  lo <- 10^floor(log10(min(z$floor, na.rm = TRUE)))

  p <- ggplot2::ggplot(z, ggplot2::aes(p, lab)) +
    # a neutral reference line (palette v3: red would read as the SUS group)
    ggplot2::geom_vline(xintercept = 0.05, linetype = "22",
                        linewidth = hv_lw("reference_pt"), colour = "grey35") +
    ggplot2::geom_segment(ggplot2::aes(x = floor, xend = p, yend = lab),
                          colour = "grey85", linewidth = hv_lw("reference_pt")) +
    ggplot2::geom_point(ggplot2::aes(x = floor), colour = "grey60", size = 1,
                        shape = 1) +
    ggplot2::geom_point(size = 1.4, colour = "#1F3D52") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("p = %.2f", p)),
                       hjust = -0.3, family = fam, size = hf_sz(5.0)) +
    ggplot2::scale_x_log10(limits = c(lo, 22),
                           breaks = c(0.001, 0.01, 0.05, 1),
                           labels = c("0.001", "0.01", "0.05", "1")) +
    ggplot2::labs(x = "exact whole-network p",
                  y = NULL,
                  caption = paste0(
                    "Open circle = smallest
attainable p. Log axis.")) +
    hf_theme(grid = "x") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = HF_MIN_PT),
                   axis.text.y = ggplot2::element_text(size = HF_MIN_PT))
  z$reading <- paste0(
    "an informative null: the enumeration had resolution to ",
    sprintf("%.4f", min(z$floor, na.rm = TRUE)),
    " and found no whole-network group difference. The axis is log10 because ",
    "on a linear axis the attainable floor and the 0.05 rule both collapse ",
    "onto the y axis")
  write_csv_safe(z, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# ==========================================================================
# ED7 a. Counts, not percentages
# ==========================================================================
#
# Defect 3. The quantity is k of n with n between 7 and 37, so the honest
# encoding is the count: a bar running 0 -> n_hits with the "outside" portion
# filled and the remainder left open. Bar length now carries n (which differs
# five-fold across rows and was previously invisible), and the discriminating
# information is the open tail rather than the last 11% of six near-identical
# bars. The two subsets that are the SAME 15 proteins under two definitions are
# labelled as such instead of reading as independent agreement.
f9_ed_identity_subsets <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  c1 <- hv_read_csv(repo_path(panel$primary_source))
  c1$lab <- gsub("_", " ", c1$subset)
  # S23: "claimable" is pipeline language and must not reach a reader
  c1$lab <- sub("^claimable across datasets$",
                "robustness-qualified across datasets", c1$lab)
  c1$primary <- c1$subset == "CA2_SLM_robustness_qualified"
  c1$pct <- 100 * c1$fraction_outside_baseline_affinity

  # rows that are the identical protein set under two subset definitions
  key <- paste(c1$n_hits, c1$effect_outside_baseline_affinity, c1$n_CA2_SLM)
  dup <- key %in% key[duplicated(key)]
  c1$lab[dup] <- paste0(c1$lab[dup], " †")
  c1$lab <- factor(c1$lab, levels = rev(c1$lab))

  p <- ggplot2::ggplot(c1, ggplot2::aes(y = lab)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = n_hits, yend = lab),
                          colour = "grey82", linewidth = 1.9,
                          lineend = "butt") +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = effect_outside_baseline_affinity,
                                       yend = lab, colour = primary),
                          linewidth = 1.9, lineend = "butt") +
    ggplot2::geom_text(ggplot2::aes(x = n_hits,
                                    label = sprintf("%d/%d",
                                                    effect_outside_baseline_affinity,
                                                    n_hits)),
                       hjust = -0.22, family = fam, size = hf_sz(5.0),
                       colour = "grey25") +
    ggplot2::scale_colour_manual(values = c("TRUE" = "#1F3D52",
                                            "FALSE" = "#7C8A93"),
                                 guide = "none") +
    ggplot2::scale_x_continuous(limits = c(0, 42), expand = c(0, 0),
                                breaks = c(0, 10, 20, 30, 37)) +
    ggplot2::labs(
      x = "proteins whose strongest effect is outside the canonical control-animal affinity set (of n)",
      y = NULL,
      caption = paste0(
        "Bar length is n, the size of the subset; the filled portion is the ",
        "count outside the control-animal affinity set. † marks two rows ",
        "that ",
        "are the SAME 15 proteins under two subset definitions.")) +
    hf_theme(grid = "x") +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = HF_MIN_PT),
                   axis.text.x = ggplot2::element_text(size = HF_MIN_PT))
  c1$note <- paste0(
    "the result survives every robustness restriction; bar length encodes the ",
    "subset size n, which ranges from 7 to 37 and was not visible when the ",
    "panel plotted percentages on a 0-122 axis")
  write_csv_safe(c1, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# ==========================================================================
# F2 b. Proteome depth per acquisition
# ==========================================================================
f9_depth_compact <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  # PB-02. This panel used to read the tidy CSV that the manuscript_figures_v2
  # entry point writes into results/source_data/manuscript/figure_02/. That made
  # a candidate figure depend on the output of the generation it was meant to
  # replace: retiring v2 would have removed the only producer of its input. The
  # acquisition workbook is the canonical stage input and is what v2 itself
  # declares as the primary source, so this reads it directly and no
  # intermediate is needed. Every column this renderer uses - celltype_layer,
  # Proteins.Identified and exclude - is present in the workbook, and the
  # exclusion is applied here rather than inherited, so the plotted set is
  # unchanged.
  src <- repo_path(panel$primary_source)
  d <- if (grepl("[.]xlsx?$", src, ignore.case = TRUE)) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("f9_depth_compact: package 'readxl' is required to read ", src,
           call. = FALSE)
    }
    as.data.frame(readxl::read_excel(src), stringsAsFactors = FALSE)
  } else {
    hv_read_csv(src)
  }
  cn <- intersect(c("Proteins.Identified", "n_proteins", "proteins_identified"),
                  names(d))[1]
  if (is.na(cn)) stop("f9_depth_compact: no protein-count column", call. = FALSE)
  d$val <- as.numeric(d[[cn]])
  ccol <- intersect(c("celltype_layer", "qc_compartment", "dataset"), names(d))[1]
  if (is.na(ccol)) stop("f9_depth_compact: no compartment column", call. = FALSE)
  map <- c(neuron_neuropil = "neuron_neuropil", neuropil = "neuron_neuropil",
           neuron_soma = "neuron_soma", soma = "neuron_soma",
           microglia = "microglia")
  d$dataset <- unname(map[as.character(d[[ccol]])])
  if ("exclude" %in% names(d)) d <- d[!(d$exclude %in% TRUE), , drop = FALSE]
  d <- d[!is.na(d$dataset) & is.finite(d$val), , drop = FALSE]
  d$lab <- factor(sg_compartment_label(d$dataset),
                  levels = sg_compartments()$short)
  d <- d[!is.na(d$lab), , drop = FALSE]
  d <- d[order(d$lab, d$val), , drop = FALSE]

  # Part-26. DESCRIPTIVE violin, no test. The frozen renderer drew 323 raw
  # points whose horizontal position was a function of rank; the points are
  # kept but the distribution shape is now stated by a density outline, which
  # is what the panel is actually claiming. A violin is appropriate here only
  # because every group clears the small-sample threshold at which smoothed
  # densities become meaningless (n = 180 / 71 / 72 acquisitions), and the
  # neuropil distribution is multimodal, which a box plot would have hidden.
  #
  # The two n are NOT the same number and are labelled separately: the violin
  # summarises ACQUISITIONS, while the biological replicate is the animal
  # (9 animals, 3 per group). Nothing here is a between-group comparison.
  acq <- table(d$lab)
  n_animals <- if ("AnimalID" %in% names(d))
    length(unique(d$AnimalID[!is.na(d$AnimalID)])) else NA_integer_

  # 2026-10-28: drawn as in the hand-edited figure - solid violins in the
  # compartment colours with a white median and interquartile box, the n in
  # the panel; the acquisitions themselves are in the sidecar.
  q <- function(p) function(v) stats::quantile(v, p, names = FALSE)
  # two short lines fit the 26-mm box; the animals are stated in the legend
  ntxt <- paste0("n = ", paste(as.integer(acq), collapse = ", "), "\nacquisitions")
  p <- ggplot2::ggplot(d, ggplot2::aes(as.integer(lab), val)) +
    ggplot2::geom_violin(ggplot2::aes(group = lab, fill = lab), width = 0.62,
                         colour = NA, bw = "nrd0", trim = TRUE) +
    ggplot2::stat_summary(fun.min = q(0.25), fun.max = q(0.75), fun = stats::median,
                          geom = "crossbar", width = 0.2, linewidth = 0.22,
                          colour = "white", fill = NA) +
    # the n sits below the lowest acquisition, clear of the neuropil tail
    ggplot2::annotate("text", x = 3.62, y = min(d$val) - 0.13 * diff(range(d$val)), label = ntxt, hjust = 1,
                      vjust = 0, lineheight = 0.95, family = fam,
                      size = hf_sz(HF_MIN_PT), colour = "black") +
    ggplot2::scale_fill_manual(
      values = stats::setNames(
        unname(hv_dataset_colours()[sg_compartment_levels()]),
        sg_compartments()$short), guide = "none") +
    ggplot2::scale_x_continuous(breaks = 1:3, labels = sg_compartments()$short,
                                limits = c(0.5, 3.62)) +
    # thousands separated by a comma
    ggplot2::scale_y_continuous(labels = function(x) formatC(x, format = "d", big.mark = ","),
                                expand = ggplot2::expansion(mult = c(0.05, 0.04))) +
    ggplot2::labs(x = NULL, y = "Proteins identified") +
    hf_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = HF_MIN_PT, angle = 35,
                                          hjust = 1, colour = "black"),
      axis.text.y = ggplot2::element_text(size = HF_MIN_PT, colour = "black"))
  out <- d[, c("dataset", "val")]
  out$n_acquisitions_in_compartment <- as.integer(acq[as.character(d$lab)])
  out$n_biological_replicates <- n_animals
  out$encoding_note <- paste0(
    "descriptive violin of the acquisition-level distribution with a median and ",
    "interquartile box; every acquisition is listed here; the biological ",
    "replicate is the animal, not the acquisition, and no group comparison is ",
    "made in this panel")
  write_csv_safe(out, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# ==========================================================================
# F2 e. Compartment identity, with censoring made visible
# ==========================================================================
#
# Defect 4. The plotted column is capped for display, so the three strongest
# soma cells were drawn as the identical darkest red and the released sidecar
# carried only the capped value. The heatmap stays - the audit confirmed a dot
# plot would overlap in 6 of 10 rows - but censored tiles are now marked and
# BOTH the displayed and the true value are written to the sidecar.
f9_compartment <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  d <- hv_read_csv(repo_path(panel$primary_source))
  mcol <- intersect(c("marker_label", "marker_gene", "marker", "GeneSymbol",
                      "gene"), names(d))[1]
  ccol <- intersect(c("compartment", "dataset", "celltype_layer",
                      "qc_compartment"), names(d))[1]
  vcol <- intersect(c("displayed_centered_log2", "median_centered_log2",
                      "z", "value"), names(d))[1]
  if (any(is.na(c(mcol, ccol, vcol))))
    stop("f9_compartment: cannot locate marker/compartment/value columns",
         call. = FALSE)
  tcol <- intersect(c("median_centered_log2", "z", "value"), names(d))[1]
  map <- c(neuron_neuropil = "neuron_neuropil", neuropil = "neuron_neuropil",
           neuron_soma = "neuron_soma", soma = "neuron_soma",
           microglia = "microglia")
  d$ds <- unname(map[as.character(d[[ccol]])])
  d <- d[!is.na(d$ds), , drop = FALSE]
  d$marker <- as.character(d[[mcol]])
  d$val <- as.numeric(d[[vcol]])
  d$true_val <- if (!is.na(tcol)) as.numeric(d[[tcol]]) else d$val
  d$comp <- factor(sg_compartment_label(d$ds), levels = sg_compartments()$short)
  mk <- unique(d$marker)
  # markers across, compartments down (2026-10-20): square tiles fit the box;
  # a gap between the marker classes (2026-10-28), as in the hand-edited figure
  kcol <- intersect("marker_class", names(d))[1]
  mclass <- if (!is.na(kcol)) as.character(d[[kcol]])[match(mk, d$marker)] else rep("", length(mk))
  mx <- seq_along(mk) + 0.45 * (match(mclass, unique(mclass)) - 1)
  d$xpos <- mx[match(d$marker, mk)]
  ushort <- c("Soma markers" = "Soma", "Neuropil markers" = "Neuropil", "Microglia/PVM markers" = "Microglia/PVM")
  ch <- do.call(rbind, lapply(unique(mclass), function(k) {
    x <- mx[mclass == k]
    data.frame(x0 = min(x) - 0.44, x1 = max(x) + 0.44, xm = mean(range(x)),
               label = if (k %in% names(ushort)) ushort[[k]] else k)
  }))
  d$ypos <- nlevels(d$comp) + 1L - as.integer(d$comp)
  capcol <- intersect("centered_log2_display_cap", names(d))[1]
  cap <- if (!is.na(capcol)) max(abs(as.numeric(d[[capcol]])), na.rm = TRUE)
         else max(abs(d$val), na.rm = TRUE)
  lim <- cap * c(-1, 1)
  d$censored <- is.finite(d$true_val) & abs(d$true_val) > abs(lim[2]) + 1e-9
  cen <- d[d$censored, , drop = FALSE]

  p <- ggplot2::ggplot(d, ggplot2::aes(xpos, ypos)) +
    # hairline outline (house heatmap style): a value near zero is white, and must still read as a cell
    ggplot2::geom_tile(ggplot2::aes(fill = val), width = hv_tile(), height = hv_tile(),
                       colour = hv_tile_border(), linewidth = hv_lw("tile_border_pt")) +
    # only the tail that actually saturates is marked, so the colourbar
    # never implies an overflow bin that does not exist
    hv_diverging(limits = lim, name = "log2 vs\nprotein\nmedian", labels = function(b) {
      s <- hv_minus(format(b, trim = TRUE))
      hi <- any(is.finite(d$true_val) & d$true_val > lim[2] + 1e-9)
      lo <- any(is.finite(d$true_val) & d$true_val < lim[1] - 1e-9)
      ok <- which(is.finite(b))
      if (length(ok)) {
        if (hi) s[ok[which.max(b[ok])]] <-
          paste0("\u2265", s[ok[which.max(b[ok])]])
        if (lo) s[ok[which.min(b[ok])]] <-
          paste0("\u2264", s[ok[which.min(b[ok])]])
      }
      s
    }) +
    # limits on the coordinate system, not the scales, so the class headers above the tiles are kept
    ggplot2::scale_x_continuous(breaks = mx, labels = mk) +
    ggplot2::scale_y_continuous(breaks = seq_len(nlevels(d$comp)),
                                labels = rev(levels(d$comp))) +
    # marker-class headers over a thin rule, the classes of panel d (2026-10-28)
    ggplot2::annotate("segment", x = ch$x0, xend = ch$x1, y = nlevels(d$comp) + 0.75,
                      yend = nlevels(d$comp) + 0.75, linewidth = hv_lw("axis_pt"), colour = "black") +
    ggplot2::annotate("text", x = ch$xm, y = nlevels(d$comp) + 0.95, label = ch$label, family = fam,
                      size = hf_sz(HF_MIN_PT), colour = "black", vjust = 0) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, max(mx) + 0.5), ylim = c(0.5, nlevels(d$comp) + 0.5),
                         expand = FALSE, clip = "off") +
    ggplot2::guides(fill = ggplot2::guide_colourbar(display = "rectangles", nbin = 40)) +
    ggplot2::labs(x = NULL, y = NULL) +
    hf_theme_tile() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = HF_MIN_PT, face = "italic", colour = "black",
                                          angle = 45, hjust = 1, vjust = 1),
      axis.text.y = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
      legend.text = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
      legend.key.height = ggplot2::unit(3, "mm"),
      legend.key.width = ggplot2::unit(1.4, "mm"),
      plot.margin = ggplot2::margin(4, 1, 1, 1, "mm"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      plot.caption = ggplot2::element_text(size = HF_MIN_PT, colour = "grey30",
                                           hjust = 0))
  if (nrow(cen)) {
    # a censored tile is marked and its true value printed, so the colour ramp
    # never silently equates values it cannot separate. Dark ink: the
    # saturated ends are the palette's light yellow and mid blue (v3), where
    # white text would not read
    p <- p +
      ggplot2::geom_text(data = cen,
                         ggplot2::aes(xpos, ypos,
                                      label = sprintf("%.1f", true_val)),
                         family = fam, size = hf_sz(5.0), colour = "grey10")
  }
  out <- d[, c("marker", "ds", "comp", "val", "true_val", "censored")]
  names(out) <- c("marker", "dataset", "compartment", "displayed_value",
                  "true_value", "colour_scale_censored")
  write_csv_safe(out, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# ==========================================================================
# ED3 b. CA2-SLM missingness, paired by animal
# ==========================================================================
#
# Sorting 18 acquisitions by value destroyed the two structural facts the page
# depends on. (1) The hemisphere pairing: 755_L was rank 1 and 755_R rank 5, so
# the reader could not see that BOTH QC failures are one hemisphere of an animal
# whose other hemisphere is fine (755: 0.248 vs 0.111; 764: 0.160 vs 0.101) -
# which is exactly the structure panel e's hemisphere-dropping analysis acts on.
# (2) The 9 animals are the biological replicates at n = 3 per group, and group
# was relegated to a fill colour. Animals now sit in three labelled group blocks
# with their two hemispheres joined, so pairing and group are both positional.
f9_ed_ca2_missingness <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  s <- hv_read_csv(repo_path(panel$primary_source))
  s$AnimalID <- as.character(s$AnimalID)
  s$flag <- ifelse(s$qc_flag == "PASS", "", as.character(s$qc_flag))
  grp <- c("CON", "RES", "SUS")
  s$StressGroup <- factor(as.character(s$StressGroup), levels = grp)
  s <- s[order(s$StressGroup, s$AnimalID), , drop = FALSE]

  an <- unique(s[, c("AnimalID", "StressGroup")])
  an <- an[order(an$StressGroup, an$AnimalID), , drop = FALSE]
  # two blank slots separate the three group blocks
  an$slot <- seq_len(nrow(an)) + (as.integer(an$StressGroup) - 1L) * 1.0
  s$slot <- an$slot[match(s$AnimalID, an$AnimalID)]
  s$x <- s$slot + ifelse(s$hemisphere == "L", -0.19, 0.19)

  hdr <- do.call(rbind, lapply(split(an, an$StressGroup), function(z)
    data.frame(g = as.character(z$StressGroup[1]), mid = mean(z$slot),
               lo = min(z$slot) - 0.42, hi = max(z$slot) + 0.42,
               stringsAsFactors = FALSE)))
  ytop <- max(s$fraction_missing_preimputation) * 1.26
  gc <- hv_group_colours()

  p <- ggplot2::ggplot(s) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = fraction_missing_preimputation,
                                    group = AnimalID),
                       colour = "grey72", linewidth = 0.22) +
    ggplot2::geom_point(ggplot2::aes(x = x, y = fraction_missing_preimputation,
                                     colour = StressGroup,
                                     shape = hemisphere), size = 1.15,
                        stroke = 0.4) +
    ggplot2::geom_text(data = s[nzchar(s$flag), , drop = FALSE],
                       ggplot2::aes(x = x, y = fraction_missing_preimputation,
                                    label = flag),
                       vjust = -0.85, family = fam, size = hf_sz(5.0),
                       colour = "grey20") +
    ggplot2::geom_segment(data = hdr,
                          ggplot2::aes(x = lo, xend = hi, y = ytop, yend = ytop),
                          inherit.aes = FALSE, colour = "grey40",
                          linewidth = 0.3) +
    ggplot2::geom_text(data = hdr,
                       ggplot2::aes(x = mid, y = ytop * 1.06, label = g),
                       inherit.aes = FALSE, family = fam, size = hf_sz(5.4),
                       fontface = "bold", colour = "grey15") +
    ggplot2::scale_colour_manual(values = gc, guide = "none") +
    ggplot2::scale_shape_manual(values = c("L" = 1, "R" = 16), name = "Hemisphere") +
    ggplot2::scale_x_continuous(breaks = an$slot, labels = an$AnimalID,
                                expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::scale_y_continuous(limits = c(0, ytop * 1.13),
                                expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = "Fraction missing\nbefore imputation") +
    hf_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = HF_MIN_PT),
                   axis.text.y = ggplot2::element_text(size = HF_MIN_PT),
                   legend.position = "right",
                   legend.key.size = ggplot2::unit(2.6, "mm"))
  s$pairing_note <- paste0(
    "animals are the biological replicates (n = 3 per group); hemispheres are ",
    "paired within animal so the unilateral nature of both QC failures is ",
    "visible without reading the caption")
  write_csv_safe(s, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# ==========================================================================
# ED3 e. CA2-SLM sensitivity, as a paired dumbbell on one axis
# ==========================================================================
#
# The scatter put the same quantity on both axes and asked the reader to judge
# distance from the identity line, but the 175 x 54 mm box has no fixed aspect:
# measured from the rendered SVG the x scale was 32.62 mm per log2FC and the y
# scale 11.13 mm per log2FC, a 2.93x anisotropy, so the identity line was drawn
# at 18.8 degrees and the quantity the panel is about - how much effect is lost -
# was compressed threefold against the nuisance axis. One shared axis removes
# the anisotropy entirely and raises the resolution of the loss by ~38%. It also
# names all 28 proteins and blocks the 6 that qualify, so the panel that
# underwrites "28 canonical -> 6 qualified" finally identifies the survivors.
f9_ed_ca2_sensitivity <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  r <- hv_read_csv(repo_path(panel$primary_source))
  z <- data.frame(
    gene = r$gene_symbol, canonical = r$canonical_log2FC_SUS_minus_RES,
    hemi = r$effect_dropping_QC_failed_hemispheres,
    cls = r$CA2_SLM_robustness_class, stringsAsFactors = FALSE)
  z <- z[is.finite(z$canonical) & is.finite(z$hemi), , drop = FALSE]
  z$shrink <- abs(z$canonical) - abs(z$hemi)
  ORD <- c("robust_to_missingness_and_QC", "not_claimable_due_to_QC",
           "insufficient_observed_data")
  z$cls <- factor(z$cls, levels = ORD)
  z <- z[order(z$cls, -abs(z$canonical)), , drop = FALSE]
  n <- nrow(z)
  half <- ceiling(n / 2)
  z$col_id <- rep(c(1L, 2L), times = c(half, n - half))
  z$row <- c(seq_len(half), seq_len(n - half))
  z$ypos <- -z$row
  # gene names are each column's own axis labels, so no label can meet a segment
  z$gene_f <- factor(z$gene, levels = rev(z$gene))
  COL <- f9_qc_class_colours()
  # the class counts of all canonical CA2-SLM proteins, carried by the key (the
  # separate count panel was dropped)
  n_cls <- table(factor(r$CA2_SLM_robustness_class, levels = ORD))
  pts <- rbind(
    data.frame(x = z$canonical, gene_f = z$gene_f, cls = z$cls, col_id = z$col_id,
               which = "canonical"),
    data.frame(x = z$hemi, gene_f = z$gene_f, cls = z$cls, col_id = z$col_id,
               which = "QC-failed hemispheres removed"))

  p <- ggplot2::ggplot(z) +
    ggplot2::geom_vline(xintercept = 0, linewidth = hv_lw("reference_pt"),
                        colour = "grey78") +
    ggplot2::geom_segment(ggplot2::aes(x = canonical, xend = hemi, y = gene_f,
                                       yend = gene_f, colour = cls),
                          linewidth = 0.42, lineend = "round", alpha = 0.75) +
    ggplot2::geom_point(data = pts, ggplot2::aes(x = x, y = gene_f, colour = cls,
                                                 shape = which),
                        size = 1.05, fill = "white", stroke = 0.4) +
    ggplot2::scale_shape_manual(values = c("canonical" = 16,
                                           "QC-failed hemispheres removed" = 21),
                                name = NULL,
                                guide = ggplot2::guide_legend(order = 2,
                                  override.aes = list(colour = "grey30"))) +
    ggplot2::scale_colour_manual(values = COL, name = NULL,
                                 labels = function(k) sprintf("%s (%d)",
                                   f9_qc_class_label(k), as.integer(n_cls[k])),
                                 guide = ggplot2::guide_legend(order = 1)) +
    ggplot2::scale_x_continuous(limits = c(-3.0, 2.1), breaks = c(-2, -1, 0, 1, 2),
                                labels = hv_minus_labels, expand = c(0, 0)) +
    ggplot2::facet_wrap(~col_id, nrow = 1, scales = "free_y") +
    ggplot2::labs(x = "log2 fold change (SUS − RES)", y = NULL) +
    hf_theme(grid = "x") +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = HF_MIN_PT, face = "italic",
                                          colour = "grey20"),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = HF_MIN_PT),
      strip.text = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.spacing.y = ggplot2::unit(0, "mm"),
      legend.key.height = ggplot2::unit(1.8, "mm"),
      panel.spacing.x = ggplot2::unit(3.0, "mm"),
      # clear of the panel letter, which is drawn over the box's top-left corner
      plot.margin = ggplot2::margin(3, 1, 1, 1, "mm"))
  z$reading <- paste0(
    "an open circle to the right of its filled circle means the effect shrank ",
    "towards zero when the QC-failed hemispheres were dropped; both endpoints ",
    "are log2FC on one shared axis, so no aspect-ratio distortion is possible")
  write_csv_safe(z, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# ==========================================================================
# F2 d. The baseline spatial fingerprint, ordered by what it is about
# ==========================================================================
#
# Part-26. The centrepiece of Figure 2 - its largest panel at 175 x 50 mm -
# presented its 19 genes in ALPHABETICAL order (ADCY9, ANKRD63, ATP1A1,
# ATP8A1, CAMK1D, ...), which is a random permutation with respect to the
# spatial structure the panel exists to show. Rows are now seriated by each
# gene's baseline peak spatial unit, so the fingerprint reads as a block
# structure instead of having to be reconstructed cell by cell.
#
# The ordering is PHENOTYPE-BLIND by construction: the peak is taken from
# con_z, which is CON-only baseline abundance, and no stress contrast, group
# label or phenotype statistic enters the sort. Ties break on the gene symbol,
# so the order is deterministic.
f9_fingerprint <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  z <- hv_read_csv(repo_path(panel$primary_source))
  z$sg_unit <- sg_resolve_unit(z$spatial_unit, z$dataset)
  # laid out as Figure 3b: square tiles, compartments a gap apart, headers on top
  # (the column positions are fixed before the rows, so n_rows is set below)
  lay0 <- sg_atlas_layout(z$sg_unit, z$dataset, 0)
  z$xpos <- lay0$x_of(z$sg_unit, z$dataset)

  # each gene's own compartment, and its baseline peak unit within it
  gk <- unique(z[, c("gene", "contrast")])
  gk <- gk[order(gk$contrast, gk$gene), , drop = FALSE]
  gk <- gk[!duplicated(gk$gene), , drop = FALSE]
  pk <- do.call(rbind, lapply(split(z, z$gene), function(g) {
    g <- g[is.finite(g$con_z), , drop = FALSE]
    if (!nrow(g)) return(NULL)
    i <- which.max(g$con_z)
    data.frame(gene = g$gene[1], peak_xpos = g$xpos[i],
               peak_unit = g$sg_unit[i], peak_dataset = g$dataset[i],
               peak_con_z = g$con_z[i], stringsAsFactors = FALSE)
  }))
  ord <- merge(gk, pk, by = "gene", all.x = TRUE, sort = FALSE)
  ord <- ord[order(ord$peak_xpos, ord$gene), , drop = FALSE]
  lev <- ord$gene
  z$ypos <- match(z$gene, rev(lev))
  ny <- length(lev)
  lay <- sg_atlas_layout(z$sg_unit, z$dataset, ny, vertical_units = TRUE)

  # the manuscript's fixed z limit (palette v3.2, two SD): a single-protein z
  # beyond it takes full colour, and the sidecar keeps the uncapped con_z
  p <- ggplot2::ggplot(z, ggplot2::aes(xpos, ypos)) +
    ggplot2::geom_tile(ggplot2::aes(fill = con_z), width = hv_tile(), height = hv_tile()) +
    hv_diverging(measure = "z", name = "Baseline abundance\n(CON z-score)") +
    sg_atlas_headers(lay, fam, hf_sz(5.0), hf_sz(5.4)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = seq_len(ny), labels = rev(lev), expand = c(0, 0)) +
    ggplot2::coord_fixed(ratio = 1, xlim = lay$xlim, ylim = lay$ylim, clip = "off") +
    ggplot2::labs(x = NULL, y = NULL) +
    hf_theme_tile() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = HF_MIN_PT, face = "italic"),
      legend.title = ggplot2::element_text(size = hf_pt(5.4)),
      legend.position = "right",
      legend.key.width = ggplot2::unit(1.6, "mm"),
      legend.key.height = ggplot2::unit(3.6, "mm"))
  out <- merge(z, pk[, c("gene", "peak_unit", "peak_dataset")], by = "gene",
               all.x = TRUE, sort = FALSE)
  out <- out[order(-out$ypos, out$xpos), , drop = FALSE]
  # the peak is a GENE-level attribute over the whole fingerprint, so its
  # compartment is gene_peak_dataset and not the row dataset; naming them
  # explicitly stops a reader joining row dataset to peak unit
  names(out)[names(out) == "peak_unit"] <- "gene_peak_unit"
  names(out)[names(out) == "peak_dataset"] <- "gene_peak_dataset"
  out$row_order_rule <- paste0(
    "rows are seriated by baseline peak spatial unit, taken from CON-only ",
    "con_z; no phenotype information enters the ordering. Ties break on gene ",
    "symbol, so the order is deterministic")
  out$colour_limit <- hv_diverging_limit("z")   # the limit the tiles draw
  write_csv_safe(out, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# ==========================================================================
# Publication terminology for the CA2-SLM robustness classes
# ==========================================================================
#
# Part-25 section 17. The stored classification KEY is unchanged - renaming it
# would break every downstream join - but "not claimable due to QC" is pipeline
# language and must not reach a reader. These are the publication-facing forms,
# used in the artwork, the legends and the statistics tables alike.
f9_qc_class_labels <- c(
  robust_to_missingness_and_QC = "robustness-qualified",
  not_claimable_due_to_QC      = "QC-sensitive",
  insufficient_observed_data   = "insufficient observed data",
  not_in_CA2_SLM_never_at_risk = "not in CA2-SLM; never at risk")

f9_qc_class_label <- function(x) {
  x <- as.character(x)
  out <- unname(f9_qc_class_labels[x])
  out[is.na(out)] <- gsub("_", " ", x[is.na(out)])
  out
}

# The colours of the robustness classes, one source for ED3 d and e: the
# palette's claimability family (robust = claimable, QC-sensitive = not
# claimable) and a mid neutral grey for insufficient data. Palette v3: the old
# vermillion read as the SUS red and the old light warm grey as the RES grey.
f9_qc_class_colours <- function() {
  cl <- hv_claim_colours()
  c(robust_to_missingness_and_QC = unname(cl[["claimable"]]),
    not_claimable_due_to_QC = unname(cl[["not_claimable"]]),
    insufficient_observed_data = "grey60")
}

# ==========================================================================
# ED1 b. Bilateral precision gain, with a key
# ==========================================================================
#
# Part-25 section 15. The frozen renderer drew a pale endpoint and a dark
# endpoint with no key anywhere on the panel, so which end was one hemisphere
# and which was the bilateral mean had to be reverse-engineered from the axis
# title. The marks and the data are unchanged; a two-item key is added, and the
# median intraclass correlations are stated as descriptive precision context.
# No significance test is applied here and none is implied.
f9_ed_precision <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  d <- hv_read_csv(repo_path(panel$primary_source))
  n_all <- nrow(d)
  d <- d[is.finite(d$ICC_single_side) & is.finite(d$ICC_bilateral_mean), ,
         drop = FALSE]
  # an endpoint is missing only when its variance components were singular or
  # the fit did not converge, which is decided upstream. The exclusion is
  # principled, but a reader counting lines could not know it happened.
  n_drop <- n_all - nrow(d)
  n_drop_note <- if (n_drop > 0L) sprintf(paste0(
    "%d of %d endpoints are not drawn: their variance components were ",
    "singular or the fit did not converge.\n"), n_drop, n_all) else ""
  lab <- c(wgcna_module_eigengene = "WGCNA modules",
           reference_marker_score = "Reference markers",
           empirical_compartment_score = "Compartment scores")
  d$cls <- unname(lab[d$endpoint_class])
  d$cls <- factor(d$cls, levels = rev(unname(lab)))
  d$ymid <- as.numeric(d$cls)
  d <- d[order(d$cls, d$endpoint_id, d$dataset), , drop = FALSE]
  d$yoff <- unlist(lapply(split(seq_len(nrow(d)), d$cls), function(ix) {
    n <- length(ix); if (n == 1L) 0 else seq(-0.3, 0.3, length.out = n)
  }), use.names = FALSE)
  d$ypos <- d$ymid + d$yoff

  sm <- do.call(rbind, lapply(split(d, d$cls), function(z) data.frame(
    cls = z$cls[1], ymid = z$ymid[1], n = nrow(z),
    single = stats::median(z$ICC_single_side),
    bilateral = stats::median(z$ICC_bilateral_mean), stringsAsFactors = FALSE)))
  SINGLE <- unname(hv_evidence_colours()["descriptive"])
  BOTH <- unname(hv_evidence_colours()["supported"])
  ytop <- nrow(sm) + 0.92

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = d,
      ggplot2::aes(x = ICC_single_side, xend = ICC_bilateral_mean,
                   y = ypos, yend = ypos),
      colour = "grey88", linewidth = hv_lw("reference_pt")) +
    ggplot2::geom_segment(data = sm,
      ggplot2::aes(x = single, xend = bilateral, y = ymid, yend = ymid),
      colour = "grey45", linewidth = hv_lw("data_pt") * 1.6,
      arrow = grid::arrow(length = ggplot2::unit(1.1, "mm"), type = "closed")) +
    ggplot2::geom_point(data = sm, ggplot2::aes(single, ymid), shape = 16,
                        colour = SINGLE, size = 1.5) +
    ggplot2::geom_point(data = sm, ggplot2::aes(bilateral, ymid), shape = 16,
                        colour = BOTH, size = 1.5) +
    # the key, drawn in data space so it cannot drift from the marks; its two
    # entries are stacked so that a narrow panel holds them
    ggplot2::annotate("point", x = 0.04, y = ytop, shape = 16,
                      colour = SINGLE, size = 1.5) +
    ggplot2::annotate("text", x = 0.075, y = ytop, hjust = 0, family = fam,
                      size = hf_sz(5.0), colour = "grey30",
                      label = "one hemisphere") +
    ggplot2::annotate("point", x = 0.04, y = ytop - 0.22, shape = 16,
                      colour = BOTH, size = 1.5) +
    ggplot2::annotate("text", x = 0.075, y = ytop - 0.22, hjust = 0, family = fam,
                      size = hf_sz(5.0), colour = "grey30",
                      label = "bilateral mean") +
    ggplot2::scale_y_continuous(breaks = sm$ymid, labels = as.character(sm$cls),
                                limits = c(0.4, ytop + 0.3)) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1),
                                expand = c(0, 0)) +
    ggplot2::labs(x = "ICC:  one hemisphere → bilateral mean", y = NULL) +
    hf_theme(grid = "x") +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = HF_MIN_PT),
                   axis.text.x = ggplot2::element_text(size = HF_MIN_PT),
                   # room for the last tick label, "1.0", at the axis end
                   plot.margin = ggplot2::margin(1, 3, 1, 1, "mm"))
  out <- d[, c("endpoint_class", "endpoint_id", "dataset", "ICC_single_side",
               "ICC_bilateral_mean")]
  out$medians <- paste0(
    "median ICC ", paste(sprintf("%.2f", sm$single), collapse = " / "), " -> ",
    paste(sprintf("%.2f", sm$bilateral), collapse = " / "), " for ",
    paste(as.character(sm$cls), collapse = " / "), "; ", n_drop_note)
  out$reading <- paste0(
    "light symbol = one hemisphere, dark symbol = bilateral mean; the panel ",
    "is descriptive reliability context and carries no hypothesis test")
  write_csv_safe(out, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# ==========================================================================
# ED8 d. Edge-behaviour coupling, stated as a limited-power null
# ==========================================================================
#
# Part-25 section 25. Derived from the frozen s5_ed_coupling with only the
# reader-facing wording changed: "claimable" is pipeline language, and a
# negative multiple-testing result must be stated as one. With 9 animals a
# single correlation has very little power, so the panel now says that
# explicitly rather than letting the null read as evidence of absence.
f9_ed_coupling <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  x <- hv_read_csv(repo_path(panel$primary_source))
  fam <- hv_palette()$typography$family
  keep <- c("Edge", "Outcome", "Change", "window", "estimate",
            "conf.low", "conf.high", "p.value", "n",
            "p.adj_BH_within_outcome", "p.adj_BH_all_edge_phenotype_tests")
  # The stored BH family has 96 rows keyed on (Analysis, Phase, Change, window,
  # Edge, Outcome), but Phase is structurally inapplicable to the three
  # per-animal outcomes (composite z, corticosterone change, sucrose
  # preference): those are not phase-resolved, so Active and Inactive carry the
  # identical test. Only Movement AUC is genuinely phase-resolved. Collapsing on
  # the statistical content keeps every distinct test and drops only the
  # duplicated ones. Note this means BH was applied over 96 slots for 48 tests,
  # which is conservative and so cannot manufacture the null reported here.
  # Movement AUC is the one outcome with three distinct tests per edge (dark
  # phase, light phase, and dark phase over the first change only); they are
  # told apart by symbol, with a key
  x$test <- ifelse(x$Outcome != "Movement_AUC_z_vs_CON", "one test",
                   ifelse(x$Analysis == "firstChangeActive", "dark phase, first change",
                          ifelse(x$Phase == "Active", "dark phase", "light phase")))
  u <- unique(x[, c(keep, "test")])
  dupkey <- paste(x$Edge, x$Outcome, x$estimate, x$p.value, sep = "\r")
  u$n_stored_rows <- as.integer(table(dupkey)[
    paste(u$Edge, u$Outcome, u$estimate, u$p.value, sep = "\r")])
  u$excludes_zero <- u$conf.low > 0 | u$conf.high < 0
  u$edge_lab <- sub(" - ", "–", toupper(gsub("_", " ", u$Edge)), fixed = TRUE)
  u$out_lab <- factor(c(CombZ = "Integrated resilience",
                        delta_cort = "Corticosterone change",
                        Movement_AUC_z_vs_CON = "Movement AUC vs CON",
                        sucrose_pref = "Sucrose preference")[u$Outcome],
                      levels = c("Integrated resilience", "Corticosterone change",
                                 "Sucrose preference", "Movement AUC vs CON"))
  # deterministic within-cell ordering so repeated builds are byte-identical
  u <- u[order(u$out_lab, u$edge_lab, u$test, u$window, u$Change, u$estimate), ]
  u$row <- stats::ave(seq_len(nrow(u)),
                      paste(u$out_lab, u$edge_lab), FUN = seq_along)
  u$ypos <- as.integer(factor(u$edge_lab, levels = rev(sort(unique(u$edge_lab))))) +
    (u$row - mean(unique(u$row))) * 0.22
  p <- ggplot2::ggplot(u, ggplot2::aes(estimate, ypos)) +
    ggplot2::geom_vline(xintercept = 0, linewidth = hv_lw("reference_pt"),
                        colour = "grey55") +
    ggplot2::geom_segment(ggplot2::aes(x = conf.low, xend = conf.high,
                                       yend = ypos, colour = excludes_zero),
                          linewidth = hv_lw("reference_pt")) +
    ggplot2::geom_point(ggplot2::aes(colour = excludes_zero, shape = test), size = 0.8) +
    ggplot2::scale_shape_manual(values = c("one test" = 16, "dark phase" = 16,
                                           "light phase" = 17,
                                           "dark phase, first change" = 15),
                                breaks = c("dark phase", "light phase",
                                           "dark phase, first change"),
                                name = "Movement AUC:",
                                guide = ggplot2::guide_legend(
                                  override.aes = list(colour = "grey40", size = 1.2))) +
    # an interval excluding zero in the palette's 'supported' ink, the rest grey
    # (palette v3: the old red would read as the SUS group)
    ggplot2::scale_colour_manual(values = c("TRUE" = unname(hv_evidence_colours()[["supported"]]),
                                            "FALSE" = "grey60"),
                                 guide = "none") +
    ggplot2::scale_y_continuous(
      breaks = seq_along(unique(u$edge_lab)),
      labels = rev(sort(unique(u$edge_lab))), expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::scale_x_continuous(limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1),
                                labels = hv_minus_labels) +
    ggplot2::facet_wrap(~out_lab, nrow = 1) +
    ggplot2::labs(x = "Pearson r (95% CI)", y = NULL) +
    hf_theme(grid = "x") +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 5),
                   panel.spacing.x = ggplot2::unit(1.6, "mm"),
                   legend.position = "bottom",
                   legend.margin = ggplot2::margin(0, 0, 0, 0),
                   legend.box.margin = ggplot2::margin(-1, 0, 0, 0, "mm"),
                   legend.key.size = ggplot2::unit(2.4, "mm"))
  u$reading <- sprintf(
    paste0("%d stored rows collapse to %d distinct tests; %d of %d intervals ",
           "exclude zero and the smallest BH value is %.2f within-outcome ",
           "(%.2f across the whole family), so no edge-behaviour association ",
           "survived multiple-testing correction. With 9 animals a single ",
           "correlation has very low power, so this is a limited-power ",
           "negative result and not evidence of absence"),
    nrow(x), nrow(u), sum(u$excludes_zero), nrow(u),
    min(u$p.adj_BH_within_outcome), min(u$p.adj_BH_all_edge_phenotype_tests))
  write_csv_safe(u, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# ==========================================================================
# ED3 d. CA2-SLM robustness classes, in publication terminology
# ==========================================================================
#
# Part-25 section 17. Same classification and same counts; the stored key is
# unchanged and only the displayed label changes, because "not claimable due
# to QC" is pipeline language.
f9_ed_ca2_classes <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  r <- hv_read_csv(repo_path(panel$primary_source))
  fam <- hv_palette()$typography$family
  z <- as.data.frame(table(cls = r$CA2_SLM_robustness_class),
                     stringsAsFactors = FALSE)
  z$lab <- f9_qc_class_label(z$cls)
  z$lab <- factor(z$lab, levels = z$lab[order(z$Freq)])
  z$col <- unname(f9_qc_class_colours()[z$cls])
  # a constant fill vector is applied in row order: z$col is already aligned with
  # the rows of z (reordering it by Freq swapped the robust and insufficient
  # colours relative to ED3e)
  p <- ggplot2::ggplot(z, ggplot2::aes(Freq, lab)) +
    ggplot2::geom_col(fill = z$col, width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = Freq), hjust = -0.35,
                       family = fam, size = hv_size(5.4)) +
    ggplot2::scale_x_continuous(limits = c(0, max(z$Freq) * 1.22),
                                expand = c(0, 0)) +
    ggplot2::labs(x = "proteins of the canonical 28", y = NULL) +
    hv_theme(grid = "x") +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 5))
  write_csv_safe(z[, c("cls", "Freq")], csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}
