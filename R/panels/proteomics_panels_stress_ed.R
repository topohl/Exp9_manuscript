# Extended Data panels of the stress analyses added on 2026-10-29 (pRoteomics test_stress_exposure,
# test_unit_specificity, compare_external_stress_studies): published stress datasets against ours, the agreement
# of the published datasets with each other, the CA2 microglia ratio of stress exposure, the unit specificity of
# the CA2 SLM SUS-RES effect and the post hoc Npy lead.
#
# DOWNSTREAM ONLY. These read the statistics computed in pRoteomics; no model, no test, no p-value here.

hs_input <- function(panel, name) {   # one declared input per table, read by name
  deps <- c(panel$primary_source, unlist(panel$input_dependencies))
  f <- grep(name, deps, value = TRUE, fixed = TRUE)
  if (length(f) != 1L) stop(panel$id, ": input ", name, " not declared once", call. = FALSE)
  hv_read_csv(repo_path(f))
}

HS_CONTRASTS <- c(SUS_vs_RES = "SUS–RES", SUS_vs_CON = "SUS–CON", RES_vs_CON = "RES–CON",
                  stressed_vs_CON = "Stressed–CON")
HS_LEVELS <- c(gene = "Genes", go_bp = "GO programmes")
HS_SIGNATURES <- c(
  Schmidt2010_dCA1 = "Schmidt 2010, dorsal CA1", Schmidt2010_dDG = "Schmidt 2010, dorsal DG",
  Henningsen2012_CA = "Henningsen 2012, CA (protein)", Bagot2016_vHIP_28d = "Bagot 2016, vHIP 28 d",
  Bagot2016_vHIP_48h = "Bagot 2016, vHIP 48 h", Bagot2017_vHIP = "Bagot 2017, vHIP", Laine2018_vHPC = "Laine 2018, vHIP",
  Ayash2023_vHC = "Ayash 2023, vHIP", Butto2024_vHip_nuclei = "Butto 2024, vHIP nuclei",
  Bhatti2022_PV_TRAP = "Bhatti 2022, PV translatome", Lehmann2018_microglia = "Lehmann 2018, microglia",
  Sturman2021_vHC = "Sturman 2021, vHIP", Lyons2025_CA1 = "Lyons 2025, CA1", Lyons2025_CA3 = "Lyons 2025, CA3",
  Weber2019_microglia = "Weber 2019, microglia", Herve2017_DG = "Herve 2017, DG")
hs_p <- function(p) ifelse(p < 0.001, "<0.001", formatC(signif(p, 2), format = "fg", digits = 2, flag = "#"))
hs_num <- function(x, digits = 2, plus = FALSE) {   # fixed decimals with a true minus sign
  out <- formatC(abs(x), format = "f", digits = digits)
  ifelse(x < 0, paste0("−", out), if (plus) paste0("+", out) else out)
}

# a. Agreement of our SUS / RES / CON differences with published stress datasets
hf_ed_external_concordance <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  sg <- hs_input(panel, "external_concordance_signatures")
  om <- hs_input(panel, "external_concordance_omnibus")
  sg <- sg[sg$set == "primary" & sg$level %in% names(HS_LEVELS), , drop = FALSE]
  om <- om[om$level %in% names(HS_LEVELS), , drop = FALSE]
  if (!nrow(sg) || !nrow(om)) stop("hf_ed_external_concordance: no rows", call. = FALSE)
  yl <- rev(names(HS_CONTRASTS))
  sg$y <- match(sg$contrast, yl); om$y <- match(om$contrast, yl)
  sg$facet <- factor(HS_LEVELS[sg$level], levels = HS_LEVELS); om$facet <- factor(HS_LEVELS[om$level], levels = HS_LEVELS)
  k <- stats::ave(sg$mean_rho, sg$level, sg$contrast, FUN = function(v) rank(v, ties.method = "first"))
  n <- stats::ave(sg$mean_rho, sg$level, sg$contrast, FUN = length)
  sg$yj <- sg$y + (k - (n + 1) / 2) / pmax(n, 1) * 0.5
  om$label <- paste0("italic(P)*'=", hs_p(om$p_all_1680), "'")
  xr <- tapply(c(sg$mean_rho, om$null_q95), c(as.character(sg$facet), as.character(om$facet)), function(v) max(abs(v)))
  om$xlab <- xr[as.character(om$facet)] * 1.12
  p <- ggplot2::ggplot() +
    ggplot2::geom_vline(xintercept = 0, linewidth = hv_lw("reference_pt"), colour = "grey70") +
    ggplot2::geom_point(data = sg, ggplot2::aes(mean_rho, yj), colour = "grey60", size = 0.7, stroke = 0) +
    ggplot2::geom_segment(data = om, ggplot2::aes(x = null_q95, xend = null_q95, y = y - 0.32, yend = y + 0.32),
                          linewidth = hv_lw("data_pt"), colour = "grey35") +
    ggplot2::geom_point(data = om, ggplot2::aes(mean_rho, y), shape = 18, size = 1.9, colour = "black") +
    ggplot2::geom_text(data = om, ggplot2::aes(x = xlab, y = y, label = label), parse = TRUE, hjust = 0, family = fam,
                       size = hf_sz(HF_MIN_PT), colour = "black") +
    ggplot2::facet_wrap(~ facet, nrow = 1, scales = "free_x") +
    ggplot2::scale_y_continuous(breaks = seq_along(yl), labels = unname(HS_CONTRASTS[yl]), expand = ggplot2::expansion(add = 0.5)) +
    ggplot2::scale_x_continuous(labels = hv_minus_labels, breaks = scales::breaks_pretty(3),
                                expand = ggplot2::expansion(mult = c(0.08, 0.45))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = "Agreement with this study (Spearman ρ, mean over matched units)", y = NULL) +
    hf_theme() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
                   axis.title.x = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
                   strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
                   panel.spacing.x = ggplot2::unit(4, "mm"), plot.margin = ggplot2::margin(1, 1, 0.5, 0.5, "mm"))
  out <- rbind(
    data.frame(row_type = "signature", level = sg$level, contrast = sg$contrast, signature = sg$signature,
               signature_label = unname(HS_SIGNATURES[sg$signature]), n_signatures = NA_integer_, n_units = sg$n_units,
               mean_rho = sg$mean_rho, null_median = sg$null_median, null_q95 = sg$null_q95, p_all_1680 = sg$p_all_1680,
               p_contrast_specific = sg$p_contrast_specific, q_bh = sg$q_bh, stringsAsFactors = FALSE),
    data.frame(row_type = "mean_over_signatures", level = om$level, contrast = om$contrast, signature = om$signatures,
               signature_label = NA_character_, n_signatures = om$n_signatures, n_units = NA_integer_, mean_rho = om$mean_rho,
               null_median = om$null_median, null_q95 = om$null_q95, p_all_1680 = om$p_all_1680,
               p_contrast_specific = om$p_contrast_specific, q_bh = NA_real_, stringsAsFactors = FALSE))
  out$legend_text_moved <- paste0(
    "Spearman correlation of our per-unit t (Figure 3 model) with each published dataset's statistic over shared genes, ",
    "averaged over the units matched to that dataset; GO programmes: the same on GO biological-process term means; grey: ",
    "each published dataset; diamond: mean over the datasets with that contrast; tick: 95th percentile of the mean over all ",
    "1,680 assignments of the nine animals to three groups of three; P: one-sided share of assignments at least as large ",
    "(contrast-specific P with the third group fixed in the source data)")
  write_csv_safe(out, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# b. How much the published datasets agree with each other, and with ours
hf_ed_external_agreement <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  ag <- hs_input(panel, "external_signature_agreement")
  sg <- hs_input(panel, "external_concordance_signatures")
  ag <- ag[ag$set_a == "primary" & ag$set_b == "primary", , drop = FALSE]
  sg <- sg[sg$set == "primary" & sg$level == "gene", , drop = FALSE]
  xl <- names(HS_CONTRASTS)
  ag$x <- match(ag$contrast, xl) - 0.17; sg$x <- match(sg$contrast, xl) + 0.17
  jit <- function(d, w) {   # deterministic, symmetric spread: alternate sides in order of the value
    k <- stats::ave(d$rho, d$contrast, FUN = function(v) rank(v, ties.method = "first"))
    ((k %% 5) - 2) / 4 * w / 2 }
  ag$xj <- ag$x + jit(ag, 0.22)
  sg$rho <- sg$mean_rho; sg$xj <- sg$x + jit(sg, 0.22)
  md <- stats::aggregate(rho ~ contrast + x, data = ag, FUN = stats::median)
  md2 <- stats::aggregate(rho ~ contrast + x, data = sg, FUN = stats::median)
  key <- data.frame(x = c(0.6, 0.6), y = max(c(ag$rho, sg$rho)) * c(1.2, 1.05), colour = c("grey65", "black"),
                    label = c("published vs published", "this study vs published"))
  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, linewidth = hv_lw("reference_pt"), colour = "grey70") +
    ggplot2::geom_point(data = ag, ggplot2::aes(xj, rho), colour = "grey65", size = 0.6, stroke = 0) +
    ggplot2::geom_point(data = sg, ggplot2::aes(xj, rho), colour = "black", size = 0.7, stroke = 0) +
    ggplot2::geom_segment(data = rbind(md, md2), ggplot2::aes(x = x - 0.14, xend = x + 0.14, y = rho, yend = rho),
                          linewidth = 0.45, colour = "black") +
    ggplot2::geom_point(data = key, ggplot2::aes(x, y), colour = key$colour, size = 0.8, stroke = 0) +
    ggplot2::geom_text(data = key, ggplot2::aes(x + 0.12, y, label = label), hjust = 0, family = fam,
                       size = hf_sz(HF_MIN_PT), colour = "black") +
    ggplot2::scale_x_continuous(breaks = seq_along(xl), labels = unname(HS_CONTRASTS[xl]), expand = ggplot2::expansion(add = 0.4)) +
    ggplot2::scale_y_continuous(labels = hv_minus_labels, breaks = scales::breaks_pretty(4)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = NULL, y = "Gene-level agreement (Spearman ρ)") +
    hf_theme() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
                   axis.title.y = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
                   axis.ticks.x = ggplot2::element_blank(), plot.margin = ggplot2::margin(1, 1, 0.5, 0.5, "mm"))
  out <- rbind(
    data.frame(row_type = "published_pair", contrast = ag$contrast, signature_a = ag$signature_a, signature_b = ag$signature_b,
               n_genes = ag$n_genes, rho = ag$rho, stringsAsFactors = FALSE),
    data.frame(row_type = "ours_vs_published", contrast = sg$contrast, signature_a = "this study (mean over matched units)",
               signature_b = sg$signature, n_genes = NA_integer_, rho = sg$mean_rho, stringsAsFactors = FALSE))
  out$legend_text_moved <- paste0(
    "grey: Spearman correlation between two published datasets over shared genes (pairs of the primary datasets with the ",
    "contrast); black: our agreement with each published dataset (as in a, gene level); bars: medians")
  write_csv_safe(out, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# c. Stress exposure in the microglia-enriched ROIs: Aif1 (Iba1) relative to homeostatic microglia proteins
hf_ed_exposure_microglia <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  am <- hs_input(panel, "stress_exposure_panel_by_animal")
  ts <- hs_input(panel, "stress_exposure_panel_tests")
  rois <- c("CA1microglia", "CA2microglia", "CA3microglia", "DGmicroglia")
  am <- am[am$acquisitions == "all" & am$location %in% rois, , drop = FALSE]
  ts <- ts[ts$ratio == "iba1_vs_homeostatic", , drop = FALSE]
  gl <- c("CON", "RES", "SUS")
  am$x <- match(am$location, rois) + (match(am$group, gl) - 2) * 0.22
  k <- stats::ave(seq_len(nrow(am)), am$location, am$group, FUN = seq_along)
  am$xj <- am$x + (k - 2) * 0.05
  mn <- stats::aggregate(iba1_vs_homeostatic ~ location + group + x, data = am, FUN = mean)
  main <- ts[ts$comparison == "stressed_vs_con" & ts$acquisitions == "all" & ts$location %in% rois, , drop = FALSE]
  main$xpos <- match(main$location, rois)
  main$label <- sprintf("%s\n%s", hs_num(main$estimate, plus = TRUE), paste0("P=", hs_p(main$p_permutation)))
  ytop <- max(am$iba1_vs_homeostatic) + 0.08 * diff(range(am$iba1_vs_homeostatic))
  p <- ggplot2::ggplot(am, ggplot2::aes(xj, iba1_vs_homeostatic)) +
    ggplot2::geom_point(ggplot2::aes(colour = group), size = 0.9, stroke = 0) +
    ggplot2::geom_segment(data = mn, ggplot2::aes(x = x - 0.08, xend = x + 0.08, y = iba1_vs_homeostatic,
                                                  yend = iba1_vs_homeostatic, colour = group), inherit.aes = FALSE, linewidth = 0.45) +
    ggplot2::geom_text(data = main, ggplot2::aes(x = xpos, y = ytop, label = label), inherit.aes = FALSE, vjust = 0,
                       lineheight = 0.9, family = fam, size = hf_sz(HF_MIN_PT), colour = "black") +
    ggplot2::scale_colour_manual(values = hv_group_colours(), name = NULL) +
    ggplot2::scale_x_continuous(breaks = seq_along(rois), labels = sub("microglia$", "", rois), expand = ggplot2::expansion(add = 0.45)) +
    ggplot2::scale_y_continuous(labels = hv_minus_labels, breaks = scales::breaks_pretty(4),
                                expand = ggplot2::expansion(mult = c(0.05, 0.28))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = "Microglia-enriched ROI", y = "log2 Aif1 (Iba1) : P2ry12, Csf1r") +
    hf_theme() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
                   axis.title = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
                   axis.ticks.x = ggplot2::element_blank(), legend.position = "top",
                   legend.key.size = ggplot2::unit(2, "mm"), legend.margin = ggplot2::margin(0, 0, -1, 0, "mm"),
                   plot.margin = ggplot2::margin(1, 1, 0.5, 0.5, "mm"))
  out <- am[, c("AnimalID", "group", "location", "iba1_vs_homeostatic", "complement_vs_homeostatic", "homeostatic_vs_neuronal",
                "choroid_vs_neuronal", "n_acquisitions")]
  out$row_type <- "animal_roi"
  tt <- ts[, c("comparison", "acquisitions", "location", "ratio", "estimate", "ci_low", "ci_high", "n_group", "n_reference",
               "p_permutation", "n_labellings", "complete_separation", "p_adjust", "role", "animals")]
  tt$row_type <- "test"
  all_cols <- union(names(out), names(tt))
  out[setdiff(all_cols, names(out))] <- NA; tt[setdiff(all_cols, names(tt))] <- NA
  out <- rbind(out[, all_cols], tt[, all_cols])
  out$legend_text_moved <- paste0(
    "animal mean of within-acquisition log2 Aif1 minus mean log2 P2ry12, Csf1r (every protein observed in the acquisition); ",
    "bars: group means; numbers: stressed (RES + SUS) minus CON with the exact permutation P over the 84 labellings of the ",
    "nine animals (BH over the declared panel, left- and right-hemisphere and without-A765 checks, and SUS - RES in the ",
    "source data); the ratio was seen in an earlier exploration of these animals, so it is not an independent confirmation")
  write_csv_safe(out, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# d. Is the CA2 SLM SUS-RES difference specific to CA2 SLM? Direct contrasts between units
hf_ed_ca2_specificity <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  sm <- hs_input(panel, "unit_specificity_summary")
  cp <- hs_input(panel, "unit_specificity_ca2_slm_proteins")
  rows <- data.frame(stringsAsFactors = FALSE,
    kind = c("per_unit", "specificity", "specificity", "specificity", "prior", "specificity"),
    comparison = c("CA2slm", "CA2slm vs other laminae", "CA2slm vs CA1slm", "CA2slm vs CA2sr", "CA3sr vs CA3so",
                   "CA2microglia vs other ROIs"),
    label = c("CA2 SLM (unit alone)","CA2 SLM − mean of other laminae", "CA2 SLM − CA1 SLM", "CA2 SLM − CA2 SR",
              "CA3 SR − CA3 SO", "CA2 − other microglia ROIs"))
  d <- merge(rows, sm, by = c("kind", "comparison"))
  d <- d[d$effect %in% c("SUS_minus_RES", "stressed_minus_CON"), , drop = FALSE]
  if (nrow(d) != 12L) stop("hf_ed_ca2_specificity: expected 6 comparisons x 2 effects", call. = FALSE)
  d$y <- match(d$label, rev(rows$label)) + ifelse(d$effect == "SUS_minus_RES", 0.18, -0.18)
  d$fill <- ifelse(d$effect == "SUS_minus_RES", "SUS–RES", "Stressed–CON")
  xmax <- max(d$n_fdr_005)
  p <- ggplot2::ggplot(d, ggplot2::aes(xmin = 0, xmax = n_fdr_005, ymin = y - 0.16, ymax = y + 0.16, fill = fill)) +
    ggplot2::geom_rect() +
    ggplot2::geom_text(ggplot2::aes(x = n_fdr_005 + xmax * 0.02, y = y, label = n_fdr_005), hjust = 0, family = fam,
                       size = hf_sz(HF_MIN_PT), colour = "black") +
    ggplot2::scale_fill_manual(values = c(`SUS–RES` = "grey25", `Stressed–CON` = "grey70"), name = NULL,
                               breaks = c("SUS–RES", "Stressed–CON")) +
    ggplot2::scale_y_continuous(breaks = seq_len(nrow(rows)), labels = rev(rows$label), expand = ggplot2::expansion(add = 0.5)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(x = "Proteins with FDR < 0.05", y = NULL) +
    hf_theme() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
                   axis.title.x = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
                   axis.ticks.y = ggplot2::element_blank(), legend.position = "top",
                   legend.key.size = ggplot2::unit(2, "mm"), legend.margin = ggplot2::margin(0, 0, -1, 0, "mm"),
                   plot.margin = ggplot2::margin(1, 2, 0.5, 0.5, "mm"))
  out <- d[, c("dataset", "kind", "comparison", "label", "effect", "n_proteins", "n_fdr_005", "n_fdr_010", "share_p_below_005",
               "prop_true_null", "animal_block_correlation")]
  out$row_type <- "contrast_summary"
  cp$row_type <- "figure_3a_ca2_slm_protein"
  all_cols <- union(names(out), names(cp))
  out[setdiff(all_cols, names(out))] <- NA; cp[setdiff(all_cols, names(cp))] <- NA
  out <- rbind(out[, all_cols], cp[, all_cols])
  out$legend_text_moved <- paste0(
    "one limma model per compartment over all units with the animal blocked (duplicateCorrelation); bars: proteins with BH ",
    "FDR < 0.05 for the SUS - RES (dark) or stressed - CON (light) difference of a unit alone or of a direct contrast between ",
    "units (protein-level inference); the 28 CA2 SLM proteins of Fig. 3a with their estimates in the three CA2 SLM contrasts ",
    "(figure_3a_ca2_slm_protein rows; BH within these proteins) in the source data")
  write_csv_safe(out, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# e. Post hoc lead: Npy lower in the three susceptible animals
hf_ed_npy_lead <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  an <- hs_input(panel, "stress_exposure_npy_by_animal")
  ts <- hs_input(panel, "stress_exposure_npy_tests")
  units <- unique(an$unit[an$unit != "score over units"])
  lab_unit <- function(u) ifelse(grepl("microglia$", u), sub("microglia$", " microglia", u),
                                 paste(sub("^(CA[0-9]|DG).*$", "\\1", u), toupper(sub("^(CA[0-9]|DG)", "", u))))
  xl <- c(units, "score over units")
  an$x <- match(an$unit, xl) + ifelse(an$unit == "score over units", 0.6, 0) + (match(an$group, c("CON", "RES", "SUS")) - 2) * 0.2
  k <- stats::ave(seq_len(nrow(an)), an$unit, an$group, FUN = seq_along)
  an$xj <- an$x + (k - 2) * 0.05
  sc <- an[an$unit == "score over units", , drop = FALSE]
  mn <- stats::aggregate(official_centred ~ group + x, data = sc, FUN = mean)
  t1 <- ts[ts$values == "official_centred" & ts$comparison == "SUS vs CON+RES", , drop = FALSE]
  if (nrow(t1) != 1L) stop("hf_ed_npy_lead: SUS vs CON+RES test missing", call. = FALSE)
  txt <- sprintf("SUS − others %s log2 [%s, %s]\nP=%s (84 labellings); post hoc",
                 hs_num(t1$estimate), hs_num(t1$ci_low), hs_num(t1$ci_high), hs_p(t1$p_permutation))
  xs <- c(seq_along(units), length(xl) + 0.6)
  p <- ggplot2::ggplot(an, ggplot2::aes(xj, official_centred)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = hv_lw("reference_pt"), colour = "grey70") +
    ggplot2::geom_vline(xintercept = length(units) + 0.8, linewidth = hv_lw("reference_pt"), colour = "grey80") +
    ggplot2::geom_point(ggplot2::aes(colour = group), size = 0.9, stroke = 0) +
    ggplot2::geom_segment(data = mn, ggplot2::aes(x = x - 0.08, xend = x + 0.08, y = official_centred, yend = official_centred,
                                                  colour = group), inherit.aes = FALSE, linewidth = 0.45) +
    ggplot2::annotate("text", x = length(units) + 1.6, y = max(an$official_centred), label = txt, hjust = 1, vjust = 1,
                      lineheight = 0.9, family = fam, size = hf_sz(HF_MIN_PT), colour = "black") +
    ggplot2::scale_colour_manual(values = hv_group_colours(), name = NULL) +
    ggplot2::scale_x_continuous(breaks = xs, labels = c(lab_unit(units), "Score"), expand = ggplot2::expansion(add = 0.5)) +
    ggplot2::scale_y_continuous(labels = hv_minus_labels, breaks = scales::breaks_pretty(4)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = NULL, y = "Npy, centred on the unit (log2)") +
    hf_theme() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
                   axis.title.y = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
                   axis.ticks.x = ggplot2::element_blank(), legend.position = "top",
                   legend.key.size = ggplot2::unit(2, "mm"), legend.margin = ggplot2::margin(0, 0, -1, 0, "mm"),
                   plot.margin = ggplot2::margin(1, 1, 0.5, 0.5, "mm"))
  out <- an[, c("protein", "unit", "AnimalID", "group", "official_value", "official_centred", "observed_value", "observed_centred")]
  out$row_type <- ifelse(out$unit == "score over units", "animal_score", "animal_unit")
  tt <- ts
  tt$row_type <- "test"
  all_cols <- union(names(out), names(tt))
  out[setdiff(all_cols, names(out))] <- NA; tt[setdiff(all_cols, names(tt))] <- NA
  out <- rbind(out[, all_cols], tt[, all_cols])
  out$legend_text_moved <- paste0(
    "Npy in the units where it is observed in every acquisition: animal-level value (Figure 3 matrices) centred on the ",
    "unit's mean over the nine animals; score: mean over these units; selected after a probe of glucocorticoid and ",
    "neuropeptide proteins (post hoc lead, not a confirmation); exact label tests (SUS vs the other six animals, 84 ",
    "labellings; pairwise, 20), the observed-value version and the rank among all proteins observed in the same units ",
    "in the source data")
  write_csv_safe(out, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}
