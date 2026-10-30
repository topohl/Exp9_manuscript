# Extended Data panels added on 2026-10-30 (pRoteomics describe_proteomic_animals and couple_network_edges_to_outcomes,
# both on the primary tree): quality control across every unit and the design of the nine animals (Extended Data 6e-g),
# every animal's values of the Figure 3e-g exemplar proteins (Extended Data 7c-e, under each curve), and the edge-behaviour coupling on the
# current outcomes (Extended Data 8c).
#
# DOWNSTREAM ONLY. These read the tables computed in pRoteomics (and, for 6g, the frozen behaviour bundle); no model, no
# test, no P value here. hs_input() is defined in R/panels/proteomics_panels_stress_ed.R.

HA_COMPARTMENTS <- c(neuron_neuropil = "Neuropil", neuron_soma = "Soma", microglia = "Microglia ROI")
HA_UNITS <- list(
  neuron_neuropil = c("CA1_so", "CA1_sr", "CA1_slm", "CA2_so", "CA2_sr", "CA2_slm", "CA3_so", "CA3_sr", "DG_mo", "DG_po"),
  neuron_soma = c("CA1_sp", "CA2_sp", "CA3_sp", "DG_sg"),
  microglia = c("CA1_microglia", "CA2_microglia", "CA3_microglia", "DG_microglia"))
ha_unit_label <- function(u) {
  u <- sub("_microglia$", "", u); u <- sub("_(sp|sg)$", "", u)
  r <- sub("_.*$", "", u); l <- toupper(sub("^[^_]*_?", "", u))
  ifelse(nzchar(l), paste(r, l), r)
}
HA_GROUPS <- c("CON", "RES", "SUS")

# 6e. Missingness before imputation of every acquisition, in every unit
hf_ed_missing_all <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  m <- hs_input(panel, "acquisition_missingness")
  m$compartment <- factor(HA_COMPARTMENTS[m$dataset], levels = HA_COMPARTMENTS)
  m$unit_i <- mapply(function(d, u) match(u, HA_UNITS[[d]]), m$dataset, m$unit)
  if (anyNA(m$unit_i)) stop("hf_ed_missing_all: an unknown unit", call. = FALSE)
  m$x <- m$unit_i + (match(m$group, HA_GROUPS) - 2) * 0.24 + ifelse(m$hemisphere == "Left", -0.05, 0.05)
  m$group <- factor(m$group, levels = HA_GROUPS)
  brk <- do.call(rbind, lapply(names(HA_UNITS), function(d) data.frame(compartment = HA_COMPARTMENTS[[d]],
    x = seq_along(HA_UNITS[[d]]), label = ha_unit_label(HA_UNITS[[d]]), stringsAsFactors = FALSE)))
  brk$compartment <- factor(brk$compartment, levels = HA_COMPARTMENTS)
  p <- ggplot2::ggplot(m, ggplot2::aes(x, fraction_missing)) +
    ggplot2::geom_point(ggplot2::aes(colour = group, shape = hemisphere), size = 0.75, stroke = 0.3, fill = "white") +
    ggplot2::geom_text(data = brk, ggplot2::aes(x = x, y = -Inf, label = label), inherit.aes = FALSE, vjust = 1.4,
                       family = fam, size = hf_sz(HF_MIN_PT), colour = "black") +
    ggplot2::facet_grid(~ compartment, scales = "free_x", space = "free_x") +
    ggplot2::scale_colour_manual(values = hv_group_colours(), name = NULL) +
    ggplot2::scale_shape_manual(values = c(Left = 21, Right = 16), name = NULL, labels = c(Left = "left", Right = "right")) +
    ggplot2::scale_x_continuous(breaks = NULL, expand = ggplot2::expansion(add = 0.5)) +
    ggplot2::scale_y_continuous(labels = function(v) paste0(round(100 * v), "%"), breaks = scales::breaks_pretty(4),
                                limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = NULL, y = "Missing (%)") +
    hf_theme() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
                   axis.title = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
                   strip.text = ggplot2::element_text(size = hf_pt(5.4), colour = "black", face = "bold", hjust = 0),
                   strip.background = ggplot2::element_blank(), panel.spacing.x = ggplot2::unit(3, "mm"),
                   legend.position = "top", legend.justification = "right", legend.key.size = ggplot2::unit(2, "mm"),
                   legend.text = ggplot2::element_text(size = HF_MIN_PT), legend.margin = ggplot2::margin(0, 0, -1, 0, "mm"),
                   plot.margin = ggplot2::margin(1, 1, 3.5, 0.5, "mm"))
  out <- m[, c("dataset", "compartment", "unit", "acquisition", "animal_id", "group", "hemisphere", "plate", "run_order",
               "n_proteins_analysed", "n_missing", "fraction_missing", "n_quantified_all_features")]
  out$reading <- paste0("fraction of the compartment's analysed proteins (missing in at most 70% of its acquisitions: ",
                        "5,054 neuropil, 5,538 soma, 5,229 microglia-enriched ROI) missing before imputation; one point per acquisition")
  write_csv_safe(out, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# 6f. Within-compartment PCA: the share of each PC explained by anatomy, animal, group, plate and hemisphere
hf_ed_pca_terms <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  t <- hs_input(panel, "compartment_pca_terms")
  terms <- c(unit = "Unit", animal = "Animal", group = "Group", plate = "MS plate", hemisphere = "Hemisphere")
  t$col <- match(t$term, names(terms))
  t$compartment <- factor(HA_COMPARTMENTS[t$dataset], levels = HA_COMPARTMENTS)
  t$pc_i <- as.integer(sub("^PC", "", t$PC))
  t$y <- 6L - t$pc_i
  # values below .05 are left blank (they are in the source data), 1.00 reads 1
  t$label <- ifelse(t$eta_squared < 0.05, "", ifelse(t$eta_squared >= 0.995, "1", sub("^0[.]", ".", formatC(t$eta_squared, format = "f", digits = 2))))
  p <- ggplot2::ggplot(t, ggplot2::aes(col, y)) +
    ggplot2::geom_tile(ggplot2::aes(fill = eta_squared), width = hv_tile(), height = hv_tile()) +
    ggplot2::geom_text(ggplot2::aes(label = label, colour = eta_squared > 0.6), family = fam, size = hf_sz(HF_MIN_PT)) +
    ggplot2::facet_wrap(~ compartment, nrow = 1) +
    ggplot2::scale_fill_gradient(low = "white", high = hv_evidence_colours()[["supported"]], limits = c(0, 1),
                                 name = NULL, breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "black"), guide = "none") +
    ggplot2::scale_x_continuous(breaks = seq_along(terms), labels = unname(terms), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = 1:5, labels = paste0("PC", 5:1), expand = c(0, 0)) +
    ggplot2::coord_fixed(ratio = 1, clip = "off") +
    ggplot2::labs(x = NULL, y = NULL) +
    hf_theme_tile() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = HF_MIN_PT, colour = "black", angle = 60, hjust = 1, vjust = 1),
                   axis.text.y = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
                   strip.text = ggplot2::element_text(size = HF_MIN_PT, colour = "black", face = "bold"),
                   strip.background = ggplot2::element_blank(), panel.spacing.x = ggplot2::unit(2, "mm"),
                   legend.position = "right", legend.title = ggplot2::element_text(size = HF_MIN_PT),
                   legend.text = ggplot2::element_text(size = HF_MIN_PT), legend.key.width = ggplot2::unit(1.6, "mm"),
                   legend.key.height = ggplot2::unit(3, "mm"), legend.margin = ggplot2::margin(0, 0, 0, -1, "mm"),
                   plot.margin = ggplot2::margin(0.5, 0.5, 0.5, 0.5, "mm"))
  out <- t[, c("dataset", "compartment", "PC", "variance_explained", "term", "eta_squared")]
  out$reading <- paste0("PCA of the 4,242 proteins quantified in every compartment (the matrix of Figure 2c) within one ",
                        "compartment's acquisitions; eta squared of a one-way fit of each PC on each term; descriptive")
  write_csv_safe(out, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# 6g. The nine animals: group, behaviour batch, MS plate, and where each sits among the males on the ISS and early movement
hf_ed_animal_design <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  d <- hs_input(panel, "proteomic_animal_design")
  d <- d[order(match(d$group, HA_GROUPS), d$ISS), , drop = FALSE]
  d$y <- rev(seq_len(nrow(d))) + (3 - match(d$group, HA_GROUPS)) * 0.4
  cols <- c("Animal", "Batch", "MS plate", "ISS (rank)", "Early movement (rank)", "Missing")
  cx <- c(1, 2.1, 3.0, 4.35, 6.0, 7.35)
  cells <- data.frame(y = rep(d$y, each = length(cols)), x = rep(cx, nrow(d)), label = as.vector(t(cbind(
    d$animal_id, d$batch, d$plate, sprintf("%s (%d/%d)", hs_num(d$ISS), d$ISS_rank_among_males, d$n_males_with_ISS),
    sprintf("%s (%d/%d)", hs_num(d$early_movement), d$early_movement_rank_among_tracked_males, d$n_tracked_males),
    paste0(formatC(100 * d$median_fraction_missing, format = "f", digits = 1), "%")))), stringsAsFactors = FALSE)
  head <- data.frame(x = cx, y = max(d$y) + 1, label = cols, stringsAsFactors = FALSE)
  grp <- unique(d[, c("group")]); gl <- data.frame(group = grp, y = tapply(d$y, d$group, mean)[grp], stringsAsFactors = FALSE)
  p <- ggplot2::ggplot() +
    ggplot2::geom_text(data = cells, ggplot2::aes(x, y, label = label), family = fam, size = hf_sz(HF_MIN_PT), colour = "black") +
    ggplot2::geom_text(data = head, ggplot2::aes(x, y, label = label), family = fam, size = hf_sz(HF_MIN_PT), fontface = "bold",
                       colour = "black", lineheight = 0.9) +
    ggplot2::geom_text(data = gl, ggplot2::aes(x = 0.05, y = y, label = group, colour = group), hjust = 0, family = fam,
                       size = hf_sz(5.4), fontface = "bold") +
    ggplot2::annotate("segment", x = 0, xend = 8.1, y = max(d$y) + 0.55, yend = max(d$y) + 0.55, linewidth = 0.2, colour = "grey45") +
    ggplot2::scale_colour_manual(values = hv_group_colours(), guide = "none") +
    ggplot2::scale_x_continuous(limits = c(0, 8.1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(min(d$y) - 0.6, max(d$y) + 1.5), expand = c(0, 0)) +
    ggplot2::theme_void(base_family = fam) + ggplot2::theme(plot.margin = ggplot2::margin(1, 1, 1, 1, "mm"))
  out <- d
  out$reading <- paste0("ISS rank among the 59 males with an ISS (1 = lowest); early movement rank among the 53 RFID-tracked ",
                        "males (1 = most active); missing = median over the animal's acquisitions; batch = behavioural cohort")
  write_csv_safe(out, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# 7c-e (lower part). Every animal's value of the seven exemplar proteins of Figure 3e-g (log2, centred on the three
# controls), drawn by f9_ed_gsea_curve under its running enrichment when the panel declares the table (2026-10-30).
HA_LFC_LIMIT <- 0.5   # display cap of the centred log2 values (|value| > 0.5 takes full colour; values in the source data)
ha_exemplar_heat <- function(x, fam = hf_fam()) {
  genes <- unique(x$gene)                          # Figure 3 order (ranked leading edge)
  an <- unique(x[, c("animal_id", "group")]); an <- an[order(match(an$group, HA_GROUPS), an$animal_id), , drop = FALSE]
  an$x <- seq_len(nrow(an)) + (match(an$group, HA_GROUPS) - 1) * 0.5
  x$x <- an$x[match(x$animal_id, an$animal_id)]
  x$y <- match(x$gene, rev(genes))
  hdr <- do.call(rbind, lapply(HA_GROUPS, function(g) data.frame(group = g, x = mean(an$x[an$group == g]))))
  ggplot2::ggplot(x, ggplot2::aes(x, y)) +
    ggplot2::geom_tile(ggplot2::aes(fill = log2_vs_con_mean), width = hv_tile(), height = hv_tile()) +
    ggplot2::geom_text(data = hdr, ggplot2::aes(x = x, y = length(genes) + 1, label = group, colour = group), inherit.aes = FALSE,
                       family = fam, size = hf_sz(HF_MIN_PT)) +
    hv_diverging(limits = c(-HA_LFC_LIMIT, HA_LFC_LIMIT), name = NULL, oob = scales::squish, guide = "none") +
    ggplot2::scale_colour_manual(values = hv_group_colours(), guide = "none") +
    ggplot2::scale_x_continuous(breaks = an$x, labels = an$animal_id, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = seq_along(genes), labels = rev(genes), expand = c(0, 0), position = "right") +
    ggplot2::coord_fixed(ratio = 1, clip = "off", ylim = c(0.5, length(genes) + 0.5)) +
    ggplot2::labs(x = NULL, y = NULL) +
    hf_theme_tile() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = HF_MIN_PT, face = "italic", colour = "black"),
                   axis.text.x = ggplot2::element_text(size = HF_MIN_PT, colour = "black", angle = 90, vjust = 0.5, hjust = 1),
                   plot.margin = ggplot2::margin(2.2, 0.5, 0.5, 0.5, "mm"))
}

# 8c. Network edges against the current behaviour outcomes (rerun 2026-10-30): an underpowered null
hf_ed_coupling <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  u <- hs_input(panel, "edge_outcome_correlations")
  outs <- c(CombZ = "ISS", cort_delta = "Corticosterone rise", suc_pref = "Sucrose preference",
            early_movement = "Early movement (CC1)", movement_cc1_cc4_mean = "Movement (CC1–CC4)")
  if (!setequal(unique(u$Outcome), names(outs)) || nrow(u) != 40L) stop("hf_ed_coupling: expected 8 edges x 5 outcomes", call. = FALSE)
  u$out_lab <- factor(outs[u$Outcome], levels = outs)
  u$edge_lab <- sub(" - ", "–", toupper(gsub("_", " ", u$Edge)), fixed = TRUE)
  lev <- rev(sort(unique(u$edge_lab))); u$ypos <- match(u$edge_lab, lev)
  u$excludes_zero <- u$conf.low > 0 | u$conf.high < 0
  p <- ggplot2::ggplot(u, ggplot2::aes(estimate, ypos)) +
    ggplot2::geom_vline(xintercept = 0, linewidth = hv_lw("reference_pt"), colour = "grey55") +
    ggplot2::geom_segment(ggplot2::aes(x = conf.low, xend = conf.high, yend = ypos, colour = excludes_zero), linewidth = hv_lw("reference_pt")) +
    ggplot2::geom_point(ggplot2::aes(colour = excludes_zero), size = 0.8, shape = 16) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = unname(hv_evidence_colours()[["supported"]]), `FALSE` = "grey60"), guide = "none") +
    ggplot2::scale_y_continuous(breaks = seq_along(lev), labels = lev, expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::scale_x_continuous(limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1), labels = hv_minus_labels) +
    ggplot2::facet_wrap(~ out_lab, nrow = 1) +
    ggplot2::labs(x = "Pearson r (95% CI)", y = NULL) +
    hf_theme() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
                   axis.title = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
                   strip.text = ggplot2::element_text(size = HF_MIN_PT, colour = "black"), strip.background = ggplot2::element_blank(),
                   panel.spacing.x = ggplot2::unit(1.6, "mm"), plot.margin = ggplot2::margin(1, 1.5, 0.5, 0.5, "mm"))
  out <- u[, c("Edge", "Outcome", "outcome_label", "n", "estimate", "conf.low", "conf.high", "statistic", "df", "p.value",
               "p.adj_BH_within_outcome", "p.adj_BH_all")]
  out$reading <- sprintf(paste0("nine male animals; Pearson r with the Fisher-z 95%% interval; smallest BH-adjusted P within an outcome %.2f ",
                                "(%.2f across all 40 tests); a limited-power negative result, not evidence of absence"),
                         min(u$p.adj_BH_within_outcome), min(u$p.adj_BH_all))
  write_csv_safe(out, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}
