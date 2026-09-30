# Figure 3 adaptation architecture.
#
# This layer is deliberately downstream-only. It reads frozen GO/GSEA and
# protein-level publication-source exports, computes descriptive programme
# summaries, and renders them. It does not refit enrichment, differential
# abundance, or any other inferential model.
#
# The programmes are the seven manuscript_go_themes_v3 rows read from
# f9_atlas_themes(), the registry the ED6 atlases also use, so Figure 3b is the
# same theme aggregation as the SUS-RES atlas it replaced. programme_id keeps
# the identifiers the frozen figure_03_adaptation selection was exported with.

f3a_programmes <- function() {
  themes <- f9_atlas_themes()
  ids <- c(rna_processing_splicing_rnp = "rna_rnp",
           ribosome_translation = "ribosome_translation",
           chromatin_organization = "chromatin_nuclear",
           mitochondrial_respiration_oxphos = "mitochondria_oxphos",
           synaptic_signaling_vesicle = "synapse_vesicle",
           neuron_projection_development = "neuron_projection",
           autophagy_lysosome_endosome = "autophagy_endolysosomal")
  colours <- c(rna_processing_splicing_rnp = "#6F8FA8",
               ribosome_translation = "#A9853D",
               chromatin_organization = "#556B5D",
               mitochondrial_respiration_oxphos = "#B45F4B",
               synaptic_signaling_vesicle = "#315B7D",
               neuron_projection_development = "#743100",
               autophagy_lysosome_endosome = "#7B6A91")
  if (!setequal(themes$theme_id, names(ids)))
    stop("Figure 3 programme ids do not cover the v3 theme registry.",
         call. = FALSE)
  data.frame(
    programme_id = unname(ids[themes$theme_id]),
    source_theme_id = themes$theme_id,
    programme_label = themes$label,
    programme_short = themes$label,
    programme_order = seq_len(nrow(themes)),
    colour = unname(colours[themes$theme_id]),
    stringsAsFactors = FALSE)
}

f3a_exemplars <- function() {
  data.frame(
    exemplar = 1:3,
    programme_id = c("synapse_vesicle", "rna_rnp", "mitochondria_oxphos"),
    dataset = c("neuron_neuropil", "neuron_soma", "microglia"),
    spatial_unit = c("CA3_sr", "CA2_sp", "CA1"),
    term_id = c("GO:0099536", "GO:0006397", "GO:0006119"),
    location_label = c("Neuropil CA3 SR", "Soma CA2", "Microglia ROI CA1"),
    stringsAsFactors = FALSE)
}

f3a_compartment_label <- function(dataset) {
  unname(c(neuron_neuropil = "Neuropil", neuron_soma = "Soma",
           microglia = "Microglia ROI")[dataset])
}

f3a_unit_label <- function(x) {
  toupper(gsub("_", " ", x, fixed = TRUE))
}

f3a_programme_terms <- function(inventory) {
  req <- c("dataset", "spatial_unit", "contrast", "GO_ID",
           "GO_description", "NES", "BH_FDR", "theme_id",
           "theme_claim_eligible")
  if (!all(req %in% names(inventory)))
    stop("Figure 3 adaptation inventory schema mismatch.", call. = FALSE)
  # The inventory collapses a term assigned to two themes into one row with
  # "a;b", so each theme is matched as a whole ;-delimited entry. Claim
  # eligibility is required as in f9_atlas_cells().
  eligible <- inventory$theme_claim_eligible %in% TRUE
  programmes <- f3a_programmes()
  out <- lapply(seq_len(nrow(programmes)), function(i) {
    p <- programmes[i, , drop = FALSE]
    in_theme <- grepl(paste0("(^|;)", p$source_theme_id[[1]], "($|;)"),
                      as.character(inventory$theme_id))
    z <- inventory[in_theme & eligible, , drop = FALSE]
    if (!nrow(z)) stop("No terms mapped to programme: ", p$programme_id,
                       call. = FALSE)
    z$programme_id <- p$programme_id[[1]]
    z$programme_label <- p$programme_label[[1]]
    z$programme_short <- p$programme_short[[1]]
    z$programme_order <- p$programme_order[[1]]
    z$programme_colour <- p$colour[[1]]
    z
  })
  z <- do.call(rbind, out)
  z <- z[!duplicated(z[c("programme_id", "dataset", "spatial_unit",
                         "contrast", "GO_ID")]), , drop = FALSE]
  rownames(z) <- NULL
  z
}

f3a_programme_cells <- function(inventory) {
  z <- f3a_programme_terms(inventory)
  key <- paste(z$programme_id, z$dataset, z$spatial_unit, z$contrast,
               sep = "\r")
  cells <- do.call(rbind, lapply(split(seq_len(nrow(z)), key), function(ix) {
    w <- z[ix, , drop = FALSE]
    data.frame(
      programme_id = w$programme_id[[1]],
      programme_label = w$programme_label[[1]],
      programme_short = w$programme_short[[1]],
      programme_order = w$programme_order[[1]],
      programme_colour = w$programme_colour[[1]],
      dataset = w$dataset[[1]],
      compartment = f3a_compartment_label(w$dataset[[1]]),
      spatial_unit = w$spatial_unit[[1]],
      spatial_unit_label = f3a_unit_label(w$spatial_unit[[1]]),
      contrast = w$contrast[[1]],
      median_NES = stats::median(w$NES, na.rm = TRUE),
      n_fdr_supported = sum(is.finite(w$BH_FDR) & w$BH_FDR < 0.05),
      n_constituent_terms = length(unique(w$GO_ID)),
      stringsAsFactors = FALSE)
  }))
  rownames(cells) <- NULL
  cells
}

f3a_adaptation_states <- function(inventory) {
  cells <- f3a_programme_cells(inventory)
  keys <- c("programme_id", "programme_label", "programme_short",
            "programme_order", "programme_colour", "dataset", "compartment",
            "spatial_unit", "spatial_unit_label")
  res <- cells[cells$contrast == "RES - CON", c(keys, "median_NES",
                                                 "n_fdr_supported",
                                                 "n_constituent_terms")]
  sus <- cells[cells$contrast == "SUS - CON", c(keys, "median_NES",
                                                 "n_fdr_supported",
                                                 "n_constituent_terms")]
  names(res)[match(c("median_NES", "n_fdr_supported", "n_constituent_terms"),
                   names(res))] <- c("RES_CON_median_NES", "RES_CON_n_fdr",
                                    "RES_CON_n_terms")
  names(sus)[match(c("median_NES", "n_fdr_supported", "n_constituent_terms"),
                   names(sus))] <- c("SUS_CON_median_NES", "SUS_CON_n_fdr",
                                    "SUS_CON_n_terms")
  states <- merge(res, sus, by = keys, all = TRUE, sort = FALSE)
  if (anyNA(states[c("RES_CON_median_NES", "SUS_CON_median_NES",
                     "RES_CON_n_fdr", "SUS_CON_n_fdr")]))
    stop("Incomplete RES-CON/SUS-CON programme-context pairing.", call. = FALSE)
  res_supported <- states$RES_CON_n_fdr > 0L
  sus_supported <- states$SUS_CON_n_fdr > 0L
  same_direction <- sign(states$RES_CON_median_NES) ==
    sign(states$SUS_CON_median_NES)

  # Transparent five-state classification:
  #   neither arm has an FDR-supported constituent term -> little detectable;
  #   only one supported arm -> that outcome-specific remodelling state;
  #   both arms supported -> shared/parallel when the programme medians have
  #   the same sign, divergent/opposing when their signs differ.
  states$adaptation_pattern <- ifelse(
    !res_supported & !sus_supported, "little detectable adaptation",
    ifelse(res_supported & !sus_supported, "resilience-specific remodeling",
      ifelse(!res_supported & sus_supported, "susceptibility-specific remodeling",
        ifelse(same_direction, "shared / parallel", "divergent / opposing"))))
  states$support_status <- ifelse(
    res_supported & sus_supported, "both control contrasts supported",
    ifelse(res_supported, "RES-CON supported only",
      ifelse(sus_supported, "SUS-CON supported only",
             "neither control contrast supported")))
  states$classification_rule <- paste0(
    "FDR support is any constituent GO term with BH FDR < 0.05 in the named ",
    "control contrast; direction is the sign of the median NES across all ",
    "mapped constituent terms. Non-support is labelled little detectable, ",
    "never passive.")
  ex <- f3a_exemplars()
  states$exemplar <- NA_integer_
  for (i in seq_len(nrow(ex))) {
    hit <- states$programme_id == ex$programme_id[[i]] &
      states$dataset == ex$dataset[[i]] &
      states$spatial_unit == ex$spatial_unit[[i]]
    states$exemplar[hit] <- ex$exemplar[[i]]
  }
  states
}

f3a_nes_limit <- function(inventory) {
  max(abs(f3a_programme_cells(inventory)$median_NES), na.rm = TRUE)
}

f3a_dap_burden <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  z <- nv_read_csv(repo_path(panel$primary_source))
  need <- c("dataset", "unit", "display", "canonical", "claimable", "xpos")
  if (!all(need %in% names(z)) || nrow(z) != 18L)
    stop("Frozen Figure 3a source is malformed.", call. = FALSE)
  fam <- nf_fam()
  long <- rbind(
    data.frame(xpos = z$xpos, row = 2L, n = z$canonical),
    data.frame(xpos = z$xpos, row = 1L, n = z$claimable))
  long$label <- as.character(long$n)
  long$colour <- ifelse(long$n == 0L, "grey76",
                        ifelse(long$row == 2L, "grey12", "#B45F4B"))
  labs <- data.frame(
    row = c(2, 1),
    label = c(sprintf("FDR-supported DAPs (%d)", sum(z$canonical)),
              sprintf("Robustness-qualified (%d)", sum(z$claimable))))
  p <- ggplot2::ggplot(long, ggplot2::aes(xpos, row)) +
    ggplot2::geom_tile(fill = "grey97", colour = "white", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = label), colour = long$colour,
                       family = fam, size = nf_sz(5.2),
                       fontface = ifelse(long$n > 0, "bold", "plain")) +
    ggplot2::geom_text(data = labs,
      ggplot2::aes(x = -3.2, y = row, label = label), inherit.aes = FALSE,
      hjust = 0, family = fam, size = nf_sz(5.0), colour = "grey18") +
    ggplot2::scale_x_continuous(limits = c(-3.4, 18.5), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0.45, 2.55), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL) + nf_theme_tile() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(0.5, 1, 0, 1, "mm"))
  z$summary_rule <- paste0(
    "Canonical SUS-RES FDR-supported proteins and the existing CA2-SLM ",
    "robustness qualification; no new differential analysis.")
  write_csv_safe(z, csv_path)
  nv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

f3a_atlas <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  inv <- nv_read_csv(repo_path(panel$primary_source))
  cells <- f3a_programme_cells(inv)
  cells <- cells[cells$contrast == "SUS - RES", , drop = FALSE]
  programmes <- f3a_programmes()
  cells$programme_label <- factor(cells$programme_label,
    levels = rev(programmes$programme_label))
  ds_levels <- c("neuron_neuropil", "neuron_soma", "microglia")
  cells$compartment <- factor(cells$compartment,
    levels = f3a_compartment_label(ds_levels))
  unit_order <- c("CA1_so", "CA1_sr", "CA1_slm", "CA2_so", "CA2_sr",
                  "CA2_slm", "CA3_so", "CA3_sr", "DG_mo", "DG_po",
                  "CA1_sp", "CA2_sp", "CA3_sp", "DG_sg",
                  "CA1", "CA2", "CA3", "DG")
  cells$spatial_unit_label <- factor(cells$spatial_unit_label,
    levels = f3a_unit_label(unit_order))
  ex <- f3a_exemplars()
  cells$exemplar <- NA_integer_
  for (i in seq_len(nrow(ex))) {
    hit <- cells$programme_id == ex$programme_id[[i]] &
      cells$dataset == ex$dataset[[i]] &
      cells$spatial_unit == ex$spatial_unit[[i]]
    cells$exemplar[hit] <- ex$exemplar[[i]]
  }
  lim <- f3a_nes_limit(inv)
  supported <- cells[cells$n_fdr_supported > 0, , drop = FALSE]
  selected <- cells[!is.na(cells$exemplar), , drop = FALSE]
  fam <- nf_fam()
  p <- ggplot2::ggplot(cells,
      ggplot2::aes(spatial_unit_label, programme_label)) +
    ggplot2::geom_tile(ggplot2::aes(fill = median_NES), colour = "white",
                       linewidth = 0.15) +
    ggplot2::geom_point(data = supported,
      ggplot2::aes(size = n_fdr_supported), shape = 21, fill = "white",
      colour = "grey12", stroke = 0.3, alpha = 0.9) +
    ggplot2::geom_tile(data = selected, fill = NA, colour = "black",
                       linewidth = 0.65) +
    ggplot2::geom_point(data = selected, shape = 21, fill = "white",
                        colour = "black", size = 2.5, stroke = 0.35) +
    ggplot2::geom_text(data = selected,
      ggplot2::aes(label = exemplar), family = fam, fontface = "bold",
      size = nf_sz(5.0), colour = "black") +
    nv_diverging(limits = c(-lim, lim), name = "Median NES\n(SUS - RES)",
                 breaks = c(-2, 0, 2)) +
    ggplot2::scale_size_area(
      name = "FDR-supported\nconstituent GO terms",
      breaks = c(1, 5, 10, 20), limits = c(1, max(supported$n_fdr_supported)),
      max_size = 2.5) +
    ggplot2::facet_grid(. ~ compartment, scales = "free_x", space = "free_x") +
    ggplot2::labs(x = NULL, y = NULL) + nf_theme_tile() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = NF_MIN_PT, angle = 55,
                                          hjust = 1, vjust = 1),
      axis.text.y = ggplot2::element_text(size = NF_MIN_PT, colour = "grey15"),
      strip.text.x = ggplot2::element_text(size = nf_pt(5.3), face = "bold"),
      panel.spacing.x = ggplot2::unit(1.4, "mm"),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = NF_MIN_PT),
      legend.text = ggplot2::element_text(size = NF_MIN_PT),
      plot.margin = ggplot2::margin(0.5, 1, 0.5, 1, "mm"))
  cells$atlas_summary <- paste0(
    "median NES across mapped constituent terms; dot size is the number with ",
    "BH FDR < 0.05; outlined 1-3 cells link to exemplar cards")
  write_csv_safe(cells, csv_path)
  nv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

f3a_state_map <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  inv <- nv_read_csv(repo_path(panel$primary_source))
  z <- f3a_adaptation_states(inv)
  programmes <- f3a_programmes()
  cols <- stats::setNames(programmes$colour, programmes$programme_short)
  z$programme_short <- factor(z$programme_short,
                              levels = programmes$programme_short)
  z$compartment <- factor(z$compartment,
    levels = c("Neuropil", "Soma", "Microglia ROI"))
  z$supported_any <- z$RES_CON_n_fdr > 0 | z$SUS_CON_n_fdr > 0
  lim <- ceiling(max(abs(c(z$RES_CON_median_NES, z$SUS_CON_median_NES))) /
                   0.25) * 0.25 + 0.5
  # Region labels sit at the panel edge, clear of the data. Both same-sign
  # quadrants read shared / parallel and both opposite-sign quadrants
  # divergent / opposing; the lower-left one sits up the left edge because the
  # corner itself holds data.
  ann <- data.frame(
    x = c(-1, 1, -1, 1, 0, 1, 0) * lim,
    y = c(1, 1, -0.5, -1, 1, 0, 0.07) * lim,
    label = c("divergent /\nopposing", "shared /\nparallel",
              "shared /\nparallel", "divergent /\nopposing",
              "SUS-\nassociated", "RES-associated", "little detectable"),
    angle = c(0, 0, 0, 0, 0, 90, 0),
    hjust = c(0, 1, 0, 1, 0.5, 0.5, 0.5),
    vjust = c(1, 1, 0.5, 0, 1, 0, 0))
  ex <- z[!is.na(z$exemplar), , drop = FALSE]
  # Each exemplar number takes the diagonal position just outside its ring
  # that has the fewest other points near it and stays inside the panel.
  off <- 0.36
  cand <- expand.grid(dx = c(-off, off), dy = c(off, -off))
  pick <- lapply(seq_len(nrow(ex)), function(i) {
    x <- ex$RES_CON_median_NES[[i]] + cand$dx
    y <- ex$SUS_CON_median_NES[[i]] + cand$dy
    crowd <- vapply(seq_along(x), function(k)
      sum((z$RES_CON_median_NES - x[[k]])^2 +
            (z$SUS_CON_median_NES - y[[k]])^2 < 0.3^2), numeric(1))
    crowd[abs(x) > lim - 0.3 | abs(y) > lim - 0.3] <- Inf
    k <- which.min(crowd)
    c(x[[k]], y[[k]])
  })
  ex$label_x <- vapply(pick, `[[`, numeric(1), 1L)
  ex$label_y <- vapply(pick, `[[`, numeric(1), 2L)
  fam <- nf_fam()
  p <- ggplot2::ggplot(z, ggplot2::aes(RES_CON_median_NES,
                                       SUS_CON_median_NES)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey75", linewidth = 0.25) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey75", linewidth = 0.25) +
    ggplot2::annotate("rect", xmin = -0.35, xmax = 0.35,
      ymin = -0.35, ymax = 0.35, fill = "grey95", colour = NA) +
    ggplot2::geom_text(data = ann,
      ggplot2::aes(x, y, label = label, angle = angle, hjust = hjust,
                   vjust = vjust),
      inherit.aes = FALSE, family = fam, size = nf_sz(4.6),
      colour = "grey42", lineheight = 0.88) +
    ggplot2::geom_point(ggplot2::aes(colour = programme_short,
                                    shape = compartment,
                                    alpha = supported_any),
                        size = 1.65, stroke = 0.35) +
    ggplot2::geom_point(data = ex,
      shape = 21, fill = NA, colour = "black", size = 3.0, stroke = 0.65) +
    ggplot2::geom_label(data = ex,
      ggplot2::aes(label_x, label_y, label = exemplar), colour = "black",
      family = fam, fontface = "bold", size = nf_sz(4.8),
      fill = scales::alpha("white", 0.85), linewidth = 0,
      label.padding = ggplot2::unit(0.2, "mm")) +
    ggplot2::scale_colour_manual(values = cols, name = "GO programme") +
    ggplot2::scale_shape_manual(values = c(16, 17, 15), name = "Compartment") +
    ggplot2::scale_alpha_manual(values = c("FALSE" = 0.28, "TRUE" = 0.88),
      guide = "none") +
    ggplot2::coord_equal(xlim = c(-lim, lim), ylim = c(-lim, lim),
                         expand = FALSE) +
    ggplot2::labs(x = "Median NES (RES - CON)",
                  y = "Median NES (SUS - CON)") + nf_theme(grid = "both") +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = NF_MIN_PT),
      axis.title = ggplot2::element_text(size = nf_pt(5.2)),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = NF_MIN_PT),
      legend.text = ggplot2::element_text(size = NF_MIN_PT),
      legend.key.height = ggplot2::unit(2.4, "mm"),
      plot.margin = ggplot2::margin(0.5, 1, 0.5, 1, "mm"))
  write_csv_safe(z, csv_path)
  nv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

f3a_pattern_burden <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  inv <- nv_read_csv(repo_path(panel$primary_source))
  z <- f3a_adaptation_states(inv)
  patterns <- c("resilience-specific remodeling",
                "susceptibility-specific remodeling", "shared / parallel",
                "divergent / opposing", "little detectable adaptation")
  # The five columns are ~6 mm apart, narrower than any two-line label, so
  # the class names are slanted rather than stacked.
  short <- c("RES-specific", "SUS-specific", "Shared / parallel",
             "Divergent / opposing", "Little detectable")
  compartments <- c("Neuropil", "Soma", "Microglia ROI")
  grid <- expand.grid(compartment = compartments,
                      adaptation_pattern = patterns,
                      stringsAsFactors = FALSE)
  tab <- as.data.frame(table(z$compartment, z$adaptation_pattern),
                       stringsAsFactors = FALSE)
  names(tab) <- c("compartment", "adaptation_pattern", "n")
  out <- merge(grid, tab, by = c("compartment", "adaptation_pattern"),
               all.x = TRUE, sort = FALSE)
  out$n[is.na(out$n)] <- 0L
  totals <- aggregate(n ~ compartment, out, sum)
  out$total_programme_contexts <- totals$n[match(out$compartment,
                                                  totals$compartment)]
  out$fraction <- out$n / out$total_programme_contexts
  out$pattern_short <- short[match(out$adaptation_pattern, patterns)]
  out$pattern_short <- factor(out$pattern_short, levels = short)
  out$compartment <- factor(out$compartment, levels = rev(compartments))
  out$classification_rule <- unique(z$classification_rule)[[1]]
  cols <- c("#315B7D", "#B45F4B", "#708A75", "#7B6A91", "#D4D4D4")
  p <- ggplot2::ggplot(out, ggplot2::aes(pattern_short, compartment)) +
    ggplot2::geom_point(ggplot2::aes(size = fraction,
                                    colour = adaptation_pattern), alpha = 0.9) +
    ggplot2::geom_text(ggplot2::aes(label = n), family = nf_fam(),
                       size = nf_sz(4.7), colour = "black") +
    ggplot2::scale_colour_manual(values = stats::setNames(cols, patterns),
                                 guide = "none") +
    ggplot2::scale_size_area(max_size = 7, limits = c(0, 1),
      breaks = c(0.25, 0.5, 0.75), labels = scales::percent,
      name = "Fraction") +
    ggplot2::labs(x = NULL, y = NULL) + nf_theme(grid = "none") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = NF_MIN_PT, angle = 35,
                                          hjust = 1, vjust = 1),
      axis.text.y = ggplot2::element_text(size = NF_MIN_PT),
      axis.ticks = ggplot2::element_blank(),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = NF_MIN_PT),
      legend.text = ggplot2::element_text(size = NF_MIN_PT),
      legend.key.width = ggplot2::unit(3, "mm"),
      legend.box.spacing = ggplot2::unit(0.5, "mm"),
      plot.margin = ggplot2::margin(1, 0.5, 0.5, 0.5, "mm"))
  write_csv_safe(out, csv_path)
  nv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

f3a_card_data <- function(inventory, exemplar_id, n_terms = 3L) {
  ex <- f3a_exemplars()
  ex <- ex[ex$exemplar == exemplar_id, , drop = FALSE]
  if (nrow(ex) != 1L) stop("Unknown Figure 3 exemplar: ", exemplar_id,
                           call. = FALSE)
  z <- f3a_programme_terms(inventory)
  z <- z[z$programme_id == ex$programme_id & z$dataset == ex$dataset &
           z$spatial_unit == ex$spatial_unit, , drop = FALSE]
  sr <- z[z$contrast == "SUS - RES", , drop = FALSE]
  sr <- sr[order(sr$BH_FDR, -abs(sr$NES), sr$GO_ID), , drop = FALSE]
  chosen <- unique(c(ex$term_id, sr$GO_ID))
  chosen <- utils::head(chosen, n_terms)
  out <- z[z$GO_ID %in% chosen &
             z$contrast %in% c("RES - CON", "SUS - CON", "SUS - RES"),
           , drop = FALSE]
  if (nrow(out) != 3L * length(chosen) ||
      anyDuplicated(out[c("GO_ID", "contrast")]))
    stop("Exemplar card lacks an exact three-contrast term set.", call. = FALSE)
  out$term_order <- match(out$GO_ID, chosen)
  out$curve_term <- out$GO_ID == ex$term_id
  out$exemplar <- exemplar_id
  out$location_label <- ex$location_label
  out
}

f3a_card_nes_limit <- local({
  cache <- NULL
  function(inventory) {
    if (!is.null(cache)) return(cache)
    z <- do.call(rbind, lapply(1:3, function(i) f3a_card_data(inventory, i)))
    cache <<- ceiling(max(abs(z$NES), na.rm = TRUE) / 0.25) * 0.25
    cache
  }
})

f3a_exemplar_card <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  inv <- nv_read_csv(repo_path(panel$primary_source))
  idx <- as.integer(panel$exemplar)
  z <- f3a_card_data(inv, idx)
  ex <- f3a_exemplars()[f3a_exemplars()$exemplar == idx, , drop = FALSE]
  programmes <- f3a_programmes()
  prog <- programmes[programmes$programme_id == ex$programme_id, , drop = FALSE]
  contrast_order <- c("RES - CON", "SUS - CON", "SUS - RES")
  z$contrast <- factor(z$contrast, levels = contrast_order,
                       labels = c("R-C", "S-C", "S-R"))
  desc <- unique(z[c("GO_ID", "GO_description", "term_order", "curve_term")])
  desc <- desc[order(desc$term_order), , drop = FALSE]
  desc$term_label <- vapply(desc$GO_description, function(x)
    paste(utils::head(strwrap(x, width = 24L), 2L), collapse = "\n"),
    character(1))
  desc$term_label[desc$curve_term] <- paste0("Curve: ", desc$term_label[desc$curve_term])
  z$term_label <- desc$term_label[match(z$GO_ID, desc$GO_ID)]
  z$term_label <- factor(z$term_label, levels = rev(desc$term_label))
  z$supported <- is.finite(z$BH_FDR) & z$BH_FDR < 0.05
  lim <- f3a_card_nes_limit(inv)
  title <- paste0(idx, "  ", ex$location_label, "\n", prog$programme_label)
  p <- ggplot2::ggplot(z, ggplot2::aes(contrast, term_label)) +
    ggplot2::geom_tile(ggplot2::aes(fill = NES), colour = "white",
                       linewidth = 0.2) +
    ggplot2::geom_tile(data = z[z$supported, , drop = FALSE], fill = NA,
                       colour = "black", linewidth = 0.55) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", NES)),
                       family = nf_fam(), size = nf_sz(4.8), colour = "grey10") +
    nv_diverging(limits = c(-lim, lim), name = "NES", breaks = c(-2, 0, 2)) +
    ggplot2::labs(title = title, x = NULL, y = NULL,
                  caption = "outlined = constituent GO FDR < 0.05") +
    nf_theme_tile() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = nf_pt(5.4), face = "bold",
                                         lineheight = 0.95),
      plot.caption = ggplot2::element_text(size = NF_MIN_PT, colour = "grey45"),
      axis.text.x = ggplot2::element_text(size = NF_MIN_PT, face = "bold"),
      axis.text.y = ggplot2::element_text(size = NF_MIN_PT, lineheight = 0.85),
      legend.position = "none",
      plot.margin = ggplot2::margin(0.5, 1, 0.5, 1, "mm"))
  write_csv_safe(z, csv_path)
  nv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

f3a_curve <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  idx <- as.integer(panel$exemplar)
  ex <- f3a_exemplars()[f3a_exemplars()$exemplar == idx, , drop = FALSE]
  src <- repo_path("source_data", "pRoteomics", "figure_03_adaptation")
  selection <- nv_read_csv(file.path(src, "selection.csv"))
  curves <- nv_read_csv(file.path(src, "running_enrichment_curves.csv"))
  s <- selection[selection$GO_ID == ex$term_id &
                   selection$dataset == ex$dataset &
                   selection$spatial_unit == ex$spatial_unit, , drop = FALSE]
  z <- curves[curves$GO_ID == ex$term_id, , drop = FALSE]
  z <- z[order(z$rank), , drop = FALSE]
  if (nrow(s) != 1L || nrow(z) != s$n_ranked || sum(z$peak) != 1L)
    stop("Frozen enrichment curve does not match exemplar ", idx, ".",
         call. = FALSE)
  programmes <- f3a_programmes()
  prog <- programmes[programmes$programme_id == ex$programme_id, , drop = FALSE]
  ticks <- z[z$hit, , drop = FALSE]
  curve <- ggplot2::ggplot(z, ggplot2::aes(rank, running_ES)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey72", linewidth = 0.25) +
    ggplot2::geom_line(colour = prog$colour, linewidth = 0.55) +
    ggplot2::geom_vline(xintercept = s$peak_rank, colour = "grey55",
                        linewidth = 0.25, linetype = "22") +
    ggplot2::scale_x_continuous(limits = c(1, s$n_ranked),
      breaks = c(1, s$n_ranked), labels = c("SUS", "RES"),
      expand = c(0, 0)) +
    ggplot2::labs(title = paste0(s$GO_description, "  (", s$GO_ID, ")"),
      subtitle = sprintf("NES %.2f; FDR %.2g", s$NES, s$BH_FDR),
      x = NULL, y = "Running ES") + nf_theme(grid = "none") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = nf_pt(5.2), face = "bold"),
      plot.subtitle = ggplot2::element_text(size = NF_MIN_PT, colour = "grey40"),
      axis.text = ggplot2::element_text(size = NF_MIN_PT),
      axis.title.y = ggplot2::element_text(size = nf_pt(5.0)),
      plot.margin = ggplot2::margin(0.5, 1, 0, 1, "mm"))
  rug <- ggplot2::ggplot(ticks, ggplot2::aes(rank)) +
    ggplot2::geom_segment(ggplot2::aes(xend = rank, y = 0, yend = 1),
                          colour = prog$colour, linewidth = 0.13) +
    ggplot2::scale_x_continuous(limits = c(1, s$n_ranked), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::theme_void(base_family = nf_fam()) +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 1, 0.5, 1, "mm"))
  p <- patchwork::wrap_plots(curve, rug, ncol = 1, heights = c(0.82, 0.18))
  z$dataset <- ex$dataset
  z$spatial_unit <- ex$spatial_unit
  z$GO_description <- s$GO_description
  z$NES <- s$NES
  z$BH_FDR <- s$BH_FDR
  z$selection_rule <- s$selection_rule
  write_csv_safe(z, csv_path)
  nv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

f3a_protein_limit <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    ex <- f3a_exemplars()
    z <- nv_read_csv(repo_path("source_data", "pRoteomics",
                               "figure_03_adaptation",
                               "protein_zoom_values.csv"))
    z <- z[z$GO_ID %in% ex$term_id, , drop = FALSE]
    cache <<- ceiling(max(abs(z$log2FC), na.rm = TRUE) / 0.05) * 0.05
    cache
  }
})

f3a_proteins <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  idx <- as.integer(panel$exemplar)
  ex <- f3a_exemplars()[f3a_exemplars()$exemplar == idx, , drop = FALSE]
  z <- nv_read_csv(repo_path(panel$primary_source))
  z <- z[z$GO_ID == ex$term_id & z$dataset == ex$dataset &
           z$spatial_unit == ex$spatial_unit, , drop = FALSE]
  if (!nrow(z) || nrow(z) != 3L * length(unique(z$gene)))
    stop("Frozen leading-edge protein source is incomplete for exemplar ", idx,
         ".", call. = FALSE)
  result <- f9_protein_zoom_plot(
    z, f3a_protein_limit(), show_legend = idx == 3L,
    contrast_raw = c("RES - CON", "SUS - CON", "SUS - RES"),
    contrast_display = c("RES-CON", "SUS-CON", "SUS-RES"),
    axis_breaks = c(-0.8, 0, 0.8), panel_scope = "three exemplar",
    fdr_min = min(z$BH_FDR, na.rm = TRUE), mark_significant = TRUE,
    legend_position = if (idx == 3L) "bottom" else "inside")
  result$plot <- result$plot +
    ggplot2::labs(title = "Top contributing proteins") +
    ggplot2::theme(plot.title = ggplot2::element_text(
      size = nf_pt(5.2), face = "bold"))
  result$source_data$GO_ID <- ex$term_id
  result$source_data$dataset <- ex$dataset
  result$source_data$spatial_unit <- ex$spatial_unit
  result$source_data$selection_note <- paste0(
    "up to seven leading-edge genes ranked by absolute stored GSEA rank ",
    "statistic; protein effects are descriptive and not independent validation")
  write_csv_safe(result$source_data, csv_path)
  nv_save_panel(result$plot, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}
