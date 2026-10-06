#!/usr/bin/env Rscript
# Figure 3 appendix candidate. Reuses the exact f9_gsea_curve_plot renderer.
# It reads frozen pRoteomics evidence and does not recompute enrichment.

source(file.path("R", "paths.R"))
source(repo_path("R", "panels", "nature_v2_figure_utils.R"))
source(repo_path("R", "panels", "nature_final_v7_panels.R"))
source(repo_path("R", "panels", "spatial_grammar_utils.R"))
source(repo_path("R", "panels", "final_truth_v9_panels.R"))
for (pkg in c("ggplot2", "patchwork", "svglite", "ragg")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("Required plotting package is unavailable: ", pkg, call. = FALSE)
}
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L || (length(args) == 1L && !nzchar(args[[1]])))
  stop("Usage: Rscript figures/figure_03_supplementary_go.R [output_directory]",
       call. = FALSE)
out <- if (length(args)) args[[1]] else repo_path(
  "results", "figures", "manuscript_candidates",
  "supplementary_go_pathways_appendix")
if (file.exists(out)) stop("Candidate output already exists: ", out,
                           call. = FALSE)
src <- repo_path("source_data", "pRoteomics",
                 "supplementary_go_pathways_candidate")
names <- c("selection.csv", "running_enrichment_curves.csv",
           "regional_exact_term_inventory.csv", "input_manifest.csv")
mf_path <- repo_path("source_data", "pRoteomics", "manifest.csv")
mf <- utils::read.csv(mf_path, stringsAsFactors = FALSE)
for (name in names) {
  f <- file.path(src, name)
  row <- mf[mf$publication_id == "supplementary_go_pathways_candidate" &
              basename(mf$exported_file) == name, , drop = FALSE]
  if (!file.exists(f) || nrow(row) != 1L ||
      !identical(unname(tools::sha256sum(f)), row$sha256[[1]]))
    stop("Frozen candidate source missing or hash mismatch: ", name,
         call. = FALSE)
}
protein_src <- repo_path("source_data", "pRoteomics",
                         "supplementary_go_protein_zooms_candidate")
for (name in c("protein_zoom_values.csv", "protein_input_manifest.csv")) {
  f <- file.path(protein_src, name)
  row <- mf[mf$publication_id == "supplementary_go_protein_zooms_candidate" &
              basename(mf$exported_file) == name, , drop = FALSE]
  if (!file.exists(f) || nrow(row) != 1L ||
      !identical(unname(tools::sha256sum(f)), row$sha256[[1]]))
    stop("Frozen candidate protein source missing or hash mismatch: ", name,
         call. = FALSE)
}
sel <- utils::read.csv(file.path(src, names[[1]]), stringsAsFactors = FALSE)
curve <- utils::read.csv(file.path(src, names[[2]]), stringsAsFactors = FALSE)
regional <- utils::read.csv(file.path(src, names[[3]]), stringsAsFactors = FALSE)
protein <- utils::read.csv(file.path(protein_src, "protein_zoom_values.csv"),
                           stringsAsFactors = FALSE)
lead_path <- repo_path("source_data", "pRoteomics",
                       "supplementary_selection_inventories",
                       "leading_edge_protein_inventory.csv")
lead_row <- mf[basename(mf$exported_file) == basename(lead_path) &
                 mf$publication_id == "supplementary_selection_inventories", ,
               drop = FALSE]
if (nrow(lead_row) != 1L ||
    !identical(unname(tools::sha256sum(lead_path)), lead_row$sha256[[1]]))
  stop("Frozen leading-edge inventory missing or hash mismatch.", call. = FALSE)
if (nrow(sel) != 4L || !identical(sel$display_order, 1:4) ||
    anyDuplicated(sel$GO_ID) ||
    anyNA(curve[c("rank", "running_ES", "hit")]) ||
    !all(sel$GO_ID %in% curve$GO_ID) ||
    !all(sel$GO_ID %in% regional$GO_ID))
  stop("Frozen candidate source schema or membership mismatch.", call. = FALSE)
if (anyDuplicated(regional[c("GO_ID", "dataset", "spatial_unit", "contrast")]))
  stop("Duplicate exact-term regional cell.", call. = FALSE)

contrast_order <- c("RES - CON", "SUS - CON", "SUS - RES")
unit_order <- data.frame(
  dataset = c(rep("neuron_neuropil", 10), rep("neuron_soma", 4),
              rep("microglia", 4)),
  spatial_unit = c("CA1_slm", "CA1_so", "CA1_sr", "CA2_slm", "CA2_so",
                   "CA2_sr", "CA3_so", "CA3_sr", "DG_mo", "DG_po",
                   "CA1_sp", "CA2_sp", "CA3_sp", "DG_sg",
                   "CA1", "CA2", "CA3", "DG"),
  stringsAsFactors = FALSE)
unit_order$unit_key <- paste(unit_order$dataset, unit_order$spatial_unit,
                             sep = "|")
if (!all(unique(paste(regional$dataset, regional$spatial_unit, sep = "|")) %in%
         unit_order$unit_key))
  stop("Regional source contains an unregistered spatial unit.", call. = FALSE)

display <- c("Translation", "Chromatin organization",
             "Neuron projection development", "Autophagy")

# The strips and the regional matrix use the manuscript's fixed NES colour
# limit (config/manuscript_palette.yml diverging_limits$nes, palette v3.2), the
# one Figure 3 uses (Extended Data 6 follows when it is re-rendered), so a
# colour means the same NES here as there. A value beyond it takes full colour;
# the strips print every NES and the regional source data keep them uncapped.
nes_limit <- nv_diverging_limit("nes")
if (anyNA(regional$NES))
  stop("Regional exact-term inventory carries a missing NES.", call. = FALSE)
make_curve <- function(i) {
  s <- sel[i, ]
  z <- curve[curve$GO_ID == s$GO_ID, , drop = FALSE]
  z <- z[order(z$rank), , drop = FALSE]
  if (nrow(z) != s$n_ranked || !identical(z$rank, seq_len(nrow(z))) ||
      sum(z$peak) != 1L ||
      abs(z$running_ES[which(z$peak)] - s$enrichmentScore) > 1e-10)
    stop("Running curve/selection disagreement for ", s$GO_ID, call. = FALSE)
  tri <- regional[regional$GO_ID == s$GO_ID &
                    regional$dataset == s$dataset &
                    regional$spatial_unit == s$spatial_unit, , drop = FALSE]
  if (nrow(tri) != 3L ||
      !setequal(tri$contrast, contrast_order))
    stop("Three contrasts missing for ", s$GO_ID, call. = FALSE)
  tri$GSEA_FDR <- tri$BH_FDR
  prog <- data.frame(dataset = s$dataset, unit = s$spatial_unit,
                     term = s$GO_ID, label = display[[i]],
                     accent = unname(nv_dataset_colours()[[s$dataset]]),
                     stringsAsFactors = FALSE)
  ev <- list(N = s$n_ranked, runes = z$running_ES, hits = z$hit,
             peak = s$peak_rank, ES = s$enrichmentScore,
             NES = s$NES, FDR = s$BH_FDR)
  f9_gsea_curve_plot(prog, ev, tri, nes_limit, letters[[i]],
                     x_expand = ggplot2::expansion(mult = 0.02),
                     contrast_short_labels = c("R-C", "S-C", "S-R"),
                     column_role = paste0(
                       "supplementary exact-term curve and three-contrast ",
                       "NES strip; paired with the separate spatial matrix"))
}

rendered <- lapply(seq_len(nrow(sel)), make_curve)
plots <- lapply(rendered, `[[`, "plot")
curves_plot <- (plots[[1]] | plots[[2]]) / (plots[[3]] | plots[[4]])

if (nrow(protein) != 4L * 7L * 3L ||
    anyDuplicated(protein[c("GO_ID", "gene", "contrast")]) ||
    anyNA(protein[c("ProteinGroupID", "log2FC", "BH_FDR")]) ||
    !setequal(protein$GO_ID, sel$GO_ID) ||
    any(protein$BH_FDR < 0.05))
  stop("Candidate protein source is incomplete or needs significance marking.",
       call. = FALSE)
protein_limit <- ceiling(max(abs(protein$log2FC)) / 0.05) * 0.05
make_protein <- function(i) {
  z <- protein[protein$GO_ID == sel$GO_ID[[i]], , drop = FALSE]
  if (nrow(z) != 21L || length(unique(z$gene)) != 7L ||
      !setequal(z$contrast, contrast_order))
    stop("Candidate protein rows do not match Figure 3 selection layout.",
         call. = FALSE)
  f9_protein_zoom_plot(
    z, protein_limit, show_legend = i == 4L,
    contrast_raw = contrast_order,
    contrast_display = c("RES-CON", "SUS-CON", "SUS-RES"),
    axis_breaks = c(-1.5, 0, 1.5), panel_scope = "four",
    fdr_min = min(protein$BH_FDR, na.rm = TRUE))
}
protein_rendered <- lapply(seq_len(nrow(sel)), make_protein)
protein_plots <- lapply(protein_rendered, `[[`, "plot")
paired_page <- function(a, b) {
  left <- patchwork::wrap_plots(plots[[a]], protein_plots[[a]], ncol = 1,
                                heights = c(0.56, 0.44))
  right <- patchwork::wrap_plots(plots[[b]], protein_plots[[b]], ncol = 1,
                                 heights = c(0.56, 0.44))
  left | right
}
pairs_1 <- paired_page(1L, 2L)
pairs_2 <- paired_page(3L, 4L)

regional$unit_key <- paste(regional$dataset, regional$spatial_unit, sep = "|")
regional$unit_index <- match(regional$unit_key, unit_order$unit_key)
regional$display_order <- match(regional$GO_ID, sel$GO_ID)
regional$term <- factor(display[regional$display_order], levels = rev(display))
regional$contrast <- factor(regional$contrast, levels = contrast_order)
regional$supported <- is.finite(regional$BH_FDR) & regional$BH_FDR < 0.05
if (nrow(regional) != 4L * 18L * 3L || anyNA(regional$unit_index) ||
    anyNA(regional$display_order))
  stop("Regional exact-term inventory is not a complete 4 × 18 × 3 grid.",
       call. = FALSE)
unit_labels <- c("CA1 SLM", "CA1 SO", "CA1 SR", "CA2 SLM", "CA2 SO",
                 "CA2 SR", "CA3 SO", "CA3 SR", "DG MO", "DG PO",
                 "CA1 SP", "CA2 SP", "CA3 SP", "DG SG",
                 "CA1", "CA2", "CA3", "DG")
regions_plot <- ggplot2::ggplot(regional,
                                ggplot2::aes(unit_index, term, fill = NES)) +
  ggplot2::geom_tile(colour = nv_tile_border(),
                     linewidth = nv_lw("tile_border_pt")) +
  ggplot2::geom_point(data = regional[regional$supported, , drop = FALSE],
                      colour = "grey10", size = 0.45) +
  ggplot2::geom_vline(xintercept = c(10.5, 14.5),
                      linewidth = nv_lw("reference_pt"),
                      colour = "grey55") +
  ggplot2::facet_grid(contrast ~ ., switch = "y") +
  nv_diverging(measure = "nes", name = "GO-term NES") +
  ggplot2::scale_x_continuous(breaks = 1:18, labels = unit_labels,
                              limits = c(0.5, 18.5), expand = c(0, 0)) +
  ggplot2::labs(x = NULL, y = NULL) + nf_theme_tile() +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    panel.spacing.y = ggplot2::unit(3, "mm"),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1,
                                        size = NF_MIN_PT),
    axis.text.y = ggplot2::element_text(size = NF_MIN_PT),
    strip.background = ggplot2::element_rect(fill = "grey95", colour = NA),
    strip.text.y.left = ggplot2::element_text(angle = 0, face = "bold",
                                              size = NF_MIN_PT),
    legend.position = "bottom",
    # wide enough for the five labels of the fixed NES scale (<= -2 ... >= 2),
    # with the title above the bar so it cannot run into the end label
    legend.key.width = ggplot2::unit(14, "mm"),
    legend.title.position = "top")

dir.create(out, recursive = TRUE, showWarnings = FALSE)
panel_stems <- c("translation", "chromatin", "neuron_projection", "autophagy")
for (i in seq_along(rendered)) {
  stem <- paste0("supp_curve_", panel_stems[[i]])
  nv_save_panel(rendered[[i]]$plot, file.path(out, paste0(stem, ".svg")),
                85, 77)
  utils::write.csv(rendered[[i]]$source_data,
                   file.path(out, paste0(stem, "_source_data.csv")),
                   row.names = FALSE)
  protein_stem <- paste0("supp_protein_", panel_stems[[i]])
  nv_save_panel(protein_rendered[[i]]$plot,
                file.path(out, paste0(protein_stem, ".svg")), 85, 61)
  utils::write.csv(protein_rendered[[i]]$source_data,
                   file.path(out, paste0(protein_stem, "_source_data.csv")),
                   row.names = FALSE)
}
save_plot <- function(plot, stem, width_mm, height_mm) {
  svg <- file.path(out, paste0(stem, ".svg"))
  pdf <- file.path(out, paste0(stem, ".pdf"))
  png <- file.path(out, paste0(stem, ".png"))
  ggplot2::ggsave(svg, plot, device = svglite::svglite,
                  width = width_mm, height = height_mm, units = "mm", bg = "white")
  ggplot2::ggsave(pdf, plot, device = grDevices::cairo_pdf,
                  width = width_mm, height = height_mm, units = "mm", bg = "white")
  ggplot2::ggsave(png, plot, device = ragg::agg_png,
                  width = width_mm, height = height_mm, units = "mm", dpi = 300,
                  bg = "white")
}
save_plot(curves_plot, "supplementary_go_pathways_curves", 183, 172)
save_plot(pairs_1, "supplementary_go_pathways_pairs_1", 183, 150)
save_plot(pairs_2, "supplementary_go_pathways_pairs_2", 183, 150)
save_plot(regions_plot, "supplementary_go_pathways_regions", 183, 170)
utils::write.csv(sel, file.path(out, "display_selection.csv"), row.names = FALSE)
utils::write.csv(regional, file.path(out, "regional_source_data.csv"),
                 row.names = FALSE)
lead <- utils::read.csv(lead_path, stringsAsFactors = FALSE)
lead <- lead[lead$GO_ID %in% sel$GO_ID,
             c("dataset", "spatial_unit", "contrast", "GO_ID",
               "GO_description", "NES", "BH_FDR", "gene",
               "leading_edge_size_of_term"), drop = FALSE]
if (!nrow(lead) || anyNA(lead$gene))
  stop("Selected-term leading-edge inventory is absent or malformed.",
       call. = FALSE)
utils::write.csv(lead, file.path(out, "selected_term_leading_edges.csv"),
                 row.names = FALSE)
cat("Rendered supplementary candidate in ", out, "\n", sep = "")
