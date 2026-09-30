#!/usr/bin/env Rscript
# Figure 3 d-i renderer applied to every supported SUS-RES GO term in the
# seven atlas themes. This only reads frozen publication source data.
source(file.path("R", "paths.R"))
source(repo_path("R", "panels", "nature_v2_figure_utils.R"))
source(repo_path("R", "panels", "nature_final_v7_panels.R"))
source(repo_path("R", "panels", "spatial_grammar_utils.R"))
source(repo_path("R", "panels", "final_truth_v9_panels.R"))
for (pkg in c("ggplot2", "patchwork", "svglite"))
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("Required plotting package unavailable: ", pkg, call. = FALSE)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L || (length(args) == 1L && !nzchar(args[[1]])))
  stop("Usage: Rscript figures/figure_03_go_atlas_appendix.R [output_directory]",
       call. = FALSE)
out <- if (length(args)) args[[1]] else repo_path(
  "results", "figures", "manuscript_candidates", "figure_03_go_atlas_appendix_v4")
if (file.exists(out)) stop("Output already exists: ", out, call. = FALSE)
src <- repo_path("source_data", "pRoteomics", "figure_03_go_atlas_appendix")
files <- c("theme_term_index.csv", "selected_contexts.csv",
           "running_enrichment_curves.csv", "regional_exact_term_inventory.csv",
           "protein_zoom_values.csv", "input_manifest.csv")
mf <- read.csv(repo_path("source_data", "pRoteomics", "manifest.csv"),
               stringsAsFactors = FALSE)
for (name in files) {
  f <- file.path(src, name)
  row <- mf[mf$publication_id == "figure_03_go_atlas_appendix" &
              basename(mf$exported_file) == name, , drop = FALSE]
  if (!file.exists(f) || nrow(row) != 1L ||
      !identical(unname(tools::sha256sum(f)), row$sha256[[1]]))
    stop("Frozen atlas source missing or hash mismatch: ", name,
         call. = FALSE)
}
index <- read.csv(file.path(src, files[[1]]), stringsAsFactors = FALSE)
sel <- read.csv(file.path(src, files[[2]]), stringsAsFactors = FALSE)
curves <- read.csv(file.path(src, files[[3]]), stringsAsFactors = FALSE)
regional <- read.csv(file.path(src, files[[4]]), stringsAsFactors = FALSE)
protein <- read.csv(file.path(src, files[[5]]), stringsAsFactors = FALSE)
sup_path <- repo_path("source_data", "pRoteomics",
                      "supplementary_selection_inventories",
                      "pathway_enrichment_inventory_fdr_supported.csv")
sup_row <- mf[mf$publication_id == "supplementary_selection_inventories" &
                basename(mf$exported_file) == basename(sup_path), , drop = FALSE]
if (nrow(sup_row) != 1L ||
    !identical(unname(tools::sha256sum(sup_path)), sup_row$sha256[[1]]))
  stop("Frozen supported inventory missing or hash mismatch.", call. = FALSE)
sup <- read.csv(sup_path, stringsAsFactors = FALSE)
sup <- sup[sup$contrast == "SUS - RES" &
             sup$theme_claim_eligible %in% TRUE, , drop = FALSE]
seven <- c("rna_processing_splicing_rnp", "ribosome_translation",
           "chromatin_organization", "mitochondrial_respiration_oxphos",
           "synaptic_signaling_vesicle", "neuron_projection_development",
           "autophagy_lysosome_endosome")
expected <- unique(do.call(rbind, lapply(seq_len(nrow(sup)), function(i) {
  ids <- strsplit(sup$theme_id[[i]], ";", fixed = TRUE)[[1]]
  data.frame(theme_id = ids[ids %in% seven], GO_ID = sup$GO_ID[[i]])
})))
if (anyDuplicated(index[c("theme_id", "GO_ID")]) ||
    !setequal(paste(index$theme_id, index$GO_ID),
              paste(expected$theme_id, expected$GO_ID)) ||
    anyDuplicated(sel$GO_ID) || !setequal(index$GO_ID, sel$GO_ID) ||
    !setequal(curves$GO_ID, sel$GO_ID) ||
    !setequal(protein$GO_ID, sel$GO_ID))
  stop("Atlas term/theme coverage mismatch.", call. = FALSE)
contrast_order <- c("RES - CON", "SUS - CON", "SUS - RES")
f3 <- read.csv(repo_path("source_data", "pRoteomics", "figure_03",
                         "figure_03d_source_data.csv"),
               stringsAsFactors = FALSE)
nes_limit <- f3$shared_NES_strip_limit[[1]]
if (nrow(f3) != 1L || !is.finite(nes_limit) ||
    any(!is.finite(regional$NES)))
  stop("Figure 3/atlas NES source is malformed.", call. = FALSE)
# The full atlas spans a wider NES range than the three printed exemplars.
# Keep one common scale across every appendix strip without clipping values.
nes_limit <- max(nes_limit,
                 ceiling(max(abs(regional$NES)) / 0.05) * 0.05)
regional_key <- paste(regional$GO_ID, regional$dataset,
                      regional$spatial_unit, regional$contrast)
if (anyDuplicated(regional_key) ||
    anyDuplicated(protein[c("GO_ID", "gene", "contrast")]))
  stop("Duplicate regional or protein source row.", call. = FALSE)
short_label <- function(x) {
  x <- gsub("[- ]+", " ", x)
  lines <- strwrap(x, width = 30L)
  if (length(lines) > 2L)
    lines <- c(lines[[1]], paste0(substr(lines[[2]], 1L, 26L), "..."))
  paste(lines, collapse = "\n")
}
dir.create(out, recursive = TRUE, showWarnings = FALSE)
write.csv(index, file.path(out, "theme_term_index.csv"), row.names = FALSE)
for (t in seq_along(seven)) {
  id <- seven[[t]]
  group <- index[index$theme_id == id, , drop = FALSE]
  slug <- sprintf("theme_%02d_%s", t, id)
  base <- file.path(out, slug)
  fig_dir <- file.path(base, "figures")
  data_dir <- file.path(base, "data")
  dir.create(fig_dir, recursive = TRUE)
  dir.create(data_dir, recursive = TRUE)
  write.csv(group, file.path(data_dir, "theme_term_index.csv"), row.names = FALSE)
  write.csv(regional[regional$GO_ID %in% group$GO_ID, , drop = FALSE],
            file.path(data_dir, "regional_exact_term_inventory.csv"),
            row.names = FALSE)
  pdf_path <- file.path(fig_dir, paste0(slug, "_paired_pages.pdf"))
  grDevices::cairo_pdf(pdf_path, width = 85 / 25.4, height = 170 / 25.4,
                       onefile = TRUE)
  tryCatch({
    lim <- ceiling(max(abs(protein$log2FC[protein$GO_ID %in% group$GO_ID])) /
                     0.05) * 0.05
    axis_breaks <- c(-round(lim * 0.8, 1), 0, round(lim * 0.8, 1))
    for (j in seq_len(nrow(group))) {
      s <- group[j, ]
      id_term <- s$GO_ID
      z <- curves[curves$GO_ID == id_term, , drop = FALSE]
      z <- z[order(z$rank), , drop = FALSE]
      if (nrow(z) != s$n_ranked ||
          !identical(z$rank, seq_len(nrow(z))) ||
          sum(z$peak) != 1L || sum(z$hit) != s$setSize ||
          abs(z$running_ES[which(z$peak)] - s$enrichmentScore) > 1e-10)
        stop("Curve/source mismatch: ", id_term, call. = FALSE)
      tri <- regional[regional$GO_ID == id_term &
                        regional$dataset == s$dataset &
                        regional$spatial_unit == s$spatial_unit, , drop = FALSE]
      if (nrow(tri) != 3L || !setequal(tri$contrast, contrast_order))
        stop("Three-contrast strip incomplete: ", id_term, call. = FALSE)
      tri$GSEA_FDR <- tri$BH_FDR
      prog <- data.frame(dataset = s$dataset, unit = s$spatial_unit,
                         term = id_term, label = short_label(s$GO_description),
                         accent = unname(nv_dataset_colours()[[s$dataset]]))
      ev <- list(N = s$n_ranked, runes = z$running_ES, hits = z$hit,
                 peak = s$peak_rank, ES = s$enrichmentScore,
                 NES = s$NES, FDR = s$BH_FDR)
      curve_plot <- f9_gsea_curve_plot(
        prog, ev, tri, nes_limit, sprintf("%02d", j),
        x_expand = ggplot2::expansion(mult = 0.02),
        contrast_short_labels = c("R-C", "S-C", "S-R"),
        column_role = "Figure 3 d-f style; paired with protein panel below")
      pz <- protein[protein$GO_ID == id_term, , drop = FALSE]
      if (length(unique(pz$gene)) < 1L ||
          length(unique(pz$gene)) > 7L ||
          nrow(pz) != 3L * length(unique(pz$gene)) ||
          !setequal(pz$contrast, contrast_order) ||
          anyNA(pz[c("ProteinGroupID", "log2FC", "BH_FDR")]))
        stop("Protein zoom incomplete: ", id_term, call. = FALSE)
      prot_plot <- f9_protein_zoom_plot(
        pz, lim, show_legend = TRUE, contrast_raw = contrast_order,
        contrast_display = c("RES-CON", "SUS-CON", "SUS-RES"),
        axis_breaks = axis_breaks, panel_scope = paste(nrow(group), "theme"),
        fdr_min = min(protein$BH_FDR[protein$GO_ID %in% group$GO_ID]),
        mark_significant = TRUE, legend_position = "bottom")
      pair <- patchwork::wrap_plots(curve_plot$plot, prot_plot$plot,
                                    ncol = 1, heights = c(0.56, 0.44))
      print(pair)
      stem <- paste0(sprintf("%02d_", j), "_", gsub(":", "", id_term))
      ggplot2::ggsave(file.path(fig_dir, paste0(stem, ".svg")), pair,
                      device = svglite::svglite, width = 85, height = 170,
                      units = "mm", bg = "white")
      write.csv(z, file.path(data_dir, paste0(stem, "_curve.csv")),
                row.names = FALSE)
      write.csv(prot_plot$source_data,
                file.path(data_dir, paste0(stem, "_proteins.csv")),
                row.names = FALSE)
      write.csv(curve_plot$source_data,
                file.path(data_dir, paste0(stem, "_panel_source.csv")),
                row.names = FALSE)
    }
  }, finally = grDevices::dev.off())
  message("Rendered ", slug, ": ", nrow(group), " GO terms")
}
cat("Rendered Figure 3 GO atlas appendix in ", out, "\n", sep = "")
