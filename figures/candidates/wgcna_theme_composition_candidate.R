#!/usr/bin/env Rscript
# WGCNA module wheel with the GSEA themes each module carries (manuscript candidate).
# Reads one frozen version (WTC_VERSIONS) of the pRoteomics export wgcna_theme_composition_candidate
# and draws it; no
# composition, enrichment or module statistic is recomputed. Not promoted: candidate only.

source(file.path("R", "paths.R"))
source(repo_path("R", "panels", "proteomics_style.R"))
source(repo_path("R", "panels", "proteomics_panels_figure2.R"))
source(repo_path("R", "panels", "spatial_grammar_utils.R"))
source(repo_path("R", "panels", "proteomics_panels_main.R"))
source(repo_path("R", "panels", "figure3_adaptation_panels.R"))
source(repo_path("R", "panels", "wgcna_theme_composition_panels.R"))

WTC_W_MM <- 183
WTC_H_MM <- 168

wtc_render <- function(out, version = WTC_CURRENT) {
  id <- wtc_bundle_id(version)   # an unknown version fails before anything else
  for (pkg in c("ggplot2", "patchwork", "svglite", "ragg"))
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("Required plotting package is unavailable: ", pkg, call. = FALSE)
  if (file.exists(out)) stop("Candidate output already exists: ", out, call. = FALSE)
  x <- wtc_read(version)
  tag <- ggplot2::theme(plot.tag = ggplot2::element_text(size = hv_pt("panel_label_pt"), face = "bold",
                                                         family = hf_fam()))
  a <- wtc_wheel_plot(x) + ggplot2::labs(tag = "a") + tag
  key <- wtc_wheel_key(x)
  b <- wtc_matrix_plot(x) + ggplot2::labs(tag = "b") + tag
  c <- wtc_coherence_plot(x) + ggplot2::labs(tag = "c") + tag
  # the wheel and its key are freed from axis alignment, which would otherwise shrink the wheel
  # to the width left after the axis labels of b and c
  design <- c(patchwork::area(1, 1, 2, 1), patchwork::area(1, 2), patchwork::area(2, 2),
              patchwork::area(3, 1, 3, 2))
  sheet <- patchwork::free(a) + patchwork::free(key) + c + b +
    patchwork::plot_layout(design = design, widths = c(1.25, 1), heights = c(0.34, 0.74, 0.7))

  dir.create(out, recursive = TRUE)
  stem <- file.path(out, "wgcna_theme_composition")
  ggplot2::ggsave(paste0(stem, ".svg"), sheet, device = svglite::svglite,
                  width = WTC_W_MM, height = WTC_H_MM, units = "mm", bg = "white")
  ggplot2::ggsave(paste0(stem, ".pdf"), sheet, device = grDevices::cairo_pdf,
                  width = WTC_W_MM, height = WTC_H_MM, units = "mm", bg = "white")
  ggplot2::ggsave(paste0(stem, ".png"), sheet, device = ragg::agg_png,
                  width = WTC_W_MM, height = WTC_H_MM, units = "mm", dpi = 300, bg = "white")

  cells <- wtc_wheel_cells(x)
  utils::write.csv(cells[c("dataset", "supermodule_id", "module_id", "wheel_order", "ring", "theme_id", "value")],
                   file.path(out, "source_data_a_wheel.csv"), row.names = FALSE)
  m <- wtc_matrix_data(x)
  utils::write.csv(m[c("dataset", "level", "entity_id", "member_modules", "theme_id", "n_universe", "n_theme",
                       "n_entity", "n_overlap", "frac_theme_in_entity", "fold_enrichment", "odds_ratio",
                       "odds_ratio_ci_low", "odds_ratio_ci_high", "BH_FDR", "composition_supported")],
                   file.path(out, "source_data_b_composition.csv"), row.names = FALSE)
  cd <- wtc_coherence_data(x)
  utils::write.csv(cd$tiles[c("dataset", "theme_id", "n_theme", "n_carriers", "carriers",
                              "share_captured_by_carriers", "composition_class",
                              paste0("top_carrier_supported_in__", names(WTC_SENSITIVITIES)))],
                   file.path(out, "source_data_c_theme_coherence.csv"), row.names = FALSE)

  frozen <- repo_path("source_data", "pRoteomics", id)   # this repository's frozen import of that version
  inputs <- c(file.path(frozen, WTC_FILES),
              repo_path("source_data", "pRoteomics", "manifest.csv"),
              repo_path("config", "manuscript_palette.yml"),
              repo_path("R", "panels", c("proteomics_style.R", "proteomics_panels_figure2.R",
                                         "spatial_grammar_utils.R", "proteomics_panels_main.R",
                                         "figure3_adaptation_panels.R", "wgcna_theme_composition_panels.R")),
              repo_path("figures", "candidates", "wgcna_theme_composition_candidate.R"))
  rel <- substring(normalizePath(inputs, winslash = "/"), nchar(normalizePath(repo_path(), winslash = "/")) + 2L)
  utils::write.csv(data.frame(path = rel, sha256 = unname(tools::sha256sum(inputs))),
                   file.path(out, "render_inputs.csv"), row.names = FALSE)
  files <- setdiff(list.files(out, full.names = TRUE), file.path(out, "render_manifest.csv"))
  utils::write.csv(data.frame(file = basename(files), sha256 = unname(tools::sha256sum(files))),
                   file.path(out, "render_manifest.csv"), row.names = FALSE)
  invisible(sheet)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  version <- WTC_CURRENT
  if (length(args) && grepl("^v[0-9]+$", args[[1]])) {
    version <- args[[1]]
    args <- args[-1L]
  }
  if (length(args) > 1L || (length(args) == 1L && !nzchar(args[[1]])))
    stop("Usage: Rscript figures/candidates/wgcna_theme_composition_candidate.R [v1|v2] [new_output_directory]",
         call. = FALSE)
  wtc_render(if (length(args)) args[[1]] else
    repo_path("results", "figures", "manuscript_candidates", paste0("wgcna_theme_composition_", version)),
    version = version)
}
