#!/usr/bin/env Rscript
# Extended Data candidates for the conventional behaviour tests and the physiology (manuscript candidates, not promoted).
#   Extended Data 10  conventional behaviour tests, females and males separately:
#     a  the main readout of each test (dots, batch-adjusted mean and 95% interval, brackets where adjusted p < 0.05)
#     b  Cohen's d of the main readouts and of every measure with an adjusted p < 0.05 (all 87 measures: Supplementary)
#     c  locomotion index by group (exploratory)          d  Cohen's d of distance travelled per session (exploratory)
#   Extended Data 11  physiology and heterogeneity, females and males separately:
#     a  the main physiology readouts, marked as CombZ components where they are   b  Cohen's d of all 19 measures
#     c  CombZ of CON and SIS animals with the CON mean and 1 CON SD              d  spread SIS / CON (SD ratio)
# Reads the pinned SISanalyzer bundle (config/conventional_bundle.yml) and draws it; no statistic is computed here.

source(file.path("R", "paths.R"))
source(repo_path("R", "conventional_bundle.R"))
source(repo_path("R", "panels", "conventional_tests_panels.R"))

CT_W_MM <- 183
CT_H_MM <- 170
CT_GEN <- "conventional_tests_v1"

ct_save <- function(sheet, stem) {
  ggplot2::ggsave(paste0(stem, ".svg"), sheet, device = svglite::svglite, width = CT_W_MM, height = CT_H_MM, units = "mm", bg = "white")
  ggplot2::ggsave(paste0(stem, ".pdf"), sheet, device = grDevices::cairo_pdf, width = CT_W_MM, height = CT_H_MM, units = "mm", bg = "white")
  ggplot2::ggsave(paste0(stem, ".png"), sheet, device = ragg::agg_png, width = CT_W_MM, height = CT_H_MM, units = "mm", dpi = 300, bg = "white")
}

ct_tag <- function(sheet, P) sheet + patchwork::plot_annotation(tag_levels = "a") &
  ggplot2::theme(plot.tag = ggplot2::element_text(size = P$typography$panel_label_pt, face = "bold", family = P$typography$family, colour = "black"))

ct_render <- function(out) {
  for (pkg in c("ggplot2", "patchwork", "svglite", "ragg", "yaml"))
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Required plotting package is unavailable: ", pkg, call. = FALSE)
  if (file.exists(out)) stop("Candidate output already exists: ", out, call. = FALSE)
  pin <- ctp_pin(); ctp_verify(pin); X <- ct_read(pin); P <- X$P
  b_main <- X$measures$id[X$measures$domain == "Behaviour" & X$measures$main]
  p_main <- X$measures$id[X$measures$domain == "Physiology" & X$measures$main]
  p_all <- X$measures$id[X$measures$domain == "Physiology"]
  b_sel <- ct_selected_behaviour(X)
  stopifnot(length(b_main) == 5L, length(p_main) == 5L, length(p_all) == 19L)

  # heatmaps are freed from axis alignment: their fixed-aspect tiles would otherwise pull the neighbouring axis titles across
  ed10 <- ct_tag(ct_dots(X, b_main) /
    (patchwork::wrap_elements(full = ct_heatmap(X, b_sel)) | (ct_loco_index(X) / patchwork::wrap_elements(full = ct_loco_sessions(X)) + patchwork::plot_layout(heights = c(1, .95)))) +
    patchwork::plot_layout(heights = c(64, 100)), P)
  ed11 <- ct_tag(ct_dots(X, p_main, class_note = TRUE) /
    (patchwork::wrap_elements(full = ct_heatmap(X, p_all)) |
       (patchwork::wrap_elements(full = ct_combz_spread(X)) / patchwork::wrap_elements(full = ct_ratio_forest(X)) + patchwork::plot_layout(heights = c(1, 1.05))) +
       patchwork::plot_layout(widths = c(.9, 1.1))) +
    patchwork::plot_layout(heights = c(62, 108)), P)

  dir.create(out, recursive = TRUE)
  ct_save(ed10, file.path(out, "extended_data_10_candidate")); ct_save(ed11, file.path(out, "extended_data_11_candidate"))

  # per-panel source data: exactly the bundle rows each panel draws
  w <- function(x, f) utils::write.csv(x, file.path(out, f), row.names = FALSE, na = "")
  dots <- function(ids) { d <- lapply(CT_SEXES, function(s) ct_dot_data(X, ids, s))
    list(values = do.call(rbind, lapply(d, function(z) z$points[c("Animal", "Sex", "Batch", "Condition", "Group", "id", "value")])),
         means = do.call(rbind, lapply(d, function(z) z$means[c("Sex", "id", "Group", "n", "mean", "lower", "upper")])),
         brackets = do.call(rbind, lapply(d, function(z) if (nrow(z$brackets)) z$brackets[c("Sex", "id", "comparison", "d", "adj_p")] else NULL))) }
  a10 <- dots(b_main); w(a10$values, "ed10a_values.csv"); w(a10$means, "ed10a_group_means.csv"); w(a10$brackets, "ed10a_brackets.csv")
  w(ct_heatmap_data(X, b_sel)$cells[c("Sex", "id", "label", "comparison", "d", "adj_p", "significant")], "ed10b_heatmap.csv")
  c10 <- dots("loco_index"); w(c10$values, "ed10c_values.csv"); w(c10$means, "ed10c_group_means.csv")
  w(X$contrasts[X$contrasts$id == "loco_index", ], "ed10c_contrasts.csv")
  w(ct_loco_sessions_data(X)[c("Session", "Sex", "comparison", "d")], "ed10d_sessions.csv")
  a11 <- dots(p_main); w(a11$values, "ed11a_values.csv"); w(a11$means, "ed11a_group_means.csv"); w(a11$brackets, "ed11a_brackets.csv")
  w(ct_heatmap_data(X, p_all)$cells[c("Sex", "id", "label", "comparison", "d", "adj_p", "significant")], "ed11b_heatmap.csv")
  w(X$values[X$values$id == "CombZ", ], "ed11c_combz_values.csv"); w(X$tails, "ed11c_combz_tails.csv"); w(X$het, "ed11d_spread.csv")

  bundle <- ctp_dir(pin)
  inputs <- c(file.path(bundle, c("00_manifest.csv", CTP_REQUIRED_FILES)), repo_path(CTP_PIN_FILE), repo_path("config", "manuscript_palette.yml"),
              repo_path("R", "conventional_bundle.R"), repo_path("R", "panels", "conventional_tests_panels.R"), repo_path("figures", "conventional_tests_candidate.R"))
  rel <- substring(normalizePath(inputs, winslash = "/"), nchar(normalizePath(repo_path(), winslash = "/")) + 2L)
  w(data.frame(path = rel, sha256 = unname(tools::sha256sum(inputs))), "render_inputs.csv")
  files <- setdiff(list.files(out, full.names = TRUE), file.path(out, "render_manifest.csv"))
  w(data.frame(file = basename(files), sha256 = unname(tools::sha256sum(files))), "render_manifest.csv")
  invisible(list(ed10 = ed10, ed11 = ed11))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 1L || (length(args) == 1L && !nzchar(args[[1]])))
    stop("Usage: Rscript figures/conventional_tests_candidate.R [new_output_directory]", call. = FALSE)
  ct_render(if (length(args)) args[[1]] else repo_path("results", "figures", "manuscript_candidates", CT_GEN))
}
