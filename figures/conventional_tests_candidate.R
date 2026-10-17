#!/usr/bin/env Rscript
# Extended Data candidates for the conventional behaviour tests and the physiology (manuscript candidates, not promoted).
#   Extended Data 10  conventional behaviour tests, females and males separately:
#     a  the main readout of each test (dots, batch-adjusted mean and 95% interval, brackets where adjusted p < 0.05)
#     b  Cohen's d of the main readouts and of every measure with an adjusted p < 0.05 (all 87 measures: Supplementary)
#     c  locomotion index by group (exploratory)          d  Cohen's d of distance travelled per session (exploratory)
#   Extended Data 11  physiology and heterogeneity, females and males separately:
#     a  the main physiology readouts (four are CombZ components: legend)    b  d of the main readouts and the significant measures
#     c  CombZ of CON and SIS animals with the CON mean and 1 CON SD           d  spread SIS / CON (SD ratio)
#   Extended Data 12  the analysed automated (SLEAP) values against manual (BORIS) scoring of the batch 1 animals (males):
#     a  plus maze   b  novel object   c  social tests (identity line, stored Pearson r)
# Reads the pinned SISanalyzer bundle (config/conventional_bundle.yml) and draws it; no statistic is computed here.

source(file.path("R", "paths.R"))
source(repo_path("R", "conventional_bundle.R"))
source(repo_path("R", "panels", "conventional_tests_panels.R"))

CT_W_MM <- 183
CT_H_MM <- 170
CT_GEN <- "conventional_tests_v1"

ct_save <- function(sheet, stem, h = CT_H_MM) {
  ggplot2::ggsave(paste0(stem, ".svg"), sheet, device = svglite::svglite, width = CT_W_MM, height = h, units = "mm", bg = "white")
  ggplot2::ggsave(paste0(stem, ".pdf"), sheet, device = grDevices::cairo_pdf, width = CT_W_MM, height = h, units = "mm", bg = "white")
  ggplot2::ggsave(paste0(stem, ".png"), sheet, device = ragg::agg_png, width = CT_W_MM, height = h, units = "mm", dpi = 300, bg = "white")
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
  b_sel <- ct_selected_behaviour(X); p_sel <- ct_selected_physiology(X)
  stopifnot(length(b_main) == 5L, length(p_main) == 5L)

  # heatmaps are freed from axis alignment: their fixed-aspect tiles would otherwise pull the neighbouring axis titles across
  ed10 <- ct_tag(ct_dots(X, b_main) /
    (patchwork::wrap_elements(full = ct_heatmap(X, b_sel)) | (ct_loco_index(X) / patchwork::wrap_elements(full = ct_loco_sessions(X)) + patchwork::plot_layout(heights = c(1, .8))) +
       patchwork::plot_layout(widths = c(.8, 1.2))) +
    patchwork::plot_layout(heights = c(62, 108)), P)
  ed11 <- ct_tag(ct_dots(X, p_main) /
    (patchwork::wrap_elements(full = ct_heatmap(X, p_sel)) |
       (patchwork::wrap_elements(full = ct_combz_spread(X)) / patchwork::wrap_elements(full = ct_ratio_forest(X)) + patchwork::plot_layout(heights = c(1, 1.05))) +
       patchwork::plot_layout(widths = c(.8, 1.2))) +
    patchwork::plot_layout(heights = c(62, 108)), P)
  ed12 <- ct_tag(ct_validation(X) + patchwork::plot_annotation(title = "Males",
    theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = P$typography$axis_title_pt, family = P$typography$family))), P)

  dir.create(out, recursive = TRUE)
  ct_save(ed10, file.path(out, "extended_data_10_candidate")); ct_save(ed11, file.path(out, "extended_data_11_candidate"))
  ct_save(ed12, file.path(out, "extended_data_12_candidate"), h = 64)   # square panels: the height follows the width

  # per-panel source data: the bundle rows each panel draws, plus the stored rows its legend quotes
  w <- function(x, f) utils::write.csv(x, file.path(out, f), row.names = FALSE, na = "")
  dots <- function(ids) { d <- lapply(CT_SEXES, function(s) ct_dot_data(X, ids, s))
    list(values = do.call(rbind, lapply(d, function(z) z$points[c("Animal", "Sex", "Batch", "Condition", "Group", "id", "value")])),
         means = do.call(rbind, lapply(d, function(z) z$means[c("Sex", "id", "Group", "n", "mean", "lower", "upper")])),
         brackets = do.call(rbind, lapply(d, function(z) z$brackets[c("Sex", "id", "comparison", "d", "adj_p")]))) }
  heat <- function(ids) { z <- ct_heatmap_data(X, ids)$cells; z$name <- X$measures$label[match(z$id, X$measures$id)]
    z[c("Sex", "id", "label", "name", "comparison", "d", "adj_p", "significant")] }   # label as drawn, name in full
  a10 <- dots(b_main); w(a10$values, "ed10a_values.csv"); w(a10$means, "ed10a_group_means.csv"); w(a10$brackets, "ed10a_brackets.csv")
  w(heat(b_sel), "ed10b_heatmap.csv")
  c10 <- dots("loco_index"); w(c10$values, "ed10c_values.csv"); w(c10$means, "ed10c_group_means.csv")
  w(X$contrasts[X$contrasts$id == "loco_index", ], "ed10c_contrasts.csv"); w(X$loco_int, "ed10c_interaction.csv")
  w(ct_loco_sessions_data(X)[c("Session", "Sex", "comparison", "d")], "ed10d_sessions.csv")
  a11 <- dots(p_main); w(a11$values, "ed11a_values.csv"); w(a11$means, "ed11a_group_means.csv"); w(a11$brackets, "ed11a_brackets.csv")
  w(heat(p_sel), "ed11b_heatmap.csv")
  w(X$values[X$values$id == "CombZ", ], "ed11c_combz_values.csv"); w(X$tails, "ed11c_combz_tails.csv"); w(X$het, "ed11d_spread.csv")
  V <- ct_validation_data(X); w(V$points[c("block", "metric", "title", "unit", "calibrated_on_these_animals", "manual_source", "Code", "Animal", "boris", "sleap")], "ed12_validation_pairs.csv"); w(X$agree, "ed12_agreement.csv")

  bundle <- ctp_dir(pin)
  inputs <- c(file.path(bundle, c("00_manifest.csv", CTP_REQUIRED_FILES)), repo_path(CTP_PIN_FILE), repo_path("config", "manuscript_palette.yml"),
              repo_path("R", "conventional_bundle.R"), repo_path("R", "panels", "conventional_tests_panels.R"), repo_path("figures", "conventional_tests_candidate.R"))
  rel <- substring(normalizePath(inputs, winslash = "/"), nchar(normalizePath(repo_path(), winslash = "/")) + 2L)
  w(data.frame(path = rel, sha256 = unname(tools::sha256sum(inputs))), "render_inputs.csv")
  files <- setdiff(list.files(out, full.names = TRUE), file.path(out, "render_manifest.csv"))
  w(data.frame(file = basename(files), sha256 = unname(tools::sha256sum(files))), "render_manifest.csv")
  invisible(list(ed10 = ed10, ed11 = ed11, ed12 = ed12))
}

# ------------------------------------------------------------------ publication layer (tools/promote_manuscript_render.R)
# The three pages under their publication identities: the assembled page, one source-data table per panel (the panel's tables
# stacked, column "table"), a run manifest per identity and one producer render record, all naming the commit and whether the
# code was clean, so that the promotion tool refuses a render of other or uncommitted code.
CT_PUB <- c(extended_data_10 = "extended_data_10_candidate", extended_data_11 = "extended_data_11_candidate", extended_data_12 = "extended_data_12_candidate")
CT_PANEL_TABLES <- list(
  extended_data_10 = list(a = c("ed10a_values", "ed10a_group_means", "ed10a_brackets"), b = "ed10b_heatmap",
                          c = c("ed10c_values", "ed10c_group_means", "ed10c_contrasts", "ed10c_interaction"), d = "ed10d_sessions"),
  extended_data_11 = list(a = c("ed11a_values", "ed11a_group_means", "ed11a_brackets"), b = "ed11b_heatmap", c = c("ed11c_combz_values", "ed11c_combz_tails"), d = "ed11d_spread"),
  extended_data_12 = list(a = "Plus maze", b = "Novel object", c = "Social tests"))
CT_RECORD <- c("logs", "manuscript_figures", "conventional_tests", "render_record.yml")

ct_stack <- function(tabs) { tabs <- Filter(function(x) nrow(x) > 0, tabs); cols <- unique(unlist(lapply(tabs, names)))
  do.call(rbind, lapply(tabs, function(x) { for (k in setdiff(cols, names(x))) x[[k]] <- NA; x[cols] })) }
ct_panel_source_data <- function(dir, pid, out) {
  rd <- function(f) utils::read.csv(file.path(dir, paste0(f, ".csv")), check.names = FALSE, stringsAsFactors = FALSE)
  vapply(names(CT_PANEL_TABLES[[pid]]), function(p) { spec <- CT_PANEL_TABLES[[pid]][[p]]
    tab <- if (pid == "extended_data_12") {
      pr <- rd("ed12_validation_pairs"); ag <- rd("ed12_agreement"); assay <- c(`Plus maze` = "EPM", `Novel object` = "NOR", `Social tests` = "SocP")[[spec]]
      ct_stack(list(cbind(table = "pairs", pr[pr$block == spec, ]), cbind(table = "agreement", ag[ag$assay == assay, ])))
    } else ct_stack(lapply(spec, function(f) { x <- rd(f); if (nrow(x)) cbind(table = sub("^ed[0-9]+[a-d]_", "", f), x) else x }))
    f <- file.path(out, paste0(pid, p, "_source_data.csv")); utils::write.csv(tab, f, row.names = FALSE, na = ""); f }, "")
}

ct_publish <- function() {
  code <- git_code_state(); pin <- ctp_pin()
  tmp <- tempfile("ct_publish_"); ct_render(tmp)
  root <- normalizePath(repo_path(), winslash = "/"); rel <- function(p) substring(normalizePath(p, winslash = "/"), nchar(root) + 2L)
  sha <- function(p) unname(tools::sha256sum(p)); entry <- function(p, role) list(role = role, input_relative_path = rel(p), sha256 = sha(p))
  bundle <- file.path(ctp_dir(pin), c("00_manifest.csv", CTP_REQUIRED_FILES)); drawn <- character()
  for (pid in names(CT_PUB)) {
    asm <- path_results("figures", "manuscript", pid, "assembled"); sdd <- path_results("source_data", "manuscript", pid)
    unlink(c(asm, sdd), recursive = TRUE); dir.create(asm, recursive = TRUE); dir.create(sdd, recursive = TRUE)
    pages <- file.path(asm, paste0(pid, c(".svg", ".pdf", ".png")))
    if (!all(file.copy(file.path(tmp, paste0(CT_PUB[[pid]], c(".svg", ".pdf", ".png"))), pages))) stop("page copy failed: ", pid, call. = FALSE)
    csvs <- ct_panel_source_data(tmp, pid, sdd); drawn <- c(drawn, pages)
    log_dir <- path_results("logs", "manuscript_figures", pid); dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    yaml::write_yaml(list(timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), manuscript_layer = "figures/conventional_tests_candidate.R --publication",
      publication_id = pid, sisanalyzer_bundle_id = pin$bundle_id, render_git_commit = code$commit, render_code_clean = code$code_clean,
      repository_root_at_render = root,
      inputs = c(lapply(pages, entry, role = "figure_source"), lapply(bundle, entry, role = "source_bundle")),
      outputs = lapply(c(pages, csvs), function(p) list(path = normalizePath(p, winslash = "/"), sha256 = sha(p)))), file.path(log_dir, "run_manifest.yml"))
    cat(pid, "->", rel(asm), "+", length(csvs), "source-data tables\n")
  }
  rec <- do.call(path_results, as.list(CT_RECORD)); dir.create(dirname(rec), recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(list(timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), producer = "figures/conventional_tests_candidate.R",
    sisanalyzer_bundle_id = pin$bundle_id, sisanalyzer_bundle_manifest_sha256 = pin$manifest_sha256,
    render_git_commit = code$commit, render_code_clean = code$code_clean, render_code_dirty = code$dirty,
    files = lapply(drawn, function(p) list(path = rel(p), sha256 = sha(p)))), rec)
  cat("render record", rel(rec), "at", substr(code$commit, 1, 7), if (isTRUE(code$code_clean)) "(clean code)\n" else "(UNCOMMITTED CODE)\n")
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 1L || (length(args) == 1L && !nzchar(args[[1]])))
    stop("Usage: Rscript figures/conventional_tests_candidate.R [new_output_directory | --publication]", call. = FALSE)
  if (identical(args, "--publication")) ct_publish() else
    ct_render(if (length(args)) args[[1]] else repo_path("results", "figures", "manuscript_candidates", CT_GEN))
}
