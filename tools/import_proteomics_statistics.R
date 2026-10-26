#!/usr/bin/env Rscript
# Import the proteomic test statistics that the per-panel source data does not carry, for stats.xlsx, from the preserved
# original-centring version of pRoteomics (Analysis/_archive_proteomics_pre_renormalisation_2026-10-19: the version every
# proteomic figure shows; the original centring is the primary analysis, Tobias 2026-10-25):
#   protein_statistics.csv            Figure 3e-g leading-edge proteins: log2 fold change, moderated t, raw P, BH FDR (limma, 3 vs 3);
#                                     df.total and limma's 95% interval, recovered exactly from those (see below; added 2026-10-26)
#   term_statistics.csv               Figure 3e-g card terms: gene-set size, and NES, raw P and BH FDR of SUS - RES
#   control_signature_statistics.csv  Figure 2g / ED5d pairings: signature genes mapped onto the ranking (the GSEA set size), NES, raw P, signature FDR
#   control_term_statistics.csv       Figure 2h terms: set size, NES, raw P and BH FDR
# Rows are selected and copied as stored; only the protein df and interval are derived (exactly). Every archive file must carry the SHA-256 that the figure's own
# import recorded (source_data/pRoteomics/figure_03_adaptation/input_manifest.csv; provenance/source_manifests/
# render_inputs_manifest.csv), and every value the figures show must be reproduced exactly (gates), so the rows are of the same
# version. The archive's paths exceed 260 characters: pass a short root (subst Q: <archive>).
#
# Usage (from the repository root):  Rscript tools/import_proteomics_statistics.R Q:/
# Writes source_data/proteomics_statistics/ once (refuses an existing folder): the four tables, inputs.csv (archive file, SHA-256)
# and 00_manifest.csv (file, rows, SHA-256).

suppressWarnings(source(file.path("R", "paths.R")))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !file.exists(file.path(args[[1]], "SHA256_MANIFEST.csv")))
  stop("usage: Rscript tools/import_proteomics_statistics.R <archive root (short path)>", call. = FALSE)
ARC <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
OUT <- repo_path("source_data", "proteomics_statistics")
if (dir.exists(OUT)) stop("already imported: ", OUT, call. = FALSE)
TOL <- 1e-12
rd <- function(p) utils::read.csv(p, check.names = FALSE, stringsAsFactors = FALSE)
sha <- function(p) unname(tools::sha256sum(p))
INPUTS <- list()
pinned <- function(rel, expected) {
  p <- file.path(ARC, rel); h <- if (file.exists(p)) sha(p) else NA_character_
  if (!identical(h, expected)) stop("archive file differs from the figure's pinned SHA-256: ", rel, call. = FALSE)
  INPUTS[[length(INPUTS) + 1L]] <<- data.frame(archive_file = rel, sha256 = h, stringsAsFactors = FALSE)
  rd(p)
}
gate <- function(name, ok, detail = "") { cat(sprintf("  gate %-90s %s %s\n", name, if (isTRUE(ok)) "PASS" else "FAIL", detail))
  if (!isTRUE(ok)) stop("gate failed: ", name, call. = FALSE) }
num_eq <- function(a, b) length(a) == length(b) && length(a) > 0L && isTRUE(max(abs(as.numeric(a) - as.numeric(b))) <= TOL)

# ---------------------------------------------------------------- Figure 3e-g: proteins and card terms
ad <- rd(repo_path("source_data", "pRoteomics", "figure_03_adaptation", "input_manifest.csv"))
CONTRAST <- c(res_con = "RES\u2212CON", sus_con = "SUS\u2212CON", sus_res = "SUS\u2212RES")
fig3 <- repo_path("figures", "main", "source_data", "figure_03")
prot_rows <- list(); term_rows <- list()
for (k in c("e", "f", "g")) {
  pz <- rd(file.path(fig3, sprintf("figure_03%s_proteins_source_data.csv", k)))
  cd <- rd(file.path(fig3, sprintf("figure_03%s_card_source_data.csv", k)))
  ds <- unique(pz$dataset); stopifnot(length(ds) == 1L)
  mp <- ad[ad$role == "mapped_protein_da" & grepl(paste0("/mapped/", ds, "/"), ad$path, fixed = TRUE), ]
  stopifnot(nrow(mp) == 3L)
  for (i in seq_len(nrow(mp))) {
    key <- sub("^.*/[A-Za-z0-9]*?(res|sus)_[A-Za-z0-9]*?(con|res)\\.csv$", "\\1_\\2", mp$path[i]); lab <- CONTRAST[[key]]
    da <- pinned(mp$path[i], mp$sha256[i])
    want <- pz[pz$contrast == lab, ]
    x <- da[match(want$ProteinGroupID, da$ProteinGroupID), c("ProteinGroupID", "log2fc", "t", "pval", "padj")]
    gate(sprintf("3%s %s: every leading-edge protein found; log2FC and BH FDR = the figure's", k, lab),
         !anyNA(x$ProteinGroupID) && num_eq(x$log2fc, want$log2FC) && num_eq(x$padj, want$BH_FDR), nrow(x))
    prot_rows[[length(prot_rows) + 1L]] <- data.frame(panel = paste0("3", k), dataset = ds, spatial_unit = unique(pz$spatial_unit), contrast = lab,
      gene = want$gene, ProteinGroupID = x$ProteinGroupID, log2fc = x$log2fc, t = x$t, pval = x$pval, padj = x$padj,
      test = "moderated t (limma, robust empirical Bayes), 3 vs 3 animals", source_file = mp$path[i], stringsAsFactors = FALSE)
  }
  gs <- ad[ad$role == "stored_gsea_result" & grepl(paste0("/clusterProfiler/", ds, "/"), ad$path, fixed = TRUE), ]
  stopifnot(nrow(gs) == 1L)
  g <- pinned(gs$path, gs$sha256)
  sr <- cd[cd$contrast == "S\u2212R", ]
  y <- g[match(sr$GO_ID, g$ID), c("ID", "Description", "setSize", "NES", "pvalue", "p.adjust")]
  gate(sprintf("3%s card terms: found in the stored SUS-RES GSEA; NES, raw P and BH FDR = the figure's", k),
       !anyNA(y$ID) && num_eq(y$NES, sr$NES) && num_eq(y$pvalue, sr$raw_p) && num_eq(y$p.adjust, sr$BH_FDR), nrow(y))
  term_rows[[length(term_rows) + 1L]] <- data.frame(panel = paste0("3", k), dataset = ds, spatial_unit = unique(cd$spatial_unit), GO_ID = y$ID,
    Description = y$Description, setSize = y$setSize, NES_sus_res = y$NES, pvalue_sus_res = y$pvalue, padj_sus_res = y$p.adjust,
    note = "set size is the term's size in the ranked gene list of this unit, the same for all three contrasts; P values of 1e-10 are the GSEA floor",
    source_file = gs$path, stringsAsFactors = FALSE)
}
PROT <- do.call(rbind, prot_rows); TERM <- do.call(rbind, term_rows)
gate("protein rows: 3 exemplars x 3 contrasts x 7 proteins", nrow(PROT) == 63L, nrow(PROT))
# The archive stores log2FC, t and P but not limma's df or interval. Both are exact functions of the stored values: the moderated
# t is log2FC / SE, so SE = log2FC / t, and P = 2 pt(-|t|, df.total), so df.total is the root of that equation; the interval is
# limma's own (topTable confint = TRUE): log2FC +- qt(0.975, df.total) SE. Gates: P recomputed from t and df.total equals the
# stored P, and df.total lies between the residual df and residual + prior df of the compartment's fit (added 2026-10-26).
df_total <- function(t, p) { f <- function(d) log(2 * stats::pt(-abs(t), d)) - log(p); stats::uniroot(f, c(1, 1e5), tol = 1e-12)$root }
gate("protein rows: t is not 0", all(PROT$t != 0))
PROT$df_total <- mapply(df_total, PROT$t, PROT$pval)
se <- abs(PROT$log2fc / PROT$t)
PROT$ci_low <- PROT$log2fc - stats::qt(0.975, PROT$df_total) * se; PROT$ci_high <- PROT$log2fc + stats::qt(0.975, PROT$df_total) * se
gate("protein rows: P recomputed from t and the recovered df.total = stored P (relative 1e-10)",
     max(abs(2 * stats::pt(-abs(PROT$t), PROT$df_total) / PROT$pval - 1)) < 1e-10)
gate("protein rows: df.total is that of the compartment's fit (neuropil 60-64, soma and microglia 24-28)",
     all(ifelse(PROT$dataset == "neuron_neuropil", PROT$df_total >= 60 & PROT$df_total <= 64, PROT$df_total >= 24 & PROT$df_total <= 28)),
     paste(round(range(PROT$df_total), 2), collapse = "-"))
PROT <- PROT[, c(setdiff(names(PROT), c("df_total", "ci_low", "ci_high", "test", "source_file")), "df_total", "ci_low", "ci_high", "test", "source_file")]

# ---------------------------------------------------------------- Figure 2g / ED5d and Figure 2h: control-only GSEA
ri <- rd(repo_path("provenance", "source_manifests", "render_inputs_manifest.csv"))
pin_ri <- function(rel) { s <- unique(ri$sha256[ri$declared_path == rel]); stopifnot(length(s) == 1L); pinned(rel, s) }
e2 <- pin_ri("results/source_data/04_differential_expression_enrichment/control_spatial_identity_validation/global/figure2e_source_data.csv")
f2 <- pin_ri("results/source_data/04_differential_expression_enrichment/control_spatial_identity_validation/global/figure2f_regions_CA1layers_source_data.csv")
SIG <- e2[, c("internal_contrast", "external_signature", "expected_match", "mapped_unique_genes", "NES", "p_value", "signature_FDR", "signature_fdr_family")]
d5 <- rd(repo_path("figures", "extended_data", "source_data", "extended_data_05", "extended_data_05d_source_data.csv"))
m5 <- merge(d5, SIG, by = c("internal_contrast", "external_signature"), suffixes = c("", "_arc"))
gate("ED5d: all 30 pairings found; NES and signature FDR = the figure's", nrow(m5) == 30L && num_eq(m5$NES, m5$NES_arc) && num_eq(m5$signature_FDR, m5$signature_FDR_arc), nrow(m5))
d2h <- rd(repo_path("figures", "main", "source_data", "figure_02", "figure_02h_source_data.csv"))
TRM <- f2[, c("contrast", "ID", "Description", "setSize", "NES", "pvalue", "p.adjust")]
m2 <- merge(d2h, TRM, by = c("contrast", "Description"), suffixes = c("", "_arc"))
gate("2h: all terms found once; NES, set size and BH FDR = the figure's", nrow(m2) == nrow(d2h) && !anyDuplicated(m2[, c("contrast", "Description")]) &&
       num_eq(m2$NES, m2$NES_arc) && num_eq(m2$setSize, m2$setSize_arc) && num_eq(m2$p_adjust, m2$p.adjust), nrow(m2))
TRM <- m2[, c("contrast", "ID", "Description", "setSize", "NES", "pvalue", "p.adjust")]

# ---------------------------------------------------------------- write once
dir.create(OUT, recursive = TRUE)
TABLES <- list(protein_statistics = PROT, term_statistics = TERM, control_signature_statistics = SIG, control_term_statistics = TRM,
               inputs = do.call(rbind, INPUTS))
for (n in names(TABLES)) utils::write.csv(TABLES[[n]], file.path(OUT, paste0(n, ".csv")), row.names = FALSE, fileEncoding = "UTF-8")
f <- paste0(names(TABLES), ".csv")
man <- data.frame(file = f, rows = vapply(TABLES, nrow, 0L), sha256 = vapply(file.path(OUT, f), sha, ""),
                  archive = "Analysis/_archive_proteomics_pre_renormalisation_2026-10-19 (original centring; pRoteomics b478a13)", row.names = NULL)
utils::write.csv(man, file.path(OUT, "00_manifest.csv"), row.names = FALSE)
cat("imported", nrow(PROT), "protein rows,", nrow(TERM), "card terms,", nrow(SIG), "signature pairings,", nrow(TRM), "control terms ->", OUT, "\n")
