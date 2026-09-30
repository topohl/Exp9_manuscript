#!/usr/bin/env Rscript

# Promote a manuscript-owned render into the tracked figure tree.
#
# Renders are written to the gitignored results/ workspace. A figure whose
# analysis is owned by MMMSociability and whose source data is the pinned
# behaviour bundle is published from figures/main/ (or figures/extended_data/),
# not from a copy produced in another repository. This tool copies the assembled
# SVG, PDF and PNG byte-for-byte, and records the promoted path, its SHA-256, the
# bundle and the upstream commit in the publication registry. It computes
# nothing and never touches source_data/pRoteomics/.
#
# Usage: Rscript tools/promote_manuscript_render.R figure_01

suppressWarnings(source(file.path("R", "paths.R")))
source(repo_path("R", "behaviour_bundle.R"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("usage: Rscript tools/promote_manuscript_render.R <publication_id>", call. = FALSE)
pid <- args[[1]]
REG <- repo_path("provenance", "publication_registry", "canonical_publication_registry.csv")
reg <- utils::read.csv(REG, stringsAsFactors = FALSE, colClasses = "character")
i <- which(reg$publication_id == pid)
if (length(i) != 1L) stop("publication id not registered exactly once: ", pid, call. = FALSE)
if (!identical(reg$originating_analysis[i], "topohl/MMMSociability"))
  stop(pid, " is not an MMMSociability-owned identity; its promotion route is unchanged.", call. = FALSE)

pin <- behaviour_bundle_pin()
behaviour_bundle_verify(pin)
src_dir <- path_results("figures", "manuscript", pid, "assembled")
dst_dir <- repo_path("figures", if (startsWith(pid, "figure_")) "main" else "extended_data")
dir.create(dst_dir, recursive = TRUE, showWarnings = FALSE)
files <- paste0(pid, c(".svg", ".pdf", ".png"))
if (!all(file.exists(file.path(src_dir, files)))) stop("assembled render incomplete in ", src_dir, call. = FALSE)
if (!all(file.copy(file.path(src_dir, files), file.path(dst_dir, files), overwrite = TRUE)))
  stop("copy failed", call. = FALSE)
sha <- function(p) unname(tools::sha256sum(p))
if (any(sha(file.path(src_dir, files)) != sha(file.path(dst_dir, files)))) stop("promoted copy is not byte-exact", call. = FALSE)

reg$rendered_artifact[i] <- relative_to(file.path(dst_dir, paste0(pid, ".svg")))
reg$hash[i] <- sha(file.path(dst_dir, paste0(pid, ".svg")))
reg$canonical_source_data[i] <- paste0("source_data/MMMSociability/", pin$bundle_id)
reg$source_commit[i] <- pin$mmm_git_commit
contract <- yaml::read_yaml(repo_path("figures", "figure_contract.yml"))$figures[[reg$contract_key[i]]]
if (!is.null(contract)) reg$panels[i] <- paste(vapply(contract$panels, function(p) as.character(p$id), ""), collapse = ",")
utils::write.csv(reg, REG, row.names = FALSE)
cat("Promoted", pid, "->", relative_to(dst_dir), "\n  sha256", reg$hash[i], "\n  bundle", pin$bundle_id, "\n")
