#!/usr/bin/env Rscript

# Promote a manuscript-owned render into the tracked figure tree.
#
# Renders are written to the gitignored results/ workspace. Two kinds of
# identity are published from figures/main/ (or figures/extended_data/) rather
# than from a copy produced in another repository:
#
#   * a figure whose analysis is owned by MMMSociability and whose source data
#     is the pinned behaviour bundle; its registry row records the bundle and
#     the upstream commit;
#   * a figure this repository renders itself from frozen source data
#     (SELF_RENDERED). Its per-panel source data is copied beside the render
#     under figures/main/source_data/<id>/ with a hash manifest, and its
#     registry row records the render commit. The render must come from a
#     clean tree at HEAD, with every input and output hash in its run manifest
#     still matching.
#
# The tool copies the assembled SVG, PDF and PNG byte-for-byte and records the
# promoted path and its SHA-256 in the publication registry. It computes
# nothing and never touches source_data/pRoteomics/.
#
# Usage: Rscript tools/promote_manuscript_render.R figure_01
#        Rscript tools/promote_manuscript_render.R figure_03

suppressWarnings(source(file.path("R", "paths.R")))

SELF_RENDERED <- c("figure_03")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("usage: Rscript tools/promote_manuscript_render.R <publication_id>", call. = FALSE)
pid <- args[[1]]
REG <- repo_path("provenance", "publication_registry", "canonical_publication_registry.csv")
reg <- utils::read.csv(REG, stringsAsFactors = FALSE, colClasses = "character")
i <- which(reg$publication_id == pid)
if (length(i) != 1L) stop("publication id not registered exactly once: ", pid, call. = FALSE)
mmm <- identical(reg$originating_analysis[i], "topohl/MMMSociability")
if (!mmm && !(pid %in% SELF_RENDERED))
  stop(pid, " has no promotion route; it is packaged from source_data/pRoteomics.", call. = FALSE)

sha <- function(p) unname(tools::sha256sum(p))
contract <- yaml::read_yaml(repo_path("figures", "figure_contract.yml"))$figures[[reg$contract_key[i]]]
panel_ids <- if (is.null(contract)) character(0) else
  vapply(contract$panels, function(p) as.character(p$id), "")

if (mmm) {
  source(repo_path("R", "behaviour_bundle.R"))
  pin <- behaviour_bundle_pin()
  behaviour_bundle_verify(pin)
} else {
  # A self-rendered identity is only promoted from a render of the committed
  # tree: HEAD must be the render commit, the tree must be clean, and nothing
  # the run manifest recorded may have changed since.
  run <- yaml::read_yaml(path_results("logs", "manuscript_figures", pid, "run_manifest.yml"))
  head <- git_commit_sha()
  dirty <- system2("git", c("-C", repo_root(), "status", "--porcelain", "--untracked-files=no"),
                   stdout = TRUE)
  if (!identical(as.character(run$render_git_commit), head))
    stop(pid, " was rendered at ", run$render_git_commit, ", not at HEAD ", head,
         "; re-render before promoting.", call. = FALSE)
  if (length(dirty))
    stop("working tree has uncommitted changes; commit before promoting ", pid, ".", call. = FALSE)
  recorded <- c(lapply(run$inputs, function(x) c(x$input_resolved_path, x$sha256)),
                lapply(run$outputs, function(x) c(x$path, x$sha256)))
  changed <- Filter(function(x) !file.exists(x[[1]]) || !identical(sha(x[[1]]), x[[2]]), recorded)
  if (length(changed))
    stop("render inputs or outputs changed since the run manifest was written:\n  ",
         paste(vapply(changed, `[[`, "", 1L), collapse = "\n  "), call. = FALSE)
}

src_dir <- path_results("figures", "manuscript", pid, "assembled")
dst_dir <- repo_path("figures", if (startsWith(pid, "figure_")) "main" else "extended_data")
dir.create(dst_dir, recursive = TRUE, showWarnings = FALSE)
files <- paste0(pid, c(".svg", ".pdf", ".png"))
if (!all(file.exists(file.path(src_dir, files)))) stop("assembled render incomplete in ", src_dir, call. = FALSE)
if (!all(file.copy(file.path(src_dir, files), file.path(dst_dir, files), overwrite = TRUE)))
  stop("copy failed", call. = FALSE)
if (any(sha(file.path(src_dir, files)) != sha(file.path(dst_dir, files)))) stop("promoted copy is not byte-exact", call. = FALSE)

reg$rendered_artifact[i] <- relative_to(file.path(dst_dir, paste0(pid, ".svg")))
reg$hash[i] <- sha(file.path(dst_dir, paste0(pid, ".svg")))
if (mmm) {
  reg$canonical_source_data[i] <- paste0("source_data/MMMSociability/", pin$bundle_id)
  reg$source_commit[i] <- pin$mmm_git_commit
} else {
  sd_src <- path_results("source_data", "manuscript", pid)
  sd_dst <- file.path(dst_dir, "source_data", pid)
  expected <- paste0(pid, sub("^[0-9]+", "", panel_ids), "_source_data.csv")
  if (!length(expected) || !all(file.exists(file.path(sd_src, expected))))
    stop("per-panel source data incomplete in ", sd_src, call. = FALSE)
  dir.create(sd_dst, recursive = TRUE, showWarnings = FALSE)
  unlink(list.files(sd_dst, full.names = TRUE))
  if (!all(file.copy(file.path(sd_src, expected), file.path(sd_dst, expected))))
    stop("source-data copy failed", call. = FALSE)
  if (!setequal(list.files(sd_dst), expected) ||
      any(sha(file.path(sd_src, expected)) != sha(file.path(sd_dst, expected))))
    stop("promoted source data is not a byte-exact copy of the render", call. = FALSE)
  rows <- vapply(file.path(sd_dst, expected), function(p)
    nrow(utils::read.csv(p, check.names = FALSE)), integer(1))
  utils::write.csv(data.frame(file = expected, panel = panel_ids, rows = unname(rows),
                              sha256 = sha(file.path(sd_dst, expected)),
                              render_git_commit = head, stringsAsFactors = FALSE),
                   file.path(sd_dst, "00_manifest.csv"), row.names = FALSE)
  reg$canonical_source_data[i] <- relative_to(sd_dst)
  reg$source_commit[i] <- head
}
if (length(panel_ids)) reg$panels[i] <- paste(panel_ids, collapse = ",")
utils::write.csv(reg, REG, row.names = FALSE)
cat("Promoted", pid, "->", relative_to(dst_dir), "\n  sha256", reg$hash[i], "\n  source",
    reg$canonical_source_data[i], "\n  commit", reg$source_commit[i], "\n")
