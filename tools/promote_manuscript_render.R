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
#     registry row records the render commit. Both render layers - the producer
#     that drew the panels and the manuscript layer that assembled them - must
#     have run at HEAD on clean code, the tree must still be clean, and every
#     file either layer hashed must be unchanged.
#
# The tool copies the assembled SVG, PDF and PNG byte-for-byte and records the
# promoted path and its SHA-256 in the publication registry. It computes
# nothing and never touches source_data/pRoteomics/.
#
# Several identities may be named at once, and identities rendered at one
# commit must be: promoting one changes the tracked registry, which leaves the
# tree dirty for the next, and committing in between moves HEAD past the render
# commit. Every named identity is checked before anything is written, and the
# registry is written once.
#
# Usage: Rscript tools/promote_manuscript_render.R figure_01
#        Rscript tools/promote_manuscript_render.R figure_03
#        Rscript tools/promote_manuscript_render.R figure_02 extended_data_01 extended_data_02 \
#          extended_data_03 extended_data_06 extended_data_08

suppressWarnings(source(file.path("R", "paths.R")))

SELF_RENDERED <- c("figure_02", "figure_03", "extended_data_01", "extended_data_02",
                   "extended_data_03", "extended_data_06", "extended_data_08")
# where the producing layer of each self-rendered identity records its render;
# one producer draws every Extended Data page, so those share its record
v9_record <- function(key) c("logs", "manuscript_candidates", "final_truth_v9", key, "render_record.yml")
PRODUCER_RECORD <- c(
  list(figure_02 = v9_record("figure_02"), figure_03 = v9_record("figure_03")),
  sapply(grep("^extended_data_", SELF_RENDERED, value = TRUE),
         function(id) v9_record("extended_data"), simplify = FALSE))

args <- commandArgs(trailingOnly = TRUE)
if (!length(args))
  stop("usage: Rscript tools/promote_manuscript_render.R <publication_id> [<publication_id> ...]",
       call. = FALSE)
if (anyDuplicated(args)) stop("a publication id is named twice", call. = FALSE)
REG <- repo_path("provenance", "publication_registry", "canonical_publication_registry.csv")
reg <- utils::read.csv(REG, stringsAsFactors = FALSE, colClasses = "character")
sha <- function(p) unname(tools::sha256sum(p))
figures <- yaml::read_yaml(repo_path("figures", "figure_contract.yml"))$figures
head <- git_commit_sha()

# Everything a promotion needs, checked; nothing is written here.
plan_promotion <- function(pid) {
  i <- which(reg$publication_id == pid)
  if (length(i) != 1L) stop("publication id not registered exactly once: ", pid, call. = FALSE)
  mmm <- identical(reg$originating_analysis[i], "topohl/MMMSociability")
  if (!mmm && !(pid %in% SELF_RENDERED))
    stop(pid, " has no promotion route; it is packaged from source_data/pRoteomics.", call. = FALSE)

  contract <- figures[[reg$contract_key[i]]]
  panel_ids <- if (is.null(contract)) character(0) else
    vapply(contract$panels, function(p) as.character(p$id), "")
  p <- list(pid = pid, i = i, mmm = mmm, panel_ids = panel_ids,
            src_dir = path_results("figures", "manuscript", pid, "assembled"),
            dst_dir = repo_path("figures", if (startsWith(pid, "figure_")) "main" else "extended_data"),
            files = paste0(pid, c(".svg", ".pdf", ".png")))

  if (mmm) {
    source(repo_path("R", "behaviour_bundle.R"))
    p$pin <- behaviour_bundle_pin()
    behaviour_bundle_verify(p$pin)
  } else {
    # A self-rendered identity is only promoted from a render of the committed
    # tree, and nothing is written until every check below has passed.
    run <- yaml::read_yaml(path_results("logs", "manuscript_figures", pid, "run_manifest.yml"))
    producer <- yaml::read_yaml(do.call(path_results, as.list(PRODUCER_RECORD[[pid]])))
    for (rec in list(manuscript_layer = run, producer = producer)) {
      if (!identical(as.character(rec$render_git_commit), head))
        stop(pid, " was rendered at ", rec$render_git_commit, ", not at HEAD ", head,
             "; re-render before promoting.", call. = FALSE)
      if (!isTRUE(rec$render_code_clean))
        stop(pid, " was rendered from uncommitted code; commit and re-render before promoting.",
             call. = FALSE)
    }
    # every panel the manuscript layer copied was drawn by the recorded producer
    # run (a producer may draw only some of its pages)
    drawn <- vapply(producer$files, function(x) as.character(x$path), "")
    copied <- unlist(lapply(run$inputs, function(x)
      if (identical(x$role, "figure_source")) as.character(x$input_relative_path)))
    if (length(setdiff(copied, drawn)))
      stop(pid, " uses panels the recorded producer run did not draw:\n  ",
           paste(setdiff(copied, drawn), collapse = "\n  "), call. = FALSE)
    # every recorded file is resolved inside this repository, whatever root or
    # drive letter the render ran under
    root <- normalizePath(as.character(run$repository_root_at_render), winslash = "/", mustWork = FALSE)
    here <- function(x) {
      x <- normalizePath(x, winslash = "/", mustWork = FALSE)
      if (!startsWith(tolower(x), tolower(paste0(root, "/"))))
        stop("recorded path lies outside the render root: ", x, call. = FALSE)
      file.path(repo_root(), substring(x, nchar(root) + 2L))
    }
    recorded <- c(lapply(run$inputs, function(x) c(file.path(repo_root(), x$input_relative_path), x$sha256)),
                  lapply(run$outputs, function(x) c(here(x$path), x$sha256)),
                  lapply(producer$files, function(x) c(file.path(repo_root(), x$path), x$sha256)))
    changed <- Filter(function(x) !file.exists(x[[1]]) || !identical(sha(x[[1]]), x[[2]]), recorded)
    if (length(changed))
      stop("render inputs or outputs changed since they were recorded:\n  ",
           paste(vapply(changed, `[[`, "", 1L), collapse = "\n  "), call. = FALSE)
    p$sd_src <- path_results("source_data", "manuscript", pid)
    p$sd_dst <- file.path(p$dst_dir, "source_data", pid)
    p$expected <- paste0(pid, sub("^[0-9]+", "", panel_ids), "_source_data.csv")
    if (!length(p$expected) || !all(file.exists(file.path(p$sd_src, p$expected))))
      stop("per-panel source data incomplete in ", p$sd_src, call. = FALSE)
    hashed <- vapply(run$outputs, function(x) basename(x$path), "")
    if (!all(p$expected %in% hashed))
      stop("per-panel source data is not covered by the run manifest; re-render before promoting.",
           call. = FALSE)
    if (dir.exists(p$sd_dst) && length(list.dirs(p$sd_dst, recursive = FALSE)))
      stop(relative_to(p$sd_dst), " contains subdirectories; clear it before promoting.", call. = FALSE)
  }
  if (!all(file.exists(file.path(p$src_dir, p$files))))
    stop("assembled render incomplete in ", p$src_dir, call. = FALSE)
  p
}

if (any(args %in% SELF_RENDERED)) {
  dirty <- system2("git", c("-C", repo_root(), "status", "--porcelain", "--untracked-files=no"),
                   stdout = TRUE)
  if (length(dirty))
    stop("working tree has uncommitted changes; commit before promoting ",
         paste(args, collapse = ", "), ".", call. = FALSE)
}
plans <- lapply(args, plan_promotion)

for (p in plans) {
  i <- p$i
  dir.create(p$dst_dir, recursive = TRUE, showWarnings = FALSE)
  if (!all(file.copy(file.path(p$src_dir, p$files), file.path(p$dst_dir, p$files), overwrite = TRUE)))
    stop("copy failed", call. = FALSE)
  if (any(sha(file.path(p$src_dir, p$files)) != sha(file.path(p$dst_dir, p$files))))
    stop("promoted copy is not byte-exact", call. = FALSE)

  reg$rendered_artifact[i] <- relative_to(file.path(p$dst_dir, paste0(p$pid, ".svg")))
  reg$hash[i] <- sha(file.path(p$dst_dir, paste0(p$pid, ".svg")))
  if (p$mmm) {
    reg$canonical_source_data[i] <- paste0("source_data/MMMSociability/", p$pin$bundle_id)
    reg$source_commit[i] <- p$pin$mmm_git_commit
  } else {
    dir.create(p$sd_dst, recursive = TRUE, showWarnings = FALSE)
    unlink(list.files(p$sd_dst, full.names = TRUE))
    if (!all(file.copy(file.path(p$sd_src, p$expected), file.path(p$sd_dst, p$expected))))
      stop("source-data copy failed", call. = FALSE)
    if (!setequal(list.files(p$sd_dst), p$expected) ||
        any(sha(file.path(p$sd_src, p$expected)) != sha(file.path(p$sd_dst, p$expected))))
      stop("promoted source data is not a byte-exact copy of the render", call. = FALSE)
    rows <- vapply(file.path(p$sd_dst, p$expected), function(f)
      nrow(utils::read.csv(f, check.names = FALSE)), integer(1))
    utils::write.csv(data.frame(file = p$expected, panel = p$panel_ids, rows = unname(rows),
                                sha256 = sha(file.path(p$sd_dst, p$expected)),
                                render_git_commit = head, stringsAsFactors = FALSE),
                     file.path(p$sd_dst, "00_manifest.csv"), row.names = FALSE)
    reg$canonical_source_data[i] <- relative_to(p$sd_dst)
    reg$source_commit[i] <- head
  }
  if (length(p$panel_ids)) reg$panels[i] <- paste(p$panel_ids, collapse = ",")
  cat("Promoted", p$pid, "->", relative_to(p$dst_dir), "\n  sha256", reg$hash[i], "\n  source",
      reg$canonical_source_data[i], "\n  commit", reg$source_commit[i], "\n")
}
utils::write.csv(reg, REG, row.names = FALSE)
