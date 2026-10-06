#!/usr/bin/env Rscript

# One-off importer for the frozen render-input workspace.
#
# The figure contracts in this repository declare, per panel, a figure_source
# (the panel SVG), a primary_source (the panel's source data) and
# input_dependencies (the upstream tables the panel's numbers came from). The
# first two are what the assembler reads; the third is traceability.
#
# This tool copies the declared assets out of a pRoteomics checkout into this
# repository's own results/ workspace, at exactly the relative paths the
# contracts name, and records a SHA-256 for each in a tracked manifest. Two
# consequences matter:
#
#   * every renderer and renderer test then reads its own repository, at the
#     path it already names. No renderer or test had to be rewritten, so no
#     assertion changed meaning;
#   * nothing reads a sibling repository at runtime. This tool is the only
#     thing that ever touches PROTEOMICS_ROOT, it is run by hand, and the
#     manifest it writes is what the suite verifies against.
#
# results/ is gitignored, exactly as it is in pRoteomics: it is an imported and
# regenerable workspace, not a deliverable. The manifest is tracked, so the
# integrity of a materialised workspace is checkable without the source repo.
#
# Assets above a size ceiling are recorded as PROVENANCE_ONLY rather than
# copied. Three declared assets are large scientific tables: the 251 MB
# ontology-aware GSEA theme assignments, a 40 MB baseline profile and a 23 MB
# bilateral protein-level table. In the active contract
# (figures/figure_contract.yml) they appear only under input_dependencies, so
# the manuscript layer never reads their contents; only the superseded
# generation contract lists them as primary_source. Copying 314 MB of
# scientific tables here to satisfy an existence check on a provenance field
# would be the wrong trade, so their hashes are recorded instead.
#
# A workspace that renders the panels reading those three tables (the ED4
# module panels and the ED6 atlases; Figure 2f and ED6 also check that they
# exist) materialises them with --materialize-provenance-only. Each is copied
# only if its bytes at PROTEOMICS_ROOT are exactly the bytes the tracked
# manifest records, and nothing else happens: the manifest is not rewritten, no
# other asset is touched, and no disposition or source commit changes.
#
# Usage:
#   PROTEOMICS_ROOT=/path/to/proteomics Rscript tools/import_render_inputs.R
#   PROTEOMICS_ROOT=/path/to/proteomics Rscript tools/import_render_inputs.R --materialize-provenance-only

suppressWarnings(source(file.path("R", "paths.R")))
library(yaml)

PR <- Sys.getenv("PROTEOMICS_ROOT", unset = "")
if (!nzchar(PR) || !dir.exists(PR)) {
  stop("Set PROTEOMICS_ROOT to a pRoteomics checkout. This tool is a one-off ",
       "import; nothing at runtime reads that path.", call. = FALSE)
}
SIZE_CEILING_MB <- 8
MANIFEST <- repo_path("provenance", "source_manifests", "render_inputs_manifest.csv")

sha <- function(p) unname(tools::sha256sum(p))

if ("--materialize-provenance-only" %in% commandArgs(trailingOnly = TRUE)) {
  man <- utils::read.csv(MANIFEST, stringsAsFactors = FALSE)
  po <- man[man$disposition == "PROVENANCE_ONLY", , drop = FALSE]
  for (i in seq_len(nrow(po))) {
    rel <- po$declared_path[i]
    src <- file.path(PR, rel)
    dst <- repo_path(rel)
    if (!file.exists(src)) stop("absent at PROTEOMICS_ROOT: ", rel, call. = FALSE)
    if (!identical(sha(src), po$sha256[i])) {
      stop("not the recorded bytes (sha256 differs from the manifest): ", rel,
           call. = FALSE)
    }
    if (file.exists(dst) && identical(sha(dst), po$sha256[i])) {
      cat("present          ", rel, "\n")
      next
    }
    dir_create(dirname(dst))
    if (!file.copy(src, dst, overwrite = TRUE, copy.date = TRUE) ||
        !identical(sha(dst), po$sha256[i])) {
      stop("copy failed or changed the bytes: ", rel, call. = FALSE)
    }
    cat(sprintf("copied %7.1f MB  %s\n", po$size_mb[i], rel))
  }
  cat("manifest unchanged:", sub(paste0(repo_root(), "/"), "", MANIFEST), "\n")
  quit(save = "no", status = 0L)
}
commit <- system2("git", c("-C", shQuote(PR), "rev-parse", "HEAD"), stdout = TRUE)[1]

FIELDS <- c("figure_source", "primary_source", "input_dependencies",
            "source_data", "rendered_artifact", "assembled", "sidecar",
            "legend_source")

declared <- list()
for (cf in c("figures/figure_contract.yml",
             "figures/figure_final_truth_v9_contract.yml")) {
  p <- repo_path(cf)
  if (!file.exists(p)) next
  y <- yaml::read_yaml(p)
  ## Figures whose analysis is owned by MMMSociability are rendered here from the
  ## pinned behaviour bundle (tools/import_behaviour_bundle.R). Their panels are
  ## never imported from pRoteomics, so this tool cannot overwrite them.
  ##
  ## Figures this repository renders itself are skipped for the same reason. The
  ## a-m Figure 3 is produced here by figures/final_truth_v9_figure_03.R from
  ## source_data/pRoteomics, while pRoteomics still holds the superseded a-i
  ## panels at the same results/ paths. Figure 2 and the proteomics Extended
  ## Data pages are rendered here too (figures/final_truth_v9_figure_02.R and
  ## figures/final_truth_v9_extended_data.R) and promoted from those renders.
  LOCALLY_RENDERED <- c("figure_02", "figure_03", "extended_data_01", "extended_data_02",
                        "extended_data_03", "extended_data_06", "extended_data_08")
  if (!is.null(y$figures))
    y$figures <- Filter(function(f)
      (!identical(f$analysis_repository, "topohl/MMMSociability") ||
         is.null(f$behaviour_bundle_id)) &&
        !isTRUE(as.character(f$canonical_publication_id) %in% LOCALLY_RENDERED),
      y$figures)
  walk <- function(x) {
    if (!is.list(x)) return(invisible(NULL))
    for (nm in names(x)) {
      if (nm %in% FIELDS) {
        for (v in as.character(unlist(x[[nm]]))) {
          declared[[v]] <<- unique(c(declared[[v]], nm))
        }
      }
      walk(x[[nm]])
    }
    if (is.null(names(x))) for (e in x) walk(e)
  }
  walk(y)
}
paths <- names(declared)
paths <- paths[grepl("^(results|config|data)/", paths)]
cat("assets declared by the contracts:", length(paths), "\n")

rows <- list(); copied <- 0L; prov <- 0L; absent <- character(0); failed <- character(0)
for (rel in paths) {
  src <- file.path(PR, rel)
  if (!file.exists(src)) { absent <- c(absent, rel); next }

  ## A contract may declare a directory rather than a single file. Import its
  ## contents file by file, applying the same per-file size ceiling.
  if (dir.exists(src)) {
    kids <- list.files(src, recursive = TRUE)
    n_ok <- 0L
    for (k in kids) {
      s2 <- file.path(src, k)
      if (file.info(s2)$size / 1024^2 > SIZE_CEILING_MB) next
      d2 <- repo_path(rel, k)
      dir_create(dirname(d2))
      if (file.copy(s2, d2, overwrite = TRUE, copy.date = TRUE)) n_ok <- n_ok + 1L
    }
    copied <- copied + n_ok
    rows[[rel]] <- data.frame(
      declared_path = rel,
      contract_fields = paste(sort(declared[[rel]]), collapse = "+"),
      disposition = "IMPORTED_DIRECTORY",
      size_mb = round(sum(file.info(file.path(src, kids))$size, na.rm = TRUE) / 1024^2, 2),
      sha256 = paste0("directory:", n_ok, "_files"),
      source_repo = "topohl/pRoteomics", source_commit = commit,
      stringsAsFactors = FALSE)
    next
  }
  mb <- file.info(src)$size / 1024^2
  fields <- paste(sort(declared[[rel]]), collapse = "+")

  disp <- if (mb > SIZE_CEILING_MB) "PROVENANCE_ONLY" else "IMPORTED"
  if (disp == "IMPORTED") {
    dst <- repo_path(rel)
    dir_create(dirname(dst))
    ok <- file.copy(src, dst, overwrite = TRUE, copy.date = TRUE)
    if (!ok) {
      failed <- c(failed, rel)
      disp <- "COPY_FAILED"
    } else if (!identical(sha(src), sha(dst))) {
      stop("hash mismatch after copy: ", rel, call. = FALSE)
    } else {
      copied <- copied + 1L
    }
  } else {
    prov <- prov + 1L
  }

  rows[[rel]] <- data.frame(
    declared_path = rel, contract_fields = fields, disposition = disp,
    size_mb = round(mb, 2), sha256 = sha(src),
    source_repo = "topohl/pRoteomics", source_commit = commit,
    stringsAsFactors = FALSE)
}

man <- do.call(rbind, rows)
dir_create(dirname(MANIFEST))
utils::write.csv(man, MANIFEST, row.names = FALSE)

cat("imported        :", copied, "files,",
    round(sum(man$size_mb[man$disposition == "IMPORTED"]), 1), "MB\n")
cat("provenance-only :", prov, "files,",
    round(sum(man$size_mb[man$disposition == "PROVENANCE_ONLY"]), 1), "MB\n")
cat("unresolvable    :", length(absent), "\n")
cat("copy failures   :", length(failed), "\n")
if (length(failed)) writeLines(paste0("  ", failed))
if (prov > 0L) {
  cat("\nprovenance-only (over", SIZE_CEILING_MB, "MB, hash recorded):\n")
  s <- man[man$disposition == "PROVENANCE_ONLY", ]
  for (i in seq_len(nrow(s))) {
    cat(sprintf("  %7.1f MB  %-30s %s\n", s$size_mb[i], s$contract_fields[i],
                basename(s$declared_path[i])))
  }
}
cat("\nmanifest:", sub(paste0(repo_root(), "/"), "", MANIFEST), "\n")
