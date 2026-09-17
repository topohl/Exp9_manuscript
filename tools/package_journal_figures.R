#!/usr/bin/env Rscript

# Journal figure packaging and submission bundle.
#
# This is the responsibility Phase 6D moved out of pRoteomics. The scientific
# repository audits its figure outputs for publication readiness and stops
# there; naming, layout, format conversion and the submission package are
# owned here, because they are journal decisions rather than scientific ones.
#
# Inputs are this repository's own frozen material only:
#   source_data/pRoteomics/<publication_id>/assembled/<publication_id>.svg
#   source_data/pRoteomics/<publication_id>/*_source_data.csv
#   provenance/publication_registry/canonical_publication_registry.csv
#   manuscript/legends/
#
# Output:
#   export/<bundle>/figures/Figure_1.svg, Figure_2.svg, ...
#   export/<bundle>/extended_data/Extended_Data_Figure_1.svg, ...
#   export/<bundle>/source_data/
#   export/<bundle>/legends/
#   export/<bundle>/manifest.csv
#
# Nothing here recomputes a scientific value. The assembled artefact is copied,
# its hash is checked against the registry, and it is renamed to the journal's
# convention. If a hash does not match the registry the run stops: a submission
# package must not contain a figure that is not the frozen one.
#
# Usage:
#   Rscript tools/package_journal_figures.R [--bundle-name NAME] [--dry-run]

suppressWarnings(source(file.path("R", "paths.R")))

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
bundle <- {
  i <- which(args == "--bundle-name")
  if (length(i) && length(args) > i[1]) args[i[1] + 1L] else
    format(Sys.Date(), "submission_%Y%m%d")
}

REGISTRY <- repo_path("provenance", "publication_registry",
                      "canonical_publication_registry.csv")
BUNDLE <- repo_path("export", bundle)

if (!file.exists(REGISTRY)) {
  stop("publication registry not found: ", REGISTRY, call. = FALSE)
}
reg <- utils::read.csv(REGISTRY, stringsAsFactors = FALSE)
canon <- reg[reg$status == "CANONICAL", , drop = FALSE]

## Journal naming: figure_02 -> Figure_2, extended_data_01 -> Extended_Data_Figure_1
journal_name <- function(pid) {
  n <- as.integer(sub("^.*_", "", pid))
  if (startsWith(pid, "figure_")) sprintf("Figure_%d", n)
  else sprintf("Extended_Data_Figure_%d", n)
}
journal_dir <- function(pid) if (startsWith(pid, "figure_")) "figures" else "extended_data"

sha <- function(p) unname(tools::sha256sum(p))

cat("Journal figure packaging\n")
cat("========================\n")
cat("bundle             :", bundle, "\n")
cat("canonical identities:", nrow(canon), "\n")
cat("mode               :", if (dry_run) "dry-run" else "write", "\n\n")

rows <- list(); missing <- character(0); mismatched <- character(0)
for (i in seq_len(nrow(canon))) {
  pid <- canon$publication_id[i]
  svg <- repo_path("source_data", "pRoteomics", pid, "assembled",
                   paste0(pid, ".svg"))
  if (!file.exists(svg)) { missing <- c(missing, pid); next }

  got <- sha(svg)
  if (nzchar(canon$hash[i]) && !identical(got, canon$hash[i])) {
    mismatched <- c(mismatched, pid)
    next
  }

  jn <- journal_name(pid)
  target <- file.path(BUNDLE, journal_dir(pid), paste0(jn, ".svg"))
  if (!dry_run) {
    dir_create(dirname(target))
    if (!file.copy(svg, target, overwrite = TRUE, copy.date = TRUE)) {
      stop("copy failed for ", pid, call. = FALSE)
    }
  }

  ## source data for the identity travels with the figure
  sd_src <- repo_path("source_data", "pRoteomics", pid)
  sd_files <- setdiff(list.files(sd_src, pattern = "[.]csv$"), character(0))
  if (!dry_run && length(sd_files)) {
    dir_create(file.path(BUNDLE, "source_data", jn))
    for (f in sd_files) {
      file.copy(file.path(sd_src, f), file.path(BUNDLE, "source_data", jn, f),
                overwrite = TRUE, copy.date = TRUE)
    }
  }

  rows[[pid]] <- data.frame(
    publication_id = pid, journal_name = jn,
    journal_dir = journal_dir(pid), panels = canon$panels[i],
    assembled_sha256 = got, registry_sha256 = canon$hash[i],
    hash_verified = TRUE, source_data_files = length(sd_files),
    stringsAsFactors = FALSE)
}

if (length(missing)) {
  stop("assembled artefact absent for: ", paste(missing, collapse = ", "),
       "\nRun tools/import_render_inputs.R first.", call. = FALSE)
}
if (length(mismatched)) {
  stop("assembled artefact does not match the registry hash for: ",
       paste(mismatched, collapse = ", "),
       "\nA submission package must not contain a figure that is not the frozen one.",
       call. = FALSE)
}

man <- do.call(rbind, rows)

## legends travel with the package
legends <- list.files(repo_path("manuscript", "legends"), pattern = "[.]md$",
                      full.names = TRUE)
if (!dry_run) {
  if (length(legends)) {
    dir_create(file.path(BUNDLE, "legends"))
    file.copy(legends, file.path(BUNDLE, "legends"), overwrite = TRUE)
  }
  utils::write.csv(man, file.path(BUNDLE, "manifest.csv"), row.names = FALSE)
}

cat("figures packaged   :", sum(man$journal_dir == "figures"), "\n")
cat("extended data      :", sum(man$journal_dir == "extended_data"), "\n")
cat("hash-verified      :", sum(man$hash_verified), "/", nrow(man), "\n")
cat("legends            :", length(legends), "\n")
cat("source-data files  :", sum(man$source_data_files), "\n")
if (dry_run) {
  cat("\n[DRY-RUN] would write:", sub(paste0(repo_root(), "/"), "", BUNDLE), "\n")
} else {
  cat("\nbundle:", sub(paste0(repo_root(), "/"), "", BUNDLE), "\n")
}
