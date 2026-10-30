testthat::local_edition(3)

source(testthat::test_path("..", "..", "R", "paths.R"))

# Freshness gate for the proteomic inputs taken from the pRoteomics integration steps
# (provenance/source_manifests/primary_inputs.csv; tools/import_primary_inputs.R). Those steps run
# as optional pipeline steps, and a failed rerun leaves its previous tables in place, so each step
# writes manifests/run_status.yml: "completed" only when the run finished, with the SHA-256 of every
# table it wrote. An imported file must be a table of a completed run. Needs the primary tree
# (PROTEOMICS_PRIMARY_ROOT, else R:/ or Analysis/proteomics_original); skips without it.

primary_tree_root <- function() {
  candidates <- c(Sys.getenv("PROTEOMICS_PRIMARY_ROOT", unset = ""), "R:/",
                  "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/proteomics_original")
  candidates <- candidates[nzchar(candidates)]
  hit <- candidates[file.exists(file.path(candidates, "pipeline.yml")) &
                      file.exists(file.path(candidates, "config", "normalisation.yml"))]
  if (!length(hit)) return("")
  norm <- yaml::read_yaml(file.path(hit[[1]], "config", "normalisation.yml"))
  if (!identical(norm$sample_centring, norm$approved_sample_centring)) return("")
  sub("/+$", "", hit[[1]])
}

# The integration step that wrote each imported file: a results/integration/<step>/<scope>/tables/
# path names it; the selection inventories are exported byte copies of
# build_display_selection_inventories tables.
integration_producer <- function(source_path) {
  if (grepl("^results/integration/[^/]+/[^/]+/tables/", source_path)) {
    parts <- strsplit(source_path, "/", fixed = TRUE)[[1]]
    return(c(owner = parts[[3]], scope = parts[[4]]))
  }
  if (startsWith(source_path, "exports/publication_source_data/supplementary_selection_inventories/")) {
    return(c(owner = "build_display_selection_inventories", scope = "global"))
  }
  NULL
}

testthat::test_that("imported integration inputs are tables of a completed pRoteomics run", {
  root <- primary_tree_root()
  testthat::skip_if(!nzchar(root), "primary pRoteomics tree not available")
  inputs <- utils::read.csv(repo_path("provenance", "source_manifests", "primary_inputs.csv"),
                            stringsAsFactors = FALSE)
  checked <- 0L
  for (i in seq_len(nrow(inputs))) {
    producer <- integration_producer(inputs$source_path[[i]])
    if (is.null(producer)) next
    status_path <- file.path(root, "results", "integration", producer[["owner"]], producer[["scope"]],
                             "manifests", "run_status.yml")
    info <- paste(inputs$declared_path[[i]], "<-", producer[["owner"]])
    testthat::expect_true(file.exists(status_path), info = info)
    if (!file.exists(status_path)) next
    status <- yaml::read_yaml(status_path)
    testthat::expect_identical(status$status, "completed", info = info)
    testthat::expect_identical(status$tables_sha256[[basename(inputs$source_path[[i]])]],
                               inputs$sha256[[i]], info = info)
    checked <- checked + 1L
  }
  testthat::expect_gt(checked, 0L)
})
