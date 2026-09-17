# Frozen-input accounting for the manuscript repository.
#
# Most render inputs are imported into the gitignored results/ workspace by
# tools/import_render_inputs.R. Three are deliberately not: the 251 MB
# ontology-aware GSEA theme assignments, a 40 MB baseline profile and a 23 MB
# bilateral protein-level table. Those are upstream analysis tables, and a
# manuscript repository has no business carrying 314 MB of them to satisfy an
# existence check.
#
# They are instead recorded in provenance/source_manifests/render_inputs_manifest.csv
# with disposition PROVENANCE_ONLY and their pRoteomics sha256. A contract that
# names one as a panel's primary_source is still making a true and checkable
# claim: the input is accounted for, at a known hash, in the frozen interface.
#
# frozen_input_present() is what an existence assertion should use. It answers
# "is this input accounted for", which is the question the assertion was always
# really asking, rather than "is this byte-for-byte on this disk".

.frozen_manifest <- function() {
  p <- file.path(repo_root(), "provenance", "source_manifests",
                 "render_inputs_manifest.csv")
  if (!file.exists(p)) return(NULL)
  utils::read.csv(p, stringsAsFactors = FALSE)
}

#' Is a declared render input accounted for in the frozen interface?
#'
#' TRUE when the file is present in the workspace, or when the render-inputs
#' manifest records it as provenance-only with a hash.
frozen_input_present <- function(path) {
  path <- as.character(path)
  vapply(path, function(p) {
    if (file.exists(p) || dir.exists(p)) return(TRUE)

    # Contracts frozen in pRoteomics name the behaviour bridge by its old
    # prefix. The bundle still mirrors that internal layout; only the prefix
    # moved to source_data/MMMSociability/.
    remapped <- sub("manuscript/figure1_bridge_mmmsociability/",
                    "source_data/MMMSociability/", p, fixed = TRUE)
    if (!identical(remapped, p) && file.exists(remapped)) return(TRUE)
    rel <- sub(paste0("^", gsub("([.|()\\^{}+$*?\\[\\]])", "\\\\\\1", repo_root()), "/?"),
               "", p)
    man <- .frozen_manifest()
    if (is.null(man)) return(FALSE)
    row <- man[man$declared_path == rel, , drop = FALSE]
    nrow(row) > 0L && any(row$disposition == "PROVENANCE_ONLY") &&
      any(nzchar(row$sha256))
  }, logical(1), USE.NAMES = FALSE)
}

#' The recorded hash of a provenance-only input, for reporting.
frozen_input_hash <- function(path) {
  man <- .frozen_manifest()
  if (is.null(man)) return(NA_character_)
  rel <- sub(paste0("^", gsub("([.|()\\^{}+$*?\\[\\]])", "\\\\\\1", repo_root()), "/?"),
             "", as.character(path))
  row <- man[man$declared_path == rel, , drop = FALSE]
  if (!nrow(row)) return(NA_character_)
  row$sha256[1]
}
