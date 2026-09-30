# Explicit frozen Stage 31b import, isolated from canonical Figure 1 assets.
source(file.path("R", "paths.R"))
source(repo_path("R", "panels", "cc4_phase_groups_candidate.R"))
import_cc4_phase_groups <- function(source_dir, bundle_id) {
  if (!grepl("^cc4_phase_groups_[A-Za-z0-9_-]+$", bundle_id)) stop("Invalid CC4 bundle ID.", call. = FALSE)
  cc4_verify_bundle(source_dir)
  upstream <- "MMMSociability_candidates"
  dest <- repo_path("source_data", upstream, bundle_id)
  if (file.exists(dest)) stop("Frozen import already exists.", call. = FALSE)
  m <- utils::read.csv(file.path(source_dir,"manifest.csv"),stringsAsFactors=FALSE)
  names <- c(m$File,"manifest.csv")
  hashes <- unname(tools::sha256sum(file.path(source_dir,names)))
  dir.create(dest,recursive=TRUE)
  if (!all(file.copy(file.path(source_dir,names),file.path(dest,names),overwrite=FALSE)) ||
      !identical(hashes,unname(tools::sha256sum(file.path(dest,names))))) stop("CC4 import incomplete or changed.",call.=FALSE)
  cc4_verify_bundle(dest)
  cat("Verified frozen CC4 import: ",dest,"\n",sep="")
  invisible(dest)
}
if (sys.nframe()==0L) {
  args <- commandArgs(trailingOnly=TRUE)
  if (length(args)!=2L) stop("Usage: Rscript tools/import_cc4_phase_groups.R <Stage31b_output> <cc4_phase_groups_bundle_id>",call.=FALSE)
  import_cc4_phase_groups(args[1],args[2])
}
