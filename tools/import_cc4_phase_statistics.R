source(file.path("R","paths.R"))
source(repo_path("R","panels","cc4_phase_statistics_candidate.R"))
import_cc4_statistics<-function(source_dir,bundle_id) {
  if(!grepl("^cc4_phase_statistics_[A-Za-z0-9_-]+$",bundle_id)) stop("Invalid statistical bundle ID.",call.=FALSE)
  m<-cc4_stats_verify_bundle(source_dir)
  upstream<-"MMMSociability_candidates";dest<-repo_path("source_data",upstream,bundle_id)
  if(file.exists(dest)) stop("Frozen statistical import already exists.",call.=FALSE)
  files<-c(m$File,"manifest.csv");hashes<-unname(tools::sha256sum(file.path(source_dir,files)))
  for(f in files) {
    dir.create(dirname(file.path(dest,f)),recursive=TRUE,showWarnings=FALSE)
    if(!file.copy(file.path(source_dir,f),file.path(dest,f),overwrite=FALSE)) stop("Statistical import failed.",call.=FALSE)
  }
  if(!identical(hashes,unname(tools::sha256sum(file.path(dest,files))))) stop("Statistical import changed.",call.=FALSE)
  cc4_stats_verify_bundle(dest);invisible(dest)
}
if(sys.nframe()==0L) {
  args<-commandArgs(trailingOnly=TRUE)
  if(length(args)!=2L) stop("Usage: Rscript tools/import_cc4_phase_statistics.R <Stage31c_run> <bundle_id>",call.=FALSE)
  import_cc4_statistics(args[1],args[2])
}
