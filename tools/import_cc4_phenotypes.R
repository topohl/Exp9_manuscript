source(file.path("R","paths.R"))
source(repo_path("R","panels","cc4_phenotype_candidate.R"))
import_cc4_phenotypes<-function(source_dir,bundle_id) {
  if(!grepl("^cc4_phenotypes_[A-Za-z0-9_-]+$",bundle_id))stop("Invalid phenotype bundle ID.",call.=FALSE)
  m<-cc4g_verify(source_dir);upstream<-"MMMSociability_candidates"
  dest<-repo_path("source_data",upstream,bundle_id)
  if(file.exists(dest))stop("Frozen phenotype import already exists.",call.=FALSE)
  names<-c(m$File,"manifest.csv");hashes<-unname(tools::sha256sum(file.path(source_dir,names)))
  for(f in names){dir.create(dirname(file.path(dest,f)),recursive=TRUE,showWarnings=FALSE)
    if(!file.copy(file.path(source_dir,f),file.path(dest,f),overwrite=FALSE))stop("Phenotype import failed.",call.=FALSE)}
  if(!identical(hashes,unname(tools::sha256sum(file.path(dest,names)))))stop("Phenotype import changed.",call.=FALSE)
  cc4g_verify(dest);invisible(dest)
}
if(sys.nframe()==0L){args<-commandArgs(trailingOnly=TRUE);if(length(args)!=2L)stop("Usage: Rscript tools/import_cc4_phenotypes.R <Stage31d_run> <bundle_id>",call.=FALSE);import_cc4_phenotypes(args[1],args[2])}
