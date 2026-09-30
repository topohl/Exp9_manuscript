source(file.path("R","paths.R"))
source(repo_path("R","vendor","plotting_nature.R"))
source(repo_path("R","panels","cc4_phase_groups_nature.R"))
source(repo_path("R","panels","cc4_phase_statistics_candidate.R"))
render_cc4_statistics<-function(bundle_id,output_dir) {
  if(!grepl("^cc4_phase_statistics_[A-Za-z0-9_-]+$",bundle_id)) stop("Invalid statistical bundle ID.",call.=FALSE)
  if(file.exists(output_dir)) stop("Statistical candidate output already exists.",call.=FALSE)
  upstream<-"MMMSociability_candidates";src<-repo_path("source_data",upstream,bundle_id)
  d<-cc4_stats_read_bundle(src);p<-cc4_stats_plot(d)
  dir.create(output_dir,recursive=TRUE)
  for(ext in c("png","pdf","svg")) ggplot2::ggsave(file.path(output_dir,paste0("cc4_primary_statistics.",ext)),p,
    device=switch(ext,png=ragg::agg_png,pdf=grDevices::cairo_pdf,svg=svglite::svglite),width=183,height=85,units="mm",dpi=300,bg="white")
  inputs<-c(file.path(src,"manifest.csv"),repo_path("R","vendor","plotting_nature.R"),repo_path("R","panels","cc4_phase_groups_nature.R"),
    repo_path("R","panels","cc4_phase_statistics_candidate.R"),repo_path("figures","cc4_phase_statistics_candidate.R"))
  utils::write.csv(data.frame(Path=inputs,SHA256=unname(tools::sha256sum(inputs))),file.path(output_dir,"render_inputs.csv"),row.names=FALSE)
  files<-list.files(output_dir,full.names=TRUE)
  utils::write.csv(data.frame(File=basename(files),SHA256=unname(tools::sha256sum(files))),file.path(output_dir,"render_manifest.csv"),row.names=FALSE)
  invisible(p)
}
if(sys.nframe()==0L) {
  args<-commandArgs(trailingOnly=TRUE)
  if(length(args)!=2L) stop("Usage: Rscript figures/cc4_phase_statistics_candidate.R <bundle_id> <new_output_dir>",call.=FALSE)
  render_cc4_statistics(args[1],args[2])
}
