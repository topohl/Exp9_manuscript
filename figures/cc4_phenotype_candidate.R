source(file.path("R","paths.R"))
source(repo_path("R","vendor","plotting_nature.R"))
source(repo_path("R","panels","cc4_phase_groups_candidate.R"))
source(repo_path("R","panels","cc4_phase_groups_nature.R"))
source(repo_path("R","panels","cc4_phenotype_candidate.R"))
render_cc4_phenotypes<-function(bundle_id,descriptive_id,output_dir) {
  if(!grepl("^cc4_phenotypes_[A-Za-z0-9_-]+$",bundle_id)||!grepl("^cc4_phase_groups_[A-Za-z0-9_-]+$",descriptive_id))stop("Invalid bundle IDs.",call.=FALSE)
  if(file.exists(output_dir))stop("Candidate output already exists.",call.=FALSE)
  upstream<-"MMMSociability_candidates"
  src<-repo_path("source_data",upstream,bundle_id);ds<-repo_path("source_data",upstream,descriptive_id)
  stats<-cc4g_read(src);descriptive<-cc4_read_bundle(ds)
  # This candidate uses identical per-group populations for trajectory and model.
  cc4g_compare_rosters(utils::read.csv(file.path(src,"animal_period_input.csv"),colClasses="character"),
    utils::read.csv(file.path(ds,"animal_phase_activity.csv"),colClasses="character"))
  n<-subset(descriptive$population,PhaseLabel%in%c("I2","A2"))
  expected<-aggregate(PrimaryAnimals~Sex+Phase+Group,n,sum)
  pop<-merge(expected,stats$population,by=c("Sex","Phase","Group"))
  if(nrow(pop)!=12L||any(pop$PrimaryAnimals!=pop$Animals))stop("Trajectory/model population mismatch.",call.=FALSE)
  p<-cc4g_figure(stats,descriptive)
  dir.create(output_dir,recursive=TRUE)
  for(ext in c("png","pdf","svg"))ggplot2::ggsave(file.path(output_dir,paste0("cc4_phenotype_response.",ext)),p,
    device=switch(ext,png=ragg::agg_png,pdf=grDevices::cairo_pdf,svg=svglite::svglite),width=183,height=230,units="mm",dpi=300,bg="white")
  paths<-c(file.path(src,"manifest.csv"),file.path(ds,"manifest.csv"),repo_path("R","paths.R"),repo_path("R","vendor","plotting_nature.R"),
    repo_path("R","panels","cc4_phase_groups_candidate.R"),repo_path("R","panels","cc4_phase_groups_nature.R"),
    repo_path("R","panels","cc4_phenotype_candidate.R"),repo_path("figures","cc4_phenotype_candidate.R"))
  utils::write.csv(data.frame(Path=paths,SHA256=unname(tools::sha256sum(paths))),file.path(output_dir,"render_inputs.csv"),row.names=FALSE)
  capture.output(sessionInfo(),file=file.path(output_dir,"render_session_info.txt"))
  files<-list.files(output_dir,full.names=TRUE)
  utils::write.csv(data.frame(File=basename(files),SHA256=unname(tools::sha256sum(files))),file.path(output_dir,"render_manifest.csv"),row.names=FALSE)
  invisible(p)
}
if(sys.nframe()==0L){args<-commandArgs(trailingOnly=TRUE);if(length(args)!=3L)stop("Usage: Rscript figures/cc4_phenotype_candidate.R <phenotype_bundle> <descriptive_bundle> <new_output>",call.=FALSE);render_cc4_phenotypes(args[1],args[2],args[3])}
