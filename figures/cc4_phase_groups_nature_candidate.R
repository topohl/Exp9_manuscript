# Nature-format presentation candidate; scientific results remain frozen.
source(file.path("R","paths.R"))
source(repo_path("R","vendor","plotting_nature.R"))
source(repo_path("R","panels","cc4_phase_groups_candidate.R"))
source(repo_path("R","panels","cc4_phase_groups_nature.R"))

render_cc4_nature <- function(bundle_id,output_dir) {
  if (!grepl("^cc4_phase_groups_[A-Za-z0-9_-]+$",bundle_id)) stop("Invalid CC4 bundle ID.",call.=FALSE)
  if (file.exists(output_dir)) stop("Candidate output already exists.",call.=FALSE)
  upstream <- "MMMSociability_candidates"
  src <- repo_path("source_data",upstream,bundle_id)
  d <- cc4_read_bundle(src)
  plots <- cc4_nature_plots(d)
  dir.create(output_dir,recursive=TRUE)
  for (name in names(plots)) {
    height <- if (name=="cc4_nature_group_contrasts") 145 else 120
    ggplot2::ggsave(file.path(output_dir,paste0(name,".png")),plots[[name]],device=ragg::agg_png,width=183,height=height,units="mm",dpi=300,bg="white")
    ggplot2::ggsave(file.path(output_dir,paste0(name,".pdf")),plots[[name]],device=grDevices::cairo_pdf,width=183,height=height,units="mm",bg="white")
    ggplot2::ggsave(file.path(output_dir,paste0(name,".svg")),plots[[name]],device=svglite::svglite,width=183,height=height,units="mm",bg="white")
  }
  evidence <- c(file.path(src,"manifest.csv"),repo_path("R","vendor","plotting_nature.R"),
    repo_path("R","panels","cc4_phase_groups_candidate.R"),repo_path("R","panels","cc4_phase_groups_nature.R"),
    repo_path("figures","cc4_phase_groups_nature_candidate.R"))
  utils::write.csv(data.frame(Path=evidence,SHA256=unname(tools::sha256sum(evidence))),file.path(output_dir,"render_inputs.csv"),row.names=FALSE)
  files <- list.files(output_dir,full.names=TRUE)
  utils::write.csv(data.frame(File=basename(files),SHA256=unname(tools::sha256sum(files))),file.path(output_dir,"render_manifest.csv"),row.names=FALSE)
  invisible(plots)
}
if (sys.nframe()==0L) {
  args <- commandArgs(trailingOnly=TRUE)
  if (length(args)!=2L) stop("Usage: Rscript figures/cc4_phase_groups_nature_candidate.R <bundle_id> <new_output_dir>",call.=FALSE)
  render_cc4_nature(args[1],args[2])
}
