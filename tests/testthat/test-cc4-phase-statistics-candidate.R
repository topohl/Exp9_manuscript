source(testthat::test_path("..","..","R","paths.R"))
source(repo_path("R","vendor","plotting_nature.R"))
source(repo_path("R","panels","cc4_phase_groups_nature.R"))
source(repo_path("R","panels","cc4_phase_statistics_candidate.R"))

testthat::test_that("statistical forest displays frozen values and withholds gated rows", {
  d<-expand.grid(Sex=c("Female","Male"),Phase=c("Inactive","Active"),stringsAsFactors=FALSE)
  d$Estimate<-c(.8,.9,.95,1.1);d$Lower95<-c(.5,.6,.8,.85);d$Upper95<-c(1.3,1.4,1.1,1.3)
  d$P<-c(.5,.6,.7,.8);d$HolmP<-1;d$Status<-"PASS";d$Cages<-15L;d$CONCages<-3L;d$Animals<-58L;d$BootstrapDraws<-4999L
  original<-d;p<-cc4_stats_plot(d)
  testthat::expect_identical(d,original)
  testthat::expect_identical(p[[1]]$layers[[2]]$data$Estimate,d$Estimate)
  testthat::expect_identical(p[[1]]$layers[[2]]$data$Lower95,d$Lower95)
  testthat::expect_true(all(grepl("P = 1.000",p[[2]]$data$Text,fixed=TRUE)))
  d$Status[1]<-"WITHHELD_BOOTSTRAP_GATE";d$Lower95[1]<-d$Upper95[1]<-d$P[1]<-d$HolmP[1]<-NA_real_
  p<-cc4_stats_plot(d)
  testthat::expect_equal(nrow(p[[1]]$layers[[2]]$data),3L)
  testthat::expect_match(p[[2]]$data$Text[1],"withheld")
  grDevices::cairo_pdf(tempfile(fileext=".pdf"),width=7.2,height=3.4)
  on.exit(grDevices::dev.off(),add=TRUE)
  testthat::expect_no_warning(patchwork::patchworkGrob(p))
})

testthat::test_that("frozen statistical manifest rejects unsafe or missing paths", {
  tmp<-tempfile();dir.create(tmp)
  testthat::expect_error(cc4_stats_verify_bundle(tmp),"missing")
  utils::write.csv(data.frame(File="../outside.csv",SHA256="abc"),file.path(tmp,"manifest.csv"),row.names=FALSE)
  testthat::expect_error(cc4_stats_verify_bundle(tmp),"Invalid")
})
