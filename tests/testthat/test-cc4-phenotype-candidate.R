source(testthat::test_path("..","..","R","paths.R"))
source(repo_path("R","vendor","plotting_nature.R"))
source(repo_path("R","panels","cc4_phase_groups_candidate.R"))
source(repo_path("R","panels","cc4_phase_groups_nature.R"))
source(repo_path("R","panels","cc4_phenotype_candidate.R"))

phenotype_fixture<-function() {
  context<-expand.grid(Sex=c("Female","Male"),Phase=c("Inactive","Active"),stringsAsFactors=FALSE)
  o<-transform(context,P=.2,HolmP=.8,Status="PASS")
  c<-merge(context,data.frame(Contrast=c("RES-CON","SUS-CON","SUS-RES")))
  c<-transform(c,Estimate=1.1,Lower95=.7,Upper95=1.6,P=.3,HolmP=1,TestStatus="PASS",IntervalStatus="PASS")
  b<-merge(context,data.frame(Hypothesis=c("omnibus","RES-CON","SUS-CON","SUS-RES","full")))
  list(omnibus=o,contrasts=c,bootstrap=transform(b,Status="PASS",Used=4999),population=data.frame(Animals=1))
}
fixture_write<-function(d,path) {
  dir.create(path,recursive=TRUE,showWarnings=FALSE)
  for(n in names(d))utils::write.csv(d[[n]],file.path(path,paste0(if(n=="bootstrap")"bootstrap_status" else n,".csv")),row.names=FALSE)
  utils::write.csv(data.frame(Valid=TRUE),file.path(path,"fit_diagnostics.csv"),row.names=FALSE)
  utils::write.csv(data.frame(AnimalKey="synthetic"),file.path(path,"animal_period_input.csv"),row.names=FALSE)
  writeLines("Synthetic fixture; no scientific inference.",file.path(path,"protocol.txt"))
  f<-list.files(path,full.names=TRUE);f<-f[basename(f)!="manifest.csv"]
  utils::write.csv(data.frame(File=basename(f),SHA256=unname(tools::sha256sum(f))),file.path(path,"manifest.csv"),row.names=FALSE)
}

testthat::test_that("frozen inference requires complete, final, verified bootstrap evidence", {
  tmp<-tempfile("phenotype_fixture_");d<-phenotype_fixture();fixture_write(d,tmp)
  testthat::expect_equal(cc4g_read(tmp),d)
  d$bootstrap$Used[1]<-199;fixture_write(d,tmp)
  testthat::expect_error(cc4g_read(tmp),"evidence")
  d<-phenotype_fixture();d$contrasts$TestStatus[1]<-"VALIDATION_ONLY_NO_INFERENCE";fixture_write(d,tmp)
  testthat::expect_error(cc4g_read(tmp),"Validation-only")
  d<-phenotype_fixture();d$contrasts$Sex[1]<-"Unknown";fixture_write(d,tmp)
  testthat::expect_error(cc4g_read(tmp),"Incomplete")
  d<-phenotype_fixture();d$omnibus$P[1]<-NA_real_;fixture_write(d,tmp)
  testthat::expect_error(cc4g_read(tmp),"omnibus evidence")
  d<-phenotype_fixture();d$contrasts$Lower95[1]<--1;fixture_write(d,tmp)
  testthat::expect_error(cc4g_read(tmp),"interval")
  fixture_write(phenotype_fixture(),tmp);cat("tampered",file=file.path(tmp,"protocol.txt"),append=TRUE)
  testthat::expect_error(cc4g_read(tmp),"hash mismatch")
  testthat::expect_equal(cc4g_p_label(c(.0002,.2)),c("P < 0.001","P = 0.200"))
})

testthat::test_that("equal counts cannot conceal different animals or cages", {
  m<-data.frame(Sex="Female",Phase="Inactive",Group="RES",Batch="B3",CageID="B3|1",AnimalKey=c("00314","OR424"),Period="Reference")
  p<-m[,setdiff(names(m),"Period")];p$PhaseLabel<-"I2";p$IncludedInPrimary<-TRUE
  testthat::expect_true(cc4g_compare_rosters(m,p[2:1,]))
  p$AnimalKey[1]<-"314"
  testthat::expect_error(cc4g_compare_rosters(m,p),"identity mismatch")
  p$AnimalKey[1]<-"00314";p$CageID[1]<-"B3|2"
  testthat::expect_error(cc4g_compare_rosters(m,p),"identity mismatch")
  p$CageID[1]<-"B3|1";p<-rbind(p,p[1,])
  testthat::expect_error(cc4g_compare_rosters(m,p),"Invalid roster")
})

testthat::test_that("eight panels use frozen values, shared scales and the behavioural palette", {
  upstream<-"MMMSociability_candidates"
  d<-cc4_read_bundle(repo_path("source_data",upstream,"cc4_phase_groups_20260929_v2"))
  s<-phenotype_fixture();original<-list(d=d,s=s);p<-cc4g_panels(s,d)
  testthat::expect_length(p,8L)
  for(i in 1:8)testthat::expect_identical(p[[i]]$labels$tag,letters[i])
  for(i in 1:4) {
    sex<-if(i%%2==1)"Female" else "Male";phase<-if(i<=2)"Inactive" else "Active"
    expected<-subset(d$summary,Sex==sex&Phase==phase&Measure=="Rate"&PhaseLabel!="A1")
    testthat::expect_equal(p[[i]]$data$Estimate,expected$Estimate)
    testthat::expect_equal(p[[i]]$scales$get_scales("fill")$palette(3),
                           unlist(yaml::read_yaml(repo_path("config","manuscript_palette.yml"))$group)[c("CON","RES","SUS")])
    testthat::expect_equal(p[[i+4]]$data$Estimate,subset(s$contrasts,Sex==sex&Phase==phase)$Estimate)
  }
  testthat::expect_equal(p[[1]]$scales$get_scales("y")$limits,p[[2]]$scales$get_scales("y")$limits)
  testthat::expect_equal(p[[3]]$scales$get_scales("y")$limits,p[[4]]$scales$get_scales("y")$limits)
  for(i in 6:8)testthat::expect_equal(p[[5]]$scales$get_scales("x")$limits,p[[i]]$scales$get_scales("x")$limits)
  grDevices::cairo_pdf(tempfile(fileext=".pdf"),width=7.2,height=9.1)
  on.exit(grDevices::dev.off(),add=TRUE)
  testthat::expect_no_warning(patchwork::patchworkGrob(cc4g_figure(s,d)))
  s$contrasts$TestStatus[1]<-"WITHHELD_OBSERVED_NUMERICAL_FAILURE"
  s$contrasts$IntervalStatus[1]<-"WITHHELD_REFIT_FAILURES_OVER_5_PERCENT"
  s$contrasts$P[1]<-s$contrasts$HolmP[1]<-s$contrasts$Lower95[1]<-s$contrasts$Upper95[1]<-NA_real_
  testthat::expect_no_warning(patchwork::patchworkGrob(cc4g_figure(s,d)))
  testthat::expect_identical(d,original$d)
  old_wd<-getwd();on.exit(setwd(old_wd),add=TRUE);setwd(repo_path())
  source(repo_path("figures","cc4_phenotype_candidate.R"))
  testthat::expect_error(render_cc4_phenotypes("cc4_phenotypes_test","cc4_phase_groups_test",tempdir()),"already exists")
})
