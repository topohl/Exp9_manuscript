source(testthat::test_path("..", "..", "R", "paths.R"))
source(repo_path("R", "vendor", "plotting_nature.R"))
source(repo_path("R", "panels", "cc4_phase_groups_candidate.R"))
upstream <- "MMMSociability"
src <- repo_path("source_data", upstream, "cc4_phase_groups_20260929_v2")

testthat::test_that("CC4 source is complete and hash verified", {
  d <- cc4_read_bundle(src)
  testthat::expect_equal(nrow(d$summary),108L)
  testthat::expect_true(all(d$summary$Batches==3L))
  testthat::expect_setequal(unique(d$summary$PhaseLabel),c(paste0("I",2:5),paste0("A",1:5)))
  testthat::expect_identical(nature_palette("group"),c(CON="#3E3C6F",RES="#C6C3BB",SUS="#E63A48"))
  p <- cc4_candidate_plots(d)
  testthat::expect_named(p,c("cc4_group_activity","cc4_group_changes","cc4_group_differences"))
  # Use the production Cairo device; the default PostScript device cannot
  # resolve installed Windows Arial and gives spurious font warnings.
  grDevices::cairo_pdf(tempfile(fileext=".pdf"),width=7.2,height=5.5)
  on.exit(grDevices::dev.off(),add=TRUE)
  for (plot in p) testthat::expect_no_warning(patchwork::patchworkGrob(plot))
})

testthat::test_that("CC4 rendering fails on tampered inputs and existing outputs", {
  tmp <- tempfile("cc4_frozen_");dir.create(tmp)
  files <- list.files(src,full.names=TRUE)
  testthat::expect_true(all(file.copy(files,tmp)))
  cat("\n",file=file.path(tmp,"group_phase_summary.csv"),append=TRUE)
  testthat::expect_error(cc4_read_bundle(tmp),"hash mismatch")
  withr::local_dir(repo_root())
  source(repo_path("figures","cc4_phase_groups_candidate.R"))
  testthat::expect_error(render_cc4_phase_groups("cc4_phase_groups_20260929_v2",tmp),"already exists")
})
