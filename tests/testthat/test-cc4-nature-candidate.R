source(testthat::test_path("..","..","R","paths.R"))
source(repo_path("R","vendor","plotting_nature.R"))
source(repo_path("R","panels","cc4_phase_groups_candidate.R"))
source(repo_path("R","panels","cc4_phase_groups_nature.R"))
upstream <- "MMMSociability"
src <- repo_path("source_data",upstream,"cc4_phase_groups_20260929_v2")

testthat::test_that("Nature panels retain frozen estimates and readable units", {
  d <- cc4_read_bundle(src);before<-d
  p <- cc4_nature_plots(d)
  testthat::expect_identical(d,before)
  testthat::expect_named(p,c("cc4_nature_activity","cc4_nature_changes","cc4_nature_group_contrasts"))
  grDevices::cairo_pdf(tempfile(fileext=".pdf"),width=7.2,height=4.7)
  on.exit(grDevices::dev.off(),add=TRUE)
  for (plot in p) testthat::expect_no_warning(patchwork::patchworkGrob(plot))
  # Each panel carries explicit visible lettering, fixed sex-paired scales,
  # and values directly from its declared sex/phase subset of the frozen table.
  for (name in names(p)) {
    for (i in 1:4) {
      panel <- p[[name]][[i]]
      testthat::expect_identical(panel$labels$tag,letters[i])
      testthat::expect_equal(panel$theme$axis.text$size,6)
      testthat::expect_true(panel$theme$plot.tag$size<=7)
      if (name!="cc4_nature_group_contrasts") {
        family <- if (i<=2) "Inactive" else "Active"
        sex <- if (i%%2==1) "Female" else "Male"
        measure <- if (name=="cc4_nature_activity") "Rate" else "Change"
        expected<-subset(d$summary,Sex==sex & Phase==family & Measure==measure)
        if (measure=="Change") expected<-subset(expected,PhaseLabel %in% c("I3","I4","I5","A3","A4","A5"))
        testthat::expect_equal(panel$data$Estimate,expected$Estimate)
        testthat::expect_equal(panel$data$Lower95,expected$Lower95)
      }
    }
    testthat::expect_equal(p[[name]][[1]]$scales$get_scales(if (name=="cc4_nature_group_contrasts") "x" else "y")$limits,
      p[[name]][[2]]$scales$get_scales(if (name=="cc4_nature_group_contrasts") "x" else "y")$limits)
  }
})
