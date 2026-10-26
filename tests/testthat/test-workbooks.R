# The Excel builders (R/workbooks.R): per-figure source data and the statistics overview. Everything is written to a
# temporary folder; the paper folder is never touched.

source(testthat::test_path("..", "..", "R", "paths.R"))
source(repo_path("R", "workbooks.R"))
skip_if_not_installed("openxlsx")

test_that("panel sheets are named as the journal names panels, within Excel's limits", {
  expect_identical(wb_panel_sheet("figure_01", "1a"), "Fig. 1a")
  expect_identical(wb_panel_sheet("figure_03", "3e_card"), "Fig. 3e card")
  expect_identical(wb_panel_sheet("extended_data_04", "04b"), "Extended Data Fig. 4b")
  expect_error(wb_panel_sheet("extended_data_04", "04b/x"), "not a valid sheet name")
})

test_that("a source-data workbook holds one sheet per panel, multi-part panels as titled blocks, and refuses edited tables", {
  dir <- tempfile("sd_"); dir.create(dir)
  a <- data.frame(Sex = c("Female", "Male"), rho = c(-0.49, -0.46))
  b <- data.frame(table = c("animals", "animals", "statistics"), id = c("x", "y", NA), value = c(1.5, 2.5, NA),
                  Quantity = c(NA, NA, "LOAO R2"), Value = c(NA, NA, 0.218))
  utils::write.csv(a, file.path(dir, "figure_09a_source_data.csv"), row.names = FALSE, na = "")
  utils::write.csv(b, file.path(dir, "figure_09b_source_data.csv"), row.names = FALSE, na = "")
  files <- c("figure_09a_source_data.csv", "figure_09b_source_data.csv")
  utils::write.csv(data.frame(file = files, panel = c("9a", "9b"), rows = c(2L, 3L),
                              sha256 = unname(tools::sha256sum(file.path(dir, files))), render_git_commit = "abc"),
                   file.path(dir, "00_manifest.csv"), row.names = FALSE)
  out <- tempfile(fileext = ".xlsx")
  wb_source_data("figure_09", dir, out, list(title = "Figure 9", analysis = "here", source_commit = "def",
                                             panels = list(`9a` = "rho by sex")), list(commit = "ghi", clean = TRUE))
  expect_identical(openxlsx::getSheetNames(out), c("about", "Fig. 9a", "Fig. 9b"))
  pa <- openxlsx::read.xlsx(out, sheet = "Fig. 9a")
  expect_equal(pa$rho, c(-0.49, -0.46))
  pb <- openxlsx::read.xlsx(out, sheet = "Fig. 9b", colNames = FALSE)
  expect_identical(pb[[1]][1], "animals")
  expect_true("statistics" %in% pb[[1]])
  write("1,2", file.path(dir, "figure_09a_source_data.csv"), append = TRUE)
  expect_error(wb_source_data("figure_09", dir, out, list(title = "x", panels = list()), list(commit = "x", clean = TRUE)),
               "differs from its manifest")
})

test_that("every row of the statistics map resolves to stored values", {
  map <- utils::read.csv(repo_path("figures", "contracts", "stats_map.csv"), stringsAsFactors = FALSE,
                         colClasses = "character", na.strings = "")
  tabs <- stats_tables(map)
  expect_true(all(c("Fig. 1", "Text", "Sensitivity", "Group sizes") %in% names(tabs)))
  expect_true(all(vapply(tabs, nrow, 1L) > 0L))
  expect_false(any(unlist(lapply(tabs, function(x) grepl("\\bNA\\b", x$Quantity)))))
  # anchors: the pooled association and the cohort read from the pinned behaviour bundle
  v <- stats_vars()
  c2 <- utils::read.csv(repo_path(v[["ebb"]], "C2_estimates.csv"), stringsAsFactors = FALSE)
  rho <- c2$estimate[c2$source == "stage09" & c2$model_id == "stage09_association" & c2$construct == "Movement_mean"]
  t <- tabs[["Text"]]
  expect_equal(t$Estimate[t$Quantity == "Spearman rho, mean movement vs CombZ (pooled)"], rho)
  a2 <- utils::read.csv(repo_path(v[["ebb"]], "A2_prediction_animals.csv"), stringsAsFactors = FALSE)
  g <- tabs[["Group sizes"]]
  expect_equal(g$Estimate[g$Quantity == "RFID cohort (behaviour analyses): all"], nrow(a2))
  expect_equal(sum(g$Estimate[grepl("^RFID cohort .*: (Female|Male), ", g$Quantity)]), nrow(a2))
})

test_that("every statistics row has a 95% CI around its estimate, or a declared reason for none", {
  map <- utils::read.csv(repo_path("figures", "contracts", "stats_map.csv"), stringsAsFactors = FALSE,
                         colClasses = "character", na.strings = "")
  all <- do.call(rbind, lapply(stats_tables(map), function(x) x[, c("Estimate", "CI low", "CI high", "Note", ".ci_na")]))
  ci <- !all$.ci_na
  expect_false(any(is.na(all$`CI low`[ci]) | is.na(all$`CI high`[ci])))
  expect_true(all(grepl("no CI: ", all$Note[!ci], fixed = TRUE)))
  e <- suppressWarnings(as.numeric(all$Estimate[ci]))
  expect_true(all(as.numeric(all$`CI low`[ci]) <= e & e <= as.numeric(all$`CI high`[ci]), na.rm = TRUE))
  m <- map[1, ]; m$ci_none <- NA; m$ci_low <- NA; m$ci_high <- NA; m$estimate <- "susceptibility_threshold"
  expect_error(stats_tables(m), "no CI and no declared reason")
})

test_that("stats.xlsx has an about sheet and one sheet per map sheet", {
  map <- utils::read.csv(repo_path("figures", "contracts", "stats_map.csv"), stringsAsFactors = FALSE,
                         colClasses = "character", na.strings = "")
  out <- tempfile(fileext = ".xlsx")
  wb_stats(stats_tables(map), out, list(commit = "abc", clean = TRUE))
  expect_identical(openxlsx::getSheetNames(out), c("about", unique(map$sheet)))
})
