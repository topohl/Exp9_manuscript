# The conventional-tests Extended Data candidates (figures/conventional_tests_candidate.R): Extended Data 10 and 11.
#
# Portable: the pinned SISanalyzer bundle and the panel library only. The entry script is never sourced, so nothing is
# rendered; each check compares what the panels would draw with the frozen bundle.

source(testthat::test_path("..", "..", "R", "paths.R"))
source(repo_path("R", "conventional_bundle.R"))
source(repo_path("R", "panels", "conventional_tests_panels.R"))
testthat::skip_if_not(file.exists(repo_path(CTP_PIN_FILE)), "no conventional-tests bundle pinned")
X <- ct_read()
MAINS <- list(Behaviour = X$measures$id[X$measures$domain == "Behaviour" & X$measures$main],
              Physiology = X$measures$id[X$measures$domain == "Physiology" & X$measures$main])

testthat::test_that("the pinned bundle verifies, is frozen and comes from a clean SISanalyzer commit", {
  testthat::expect_true(ctp_verify(X$pin))
  prov <- ctp_provenance(file.path(ctp_dir(X$pin), "H_provenance.csv"))
  testthat::expect_identical(prov$status, "FROZEN")
  testthat::expect_identical(prov$bundle_id, X$pin$bundle_id)
  testthat::expect_identical(prov$producer_commit, X$pin$producer_commit)
  testthat::expect_identical(prov$producer_tree_clean, "TRUE")
  testthat::expect_true(grepl(CTP_ID_PATTERN, X$pin$bundle_id))
})

testthat::test_that("every comparison is there once, for both sexes and every measure", {
  ids <- X$measures$id[X$measures$id != "CombZ"]
  testthat::expect_identical(nrow(X$contrasts), length(ids) * 2L * 4L)
  testthat::expect_false(anyDuplicated(X$contrasts[c("Sex", "id", "comparison")]) > 0)
  testthat::expect_setequal(unique(X$contrasts$comparison), CT_CMP)
  testthat::expect_setequal(unique(X$values$Sex), CT_SEXES)
})

testthat::test_that("the dots are the stored values of every animal and the means are the stored means", {
  for (dom in names(MAINS)) {
    ids <- MAINS[[dom]]; testthat::expect_length(ids, 5L)
    for (s in CT_SEXES) {
      d <- ct_dot_data(X, ids, s); v <- X$values[X$values$id %in% ids & X$values$Sex == s, ]
      testthat::expect_identical(nrow(d$points), nrow(v), info = paste(dom, s))
      k <- match(paste(d$points$Animal, d$points$id), paste(v$Animal, v$id))
      testthat::expect_false(anyNA(k)); testthat::expect_identical(d$points$value, v$value[k])
      testthat::expect_true(all(d$points$Sex == s))
      m <- X$means[X$means$id %in% ids & X$means$Sex == s, ]
      testthat::expect_identical(nrow(d$means), length(ids) * 3L); testthat::expect_identical(d$means$mean, m$mean)
    }
  }
})

testthat::test_that("a bracket is drawn exactly where the stored adjusted p of a group pair is below 0.05", {
  for (dom in names(MAINS)) for (s in CT_SEXES) {
    d <- ct_dot_data(X, MAINS[[dom]], s)
    want <- X$contrasts[X$contrasts$id %in% MAINS[[dom]] & X$contrasts$Sex == s & X$contrasts$comparison %in% CT_CMP[1:3] & X$contrasts$adj_p < .05, ]
    got <- if (nrow(d$brackets)) paste(d$brackets$id, d$brackets$comparison) else character(0)
    testthat::expect_setequal(got, paste(want$id, want$comparison))
  }
})

testthat::test_that("the behaviour heatmap shows the main readouts and every measure with a significant comparison, nothing else", {
  sel <- ct_selected_behaviour(X)
  beh <- X$measures$id[X$measures$domain == "Behaviour" & X$measures$id != "loco_index"]
  sig <- unique(X$contrasts$id[X$contrasts$id %in% beh & X$contrasts$adj_p < .05])
  testthat::expect_setequal(sel, union(MAINS$Behaviour, sig))
  H <- ct_heatmap_data(X, sel)
  testthat::expect_identical(nrow(H$cells), length(sel) * 8L)
  lim <- X$P$diverging_limits$smd
  testthat::expect_equal(H$cells$fill, pmax(pmin(H$cells$d, lim), -lim))
  testthat::expect_identical(H$cells$significant, !is.na(H$cells$adj_p) & H$cells$adj_p < .05)
})

testthat::test_that("the physiology heatmap shows all 19 measures with their stored values", {
  ids <- X$measures$id[X$measures$domain == "Physiology"]; testthat::expect_length(ids, 19L)
  H <- ct_heatmap_data(X, ids); testthat::expect_identical(nrow(H$cells), 19L * 8L)
  k <- match(paste(H$cells$Sex, H$cells$id, H$cells$comparison), paste(X$contrasts$Sex, X$contrasts$id, X$contrasts$comparison))
  testthat::expect_false(anyNA(k)); testthat::expect_identical(H$cells$d, X$contrasts$d[k])
})

testthat::test_that("the locomotion and spread panels restate the stored tables", {
  s <- ct_loco_sessions_data(X)
  testthat::expect_identical(nrow(s), 6L * 2L * 3L)
  ix <- s[s$Session == "Locomotion index", ]
  k <- match(paste(ix$Sex, ix$comparison), paste(X$contrasts$Sex[X$contrasts$id == "loco_index"], X$contrasts$comparison[X$contrasts$id == "loco_index"]))
  testthat::expect_identical(ix$d, X$contrasts$d[X$contrasts$id == "loco_index"][k])
  testthat::expect_identical(nrow(X$het), 22L)
  testthat::expect_setequal(unique(X$het$Set), c("Score", "Components", "Test readouts"))
  testthat::expect_identical(sort(unique(X$tails$Condition)), c("CON", "SIS"))
})

testthat::test_that("the candidate computes no statistic", {
  files <- c(repo_path("R", "conventional_bundle.R"), repo_path("R", "panels", "conventional_tests_panels.R"), repo_path("figures", "conventional_tests_candidate.R"))
  code <- unlist(lapply(files, function(f) { x <- readLines(f, warn = FALSE); x <- gsub('"(\\\\.|[^"\\\\])*"', '""', x, perl = TRUE); sub("#.*$", "", x) }))
  for (pat in c("(^|[^A-Za-z0-9._])lm\\(", "emmeans", "p\\.adjust", "t\\.test", "cor\\.test", "anova\\(", "(^|[^A-Za-z0-9._])sd\\(", "(^|[^A-Za-z0-9._])var\\(", "quantile\\(", "(^|[^A-Za-z0-9._])mean\\("))
    testthat::expect_false(any(grepl(pat, code, perl = TRUE)), info = pat)
})
