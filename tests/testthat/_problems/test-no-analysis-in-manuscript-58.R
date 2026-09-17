# Extracted from test-no-analysis-in-manuscript.R:58

# prequel ----------------------------------------------------------------------
source(testthat::test_path("..", "..", "R", "paths.R"))
INFERENCE <- c(
  "lm", "glm", "lmer", "glmer", "bam", "gam", "gamm",
  "cor.test", "t.test", "wilcox.test", "kruskal.test", "aov", "anova",
  "p.adjust", "gseGO", "enrichGO", "compareCluster", "fgsea", "fgseaMultilevel",
  "blockwiseModules", "pickSoftThreshold", "moduleEigengenes"
)
strip_noise <- function(lines) {
  lines <- gsub('"(\\\\.|[^"\\\\])*"', '""', lines, perl = TRUE)
  lines <- gsub("'(\\\\.|[^'\\\\])*'", "''", lines, perl = TRUE)
  sub("#.*$", "", lines)
}
renderer_files <- function() {
  c(list.files(repo_path("figures"), pattern = "[.][Rr]$",
               recursive = TRUE, full.names = TRUE),
    list.files(repo_path("R"), pattern = "[.][Rr]$",
               recursive = TRUE, full.names = TRUE))
}

# test -------------------------------------------------------------------------
files <- renderer_files()
testthat::expect_gt(length(files), 0L)
offences <- list()
for (f in files) {
    code <- strip_noise(readLines(f, warn = FALSE))
    for (fn in INFERENCE) {
      # A call site is the bare name followed by "(", not preceded by another
      # name character, "$" or ".". That keeps plot_lm_fit() and df$lm from
      # reading as a call to lm().
      pat <- paste0("(^|[^A-Za-z0-9._$])", gsub(".", "[.]", fn, fixed = TRUE), "[[:space:]]*\\(")
      hit <- grep(pat, code, perl = TRUE)
      if (length(hit)) {
        offences[[length(offences) + 1L]] <- paste0(
          sub(paste0(repo_root(), "/"), "", f), ":", hit, " calls ", fn, "()")
      }
    }
  }
offences <- unlist(offences)
testthat::expect_identical(
    offences, character(0),
    info = paste("scientific inference in the manuscript repository:\n",
                 paste(offences, collapse = "\n"))
  )
