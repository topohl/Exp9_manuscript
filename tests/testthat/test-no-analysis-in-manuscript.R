source(testthat::test_path("..", "..", "R", "paths.R"))

# Structural guard: rendering is allowed here, scientific inference is not.
#
# The check is deliberately context-aware. Comments and string literals are
# stripped before matching, because this repository legitimately *mentions*
# these functions in provenance notes, contract descriptions and the
# explanatory headers that say which upstream analysis produced a panel. A
# naive grep would flag all of that and be switched off within a week.

INFERENCE <- c(
  "lm", "glm", "lmer", "glmer", "bam", "gam", "gamm",
  "cor.test", "t.test", "wilcox.test", "kruskal.test", "aov", "anova",
  "p.adjust", "gseGO", "enrichGO", "compareCluster", "fgsea", "fgseaMultilevel",
  "blockwiseModules", "pickSoftThreshold", "moduleEigengenes"
)

# Remove quoted strings first, then trailing comments. Doing it in this order
# means a "#" inside a string cannot truncate the line, and a quote inside a
# comment cannot swallow the rest of the file.
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

testthat::test_that("figure renderers fit no models and compute no statistics", {
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
  offences <- as.character(unlist(offences))
  testthat::expect_identical(
    offences, character(0),
    info = paste("scientific inference in the manuscript repository:\n",
                 paste(offences, collapse = "\n"))
  )
})

testthat::test_that("no renderer reaches into a sibling analysis repository", {
  files <- renderer_files()
  offences <- character(0)
  for (f in files) {
    code <- readLines(f, warn = FALSE)
    hit <- grep("pRoteomics|MMMSociability", code)
    # Naming the upstream repository in a comment or provenance string is
    # expected; constructing a path into it is not.
    hit <- hit[grepl("[.][.]/|file[.]path|source[(]|setwd|readRDS|read[.]csv", code[hit])]
    if (length(hit)) {
      offences <- c(offences, paste0(sub(paste0(repo_root(), "/"), "", f), ":", hit))
    }
  }
  testthat::expect_identical(
    offences, character(0),
    info = paste("live cross-repository path:\n", paste(offences, collapse = "\n"))
  )
})
