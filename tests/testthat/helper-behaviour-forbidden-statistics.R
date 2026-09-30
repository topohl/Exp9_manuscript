# The statistics the behaviour renderers must never compute (DESIGN section 0; CANDIDATE_SPEC A).
#
# One list, shared by tests/testthat/test-figure-01-renderer.R (the Figure 1 renderer and the
# behaviour panel libraries R/panels/behaviour_*.R) and tests/testthat/test-behaviour-v101-s30-candidates.R
# (the candidate entry script and the same libraries). testthat loads helper-*.R files before
# every test file, so both scans use exactly these patterns. Each pattern is matched against the
# code lines of a file (full-line comments dropped).
#
# Every call pattern allows whitespace before the parenthesis ("mean (x)" is a call too). The
# ggplot layers that compute a statistic are matched by family, with any suffix
# (stat_summary_bin, geom_density_2d, ...). A statistic can also be passed by name to an
# apply-family or aggregation function (sapply(x, median), aggregate(y ~ g, d, mean),
# Reduce, Map, by, outer, do.call, FUN = mean, match.fun), and a merge can create an analysis
# variable; both are forbidden as well.
BEHAVIOUR_FORBIDDEN_STATS <- c(
  # model fits, tests, multiplicity adjustment, resampling
  "\\blm\\s*\\(", "\\bglm\\s*\\(", "\\blmer\\s*\\(", "cor\\.test\\s*\\(", "\\bcor\\s*\\(", "p\\.adjust\\s*\\(",
  "\\bboot\\s*\\(", "replicate\\s*\\(", "\\bt\\.test\\s*\\(", "wilcox\\.test\\s*\\(",
  # summaries and model accessors
  "\\bmean\\s*\\(", "\\bquantile\\s*\\(", "\\bmedian\\s*\\(", "\\bsd\\s*\\(", "\\bvar\\s*\\(", "\\bpredict\\s*\\(",
  "\\bfitted\\s*\\(", "\\bcoef\\s*\\(", "\\bresiduals\\s*\\(", "\\bdensity\\s*\\(", "\\becdf\\s*\\(",
  # ggplot layers that compute a statistic
  "geom_smooth\\s*\\(", "stat_smooth\\s*\\(", "stat_summary\\s*\\(",
  "(geom|stat)_(density|ecdf|summary|smooth|bin_2d|bin2d|quantile)[A-Za-z0-9_]*\\s*\\(",
  # a statistic passed by name
  "\\b((s|l|v|t|m|r|e)?apply|aggregate|ave|Map|Reduce|Filter|by|outer|do\\.call)\\s*\\([^)]*\\b(mean|median|quantile|sd|var|cor)\\b",
  "\\bFUN\\s*=\\s*[\"'`]?(mean|median|quantile|sd|var|cor)\\b",
  "\\bmatch\\.fun\\s*\\(",
  # a merge that creates an analysis variable
  "\\bmerge\\s*\\("
)
