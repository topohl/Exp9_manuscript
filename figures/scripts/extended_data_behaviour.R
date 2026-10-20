#!/usr/bin/env Rscript

# Explicit behavioural Extended Data entry point. As with Figure 1, no statistic
# is computed here and none is computed anywhere in this repository: the
# behavioural analysis is frozen upstream and imported with hashes. This
# validates the declared panel assets and materialises them.
#
# One figure, Extended Data 4: the two secondary early features, the complete
# a-priori model registry, and everything the study can say about sex. Phase 6A
# had split them into two figures; on 2026-10-20 they were merged again, because
# the registry page had a single panel.
source(file.path("R", "paths.R"))
source(repo_path("R", "manuscript_figure_utils.R"))

for (fig in c("ED_behaviour_models")) {
  manuscript_figure_main(fig)
}
