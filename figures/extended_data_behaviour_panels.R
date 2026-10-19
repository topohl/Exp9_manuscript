#!/usr/bin/env Rscript
# ================================================================
# Script: figures/extended_data_behaviour_panels.R
# Stage: manuscript_figures
# Scope: global
# Consumes: the pinned canonical behaviour bundle (config/behaviour_bundle.yml) and the pinned figure-support
#           bundle (config/figure_support_bundle.yml), source_data/MMMSociability/<bundle_id>/, through
#           R/behaviour_bundle.R and R/figure_support_bundle.R
# Produces: results/figures/manuscript/extended_data_behaviour_panels/ and its render record
# Notes: Rendering only. Reads stored bundle cells; computes no statistic.
# ================================================================
#
# Behavioural Extended Data. Figure 1 shows one model and one association. This
# figure shows the two things a reader needs in order to know that neither was
# chosen after the fact: the complete a-priori model ladder, and the formal
# feature-by-sex interaction tests.
#
# Every value comes from the bundles Figure 1 is drawn from (corrected-label rerun, 2026-10-17; until then these
# panels read the frozen bridge contracts of MMMSociability 53bc7e9, which predate the corrections of 2026-09-20 and
# 2026-10-16 and the first-night seeding fix of 2026-09-22):
#   ebb C2_estimates (source stage09): the ladder's LOAO and repeated-CV R2, the three early-feature associations and
#       the three feature-by-sex interaction estimates, each with its stored interval;
#   ebb E_multiplicity: PR1 (BH q of the associations), PR2 (permutation P of the ladder), PR3 (BH q and the
#       reject flag of the interactions);
#   fsb E1: Stage 09's sex-stratified Spearman rho (descriptive context);
#   fsb E3 (fsb_v5 on): Stage 09's early-window coverage summary.
# Nothing here is fitted, resampled or corrected. Every value printed is a cell of one of those tables.
#
# Panels:
#   ED5 a  early-window coverage (fsb E3)
#   ED5 b  the five a-priori models: leave-one-animal-out R2, the repeated grouped five-fold interval, and the
#          permutation P where one was run
#   ED9 a  the two secondary early features against later CombZ
#   ED9 b  the three feature-by-sex interaction tests with 95% CI and BH q
#   ED9 c  the sex-stratified correlations, drawn as descriptive context only
#
# WORDING DISCIPLINE. The sex panels exist to license a negative statement and
# to block a positive one. This script refuses to render if any interaction is
# rejected in its stored family (PR3) - because the moment one is, panel c stops
# being descriptive context and the figure has to be rethought.
#
# The ladder panel likewise states no winner. The models differ by amounts far
# smaller than their own intervals, and no test compares them; the panel is
# ordered by the fixed registry, never by performance.

suppressPackageStartupMessages({
  library(ggplot2)
})

paths_file <- if (file.exists(file.path("R", "paths.R"))) file.path("R", "paths.R") else file.path("..", "R", "paths.R")
source(paths_file)
source(repo_path("R", "behaviour_bundle.R"))
source(repo_path("R", "stage30_bundle.R"))
source(repo_path("R", "figure_support_bundle.R"))

PIN <- behaviour_bundle_pin()
behaviour_bundle_verify(PIN)
FSB <- fsb_bundle_pin()
fsb_bundle_verify(FSB)
OUT <- path_results("figures", "manuscript", "extended_data_behaviour_panels")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

C2 <- behaviour_bundle_table("C2_estimates", PIN)
EM <- behaviour_bundle_table("E_multiplicity", PIN)
E1 <- fsb_bundle_table("E1_movement_combz_spearman_by_sex", FSB)
s09 <- C2[C2$source == "stage09", , drop = FALSE]
cell <- function(tab, column, ...) bundle_cell(tab, column, ...)

# The registered Stage 09 ladder, in registry order, with each model's role (the registry's, carried here as display
# metadata only: the bundle stores the estimates, not the role words) and the one headline model Figure 1 draws.
LADDER_ROLE <- c(mean_only = "reference_baseline", movement_mean = "primary_behavior_only",
                 primary_behavior_family = "primary_behavior_only", movement_mean_sex = "sex_adjusted_sensitivity",
                 primary_behavior_family_sex = "sex_adjusted_sensitivity")
HEADLINE_MODEL <- "movement_mean"
FEATURES <- c("Movement_mean", "Movement_rmssd", "Entropy_acf1")

perm_p <- function(m) { r <- EM[EM$family_id == "PR2" & EM$member == paste0("permutation|", m), , drop = FALSE]
  if (nrow(r) == 1L) r$p_raw else if (!nrow(r)) NA_real_ else stop("PR2 lists ", m, " more than once.", call. = FALSE) }
ladder <- do.call(rbind, lapply(names(LADDER_ROLE), function(m) {
  id <- paste0("stage09_", m)
  data.frame(model_id = m,
             loao_r2 = cell(s09, "estimate", model_id = id, estimand = "LOAO_R2"),
             repeated_cv_mean_r2 = cell(s09, "estimate", model_id = id, estimand = "repeated_CV_mean_R2"),
             cv_r2_q025 = cell(s09, "ci_low", model_id = id, estimand = "repeated_CV_mean_R2"),
             cv_r2_q975 = cell(s09, "ci_high", model_id = id, estimand = "repeated_CV_mean_R2"),
             permutation_p = perm_p(m),
             n = cell(s09, "n", model_id = id, estimand = "LOAO_R2"),
             primary_or_exploratory = LADDER_ROLE[[m]],
             current_status = if (m == HEADLINE_MODEL) "HEADLINE PRIMARY" else "registered",
             stringsAsFactors = FALSE)
}))
pr3 <- function(f, col) cell(EM, col, family_id = "PR3", member = paste0("feature_x_Sex|", f))
e1 <- function(f, sex) cell(E1, "spearman_rho", Sex = sex, feature = f)
sexc <- do.call(rbind, lapply(FEATURES, function(f) data.frame(
  feature = f,
  interaction_estimate = cell(s09, "estimate", model_id = "stage09_feature_x_sex", construct = f),
  interaction_ci_low = cell(s09, "ci_low", model_id = "stage09_feature_x_sex", construct = f),
  interaction_ci_high = cell(s09, "ci_high", model_id = "stage09_feature_x_sex", construct = f),
  interaction_p = pr3(f, "p_raw"), interaction_q_bh = pr3(f, "p_adjusted"),
  interaction_rejected = as.logical(pr3(f, "reject_at_alpha_adjusted")),
  female_rho = e1(f, "Female"), male_rho = e1(f, "Male"), stringsAsFactors = FALSE)))
pr1 <- function(f, col) cell(EM, col, family_id = "PR1", member = paste0("spearman|", f))
assoc <- do.call(rbind, lapply(FEATURES, function(f) data.frame(
  predictor = f, outcome = "CombZ",
  n = cell(s09, "n", model_id = "stage09_association", construct = f),
  rho = cell(s09, "estimate", model_id = "stage09_association", construct = f),
  ci_low = cell(s09, "ci_low", model_id = "stage09_association", construct = f),
  ci_high = cell(s09, "ci_high", model_id = "stage09_association", construct = f),
  p_raw = pr1(f, "p_raw"), q_bh = pr1(f, "p_adjusted"), stringsAsFactors = FALSE)))

# Early-window coverage: the Stage 09 summary (fsb E3, fsb_v5 on). An older figure-support bundle does not carry it;
# then the other panels are drawn for preview and no render record is written, so nothing can be promoted.
E3 <- tryCatch(fsb_bundle_table("E3_early_window_coverage", FSB), error = function(e) NULL)
COV <- if (is.null(E3)) NULL else {
  if (nrow(E3) != 1L) stop("fsb E3 must hold exactly one coverage row.", call. = FALSE)
  E3
}

# ------------------------------------------------------------------ contracts
#
# These are not defensive niceties. Each one guards a sentence the manuscript is
# allowed to write only while the guarded condition holds.

# Panel c is drawn as descriptive context. That is only honest while no formal
# interaction is supported. If upstream ever changes one, this figure must be
# redesigned rather than quietly redrawn.
if (any(sexc$interaction_rejected))
  stop("At least one feature-by-sex interaction is rejected in its stored family (PR3). Panel c presents ",
       "stratified correlations as descriptive context, which is no longer defensible. ",
       "Redesign the panel; do not re-render it.", call. = FALSE)

# The manuscript's headline model must remain the one Figure 1 draws, and the permutation was run for exactly the
# behaviour-only models (PR2): an absent P is then a design fact, printed as "not run".
if (!identical(sort(ladder$model_id[!is.na(ladder$permutation_p)]), sort(c("movement_mean", "primary_behavior_family"))))
  stop("PR2 no longer holds exactly the two behaviour-only models.", call. = FALSE)

if (length(unique(ladder$n)) != 1L)
  stop("The ladder models are not all fitted on the same animals.", call. = FALSE)
N_ANIMALS <- unique(ladder$n)

# ---------------------------------------------------------------- house style
# Identical to figures/figure_01_panels.R, because this figure is read beside it.
source(repo_path("R", "plotting_nature.R"))

INK <- "#2B2B2B"; MUTED <- "#6E6E6E"; RULE <- "#B5B5B5"
GREEN_DARK <- "#2F6F62"
GREEN_MID <- "#6FA79B"
GREEN_LIGHT <- "#A8D5CF"

BASE_PT <- 7
BODY_PT <- 6.5
NOTE_PT <- 6

theme_ed <- function(base = BASE_PT) {
  theme_classic(base_size = base, base_family = "sans") +
    theme(
      axis.text = element_text(size = BODY_PT, colour = INK),
      axis.title = element_text(size = base, colour = INK),
      axis.line = element_line(linewidth = 0.3, colour = "black"),
      axis.ticks = element_line(linewidth = 0.3, colour = "black"),
      axis.ticks.length = unit(2, "pt"),
      strip.background = element_blank(),
      strip.text = element_text(size = base, colour = INK, face = "italic",
                                margin = margin(b = 1.5, t = 1)),
      legend.position = "top",
      legend.justification = "left",
      legend.key.height = unit(7, "pt"),
      legend.key.width = unit(11.2, "pt"),
      legend.text = element_text(size = NOTE_PT),
      legend.title = element_blank(),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, -3.5, 0),
      panel.grid = element_blank(),
      # Titles align to the figure, not to the plot panel. Every panel here is a
      # horizontal forest whose y-axis labels are long sentences, so a
      # panel-aligned title would start a third of the way across and sit
      # nowhere near the panel letter. Figure 1 keeps the ggplot default because
      # none of its panels has a label column of that width.
      plot.title.position = "plot",
      plot.caption.position = "plot",
      # Indented past the 7 mm panel-letter box the assembler paints over the
      # top-left corner of every panel. Figure-aligned titles start exactly
      # where that box sits, so without this the first word is covered.
      plot.title = element_text(size = base, colour = INK, hjust = 0,
                                face = "plain", margin = margin(b = 0.5, l = 22)),
      plot.subtitle = element_text(size = NOTE_PT, colour = "grey25",
                                   margin = margin(b = 1.5, l = 22)),
      plot.margin = margin(3.5, 3.5, 3.5, 3.5))
}

fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)

# ===================================== the a-priori model ladder (ED5 b)
#
# The ladder is drawn in registry order, bottom to top, so the reader sees the
# reference baseline first and the fixed order is visible as order. The point is
# the leave-one-animal-out estimate, which is the primary quantity; the bar is
# the repeated grouped five-fold interval, which belongs to the repeated-CV mean
# and NOT to the point. They are drawn in different ink for that reason.

# The model's role is carried in its own axis label rather than in a legend.
# A legend would cost three of the panel's 48 mm to encode five rows that are
# already named on the axis, and the roles are the reason the rows are ordered
# the way they are, so they belong beside the names.
MODEL_LABEL <- c(
  mean_only = "Intercept only  (baseline)",
  movement_mean = "Mean movement",
  primary_behavior_family = "Mean movement + RMSSD + entropy ACF1",
  movement_mean_sex = "Mean movement + sex  (sensitivity)",
  primary_behavior_family_sex = "Mean movement + RMSSD + entropy ACF1 + sex  (sensitivity)")
absent <- setdiff(ladder$model_id, names(MODEL_LABEL))
if (length(absent))
  stop("The ladder contains models this figure has no label for: ",
       paste(absent, collapse = ", "),
       "\nA new model may not be rendered under a guessed name.", call. = FALSE)

ROLE_FILL <- c(reference_baseline = "grey72",
               primary_behavior_only = GREEN_DARK,
               sex_adjusted_sensitivity = GREEN_MID)
absent <- setdiff(ladder$primary_or_exploratory, names(ROLE_FILL))
if (length(absent))
  stop("Unrecognised model role(s): ", paste(absent, collapse = ", "),
       call. = FALSE)

# Registry order as stored, reversed so row 1 of the file prints at the bottom.
la <- ladder
la$label <- factor(MODEL_LABEL[la$model_id],
                   levels = rev(MODEL_LABEL[la$model_id]))
la$role <- factor(la$primary_or_exploratory,
                  levels = c("reference_baseline", "primary_behavior_only",
                             "sex_adjusted_sensitivity"),
                  labels = c("Reference baseline", "Behaviour only",
                             "Sex-adjusted sensitivity"))

# The permutation was run for the behaviour-only models only. Saying so is the
# point of the column: an absent P is a design fact, not a missing value.
la$p_text <- ifelse(
  is.na(la$permutation_p), "not run",
  ifelse(la$permutation_p < 0.001, "P < 0.001",
         paste0("P = ", fmt(la$permutation_p, 3))))

x_lo <- min(la$cv_r2_q025, la$loao_r2)
x_hi <- max(la$cv_r2_q975, la$loao_r2)
pad <- 0.045 * (x_hi - x_lo)
x_text <- x_hi + pad

p_ladder <- ggplot(la, aes(y = .data$label)) +
  geom_vline(xintercept = 0, linetype = "22", linewidth = 0.3, colour = RULE) +
  geom_segment(aes(x = .data$cv_r2_q025, xend = .data$cv_r2_q975,
                   yend = .data$label),
               linewidth = 1.6, colour = GREEN_LIGHT, lineend = "round") +
  geom_point(aes(x = .data$repeated_cv_mean_r2), shape = 124, size = 1.5,
             colour = MUTED) +
  geom_point(aes(x = .data$loao_r2, fill = .data$role), shape = 21, size = 1.9,
             stroke = 0.3, colour = "grey20") +
  geom_text(aes(x = x_text, label = .data$p_text), hjust = 0, size = NOTE_PT / .pt,
            colour = MUTED) +
  scale_fill_manual(values = unname(ROLE_FILL[c("reference_baseline",
                                                "primary_behavior_only",
                                                "sex_adjusted_sensitivity")]),
                    drop = FALSE, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.03, 0.22))) +
  labs(x = expression("Cross-validated " * italic(R)^2),
       y = NULL,
       title = "The complete a-priori model ladder",
       subtitle = paste0(
         "Point, leave-one-animal-out estimate; bar, 2.5-97.5% across repeated grouped five-fold\n",
         "splits, with its mean as a tick. n = ", N_ANIMALS, " animals. Rows are in registry order, not\n",
         "performance order, and no test compares two models.")) +
  theme_ed() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

# ================================ feature-by-sex interactions (ED9 b)
#
# The formal test. Zero sits inside every interval, and the BH q values are
# printed rather than starred, because the result the manuscript relies on is
# the absence of an interaction and a reader is entitled to see how far from
# supported each one is.

FEATURE_LABEL <- c(Movement_mean = "Mean movement",
                   Movement_rmssd = "Movement RMSSD",
                   Entropy_acf1 = "Entropy ACF1")
absent <- setdiff(sexc$feature, names(FEATURE_LABEL))
if (length(absent))
  stop("The sex contract contains features this figure has no label for: ",
       paste(absent, collapse = ", "), call. = FALSE)

sx <- sexc
sx$label <- factor(FEATURE_LABEL[sx$feature],
                   levels = rev(FEATURE_LABEL[sx$feature]))
sx$q_text <- paste0("q = ", fmt(sx$interaction_q_bh, 2))

b_lo <- min(sx$interaction_ci_low)
b_hi <- max(sx$interaction_ci_high)
b_pad <- 0.05 * (b_hi - b_lo)

p_sex <- ggplot(sx, aes(y = .data$label)) +
  geom_vline(xintercept = 0, linetype = "22", linewidth = 0.3, colour = RULE) +
  geom_segment(aes(x = .data$interaction_ci_low, xend = .data$interaction_ci_high,
                   yend = .data$label),
               linewidth = 0.7, colour = MUTED, lineend = "round") +
  geom_point(aes(x = .data$interaction_estimate), shape = 21, size = 1.9,
             stroke = 0.3, fill = "white", colour = "grey20") +
  geom_text(aes(x = b_hi + b_pad, label = .data$q_text), hjust = 0,
            size = NOTE_PT / .pt, colour = MUTED) +
  scale_x_continuous(expand = expansion(mult = c(0.06, 0.34))) +
  labs(x = "Feature x sex interaction estimate (95% CI)",
       y = NULL,
       title = "The association did not differ detectably by sex",
       subtitle = paste0(
         "One formal interaction test per early feature,\n",
         "Benjamini-Hochberg across the three. Zero lies\n",
         "inside every interval.")) +
  theme_ed() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

# ============================ sex-stratified correlations (ED9 c)
#
# Descriptive only, and labelled as such on the panel itself rather than only in
# the legend. These two numbers are the ones most likely to be misread as a
# sex difference, so they are drawn on a shared axis with a connecting rule that
# makes their closeness the visible fact.

rho <- data.frame(
  label = rep(FEATURE_LABEL[sexc$feature], 2L),
  sex = rep(c("Female", "Male"), each = nrow(sexc)),
  rho = c(sexc$female_rho, sexc$male_rho),
  stringsAsFactors = FALSE)
rho$label <- factor(rho$label, levels = rev(FEATURE_LABEL[sexc$feature]))

link <- data.frame(
  label = factor(FEATURE_LABEL[sexc$feature],
                 levels = rev(FEATURE_LABEL[sexc$feature])),
  lo = pmin(sexc$female_rho, sexc$male_rho),
  hi = pmax(sexc$female_rho, sexc$male_rho),
  stringsAsFactors = FALSE)

SEX_SHAPE <- c(Female = 21L, Male = 24L)

p_rho <- ggplot(rho, aes(y = .data$label)) +
  geom_vline(xintercept = 0, linetype = "22", linewidth = 0.3, colour = RULE) +
  geom_segment(data = link, aes(x = .data$lo, xend = .data$hi, yend = .data$label),
               linewidth = 0.5, colour = RULE) +
  geom_point(aes(x = .data$rho, shape = .data$sex, fill = .data$sex), size = 1.9,
             stroke = 0.3, colour = "grey20") +
  scale_shape_manual(values = SEX_SHAPE) +
  scale_fill_manual(values = c(Female = GREEN_DARK, Male = "white")) +
  scale_x_continuous(expand = expansion(mult = c(0.10, 0.10))) +
  labs(x = "Spearman rho, early feature versus later CombZ",
       y = NULL,
       title = "Sex-stratified estimates, descriptive",
       subtitle = paste0(
         "Shown because panel b is a negative result. These\n",
         "are not tests, carry no correction, and may not be\n",
         "reported as a sex difference.")) +
  theme_ed() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())


# ==================== secondary early-feature associations (ED9 a)
#
# The two secondary early features against later CombZ. These are drawn as
# effect sizes rather than scatters for a reason worth stating: the frozen
# bundle exports per-animal values for Movement_mean only. figure1c carries
# AnimalID, Sex, Group, Movement_mean and CombZ and nothing else, so a
# per-animal RMSSD or entropy scatter could not be drawn without inventing the
# points. What upstream does export for all three features is the association
# contract - rho, its bootstrap interval and the BH q - and that is exactly the
# quantity the manuscript claims, so that is what this panel shows.
SECONDARY <- c(Movement_rmssd = "Movement RMSSD", Entropy_acf1 = "Entropy ACF1")
absent <- setdiff(names(SECONDARY), assoc$predictor)
if (length(absent))
  stop("the association contract no longer carries: ",
       paste(absent, collapse = ", "), call. = FALSE)

as2 <- assoc[match(names(SECONDARY), assoc$predictor), , drop = FALSE]
as2$label <- factor(unname(SECONDARY), levels = rev(unname(SECONDARY)))
as2$q_text <- paste0("q = ", fmt(as2$q_bh, 3))
# The evidence column is upstream's own adjudication and is printed verbatim,
# so the panel cannot describe an uncertain association as a supported one.
as2$ev <- ifelse(as2$ci_low < 0 & as2$ci_high > 0,
                 "interval includes zero", "interval excludes zero")

a_lo <- min(as2$ci_low); a_hi <- max(as2$ci_high)
a_pad <- 0.06 * (a_hi - a_lo)

p_assoc <- ggplot(as2, aes(y = .data$label)) +
  geom_vline(xintercept = 0, linetype = "22", linewidth = 0.3, colour = RULE) +
  geom_segment(aes(x = .data$ci_low, xend = .data$ci_high, yend = .data$label),
               linewidth = 0.7, colour = MUTED, lineend = "round") +
  geom_point(aes(x = .data$rho), shape = 21, size = 1.9, stroke = 0.3,
             fill = GREEN_DARK, colour = "grey20") +
  geom_text(aes(x = a_hi + a_pad, label = paste0(.data$q_text, "   ", .data$ev)),
            hjust = 0, size = NOTE_PT / .pt, colour = MUTED) +
  scale_x_continuous(expand = expansion(mult = c(0.06, 0.58))) +
  labs(x = expression("Spearman " * rho * ", early feature versus later CombZ"),
       y = NULL,
       title = "The two secondary early features",
       subtitle = paste0(
         "Point, Spearman rho; bar, 5000-sample percentile bootstrap interval;\n",
         "q, Benjamini-Hochberg across the three prespecified features. n = ",
         as2$n[1], " animals.\n",
         "Drawn as effect sizes because the frozen bundle exports no per-animal\n",
         "values for these two features.")) +
  theme_ed() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

# ============================ early-window coverage (ED5 a)
#
# Aggregate only: Stage 09's coverage summary of the fixed early window (fsb E3). It records the expected slot count,
# how many animals contributed all of them and how many miss leading, interior or trailing slots, and the mean and
# minimum coverage fraction. No per-animal coverage vector is exported, so the panel states the split it can support.
p_cov <- if (!is.null(COV)) {
  n_tot <- as.integer(COV$n_animals)
  n_full <- as.integer(COV$n_animals_complete)
  n_lead <- as.integer(COV$n_animals_missing_leading_only)
  n_gap <- as.integer(COV$n_animals_interior_missing) + as.integer(COV$n_animals_trailing_missing)
  slots <- as.integer(COV$expected_target_slots_per_animal)
  cov_mean <- as.numeric(COV$mean_coverage_fraction); cov_min <- as.numeric(COV$min_coverage_fraction)
  cv <- data.frame(grp = c("All expected slots", "Leading slots missing only", "Interior or trailing slots missing"),
                   n = c(n_full, n_lead, n_gap), stringsAsFactors = FALSE)
  cv <- cv[cv$n > 0L | cv$grp == "All expected slots", , drop = FALSE]
  cv$grp <- factor(cv$grp, levels = rev(cv$grp))
  cv$lab <- sprintf("%d of %d animals", cv$n, n_tot)
  ggplot(cv, aes(.data$n, .data$grp)) +
    geom_col(fill = c(GREEN_MID, GREEN_LIGHT, "grey80")[seq_len(nrow(cv))][rev(as.integer(cv$grp))],
             colour = "grey30", linewidth = 0.25, width = 0.6) +
    geom_text(aes(label = .data$lab), hjust = -0.08, size = NOTE_PT / .pt, colour = INK) +
    scale_x_continuous(limits = c(0, n_tot * 1.42), expand = c(0, 0)) +
    labs(x = "Animals", y = NULL,
         title = "Early-window coverage",
         subtitle = paste0(
           "A fixed ", COV$target_window_hours, "-h window binned at ", COV$bin_size_min, " min gives ", slots,
           " expected slots per animal; mean coverage ", fmt(100 * cov_mean, 1), "%, minimum ", fmt(100 * cov_min, 1), "%.\n",
           if (n_full == n_tot) "Every animal contributed every slot." else
             paste0(n_tot - n_full, " animals miss slots (leading only: ", n_lead, ")."),
           " Aggregate figures; the bundle exports no\nper-animal coverage vector.")) +
    theme_ed() +
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())
}

# ---------------------------------------------------------------- write out
# Authored at the exact placed size, so scale = 1.0 and native pt == printed pt.
# Two equal rows of 52 mm place 48 mm image boxes; a spans both columns.
DIMS <- list(ed09a = c(173, 48), ed09b = c(84.5, 48), ed09c = c(84.5, 48),
             ed05a = c(173, 48), ed05b = c(173, 48))
PLOTS <- list(ed09a = p_assoc, ed09b = p_sex, ed09c = p_rho, ed05a = p_cov, ed05b = p_ladder)
PLOTS <- PLOTS[!vapply(PLOTS, is.null, TRUE)]

save_svg <- function(p, id) {
  d <- DIMS[[id]]
  f <- file.path(OUT, paste0("extended_data_behaviour_", id, ".svg"))
  svglite::svglite(f, width = d[1] / 25.4, height = d[2] / 25.4, bg = "white",
                   fix_text_size = FALSE)
  print(p); grDevices::dev.off()
  f
}
written <- vapply(names(PLOTS), function(id) save_svg(PLOTS[[id]], id),
                  character(1))

cat("Behavioural Extended Data panels ->", OUT, "\n")
for (id in names(written))
  cat(sprintf("  extended_data_behaviour_%s.svg  %6d bytes  %.1f x %.0f mm\n",
              id, file.size(written[[id]]), DIMS[[id]][1], DIMS[[id]][2]))
cat("  bundles", PIN$bundle_id, "and", FSB$bundle_id, "| models:", nrow(ladder), "| interaction tests:", nrow(sexc),
    "| animals:", N_ANIMALS, "\n")
cat("  every value read from stored bundle cells; nothing computed\n")

# ---------------------------------------------------------------- render record
# Which commit drew these panels, whether that code was clean, the bundles, and the hash of every panel, so that
# tools/promote_manuscript_render.R extended_data_05 / extended_data_09 can refuse panels drawn from other or
# uncommitted code. Without fsb E3 (an older figure-support bundle) ED5 a is missing and no record is written.
if (is.null(COV)) {
  message("NOTE: the pinned figure-support bundle ", FSB$bundle_id, " has no E3 (fsb_v5 on); ED5 a not drawn and no ",
          "render record written: these panels cannot be promoted.")
} else {
  code <- git_code_state()
  record_dir <- path_results("logs", "manuscript_figures", "extended_data_behaviour_panels")
  dir.create(record_dir, recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(list(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    producer = "figures/extended_data_behaviour_panels.R",
    behaviour_bundle_id = PIN$bundle_id, behaviour_bundle_manifest_sha256 = PIN$manifest_sha256,
    figure_support_bundle_id = FSB$bundle_id, figure_support_bundle_manifest_sha256 = FSB$manifest_sha256,
    render_git_commit = code$commit, render_code_clean = code$code_clean, render_code_dirty = code$dirty,
    files = lapply(unname(written), function(x) list(path = relative_to(x), sha256 = unname(tools::sha256sum(x))))),
    file.path(record_dir, "render_record.yml"))
  cat("  render record", relative_to(file.path(record_dir, "render_record.yml")), "at", substr(code$commit, 1, 7),
      if (isTRUE(code$code_clean)) "(clean code)\n" else "(UNCOMMITTED CODE)\n")
}
