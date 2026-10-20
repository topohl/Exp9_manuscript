# Candidate legends for the corrected-outcome behavioural views

These are alternative layouts for author review, separate from the numbered manuscript figures. The renderer namespace `behaviour_v101_s30` is retained for path compatibility. Current sources are `ebb_v102_20261019_677ed97`, `s30b_v11_20261019_5482c43` and `fsb_v5_20261019_8212df4`. The preceding drafts are in git history (`git show 1e137c1:manuscript/candidates/behaviour_v101_s30/legends.md`, and `results_text.md` likewise); their numbers do not describe these refreshed views.

Every displayed statistic resolves through `figures/behaviour_v101_s30_annotation_map.csv` to a stored bundle cell. Its value, format and role are also exported with each panel's source data. No model is fitted by the renderer. The original analytic definitions were rerun on the corrected outcome after earlier results had been inspected. These layouts do not provide independent replication.

## Outcome definition and common conventions

Integrated Resilience (`CombZ`) is the available-valid-component mean of the signed conventional-test and physiology components. Components are standardized against same-sex controls with population standard deviations. Missing or invalid components are omitted from the affected reference and the animal's mean; blank organ measurements are not zero. Susceptibility is defined by a score below the same-sex control mean minus one control population standard deviation; other SIS animals are labelled resilient. Group differences in the score and its own components follow this definition and are descriptive. The component heatmaps show signed component values, with missing entries explicitly marked.

A point represents an animal unless a panel explicitly shows a control-cage mean. Intervals and adjusted values are copied from the producing analysis. Sex is nested in experimental batch. A detectable within-sex association alone does not establish a difference between sexes, and an interval including zero does not establish equivalence.

## Conservative and integrated early-behaviour alternatives

`figure_01_option1`, `figure_01_option2` and `figure_01_option2_house` arrange the design, later outcome, early behavioural comparisons, associations and held-out prediction in alternative compositions. The integrated layouts add the light-phase context described below. Later groups colour the animal marks; the prediction model uses early mean movement as its sole predictor, with neither sex nor group entered. The target is continuous CombZ. The first active phase after CC1 is the fixed early window. Batch means are retained.

Registered behavioural comparisons, their model estimates and intervals, and the adjustment printed beside them come from the canonical behaviour bundle. Sex-resolved estimates come from the corresponding sex-resolved fits. The association panels print the stored Spearman estimate, bootstrap interval and its declared BH adjustment. Held-out performance is leave-one-animal-out, compared with the intercept-only baseline. Permutation values use the full-refit outcome permutations and the displayed adjustment across the registered behaviour-only models. The repeated grouped-fold range belongs to the repeated-split analysis; it is not a confidence interval for the leave-one-animal-out point.

## Component and trajectory alternatives

`figure_01_option3_house` and `figure_01_option3_panel_d` add the component definition, descriptive control references and the registered longitudinal contrasts. Control-cage means are descriptive summaries of the animals in each stable cage. The model-derived control reference is copied separately; these quantities must not be interpreted as interchangeable measures of independent replication. Trajectory estimates and their intervals are copied from the registered tables and retain the estimand identified in the panel source data. A formal moderation test is distinct from a descriptive trajectory contrast.

`figure_01_option3b_house` uses the frozen post hoc control/resilient/susceptible comparisons from run `v1.1_5482c43`. Batch-balanced model means, contrasts and their intervals were fitted upstream. The primary model allows different cage variances for controls and SIS; the common-variance sensitivity and diagnostic flags are retained in the source tables. Holm adjustment is within measure and sex across the declared group contrasts. These post hoc estimates are distinct from the registered SIS-only comparison. The prediction and component panels retain the conventions above.

## Light-phase context

`light_phase_panel`, `light_dependence_ed` and the light-phase portion of `ed_behaviour_longitudinal_light` show the first light-phase positional readouts and their relationship to movement. The rest-like measure is time in runs without a position update of at least 40 s, rather than EEG-defined sleep. It is highly saturated in the light phase and closely tied to the position-change rate; detection gaps cannot establish stillness. Stored adjusted estimates and intervals are shown under their exploratory families. The corrected-outcome local-family results do not justify a positive between-sex claim. The movement-adjusted estimates, saturation counts, animal counts and cage counts remain available in the mapped-value table accompanying the Results draft.

## Longitudinal and light-phase candidate page

`ed_behaviour_longitudinal_light` combines registered trajectory estimates with the light-phase context. Observations across cage changes belong to the same animals. Error bars and formal comparison statistics are the stored model results, with their declared multiplicity; the light-phase rows remain exploratory. A missing or unsupported comparison is not converted into evidence that groups respond identically.

## Cookie-response candidate page

`cookie_ed` and `ed_behaviour_cookie` show the change in position-change rate during the task, calculated as the post-window rate minus its matched pre-window rate. The rate is a positional readout, not a measure of motivation. The registered windows and the source table define the analysed animals. The pre/post summaries are descriptive. Outcome-group contrasts, continuous-outcome slopes, intervals and local-family adjustments are copied from the exploratory analysis. Their failure and wording flags remain applicable; an imprecise association is not evidence of an absent biological response.

## Exploratory screen

`screen_summary` displays the stored standardized estimates and the local-family adjusted values. The broad, inactivity and task families remain exploratory; the analysis was registered after earlier behavioural results had been inspected. Pooled rows are estimation-only. A descriptive adjustment across the complete screen does not replace the declared local families. Classification letters carry the original diagnostic and sensitivity rules; they do not identify a validated adaptive mechanism.

## Domain heatmap alternatives

`ed_behaviour_domain_heatmap_picked`, `ed_behaviour_domain_heatmap_all5min` and `ed_behaviour_domain_heatmap_all10min` show the stored Hedges g cells for the selected, common finer and common coarser resolutions. Each effect uses animal means of within-context domain scores, with comparison minus reference as labelled. The effect sizes are descriptive and are not batch-adjusted. Symbols distinguish eligible registered evidence and post hoc markers, including any stored sign disagreement. Post hoc adjustments use the declared tier and phase family. Comparisons with controls in these post hoc heatmap tests treat animals as units; stable control-cage clustering is not modelled. Resolution dependence and control-cage sensitivity are reported by the stored robustness flags and must accompany interpretation. The light-phase and exploratory-state limitations remain applicable.

## Exploratory state candidate page

`ed_behaviour_hmm` shows the stored state display labels, group transition summaries, dwell-time summaries and resolution sensitivity. The state model is pooled; later group labels describe its outputs. State-transition differences are descriptive. The state architecture row is exploratory and does not receive a reportable heatmap marker. State labels are positional summaries, not validated behavioural mechanisms. The finer-resolution sensitivity changes the temporal resolution of the construct; it is not an independent cohort.
