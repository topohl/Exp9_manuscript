# Figure 1 legend

Draft legend for author review. It describes design, measures, models and
sample sizes; every statistic is printed on the panels and resolves, through
`figures/figure_01_annotation_map.csv`, to one cell of the pinned canonical
behaviour bundle (`config/behaviour_bundle.yml`). All statistics were computed
and frozen in MMMSociability (Stage 29 characterisation and Stage 09 registered
prediction, bundled by Stage 16b under configuration v1.0.1, bundle
`ebb_v101_20260929_b2ce507`); nothing on this figure is calculated in this
repository. The analytic parent of v1.0.1 is configuration v1.0.0, hashed before
any resilient-versus-susceptible model was fitted; v1.0.1 changes documentation
and terminology and binds the corrected data version (cage labels), with no
analytic change, and was made after outcome inspection. The title and any
interpretive sentence are left to the authors.

---

**Figure 1 | Home-cage RFID behaviour after cage changes in animals later
classified resilient or susceptible, and the early RFID position-change rate
against the later composite outcome.**

**(a)** Design. Radio-frequency identification (RFID) position records of
undisturbed home-cage behaviour were analysed in the first full active phase
after each cage change (18:30–06:30, 12 h), beginning an estimated 2.3–8.5 h
after the change; the first cage change (CC1) was at postnatal day 25 (P25). No
RFID recording exists before CC1. Stress-exposed (SIS) animals experienced
repeated changes of cage composition; control (CON) animals were never
regrouped. Every component of the later outcome, the composite score and the
resilient/susceptible labels were obtained after these windows. Measures are
computed from the change-only stream of RFID position records without time bins.

**(b)** Later composite outcome and the rule that defines the phenotype. Each
point is one animal (n = 117). `CombZ` is the unweighted mean of six components
(novel object recognition, sucrose preference, weight deviation, delta
corticosterone, adrenal weight, spleen weight), each z-scored against same-sex
controls with the population standard deviation, with the last three
sign-inverted. Higher `CombZ` indicates a more resilient-like outcome. Grey
line, same-sex control mean; dashed line, the susceptibility threshold one
control population standard deviation below it. SIS animals below the threshold
were classified susceptible (SUS) and the remainder resilient (RES). The panel
shows how the groups were defined, not a test of them.

**(c)** First active phase after CC1. Left, RFID position-change rate (position
changes per observed hour, h⁻¹), an index of cage-scale relocation. Right,
shared RFID-position occupancy: the fraction of co-assigned dyadic time an animal
was assigned to the same RFID position as its current tracked cage-mates, an
index of spatial overlap with cage-mates. Points, animals; hollow points in the CON colour, CON, shown for
reference and not modelled. Black points and bars, model-based RES and SUS means
with 95% confidence intervals from sex-stratified linear mixed models of SIS
animals (`y ~ Batch + group + (1 | cage epoch)`, Kenward–Roger). Printed:
RES − SUS within each sex (95% CI), and the sex difference in RES − SUS from the
pooled SIS model (`y ~ Batch + group + group:sex + (1 | cage epoch)`) with its
Holm-adjusted p across the two primary measures (family P-CC1, m = 2).
SIS animals: 46 female (28 RES, 18 SUS) and 41 male (25 RES, 16 SUS); shared
RFID-position occupancy is undefined for 2 animals without a tracked cage-mate at
CC1.

**(d)** Active phase after each of CC1–CC4. Lines, model-based RES and SUS means
with 95% confidence intervals from sex-stratified mixed models with cage change
as a categorical factor, animal and cage-epoch random intercepts, and, for
the position-change rate, an uncorrelated random slope over cage changes; dashed lines
in the CON colour, descriptive CON means. Printed: the joint Kenward–Roger F test of whether
the RES − SUS trajectory differs between sexes (three group × cage change × sex
terms in the pooled SIS model) with its Holm-adjusted p across the two primary
measures (family P-TR, m = 2). The cage-change-averaged sex moderation is a
secondary estimate reported separately.

**(e)** Early RFID position-change rate after CC1 (shown as 6 × the Stage 09
10-min mean movement, position changes h⁻¹) against later `CombZ`; one point per animal,
n = 111. Spearman ρ with a 5,000-sample percentile bootstrap 95% CI and
Benjamini–Hochberg q across three registered features. No model is fitted in
this panel and no line is drawn.

**(f)** Held-out prediction of continuous `CombZ` by the registered
movement-mean model (sole predictor early mean movement), leave-one-animal-out
(n = 111); dashed line, identity. Fill, later group; shape, sex; neither entered
the model. Right, 1,000 full-refit permutations of the outcome; vertical line,
observed value; permutation p Holm-adjusted across the two registered
behaviour-only models. The repeated grouped five-fold range is a resampling range
across repeats, not a confidence interval. Validation is internal.

Groups were assigned from later `CombZ`, so panels (c)–(e) use overlapping
animals and data and are complementary views, not independent replication. A
sex-moderation estimate whose interval includes zero indicates imprecision, not
equivalence. The primary families (P-CC1, P-TR) were fixed, with every model,
contrast and robustness rule, in configuration v1.0.0, hashed before any
resilient-versus-susceptible model was fitted; configuration v1.0.1, under which
this figure was rendered, leaves that specification unchanged.
