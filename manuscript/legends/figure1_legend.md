# Figure 1 legend

Draft legend for author review. It describes design, measures, models and
sample sizes; every statistic is printed on the panels and resolves, through
`figures/figure_01_annotation_map.csv`, to one cell of a pinned bundle: the
canonical behaviour bundle ebb_v102_20261019_677ed97 and figure-support bundle fsb_v5_20261019_8212df4, pinned in config/behaviour_bundle.yml and config/figure_support_bundle.yml. All statistics are computed upstream in MMMSociability. Configuration v1.0.2 reruns the original analytic definitions on the corrected outcome and retains the frozen v1.0.0 analytic parent. The corrected-outcome rerun follows inspection of the earlier results; the original hypotheses, model set and multiplicity rules are retained. The title and any interpretive
sentence are left to the authors.

---

**Figure 1 | Social instability stress, the later composite outcome, and home-cage
RFID behaviour of animals later classified resilient or susceptible.**

**(a)** Design. Mice received a radio-frequency identification (RFID) implant and
were then exposed to social instability stress (SIS): the composition of their cage
groups was changed every four days, four times (cage changes CC1–CC4); control
(CON) animals stayed in their groups. Shuffle start is CC1; shuffle stop is the end
of the active phase after the last change (the day after CC4). Behavioural tests
followed (sucrose preference; social preference; open field test, OFT; novel object
recognition, NOR; elevated plus maze, EPM), and blood and tissues were collected
last. Printed ages are the range of postnatal days (P) of the 117 animals with the
later outcome at each step, from each animal's date of birth and test dates in the
planning workbook and the RFID cage-change times.

**(b)** Later composite outcome (Integrated resilience, `CombZ`) and the rule that
defines the phenotype, for females (top) and males (bottom). Each point is one
animal (n = 117), ordered by `CombZ` within sex. `CombZ` is the unweighted mean of
six components, each z-scored against same-sex controls with the population
standard deviation; the heatmap shows the six as they enter `CombZ`, signed so that
higher is more resilient-like (corticosterone rise, adrenal weight and spleen
weight inverted), with the colour scale capped at ±2. A grey tile with a stroke is
a missing component. CombZ averages each animal's available valid components: 105 animals have all six, ten have five, and two have four. Unavailable or invalid measurements are omitted from the affected component's control reference and from that animal's composite. Grey line, same-sex control
mean; dashed line, the susceptibility threshold one control population standard
deviation below it; vertical dashed line, the boundary between animals below and
above it. SIS animals below the threshold were classified susceptible (SUS) and
the remainder resilient (RES). The panel shows how the groups were defined, not a
test of them.

**(c)** Behavioural domains (Stage 14), the dark and light phase blocks of CC1–CC4
pooled. Each tile is the stored Hedges *g* (small-sample corrected; comparison
minus reference within sex, from one mean per animal of the domain score, z within
sex, phase and cage change; not batch-adjusted) for RES − CON, SUS − CON and
RES − SUS, at the resolution picked for each domain; Movement is the RFID
position-change rate domain. Rows are grouped by display tier (primary, secondary,
exploratory). Dots mark the cells whose post hoc Kenward–Roger *t* test from a
linear mixed model of the domain score has a Benjamini–Hochberg *q* < 0.05 within
its tier and phase. RES − SUS tests use SIS animals with the cage episode as a
random effect; comparisons with CON treat the 12 CON animals per sex as units
(three stable cages per sex; the clustering of CON animals in cages is not
modelled). Five cells carry post hoc markers. Three are sensitive to the displayed checks: female dark-phase temporal flexibility (RES − SUS) loses its marker at five-minute resolution, and female light-phase occupancy dispersion and temporal volatility (SUS − CON) depend on retaining the B4 control cage. The two male light-phase RES − CON markers remain robust under the assessable checks. The tests are post hoc and the effect sizes descriptive. Females 12 CON, 26 RES, 20 SUS; males 12 CON, 25 RES, 16 SUS.

**(d)** Early movement (the Stage 09 mean movement in the first active phase after
CC1) against later `CombZ`, by sex; one point per animal (58 females, 53 males),
filled by later group. Printed: Spearman ρ and its Benjamini–Hochberg *q* within
sex over Stage 09's three primary features, Stage 09's descriptive sex-stratified
estimate (not an interaction test). Line and band: least-squares line with 95%
confidence band (post hoc, display only), fitted in MMMSociability (run 29c); it
is not preregistered and adds no test.

**(e)** Held-out prediction of continuous `CombZ` by the registered movement-mean
model (sole predictor early mean movement), leave-one-animal-out (n = 111); dashed
line, identity. Fill, later group; shape, sex; neither entered the model. Inset,
1,000 full-refit permutations of the outcome; red line, observed value;
permutation *P* Holm-adjusted across the two registered behaviour-only models.
Batch means are retained in the primary prediction. Validation is internal.

Groups were assigned from later `CombZ`, so panels (b)–(e) use overlapping animals
and data and are complementary views, not independent replication. The model set and analysis rules are those frozen in configuration v1.0.0 and rerun under configuration v1.0.2 on the corrected outcome, without changing the analytic definitions. This corrected-outcome rerun follows inspection of the earlier results.
