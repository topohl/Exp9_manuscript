# Candidate legends: behaviour_v101_s30 (draft for author review)

These are draft legends for author review. They describe the design, measures, models, sample sizes and inferential tier.

**Where the numbers come from**
- Every statistic on the panels, and every estimate, interval, p, q and count that these legends quote, resolves through `figures/behaviour_v101_s30_annotation_map.csv` to one stored cell of one of two bundles:
  - the pinned Stage 29 behaviour bundle `ebb_v101_20260929_b2ce507` (Stage 29 characterisation and Stage 09 registered prediction; configuration v1.0.1);
  - the pinned Stage 30 figure bundle `s30b_v10_20260929_5394f2f` (frozen Stage 30 run `v1.0_be71e2f`, registry v1.0).
- Numbers quoted here but not drawn have their own map panels (`light_legend`, `cookie_text`, `screen_legend`, and for option 2 (d) the light-phase panels' keys). Each candidate's source data list them with `src_role = legend`.
- Two kinds of number are not bundle cells: registered settings (windows, clock times, thresholds, bout lengths, bootstrap and permutation counts), and the design facts that the Figure 1 option legends carry over from the canonical Figure 1 legend (recording lag, P25).
- Nothing is calculated in this repository. The medians and quartiles on the cookie panel, and the centroid of the cookie regression line, are descriptive summaries computed in MMMSociability and exported in the Stage 30 bundle.

**Figure identities are placeholders:**
- ED X: longitudinal and light-phase behaviour;
- ED Y: home-cage cookie response;
- Supplementary Fig. S-screen: the Stage 30 screen.

The title and any interpretive sentence are left to the authors.

---

## Figure 1, option 1 (conservative)

**Figure 1 | Home-cage RFID behaviour after the first cage change in animals later classified resilient or susceptible, and the early RFID position-change rate against the later composite outcome.**

**(a) Design.** Radio-frequency identification (RFID) position records of undisturbed home-cage behaviour were analysed in the first full active phase after each cage change (18:30–06:30, 12 h).
- Recording began an estimated 2.3–8.5 h after the change. The first cage change (CC1) was at postnatal day 25 (P25), and no RFID recording exists before CC1.
- Stress-exposed (SIS) animals experienced repeated changes of cage composition. Control (CON) animals were never regrouped.
- Every component of the later outcome, the composite score and the resilient/susceptible labels were obtained after these windows.
- Measures are computed without time bins from the change-only stream of vendor-defined RFID position records. A position change is a vendor-defined relocation between RFID positions.

**(b) Later composite outcome and the rule that defines the phenotype.** Each point is one animal (n = 117).
- `CombZ` is the unweighted mean of six components: novel object recognition, sucrose preference, weight deviation, delta corticosterone, adrenal weight and spleen weight.
- Each component is z-scored against same-sex controls with the population standard deviation, and the last three are sign-inverted. Higher `CombZ` indicates a more resilient-like outcome.
- Grey line, same-sex control mean. Dashed line, the susceptibility threshold, one control population standard deviation below it.
- SIS animals below the threshold were classified susceptible (SUS) and the rest resilient (RES). The panel shows how the groups were defined; it is not a test of them.

**(c) First active phase after CC1.**
- **Measures.** Left, RFID position-change rate (position changes per observed hour), an index of cage-scale relocation. Right, shared RFID-position occupancy: the fraction of co-assigned dyadic time an animal carried the same RFID position as its current tracked cage-mates, an index of spatial overlap with cage-mates.
- **Marks.** Points, animals. Hollow grey points, CON, shown for reference and not modelled. Black points and bars, model-based RES and SUS means with 95% confidence intervals from sex-stratified linear mixed models of SIS animals (`y ~ Batch + group + (1 | cage epoch)`, Kenward–Roger).
- **Printed.** RES − SUS within each sex (95% CI). Also the female − male difference in RES − SUS ("Female − male") from the pooled SIS model (`y ~ Batch + group + group:sex + (1 | cage epoch)`), with its Holm-adjusted p across the two primary measures (family P-CC1, m = 2).
- **n.** SIS animals: 46 female (28 RES, 18 SUS) and 41 male (25 RES, 16 SUS). Shared RFID-position occupancy is undefined for 2 animals without a tracked cage-mate at CC1.

**(d) Early RFID position-change rate after CC1 against later `CombZ`.**
- The rate is shown as 6 × the Stage 09 10-min mean movement, in position changes h⁻¹. One point per animal, n = 111.
- Printed: Spearman ρ with a 5,000-sample percentile bootstrap 95% CI, and the Benjamini–Hochberg q across three registered features.
- No model is fitted in this panel and no line is drawn.

**(e) Held-out prediction of continuous `CombZ` by the registered movement-mean model**, whose sole predictor is early mean movement.
- Left: leave-one-animal-out prediction (n = 111). Dashed line, identity. Fill, later group; shape, sex; neither entered the model.
- Right: 1,000 full-refit permutations of the outcome. Vertical line, observed value. The permutation p is Holm-adjusted across the two registered behaviour-only models.
- The repeated grouped five-fold range is a resampling range across repeats, not a confidence interval.
- Validation is internal.

**Caveats across panels**
- Groups were assigned from later `CombZ`, so panels (c)–(e) use overlapping animals and data. They are complementary views, not independent replication.
- A sex-moderation estimate whose interval includes zero indicates imprecision, not equivalence.
- The primary families (P-CC1, P-TR) were fixed, with every model, contrast and robustness rule, in configuration v1.0.0, hashed before any resilient-versus-susceptible model was fitted. Configuration v1.0.1, under which this figure was rendered, leaves that specification unchanged.
- The CC1–CC4 trajectories of the second primary family (P-TR) are in Extended Data Fig. X a,b.

## Figure 1, option 2 (integrated)

Panels are lettered in reading order. (a)–(c) are as in option 1, with narrower boxes for (b) and (c). The compact light-phase panel is (d), in row 2, and option 1's (d) and (e) become (e) and (f).

**(d) Light phase after CC1 (exploratory).** RES − SUS contrasts for the light phase (06:30–18:30) that follows the first active phase after CC1, in SIS animals only. Rows: female (F), male (M), and the female − male difference in RES − SUS (F − M). Points (circles for F and M, a diamond for F − M), stored estimates; bars, 95% CIs. The diamond marks the difference row; neither shape nor fill codes significance.
- **Top, RFID position-change rate** (position changes h⁻¹): F −0.95 (95% CI −1.91 to 0.01), M 0.73 (−0.25 to 1.71), F − M −1.67 (−3.02 to −0.32). These are Stage 29 secondary estimates from sex-stratified mixed models, as in (c): estimates only, with no test. Their decision basis was post hoc context.
- **Bottom, RFID-defined sustained positional inactivity (≥40 s)**, the share of observed light-phase time spent in intervals of at least 40 s without a canonical vendor-defined RFID position change. The lower strip is in fraction of light phase. F 0.0032 (0.0006 to 0.0058), M −0.0022 (−0.0048 to 0.0005), F − M 0.0054 (0.0017 to 0.0090).
  - These are Stage 30 exploratory estimates: `y ~ Batch + group + (1 | cage epoch)` within sex, with Kenward–Roger t; the female − male difference comes from the pooled model with group:sex.
  - The printed q is the Benjamini–Hochberg value within the six registered inactivity tests only (local family, m = 6). Over all 48 screened tests the descriptive Benjamini–Hochberg q is 0.22 for the female contrast and 0.21 for the female − male difference; it is not a decision value.
  - The inactivity measure is saturated in the light phase (76 of 87 animals ≥ 0.99), so the contrast rests on the lower tail of the distribution.
  - With the same-window position-change rate as a covariate (a registered sensitivity), the female estimate was 0.0009 (0.0002 to 0.0017) and the female − male difference 0.0013 (0.000012 to 0.0025).
- The two measures are closely dependent: Spearman ρ = −0.93 across 444 group-blind animal-windows after Batch × cage-change centring, with a residual reliability of 0.08 after the rate (Extended Data Fig. X e). The inactivity measure captures coarse positional inactivity rather than EEG-defined sleep.
- Stage 30 is exploratory. Its registry was hash-frozen before any Stage 30 association was computed, but it was written after the null Stage 29 primary results and after historical light-phase inactivity and cookie results had been seen (decision basis: post hoc context).
- The animals behind these estimates are shown in Extended Data Fig. X d.

**(e) Early RFID position-change rate after CC1 against later `CombZ`; (f) held-out prediction.** As option 1 (d) and (e).

## Standalone light-phase panel (candidate; used as Extended Data Fig. X d)

**Light phase after CC1: RFID position-change rate and RFID-defined sustained positional inactivity in SIS animals later classified resilient or susceptible.**

**Window.** The light phase (06:30–18:30 on the RFID logger clock) that follows the first active phase after CC1.

**Left, light-phase RFID position-change rate** (position changes h⁻¹).
- Top: animals by sex and later group. RES are triangles and SUS are squares; CON animals are not shown and were not modelled.
- Bottom: RES − SUS in females and in males, and the female − male difference in RES − SUS, with 95% CIs.
- These are Stage 29 secondary estimates: sex-stratified `y ~ Batch + group + (1 | cage epoch)` and the pooled model with group:sex, Kenward–Roger. Their decision basis was post hoc context, and no test was performed.

**Right, RFID-defined sustained positional inactivity (≥40 s)**, as a fraction of observed light-phase time.
- **Definition.** Every interval between consecutive canonical position changes that lasts at least 40 s counts in full. Intervals are clipped to the window before they qualify, and none bridges 06:30 or 18:30.
- **What it measures.** The measure reflects intervals without a vendor-defined RFID position change. It therefore captures coarse positional inactivity rather than EEG-defined sleep, and its fractions are not EEG-derived durations.
- **Saturation.** The measure is saturated in the light phase: the y axis starts at 0.975, and 76 of 87 animals are at or above 0.99.
- **Bottom strip.** Stage 30 exploratory estimates from the same model structure, with the Benjamini–Hochberg q within the six registered inactivity tests only (local family, m = 6). Over all 48 screened tests the descriptive Benjamini–Hochberg q is 0.22 for the female contrast and 0.21 for the female − male difference; it is not a decision value.
- **Registry.** Stage 30 is exploratory. Its registry was hash-frozen before any Stage 30 association was computed, but it was written after the null Stage 29 primary results and after historical light-phase inactivity and cookie results had been seen (decision basis: post hoc context).

**Marks in both strips.** Circles, the within-sex RES − SUS estimates; the diamond, the female − male difference. The shapes mark the row type, not significance.

**n.** 46 females (28 RES, 18 SUS; 12 cages; batches B3, B4, B6) and 41 males (25 RES, 16 SUS; 12 cages; batches B1, B2, B5).

**Registered sensitivities.** These are the 60-s threshold, adjustment for the same-window position-change rate, and leave-one-batch-out refits. They are in Supplementary Table S-screen. With the same-window position-change rate as a covariate, the female inactivity estimate was 0.0009 (0.0002 to 0.0017) and the female − male difference 0.0013 (0.000012 to 0.0025).

The two columns are closely related representations of one light-phase locomotor phenotype; see Extended Data Fig. X e.

## Extended Data Fig. X (candidate): longitudinal and light-phase home-cage RFID behaviour

**Extended Data Fig. X | Home-cage RFID behaviour across cage changes and in the light phase, in SIS animals later classified resilient or susceptible.**

**(a–c) The active phase after each of CC1–CC4** (18:30–06:30), for every construct in (a)–(c). (a) RFID position-change rate and (b) shared RFID-position occupancy, the Stage 29 primary constructs; (c) the Stage 29 secondary constructs.

**(a, b) Primary constructs.**
- **Lines.** Model-based RES and SUS means with 95% CIs, from sex-stratified mixed models. The models have cage change as a categorical factor and random intercepts for animal and cage epoch; for the position-change rate they add an uncorrelated random slope over cage changes.
- Grey dashed lines, descriptive CON means.
- **Printed.** The joint Kenward–Roger F test of whether the RES − SUS trajectory differs between sexes (the three group × cage change × sex terms of the pooled SIS model), with its Holm-adjusted p across the two primary measures (family P-TR, m = 2).

**(c) Secondary constructs.**
- **Constructs.** Occupancy dispersion (entropy of occupancy across RFID positions, bits) and fragmentation (proportion of activity bouts with a single position change). Both are drawn as in (a, b).
- **Printed.** The same joint test, with its Holm-adjusted p within the secondary family S-TR-ORG.

**(d) Light phase after CC1.** The standalone light-phase panel.
- (d, left) RFID position-change rate (Stage 29 secondary estimates, not tested).
- (d, right) RFID-defined sustained positional inactivity (≥40 s) (Stage 30 exploratory estimates; q local to the six inactivity tests, m = 6; descriptive q over all 48 screened tests 0.22 for the female contrast and 0.21 for the female − male difference).
- The inactivity measure is saturated (76 of 87 animals ≥ 0.99). With the same-window position-change rate as a covariate (a registered sensitivity), the female inactivity estimate was 0.0009 (0.0002 to 0.0017) and the female − male difference 0.0013 (0.000012 to 0.0025).
- Stage 30 is exploratory; its registry was written after the null Stage 29 primary results and after historical light-phase inactivity and cookie results had been seen (decision basis: post hoc context).
- Circles, within-sex RES − SUS; diamonds, the female − male difference (row type, not significance).

**(e) Dependence of the inactivity measure on the position-change rate.**
- **Data.** All 444 light-phase animal-windows (111 animals including CON, CC1–CC4), group-blind. The points are the stored window values, not centred.
- **Printed.** The stored Spearman ρ after Batch × cage-change centring, and the stored residual reliability after the rate (the animal-level ICC across CC1–CC4 of the Batch × cage-change-centred inactivity after a spline on the rate).
- Both values come from the group-blind measurement audit, completed before the Stage 30 registry was frozen. The panel is descriptive: no line is fitted and no test is made.
- Its plot panel shares the vertical frame and the inactivity axis of (d, right), so the two read across.
- It shows why the inactivity contrast in (d, right) is read as a second representation of the light-phase position-change phenotype, rather than as evidence about EEG-defined sleep.

**Inferential tiers.** (a, b) Stage 29 primary; (c) Stage 29 secondary; (d, left) Stage 29 secondary estimate, not tested; (d, right) Stage 30 exploratory; (e) descriptive.

## Extended Data Fig. Y (candidate): home-cage cookie response

**Extended Data Fig. Y | The second home-cage cookie presentation evoked a locomotor response whose size did not detectably map onto later `CombZ` or RES/SUS status.**

**Measure.** The home-cage cookie response (second presentation, EPM+1) is the change in canonical home-cage RFID position-change rate from the hour before to the hour after presentation.
- PRE [16:00, 17:00) and POST [17:00, 18:00), on the RFID logger clock, on the day after the elevated plus maze (EPM+1). Δ60 = POST − PRE.
- It records the change in home-cage position-change rate only, not the animal's interaction with the cookie.

**Population.** SIS animals in batches B2–B6 (B1 had no home-cage cookie): 46 females (28 RES, 18 SUS; 12 cages; B3, B4, B6) and 31 males (21 RES, 10 SUS; 8 cages; B2, B5).
- One female (OR646) was re-housed with two former cage-mates, a documented protocol deviation. She is included as registered.
- The first presentation, confounded by the EPM and the cage change, and EPM+2 were not analysed.

**(a) Response by animal.** PRE and POST rates, one grey line per animal, by sex. Open point and bar, the median and interquartile range, descriptive (drawn without whiskers, unlike the model estimates ± 95% CI of Fig. 1c).

**(b) Response by later group.**
- Top: Δ60 by sex and later group.
- Bottom: registered exploratory contrasts, RES − SUS in females and in males (circles) and the female − male difference (diamond), with 95% CIs, in Δ position changes h⁻¹.
- Models: `Δ60 ~ Batch + group + (1 | cage epoch)` within sex, with the pooled group:sex model for the difference; Kenward–Roger.
- Printed: raw p, and the Benjamini–Hochberg q within the three registered cookie-group tests (local family, m = 3, as the panel subtitle states).

**(c) Response against later `CombZ`, by sex.**
- The line is the registered linear model `CombZ ~ Batch + Δ60`, fitted within sex with CR2 cluster-robust inference by cage epoch and Satterthwaite df. It is drawn as the frozen slope through the sex-resolved centroid, which equals the batch-averaged fitted line; no model was refitted for display.
- Printed: the slope with its 95% CI, in `CombZ` per position change h⁻¹ (the y unit per x unit), and the p of the female − male difference in slope.

**Registered classification and sensitivities.**
- All six cookie tests were registered exploratory tests, and all were classified as showing no evidence of association.
- Their registered sensitivities (45-min windows, alternative inference, and adjustment for the CC1 active-phase position-change rate) were all labelled compatible; leave-one-batch-out signs were not stable, as expected for estimates near zero.
- The intervals do not exclude moderate associations, particularly in males, whose estimates rest on two batches.

## Supplementary Fig. S-screen (candidate): the Stage 30 exploratory screen

**Supplementary Fig. S-screen | All 48 registered Stage 30 exploratory tests.**

**Registry.** The registry (v1.0) was hash-frozen before any Stage 30 association with RES/SUS, group or `CombZ` was computed. It was written after the null Stage 29 primary results and after historical light-phase inactivity and cookie results had been seen (decision basis: post hoc context). No result shown here is primary.

**Layout.** Columns: within females, within males, and the female − male difference (formal sex moderation). Blocks:
- **CONT:** the CC1 metric (for the cookie row, the EPM+1 response) against later `CombZ`, `CombZ ~ Batch + x` within sex, with CR2 by cage epoch and Satterthwaite df. Shown as `CombZ` per standard deviation of the metric.
- **CAT:** RES − SUS, `y ~ Batch + group + (1 | cage epoch)` within sex, Kenward–Roger. Shown in standard deviations of the metric.
- **L:** the CC1–CC4 trajectory, a joint Kenward–Roger F(3, df) test of the three `CombZ` × cage-change terms, with a thin solid reference at F = 1 (the zero lines of CONT and CAT are dashed). In the female − male column the L cell is the joint F of the sex-moderation terms (its sex difference), unsigned.

Standardization divides by, or multiplies by, the standard deviation of the metric's residuals after removing Batch. This SD is computed within each sex for the female and male columns, and across both sexes for the female − male column, so the three columns of a block share an axis but not one SD. Row labels give the metric, the window, and the local family with its number of tests (m).

**Marks.** Circles, within-sex estimates; diamonds, the female − male column. The shapes mark the column type, not significance. The word "class" heads only the female − male column; every cell carries its class letter.

**Cells.** Each cell prints the raw p and the Benjamini–Hochberg q within the test's local family. The grey letter is the registered class. The rules are applied in the order D, A, B, C:
- **D:** an inactivity test with raw p < 0.05 whose estimate, adjusted for the same-window position-change rate, has a 95% CI including zero (L: whose adjusted joint F has p ≥ 0.05), or a failed fit;
- **A:** not D; local q < 0.05, the sign stable in every leave-one-batch-out refit, and no sensitivity reversing the sign;
- **B:** not D or A; raw p < 0.05;
- **C:** not D; raw p ≥ 0.05.

Class A marks a robust exploratory signal, not a confirmation.

**Saturation.** The light-phase inactivity rows are saturated (76 of 87 animals ≥ 0.99 at CC1), so their inference rests on the lower tail.

**Not shown.** The descriptive Benjamini–Hochberg q over all 48 tests, the pooled-sex estimates (estimation only), the component estimates of the L tests and all registered sensitivities are in Supplementary Table S-screen. Stage 29 combinations are shown there as carry-over rows and were not re-tested.

**Calibration.** Group-blind null calibration rated every test acceptable, some with a qualifier (verdicts in Supplementary Table S-screen).
