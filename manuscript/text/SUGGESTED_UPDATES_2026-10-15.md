# Suggested updates for the Word text

> Behavioural numbers below are historical. Use [the corrected behavioural results of 19 October](SUGGESTED_UPDATES_2026-10-19_behaviour.md) for the current Figure 1 and behavioural Extended Data. The remaining assessments retain the dated scope stated below.

Basis: the snapshot of 2026-10-15 (`manuscript_resiliencandsusceptibility_2026-10-15.txt`; "line N" is a line of that file) and the claim register `provenance/claims/docx_claims.csv` (115 claims; 45 are looked up in the pinned source tables with `tools/check_claims.R`, of which 32 agree and 13 are the older bridge values at lines 16-17; the others were read against the sources, and the register and this note were then re-read independently on 2026-10-16 and corrected). Nothing here was applied to the Word file. Replacement numbers marked "looked up" can be re-checked with the tool; wording is a proposal.

## 1. Numbers to replace (Results 1, lines 16-17)

All from `source_data/MMMSociability/ebb_v101_20260929_b2ce507` (looked up). The current text carries the older bridge values. These are the pooled Stage 09 estimates over the 111 RFID-tracked animals. They use the CombZ and groups from before the workbook correction of 2026-10-16 (`Analysis/SIS_Analysis/WORKBOOK_LOG.csv`), as does all of Figure 1; they change once MMMSociability is rerun on the corrected workbook (section 6, decision 6).

| Where | Now in the text | Replace with |
|---|---|---|
| line 16, mean movement vs CombZ | ρ = −0.39, 95% CI [−0.55, −0.21], q = 6.9 × 10⁻⁵ | ρ = −0.41, 95% CI [−0.56, −0.23], q = 2.6 × 10⁻⁵ |
| line 16, group counts | 24 control, 49 resilient and 38 susceptible | As drawn in Figure 1: 24 / 53 / 34 (females 12 / 28 / 18, males 12 / 25 / 16); the 49 / 38 split predates the correction of 2026-09-20. With the corrected CombZ of 2026-10-16: 24 / 51 / 36 (females 12 / 26 / 20, males 12 / 25 / 16; OR419 and OR434 move to SUS). Whole cohort, corrected: 24 / 54 / 39 |
| line 16, RMSSD | ρ = −0.23, q = 0.026 | ρ = −0.24, 95% CI [−0.42, −0.04], q = 0.018 |
| line 16, entropy autocorrelation | "did not reach FDR support (ρ = −0.18, q = 0.067)" | ρ = −0.19, 95% CI [−0.36, 0.00], q = 0.048. It now passes the BH threshold of 0.05 narrowly and the interval touches zero, so the sentence needs to change, not only the numbers |
| line 17, repeated five-fold | mean R² = 0.156; range 0.116-0.179 | mean R² = 0.168; range 0.125-0.193 |
| line 17, permutation | p = 1/1001 | correct, but say which permutation: this is the unrestricted one, and 1/1001 is the smallest p a 1000-draw test can give. The within-batch permutation gives p of about 0.01 (6/1001 with the registered 1000 draws; 0.011 with 20,000) |
| line 17 | R² = 0.173; baseline −0.018 | correct as written |

Panel references: the early window is no longer a panel (Fig. 1b is now the CombZ panel). Fig. 1d (text: 1c) does not draw the pooled values above; it draws the sex-stratified correlations (females ρ = −0.43, BH q = 0.002, n = 58; males ρ = −0.44, BH q = 0.003, n = 53). Either quote the per-sex values next to Fig. 1d, or cite the pooled values without a panel letter. The prediction is Fig. 1e (text: 1d); the new Fig. 1c, the domain heatmap, is not described in the text. "Supp. Fig. 5a, 5b and 9a" are Extended Data 5a, 5b and 9a, which are still rendered from the older freeze and show the older values (`docs/WHERE_THINGS_LIVE.md`). Decision for you: re-render ED5 and ED9 from the `ebb_v101` bundle before the text is reconciled with them.

## 2. Statements the sources do not support

| Line | Statement | What the sources say | Suggestion |
|---|---|---|---|
| 1 | "(Sex)-specific trajectories" | No group-by-sex result: Stage 09 interaction q = 0.95 / 0.75 / 0.75; Stage 29 sex difference of the RES-SUS contrast Holm p = 0.52 and 0.66; sex moderation of the trajectory Holm p = 1; 0 of 174 behaviour and 0 of 38 physiology group-by-sex tests significant after correction (low power: sex is nested in batch) | Drop "(Sex)-specific", or say "in male and female mice" |
| 4 | "male and female mice" with spatial proteomics | Proteomics exists for nine males only (batches 1 and 2) | Say so in the abstract and in 2.5.3 |
| 4 | "Subsequent behavioural phenotypes reveal dynamic adaptations across repeated stress exposure rather than a simple, stable separation" | Nothing in the Results backs it. The registered trajectory test (Q2b) asks whether the RES/SUS trajectory differs by sex (F(3, 187.6) = 0.11 and F(3, 233.4) = 0.40, Holm p = 1); the sex-averaged RES-SUS change from CC1 has intervals spanning 0 at every change | Remove, or add the result that supports it |
| 4 | "stress exposure produces highly organized molecular remodeling" | Organisation is anatomical (compartment η² 0.96, group 8.5 × 10⁻⁵); the control contrasts are descriptive | "is accompanied by"; say that organisation is mainly anatomical |
| 4 | "resilience and susceptibility emerge as distinct paths that can be detected early in behaviour" | Not tested; the early-behaviour result is a continuous association with CombZ, largely at cohort level (section 3b) | Reword to the association |
| 16 | "animals that were more active during the first undisturbed night" | Movement is the rate of vendor-registered position changes, an index of cage-scale relocation, not a validated measure of locomotion; the window starts an estimated 2.3-8.5 h after the physical cage change | "had a higher position-change rate in the first active phase after the first cage change" |
| 23 | "followed by the four microglia-enriched regional contrasts (r = 0.50-0.76)" | The next three values are microglia (0.50-0.58), then one neuropil contrast (0.58); the fourth microglia contrast (DG) is 0.76 | "followed by three microglia-enriched contrasts (r = 0.50-0.58)" |
| 29 | "253 constituent GO biological-process terms" | 246 unique terms; 253 family-term assignments (7 terms sit in two families) | "246 terms (253 family-term assignments)" |
| 30 | "(Fig. 3c-f)" | Canonical Figure 3 is a-m: the three programmes are Fig. 3e, 3f, 3g; 3c is the adaptation map | Update the letters |
| 31 | "a general ... shift of resilient animals' proteome ... towards an increase of enrichment"; "active shift ... resilience is an adaptive process" | RES-CON median NES is positive in 94 of 126 cells and SUS-CON negative in 83 of 126, but the adaptation classes are symmetric (25 RES-specific, 26 SUS-specific, 12 shared, 3 divergent, 60 none of 126; descriptive, no test). The mitochondrial-respiration family is reduced in both groups only in the CA1 microglia-enriched ROI; across the 18 units it is susceptibility-specific in 10 (shared 1, resilience-specific 2, little detectable 5) | Describe the classes; keep "adaptive process" as interpretation, marked as such |
| 36 | "in both males and females" | See the first two rows | Remove or restrict |
| 40 | "ages 20-58 days" | The design record runs from SIS start at P22 to sample collection at P59 | Check against the planning workbook |
| 40 | "All experiments were performed during the light phase" | RFID was recorded 24 h; tests ran 8:00-16:00 | "All behavioural tests" |
| 42 | "between the ages of 24 days - 27 days" | SIS started at P22-29 (batch 2: P22-24, batch 3: P26-29) | Use the range from the design record |
| 13, 43, 52 | "two-week", "14 day paradigm", "twice per week" | Cage changes are 4 days apart in all six batches (4 cage changes, CC1-CC4); the SIS period is 13 days from the first to the last regrouping plus one day. The Figure 1 legend says "every four days" | "every four days (four cage changes) over 13 days" and make the legend and Methods agree |
| 49 | "inactivity lasting more than 60 seconds ... aligns with prior research (ref 16) ... exceeding 40 seconds" | The text and its own citation give different thresholds | Resolve before use; no Results statement uses it |
| 54 | Open field analysed by TSE software | 2.4 and 2.4.2 say SLEAP / DLCAnalyzer | Pick one |
| 67 | neuropil ROIs from "SO, SR and SLM of CA1-CA3" | The data have 10 neuropil units: SLM exists in CA1 and CA2 only (line 28 says ten) | "SO and SR of CA1-CA3, SLM of CA1 and CA2" |

Results 3 lines 29-30 otherwise match the source: the three programmes (GO:0099536, GO:0006397, GO:0006119) were looked up for all contrasts (13 numbers). The Results 2 numbers (acquisition counts, medians, correlations, NES range, control contrasts) were re-derived from the tracked Figure 2 tables.

## 3. New material that supports the story (proposed wording)

All analyses below are per sex, except the movement-CombZ correlation and the held-out R² of Figure 1 (n = 111), which pool the sexes. Sex is nested in batch, so a sex difference cannot be separated from a cohort difference. Folder READMEs state the statistics in full.

**3a. Results 1, after line 13 (heterogeneity).** The sentence "Social instability induced a heterogenous response in both males and females" can be given numbers:

> Few group comparisons were significant. In females and males analysed separately, 82 of 696 comparisons across 87 behavioural measures (elevated plus maze, open field, novel object recognition, social preference) were nominally significant, about 35 being expected by chance; 11 remained after a two-tier Benjamini-Hochberg correction whose main readouts were fixed at analysis time (3 when all measures were corrected as one family). All 11 are in males and none is a main readout: ten in the open field and one in the social novelty phase. In physiology, 24 of 152 comparisons were nominal and 15 remained; all 15 involve components of the composite score or measures that share data with them, so they follow from how resilient and susceptible animals were defined and are not independent evidence. No group-by-sex interaction was significant after correction (0 of 174 behavioural tests, 12 of them nominal, and 0 of 38 physiological tests); with sex nested in batch and 12 control animals per sex, this does not show that females and males respond alike. In males the open field separated resilient animals from controls (speed while moving, d = 1.55, adjusted p = 0.001), with the same sign in all three male batches; in females no behavioural comparison remained after correction (Extended Data Fig. 10a,b and 11a,b).

The heterogeneity itself can be stated directly (source: `Analysis/Resilience/Heterogeneity`, added 2026-10-16):

> Stress-exposed animals were more variable than controls rather than shifted as a group: the mean CombZ hardly differed between SIS and CON, but its SD was 1.87 times larger in stress-exposed males (95% CI 1.28-3.11, Brown-Forsythe p = 0.031) and 1.67 times larger in stress-exposed females (1.28-2.20, p = 0.13). The extra spread sat in the physiological components (in females sucrose preference, weight gain, corticosterone rise and adrenal weight; in males weight gain and corticosterone rise), not in the main readouts of the conventional tests (Extended Data Fig. 11c,d), and it was the same inside the cages after the last regrouping (within-cage SD ratio 1.65 in females and 1.85 in males; source data of Extended Data Fig. 11d).

Keep "suggestive" for females (p = 0.13; the interval excludes 1, the test does not). In males the adrenal component goes the other way only because of one control value (OQ765, a larger adrenal; checked, kept).

Sources: `Analysis/Behavior/All_tests`, `Analysis/Physiology/All_physiology`, `Analysis/Resilience/Batch_consistency`. From `Analysis/Resilience` worth adding: none of the 71 measures that share no data with CombZ was significant after correction against continuous CombZ in the analysis of all animals (smallest adjusted p 0.148 in females, 0.094 in males); one male social-contact result passed correction only within a second, SIS-only analysis. Leaving out one of the six components changes the group of 4-9 of 46 females and 2-8 of 47 males. Of the 26 significant results, 25 had the same sign in all three batches of the sex, but only 6 remained significant when any one batch was left out (descriptive: the results were selected on the pooled data, so this is not an independent confirmation).

**3b. Results 1, after line 17 (qualify the early-behaviour result).** Source: `Analysis/Behavior/RFID_vs_tests`.

> The association was weaker within batches. After removing batch effects the correlation of movement with CombZ was −0.37 in females and 0.06 in males, and the held-out R² (sexes pooled) fell from 0.216 to 0.049 after centring movement and CombZ within batch (refit within sex and centred: 0.134 in females, −0.064 in males). Movement was not consistently related to locomotion in the conventional tests: in males no movement-locomotion pair reached significance (0 of 10 pairs fixed by the analyst before the correlations were computed, not externally registered; batch-removed ρ −0.10 to 0.21, n = 53), which is absence of evidence rather than evidence of no relation; in females movement agreed with locomotion in the social preference sessions (distance travelled, ρ = 0.46, adjusted p = 0.0045) but not in the open field or plus maze. We therefore refer to movement as the position-change rate.

The R² values are refits on the corrected CombZ (Figure 1e as drawn: 0.173, on the CombZ before the correction; with the corrected CombZ the same model gives 0.216). Line 18 then needs a weaker claim. Option: "Early movement was associated with later CombZ largely between cohorts; within batches the association was weaker in females (ρ = −0.37) and absent in males (ρ = 0.06). Because batch is confounded with cohort, antenna system, board layout and cage epoch, this cannot be read as prospective information about individual animals." Or reduce line 18 to an internally validated association.

**3b2. Results 1, exploratory (locomotion and sex).** Source: `Analysis/Resilience/Locomotion` (added 2026-10-16). Label it exploratory; the index was defined after seeing that most nominal group-by-sex interactions are locomotion measures. With the corrected CombZ of 2026-10-16 no result on the index reaches P < 0.05 (group differences, both interactions, correlation with CombZ; single sessions in Extended Data Fig. 10d do, uncorrected, e.g. female social novelty SUS − CON d = 1.09, P = 0.0065), so it is at most one descriptive sentence, or Extended Data only.

> On a locomotion index (distance travelled in the five test sessions, each z-scored against the controls of the same sex), susceptible females tended to be the more active group (versus controls d = 0.59, P = 0.12) and in males resilient animals (versus controls d = 0.70, P = 0.051), but no group difference and not the group-by-sex interaction (P = 0.089) reached P < 0.05 (Extended Data Fig. 10c,d). There was no evidence that stress exposure as such changed locomotion differently in the two sexes (condition-by-sex P = 0.92). The index correlated with CombZ negatively in females (batch-removed ρ = −0.24, P = 0.07) and positively in males (ρ = 0.15, P = 0.25), neither significantly; home-cage movement shows the same sign pattern (`Analysis/Resilience/Locomotion`).

This no longer gives the title's "sex-specific" a supported content (row 1 of section 2).

**3c. Results 3, after line 31 (proteome and behaviour, nine males).** Source: `Analysis/proteomics/Behavior_coupling` (suggested wording is in its README):

> In the nine males with spatial proteomics (batches 1 and 2; 3 control, 3 resilient, 3 susceptible), 11 of 65 module and GO-theme scores, all from neuronal soma, correlated positively with Integrated Resilience (batch-adjusted Spearman ρ = 0.71-0.82, uncorrected p = 0.012-0.047). These carry only 9 distinct rankings of the animals. No association survived correction across all scores; none reached p < 0.05 in an exact permutation test with ranks formed within each batch (smallest exact p 0.065; 8 did by the t approximation); and no score differed between groups or tracked independent behavioural traits beyond chance. The pattern depends on the adrenal component of CombZ (without it 1 of 65 scores has p < 0.05).

**3d. Methods, Statistics (2.7).** The current section describes lmerTest models on 30-minute bins that no Results statement uses, and nothing on the proteomics statistics. Items to add, with their sources: the Results 1 statistics (Spearman with 5000-sample bootstrap CI, seed 123; BH over three features; leave-one-animal-out and repeated grouped 5-fold, seed 521; 1000 permutations, seed 20260811; feature-by-sex tests; md Methods M-24 to M-27); the conventional-test and physiology statistics (per sex, `lm(value ~ batch + group)`, estimated marginal means for RES − CON, SUS − CON, RES − SUS, SIS − CON, Cohen's d as difference over residual SD, Benjamini-Hochberg in two tiers with the five main readouts together, rank-based check, outlier sensitivity at |studentized residual| > 3); the resilience analyses (batch-adjusted partial Spearman, Hedges g with DerSimonian-Laird random effects and Hartung-Knapp intervals); the proteomics statistics (md Methods M-01 to M-15, M-29 to M-33).

**3e. Limitations (Discussion).** Facts to state: 4 control animals per batch (12 per sex) lived together in one control cage per batch, so comparisons with controls are also comparisons between cages (three control cages per sex) and have wide intervals; sex is nested in batch, so sex differences cannot be separated from cohort differences; RES and SUS are cut out of CombZ, so differences in its components are by construction; the early-behaviour association is largely at cohort level; proteomics covers nine males from two batches, chosen by outcome class; the 2026-10-15 analyses were fixed at analysis time, not before the experiment.

## 4. Placeholders and gaps in the Word text

- lines 6-10: `(ref)` × 2, `(refs)` × 3, and "Romeo & McEwen, 2006" inline; references 3, 4 and 13 are listed but not cited; the docx has 17 numbered references, the md draft 7 author-year references
- line 25: "Kaulich et al., 2025" is cited inline but is not in the reference list
- lines 2-3: authors and corresponding authors empty
- lines 13 and 51: "Supp. Fig. x" (the BORIS comparison; it covers EPM, NOR and social preference, not the open field, and uses the batch 1 subset); now Extended Data Fig. 12
- line 52: the sucrose-preference formula is empty ("as: ."); line 53: "(measurements here)" for the social preference arena; line 60: "microvettes (." unfinished
- lines 33-35: three Discussion headings; the first two have no text, the third has one paragraph (line 36) and bullet notes (line 37)
- missing from Methods although they feed the primary outcome: the novel object recognition protocol, the CombZ definition and its six components, the weight-gain definition, adrenal and spleen dissection, the corticosterone delta; also section number 2.5.1 appears twice
- line 47: the Shannon-entropy sentence is unfinished; 2.3 should state that Movement counts vendor position updates, with the pipeline version (v1.0.1, bundle `ebb_v101_20260929_b2ce507`) and the exclusion-list source (6 batch 1 animals without RFID)
- 2.3 "Assessment of sleep phases" (line 49) and 2.5.2 "Transcriptomics" (lines 63-64) have no Results statement; remove them or add the results
- 2.5.3.2 (lines 69-70): instrument model, gradient, DIA isolation-window scheme, search-engine version, FASTA release and search parameters are not recorded in the manuscript repository (SR-04 in `manuscript/front_matter/submission_readiness.csv`; DIA-NN is identified as the search engine from the code, its version is not); they have to come from the acquisition records

## 5. Where the figure labels point now

| Text | Now |
|---|---|
| Fig. 1a | Fig. 1a (design) |
| Fig. 1b | Fig. 1b is the CombZ panel; the early window is not drawn |
| Fig. 1c | Fig. 1d (sex-stratified association); Fig. 1c is the domain heatmap |
| Fig. 1d | Fig. 1e (prediction) |
| Fig. 2a-g | Fig. 2a-h (2h not described) |
| Fig. 3c-f | Fig. 3e-g (programmes); 3c-d are the adaptation map and pattern burden; Figure 3 is a-m |
| Supp. Fig. 1a, 1b, 2b, 3, 6a,b | ED1a, ED1b, ED2b, ED3, ED6a,b (promoted) |
| Supp. Fig. 5a, 5b, 9a | ED5a, ED5b, ED9a (older values) |
| any ED4 or ED7 reference | withheld; cannot resolve |

## 6. Decisions that are yours

1. Re-render ED5 and ED9 from `ebb_v101`, or cite the older values with that caveat.
2. Whether the title and abstract keep "(sex)-specific" and the "distinct paths" statement, given the sources.
3. Which of 3a-3e go into the Results and which into the Supplement; the conventional-test and physiology results are in Extended Data 10-12 (promoted 2026-10-17).
4. Whether the repository should carry a female proteomics plan (nothing in the proteomics can speak to females).
5. The Stage 33 workbook pin in MMMSociability is stale since the corrections of 2026-10-14/15 (`Analysis/SIS_Analysis/WORKBOOK_LOG.csv`); updating it is a separate step.
6. Whether to rerun MMMSociability on the workbook corrected 2026-10-16 (39 SUS instead of 35; for the RFID-tracked animals OR419 and OR434 move from RES to SUS, mainly because damaged organs are no longer scored, and CombZ changes in all 111 RFID-tracked animals: batch 1 novel-object D2 replaced, damaged and blank organ weights no longer scored, the 00317 novel-object component dropped on 2026-10-16 and restored on 2026-10-19 after its video was re-tracked, female corticosterone z-scores rebuilt). Figure 1, ED5, ED9 and the numbers of section 1 still use the earlier CombZ and groups; ED10, ED11 and section 3 use the corrected ones.
