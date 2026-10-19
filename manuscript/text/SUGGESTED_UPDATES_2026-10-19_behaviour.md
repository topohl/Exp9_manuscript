# Corrected behavioural results for the Word manuscript — 19 October 2026

This note supersedes the behavioural numbers in `SUGGESTED_UPDATES_2026-10-15.md`. No change to Word was made by this rerun. The claim checker reports that the live Word file has changed since the 15 October snapshot; the table below audits that dated snapshot, not an unexamined newer Word revision. Its quoted claim text and numeric values remain unchanged in `provenance/claims/docx_claims.csv`; the behavioural source lookups now point to the corrected-outcome release. The remaining manual and molecular assessments retain their original dates and source generations.

The original model definitions and multiplicity rules were rerun under configuration v1.0.2. The primary prediction retains batch means; neither the predictor nor the outcome is centred within batch. This is a corrected-outcome rerun after inspection of earlier results, with internal validation on the same experiment.

The main result is leave-one-animal-out R² = **0.2177587611 (21.8%)**, compared with 0.1727010141 (17.3%) under the preceding outcome version. The change comes from the corrected outcome, not a new predictor, model search or batch-mean removal. There are 111 RFID animals in both versions. The raw full-refit permutation P is 1/1001 = 0.000999001; the Holm-adjusted P across the two registered behaviour-only models is 0.001998002.

Repeated grouped five-fold validation has mean R² = 0.2138068090 and 2.5th–97.5th split percentiles 0.1742553453–0.2384690225. These percentiles describe repeated splits; they are **not a confidence interval for the leave-one-animal-out R²**. Additional features and sex do not increase the observed held-out R², and no test establishes a difference in performance between models.

Pooled early movement is associated with later CombZ (Spearman ρ = −0.4434254743; 5,000-sample percentile-bootstrap interval −0.5902264823 to −0.2703123374; BH q = 3.2794125 × 10⁻⁶ across the three registered features). Formal feature-by-sex interactions do not pass their BH family (q = 0.7769259272 for movement and 0.5950390708 for each secondary feature). This supports neither a sex-specific headline nor an equivalence claim. Sex is nested in experimental batch.

The full outcome cohort contains 117 animals: 24 CON, 54 RES and 39 SUS. The RFID subset contains 24 CON, 51 RES and 36 SUS: females 12/26/20 and males 12/25/16. OR419 and OR434 change from RES to SUS in the RFID subset. OQ765 remains included as CON. Animal 00317 remains SUS after its restored NOR measurement.

CombZ averages valid available components: 105 animals have six, ten have five and two have four. Blank, damaged or missing organ measurements are not scored as zero; invalid values are excluded from the affected control reference and from that animal's component mean. The corrected classification thresholds are −0.3336348567 for females and −0.4332427382 for males (same-sex control mean minus one population SD).

The following table preserves the Word snapshot's values and supplies the corresponding current cells. A recorded mismatch is a manuscript update requirement, not a failed rerun.

| Claim | Quantity | Word snapshot | Corrected source | Status |
|---|---|---:|---:|---|
| R1-07a | rho | -0.39 | -0.4434254743 | mismatch |
| R1-07b | CI low | -0.55 | -0.5902264823 | mismatch |
| R1-07c | CI high | -0.21 | -0.2703123374 | mismatch |
| R1-07d | q | 6.9e-05 | 3.279412519e-06 | mismatch |
| R1-08a | n animals | 111 | 111 | match |
| R1-08b | n females | 58 | 58 | match |
| R1-08c | n males | 53 | 53 | match |
| R1-09a | n CON | 24 | 24 | match |
| R1-09b | n RES | 49 | 51 | mismatch |
| R1-09c | n SUS | 38 | 36 | mismatch |
| R1-10a | rho | -0.23 | -0.2650459154 | mismatch |
| R1-10b | q | 0.026 | 0.007401628847 | mismatch |
| R1-11a | rho | -0.18 | -0.1918567919 | mismatch |
| R1-11b | q | 0.067 | 0.04367091831 | mismatch |
| R1-12a | LOAO R2 | 0.173 | 0.2177587611 | mismatch |
| R1-12b | baseline R2 | -0.018 | -0.01826446281 | match |
| R1-13a | repeated-CV mean R2 | 0.156 | 0.213806809 | mismatch |
| R1-13b | repeat range low | 0.116 | 0.1742553453 | mismatch |
| R1-13c | repeat range high | 0.179 | 0.2384690225 | mismatch |
| R1-14 | permutation p | 0.000999 | 0.000999000999 | match |

All rows above resolve to `source_data/MMMSociability/ebb_v102_20261019_677ed97/`. Prediction and association values were checked against the new registered tables; primary held-out predictions also agree with independent PRESS and 111 explicit held-out OLS refits. The heatmap and supporting figure bundle are rebuilt separately from their committed producers and independently checked before figure promotion. Correlation-aware support analyses retain their supporting role and do not substitute for the primary prediction result.
