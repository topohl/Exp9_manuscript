# Candidate Results text: light-phase secondary finding and cookie null (draft for author review)

This text is not in `manuscript_draft.md`. Every estimate, interval, p, q and count below is a stored cell:
- `ebb_v101_20260929_b2ce507` (Stage 29 and Stage 09);
- `s30b_v10_20260929_5394f2f` (Stage 30, copied from run `v1.0_be71e2f`).

The resolving keys are in `figures/behaviour_v101_s30_annotation_map.csv`. Numbers quoted here but not drawn on a panel are under map panels `light_legend` and `cookie_text`: the Movement-adjusted estimates, the descriptive q over all 48 tests, the cookie PRE/POST medians and IQRs, and the animal and cage counts. Clock times, windows and thresholds are registered settings, not cells. Figure identities are placeholders:
- ED X is the longitudinal and light-phase behaviour Extended Data figure;
- ED Y is the cookie Extended Data figure;
- Supplementary Fig. S-screen is the Stage 30 screen summary.

**Inferential hierarchy used throughout**

| Tier | What it is | What it can support |
|---|---|---|
| Stage 29 | Frozen primary characterisation: families P-CC1 and P-TR, fixed in the analytic configuration hashed before any RES/SUS model | Primary claims |
| Stage 09 | Registered prospective prediction | Prediction claims |
| Stage 30 | Exploratory screen. The registry was hash-frozen before any Stage 30 association was computed, but it was written after the null Stage 29 primary results and after historical light-phase inactivity and cookie results had been seen (decision basis: post hoc context) | Hypothesis-generating claims only |
| Cookie assay | A registered negative exploratory result within Stage 30 | A negative exploratory statement |

---

## Paragraph 0: bridge sentence, optional, closing the Stage 29 subsection

The frozen primary characterisation found no evidence that the RES − SUS difference in the first active phase after CC1 differed between the sexes:
- RFID position-change rate: Holm p = 0.52;
- shared RFID-position occupancy: Holm p = 0.66;
- the CC1–CC4 trajectories: Holm p = 1 for both measures (Fig. 1c; Extended Data Fig. X a,b).

All analyses below are exploratory.

## A. Light phase (exploratory)

**Framing.** The registered characterisation concerned the active phase. We therefore examined the light phase after CC1 only within an exploratory screen, whose 48 tests, local Benjamini–Hochberg families and classification rules were hash-frozen before any association was computed. The screen's registry was, however, written after the null Stage 29 primary results and after historical light-phase inactivity and cookie results had been seen (decision basis: post hoc context).

**Main result.** RES females showed greater RFID-defined sustained positional inactivity during the light phase than SUS females:
- RES − SUS 0.0032 of the light phase (95% CI 0.0006 to 0.0058);
- local BH q = 0.049 across six inactivity tests;
- 46 females in 12 cages.

This difference largely paralleled lower light-phase RFID position-change rates in the same females. That Stage 29 secondary estimate was not tested: RES − SUS −0.95 position changes h⁻¹ (−1.91 to 0.01).

**Sex moderation.** The RES − SUS difference in inactivity differed between the sexes (female − male 0.0054, 0.0017 to 0.0090; q = 0.026). The male estimate lay in the opposite direction, with an interval that includes zero (−0.0022, −0.0048 to 0.0005; 41 males in 12 cages). This mirrors the female − male estimate of the light-phase position-change contrast (−1.67, −3.02 to −0.32; Stage 29 secondary estimate, not tested).

**Why this is one measure, not two.**
- The inactivity measure is closely dependent on the position-change rate: Spearman ρ = −0.93 across 444 group-blind animal-windows after Batch × cage-change centring, with a residual reliability of 0.08 after the rate.
- It is near its ceiling: 76 of 87 animals spent at least 99% of the light phase in sustained positional inactivity.
- Adding the same-window position-change rate as a covariate (a registered sensitivity) reduced the female estimate to 0.0009 (0.0002 to 0.0017) and the female − male difference to 0.0013 (0.000012 to 0.0025).

We therefore read the two results as one light-phase phenotype seen through two closely related RFID representations, not as two independent findings. The inactivity measure counts intervals without a vendor-defined RFID position change. That is coarse positional inactivity rather than EEG-defined sleep, and it cannot separate rest from quiet wakefulness.

**Robustness and limits.** The female and interaction estimates kept their sign when each batch was left out in turn. Neither the female estimate nor the sex difference met the descriptive benchmark over all 48 screened tests (global BH q = 0.22 and 0.21), and the result awaits replication (Extended Data Fig. X d,e; Supplementary Fig. S-screen).

## B. Home-cage cookie response (exploratory; negative)

**The response.** The second home-cage cookie presentation (17:00 on the day after the elevated plus maze, EPM+1) elicited a pronounced locomotor response:
- the canonical RFID position-change rate rose from a median of 3 position changes h⁻¹ (IQR 0–8) in the hour before presentation to 28 (12–46) in the hour after;
- every one of the 77 SIS animals with a recording increased (batches B2–B6; 46 females, 31 males).

**Association with later outcome.** Individual variation in this response was not detectably associated with subsequent CombZ or RES/SUS status.
- **RES − SUS difference in the response.** Females: −5.62 position changes h⁻¹ (95% CI −15.7 to 4.49; 12 cages). Males: 0.112 (−16.6 to 16.8; 8 cages in two batches). No evidence that it depended on sex (interaction p = 0.56).
- **Slope of later CombZ on the response.** Females: −0.00212 per position change h⁻¹ (−0.0162 to 0.0120). Males: 0.000351 (−0.0183 to 0.0190). Interaction p = 0.79.

**Registered classification and sensitivities.** All six registered cookie tests were classified as showing no evidence of association. The registered sensitivities (45-min windows, alternative inference, and adjustment for the CC1 active-phase position-change rate) were all labelled compatible; leave-one-batch-out signs were not stable, as expected for estimates near zero.

**Limits.** These intervals do not exclude moderate associations, particularly in males, whose estimates rest on two batches. The assay records the change in home-cage position-change rate only, not the animal's interaction with the cookie (Extended Data Fig. Y).

---

## Wording constraints applied

**Never used:** the terms banned by DESIGN §0 and CANDIDATE_SPEC §A (the same list the candidate SVG test scans), and "no effect". The measure is only ever denied to be EEG-defined sleep, and no other use of that word appears.

**Sex moderation** is stated only through the formal female − male interaction.

**Class A in Stage 30** is described as an exploratory signal, never as a confirmation.

**The cookie measure** is always "home-cage cookie response (second presentation, EPM+1)", defined as the change in canonical home-cage RFID position-change rate from the hour before to the hour after presentation.
