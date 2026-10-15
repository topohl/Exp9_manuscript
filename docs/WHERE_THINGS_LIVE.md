# Where things live

Written 2026-10-15. This page says where each class of Exp9 result is produced, how it reaches this repository, and what does not reach it yet. The statistics are never computed here.

## The master text

The Word file `S:\Lab_Member\Tobi\Paper\E9SIS\manuscript_resiliencandsusceptibility.docx` is the master text. `manuscript/manuscript_draft.md` is an older markdown draft. It is not the master, but the provenance tables in `provenance/claims/` are keyed to it and several tests read it, so it is kept unchanged as a frozen reference.

## From source to manuscript

| What | Produced in | Enters this repository as | Rendered by |
|---|---|---|---|
| Figure 1 (home-cage RFID behaviour, CombZ, prediction) | MMMSociability (`Analysis/Behavior/RFID` holds its outputs) | frozen bundles `ebb_v101_20260929_b2ce507`, `fsb_v4_20261012_f4c2ba6` and `s30b_v10_20260929_5394f2f` under `source_data/MMMSociability/` | `figures/figure_01_panels.R`, entry point `figures/figure_01.R` |
| Figures 2 and 3; Extended Data 1, 2, 3, 6, 8 | pRoteomics (`Analysis/proteomics`) | `source_data/pRoteomics/` with `manifest.csv` | the `final_truth_v9` renderers in `figures/`, per-panel source data under `figures/main/source_data/` and `figures/extended_data/source_data/` |
| Extended Data 5 and 9 (behaviour) | MMMSociability freeze `53bc7e9` | `results/source_data/manuscript/extended_data_05` and `_09` (gitignored workspace) | `figures/extended_data_behaviour_panels.R` |
| Extended Data 4 and 7 | | | withheld; the identities are reserved in the registry |
| CC4, Stage 30, WGCNA theme and GO candidates | MMMSociability or pRoteomics | `source_data/MMMSociability_candidates/`, `source_data/pRoteomics/*_candidate`, `source_data/MMMSociability/` | the `*_candidate.R` renderers in `figures/`; see `ADD_A_CANDIDATE.md` |

Registry of what is canonical, with the commit and hash of every promoted artefact: `provenance/publication_registry/canonical_publication_registry.csv`. Hash manifests of every import: `provenance/source_manifests/`; `tools/verify_source_bundles.R` checks them.

## Results that do not enter this repository yet

These were built on 2026-10-15 under `S:\Lab_Member\Tobi\Experiments\Exp9_Social-Stress\Analysis\`. They have no bundle, no manifest and no rows in `provenance/claims/`:

- `Behavior/{EPM,OFT,NOR,SocP,All_tests}`: conventional behaviour tests, 87 measures, females and males separately.
- `Physiology/`: body weight, organs, corticosterone, sucrose, 19 measures.
- `Resilience/`: independent measures, classification check, batch consistency.
- `Behavior/RFID_vs_tests`: RFID movement versus the conventional tests; qualifies Figure 1 statements.
- `proteomics/Behavior_coupling`: proteomic scores versus behaviour, males only (9 animals).

Their code is in the SISanalyzer repository (`behaviour_tests`, `physiology_tests`, `resilience_tests`, `rfid_link_tests`, `proteomics_coupling_tests`). `Analysis/RESULTS_INDEX.csv` lists them with producer, inputs and manuscript status. To bring one in, follow `ADD_A_CANDIDATE.md`.

## Things that look wrong but are known

- `figures/FIGURE_INDEX.md` is generated and still says Figure 1 has panels `1a-1f`. The registry says `1a-1e`.
- Mentions of `docs/*.md` files that are not in this folder (`COMBZ_CANONICAL_DEFINITION.md`, `MANUSCRIPT_STATISTICAL_CONTRACT.md`, `OUTPUT_CONTRACTS.md`, `DATASETS.md`, and others) refer to the MMMSociability or pRoteomics repository. `figures/README.md` cites `docs/OUTPUT_CONTRACTS.md`, which is in pRoteomics.
- The pRoteomics commit once recorded in the README no longer resolves; see the notes under the source table in `README.md`.
- Extended Data 5 and 9 come from the older `53bc7e9` freeze and can show behaviour values that differ from the current Figure 1.
- Stage 33 of MMMSociability pins `SIS_Analysis/E9_Behavior_Data.xlsx` by SHA-256 and the pin is stale since the corrections of 2026-10-14/15 (`Analysis/SIS_Analysis/WORKBOOK_LOG.csv`).
