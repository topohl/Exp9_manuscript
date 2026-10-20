# Where things live

Written 2026-10-15. This page says where each class of Exp9 result is produced, how it reaches this repository, and what does not reach it yet. The statistics are never computed here.

## The master text

The Word file `S:\Lab_Member\Tobi\Paper\E9SIS\manuscript_resiliencandsusceptibility.docx` is the master text. `manuscript/manuscript_draft.md` is an older markdown draft. It is not the master, but the provenance tables in `provenance/claims/` are keyed to it and several tests read it, so it is kept unchanged as a frozen reference.

## From source to manuscript

| What | Produced in | Enters this repository as | Rendered by |
|---|---|---|---|
| Figure 1 (home-cage RFID behaviour, CombZ, prediction) | MMMSociability (`Analysis/Behavior/RFID` holds its outputs) | frozen bundles `ebb_v102_20261019_677ed97` and `fsb_v5_20261019_8212df4` (corrected outcome) under `source_data/MMMSociability/` | `figures/figure_01_panels.R`, entry point `figures/figure_01.R` |
| Figures 2 and 3; Extended Data 1, 2, 3, 6, 8 | pRoteomics (`Analysis/proteomics`) | `source_data/pRoteomics/` with `manifest.csv` | the `final_truth_v9` renderers in `figures/`, per-panel source data under `figures/main/source_data/` and `figures/extended_data/source_data/` |
| Extended Data 5 and 9 (behaviour) | MMMSociability | the same frozen bundles as Figure 1 | `figures/extended_data_behaviour_panels.R` |
| Extended Data 4 and 7 | | | withheld; the identities are reserved in the registry |
| Extended Data 10 to 12 (conventional tests, physiology, heterogeneity, locomotion; manual versus automated scoring) | SISanalyzer (`Analysis/Behavior/All_tests`, `Analysis/Behavior/Validation`, `Analysis/Physiology`, `Analysis/Resilience`) | frozen bundle `ctp_v5_20261019_991bed1` under `source_data/SISanalyzer/` | `figures/conventional_tests_candidate.R`, per-panel source data under `figures/extended_data/source_data/`; see `CONVENTIONAL_TESTS_CANDIDATE.md` |
| CC4, Stage 30, WGCNA theme and GO candidates | MMMSociability or pRoteomics | `source_data/MMMSociability_candidates/`, `source_data/pRoteomics/*_candidate`, `source_data/MMMSociability/` (Stage 30: `s30b_v11_20261019_5482c43`) | the `*_candidate.R` renderers in `figures/`; see `ADD_A_CANDIDATE.md` |

Registry of what is canonical, with the commit and hash of every promoted artefact: `provenance/publication_registry/canonical_publication_registry.csv`. Hash manifests of every import: `provenance/source_manifests/`; `tools/verify_source_bundles.R` checks them.

## Results that do not enter this repository yet

These were built on 2026-10-15 under `S:\Lab_Member\Tobi\Experiments\Exp9_Social-Stress\Analysis\`. Since 2026-10-16 the conventional tests, physiology, heterogeneity and locomotion results enter as Extended Data candidates through the SISanalyzer bundle (table above); the others have no bundle yet, and none has rows in `provenance/claims/`:

- `Behavior/{EPM,OFT,NOR,SocP,All_tests}`: conventional behaviour tests, 87 measures, females and males separately.
- `Physiology/`: body weight, organs, corticosterone, sucrose, 19 measures.
- `Resilience/`: independent measures, classification check, batch consistency.
- `Behavior/RFID_vs_tests`: RFID movement versus the conventional tests; qualifies Figure 1 statements.
- `proteomics/Behavior_coupling`: proteomic scores versus behaviour, males only (9 animals).

Their code is in the SISanalyzer repository (`behaviour_tests`, `physiology_tests`, `resilience_tests`, `rfid_link_tests`, `proteomics_coupling_tests`). `Analysis/RESULTS_INDEX.csv` lists them with producer, inputs and manuscript status. To bring one in, follow `ADD_A_CANDIDATE.md`.

## Things that look wrong but are known

- Mentions of `docs/*.md` files that are not in this folder (`COMBZ_CANONICAL_DEFINITION.md`, `MANUSCRIPT_STATISTICAL_CONTRACT.md`, `OUTPUT_CONTRACTS.md`, `DATASETS.md`, and others) refer to the MMMSociability or pRoteomics repository. `figures/README.md` cites `docs/OUTPUT_CONTRACTS.md`, which is in pRoteomics.
- The pRoteomics commit once recorded in the README no longer resolves; see the notes under the source table in `README.md`.
- The Stage 30 candidate family keeps the name `behaviour_v101_s30` for its paths and receipt keys although it draws from `ebb_v102` and `s30b_v11`; it is renamed at its next re-render.
