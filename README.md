# Exp9_manuscript

Figure rendering, source data and checks for the Exp9 manuscript (social stress,
spatial hippocampal proteomics).

**This repository does not own the underlying biological analyses.** It fits no
models, computes no statistics, selects no candidates and adjusts no
significance thresholds. It renders figures; the manuscript text is kept with the
Word file, outside this repository (`docs/WHERE_THINGS_LIVE.md`). Every number it
displays arrives as a frozen, hash-verified import from an upstream scientific
repository.

## Source repositories

| Source | Owns | Frozen at |
| --- | --- | --- |
| [`topohl/pRoteomics`](https://github.com/topohl/pRoteomics) | preprocessing, QC, differential abundance, WGCNA, GSEA, spatial validation, spatial networks, all statistical models and canonical result tables | tag `pre-restructure-scientific-freeze-2026-09` (peels to `0f84a103dd895161711c643d0ba1c6e14082ba9a`); later exports are listed in `source_data/pRoteomics/manifest.csv` |
| [`topohl/MMMSociability`](https://github.com/topohl/MMMSociability) | behavioural acquisition and analysis, CombZ outcome scoring, behaviour prediction models | bundles `ebb_v104_20261026_4b651ac` (MMMSociability `4b651ac`; Figure 1 and Extended Data 4, Stage 29 on outcome version ov4), `fsb_v9_20261029_abb28c6` (`f78472f`; Figure 1 support) and `s30b_v13_20261026_dab983e` (`dab983e`; Stage 30) |
| SISanalyzer (local) | conventional behaviour tests, physiology, heterogeneity and locomotion analyses of all 117 animals; the manual-versus-automated scoring validation and the habituation trials | bundle `ctp_v14_20261026_f146fe4` (SISanalyzer `f146fe4`); Extended Data 1 to 3 (`docs/CONVENTIONAL_TESTS_CANDIDATE.md`) |

Notes on these pins (checked 2026-10-15):

- The pRoteomics commit recorded here earlier (`6801edbce8a5d222f4af46e06b6db4e99f6a9761`) does not resolve in the local pRoteomics clone. Imports are verified by file hash against `provenance/source_manifests/prerestructure_freeze_manifest.csv`, so rendering does not depend on it.
- The earlier Figure 1 freeze `30a2666198ca6f23795773da1b7d0ef34e78ac37` and the Extended Data 5 and 9 freeze `53bc7e9` are superseded by the behaviour bundles; Figure 1 and Extended Data 4 (formerly Extended Data 5 and 9) render from the same pinned bundles (outcome version ov4). The bridge survives in `provenance/claims/figure1_bridge_provenance.csv`.
- The local MMMSociability clone's `origin` is `https://github.com/topohl/BehavioralDynamics.git`, not `topohl/MMMSociability`.

There is deliberately **no live cross-repository dependency**. Nothing here
calls `source("../pRoteomics/R/...")` and nothing reads an arbitrary live
results path. Both upstreams enter through frozen bundles plus a manifest:

```
pRoteomics                MMMSociability
     |                          |
  frozen source-data +       frozen behaviour
  provenance manifest        source-data bundle
     |                          |
     +----------> Exp9_manuscript <----------+
                  renders from imports only
```

A later restructure of either upstream must not break rendering here, because
rendering never addresses upstream internal folder structure.

## Layout

| Path | Contents |
| --- | --- |
| `figures/main/`, `figures/extended_data/` | canonical rendered panels and assembled figures, by publication identity |
| `R/panels/` | panel implementation libraries reached only from figure renderers |
| `source_data/pRoteomics/` | frozen scientific source-data bundle + manifest |
| `source_data/MMMSociability/` | frozen behaviour bundles + manifest (byte-exact, `-text` in `.gitattributes`) |
| `source_data/SISanalyzer/` | frozen conventional-tests bundles |
| `source_data/proteomics_statistics/` | proteomic test statistics for stats.xlsx (t, P, set sizes), rows copied from the primary pRoteomics analysis by `tools/import_proteomics_statistics.R` |
| `source_data/MMMSociability_candidates/` | CC4 candidate bundles (MMMSociability Stage 31), byte-exact; pinned in `config/cc4_candidate_bundles.yml` |
| | Only the bundles pinned in `config/*.yml` are read; the other bundle directories are earlier imports, kept unchanged for provenance. |
| `provenance/` | claim provenance, publication registry, source manifests |
| `export/` | journal submission bundles |
| `tests/` | publication integrity suite |
| `tools/` | bundle verification and rendering utilities; `check_claims.R` (looks up the numbers of the Word text in their sources) and `export_docx_text.R` (snapshots the Word file); these and `audit_manuscript_references.R` read the paper folder outside the repository (`E9_PAPER_DIR`, default `S:/Lab_Member/Tobi/Paper/E9SIS`); `build_source_data.R` and `build_stats.R` write the per-figure source data workbooks and `stats.xlsx` there |

## Publication identities

Figures are addressed by stable identity, never by renderer generation. The
historical generation names (`final_truth_v9`, `spatial_v6`, `ED1_FINAL_V9`,
`manuscript_figures_v2`) survive only inside provenance records.

| Identity | Status |
| --- | --- |
| `figure_01` | CANONICAL (behaviour, imported from MMMSociability; panels a-e, as in the registry) |
| `figure_02` | CANONICAL |
| `figure_03` | CANONICAL |
| `extended_data_01` to `_03` | CANONICAL (scoring validation, conventional tests, physiology; drawn from the SISanalyzer bundle) |
| `extended_data_04` | CANONICAL (behavioural prediction models, secondary features and sex) |
| `extended_data_05` to `_08` | CANONICAL (proteomics: bilateral agreement and spatial identity, CA2-SLM QC, enrichment atlases, networks) |
| `withheld_wgcna`, `withheld_identity_locations` | WITHHELD (formerly `extended_data_04` and `_07`) |

Extended Data are numbered by first citation in the manuscript (renumbered 2026-10-20; the
registry's `previous_publication_id` keeps the former id, and `figures/contracts/figure_contract.yml`
holds the number).

`figures/scripts/FIGURE_INDEX.md` is generated by `tools/generate_figure_index.R` and still lists Figure 1 as panels `1a-1f`; the registry (panels `1a-1e`) is the authority.

## Where to look next

- [`docs/WHERE_THINGS_LIVE.md`](docs/WHERE_THINGS_LIVE.md): where each class of result comes from and how it enters this repository, including the result folders that do not enter it yet.
- [`docs/ADD_A_CANDIDATE.md`](docs/ADD_A_CANDIDATE.md): how a new analysis becomes a candidate here without touching a canonical figure.
- The `docs/` files named `*_CANDIDATE.md` and `MANUSCRIPT_PALETTE.md` describe individual candidates and the palette. Mentions of other `docs/*.md` files in provenance records (for example `COMBZ_CANONICAL_DEFINITION.md`, `MANUSCRIPT_STATISTICAL_CONTRACT.md`) refer to the MMMSociability or pRoteomics repository, not to this one.

## What this repository owns

Everything from the frozen source-data bundle onward:

| Step | Entry point |
| --- | --- |
| panel libraries | `R/panels/` |
| figure assembly and validation | `figures/scripts/figure_01.R`, `figure_02.R`, `figure_03.R` |
| journal naming, layout, submission bundle | `tools/package_journal_figures.R` |
| bundle verification | `tools/verify_source_bundles.R` |
| render-input import (one-off, by hand) | `tools/import_render_inputs.R` |

Phase 6D moved journal figure packaging here from pRoteomics. That repository
now stops at the canonical publication source-data bundle, its manifest and a
publication-readiness audit of its own figure outputs.

`R/vendor/` holds nine libraries that pRoteomics and this repository both need
for reading canonical tables and for presentation grammar. They are vendored
byte-identical with `R/vendor/manifest.csv` recording source commit and
sha256, and every one contains zero inference calls. Vendoring rather than
reaching across repositories is what makes the runtime dependency count zero.

`results/` is a gitignored, regenerable render workspace imported by
`tools/import_render_inputs.R`; the tracked manifest under
`provenance/source_manifests/` is what the suite verifies against.

## Verifying the imports

```sh
Rscript tools/verify_source_bundles.R
```

This recomputes a SHA-256 for every imported file and compares it against the
manifests. It must pass before any rendering is trusted.

## What must never happen here

The publication integrity suite enforces these structurally:

- no model fitting or inference in a renderer (`lm(`, `glm(`, `lmer(`, `bam(`,
  `cor.test(`, `p.adjust(`, `gseGO(`, `fgsea`, `WGCNA`);
- no import that is not covered by a manifest entry;
- no figure rendered from a path outside `source_data/`;
- no publication identity that is absent from the registry.

Rendering is allowed. Scientific inference is not.
