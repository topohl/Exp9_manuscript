# Exp9_manuscript

Manuscript text and publication rendering for Exp9 (social stress, spatial
hippocampal proteomics).

**This repository does not own the underlying biological analyses.** It fits no
models, computes no statistics, selects no candidates and adjusts no
significance thresholds. It renders figures and holds prose. Every number it
displays arrives as a frozen, hash-verified import from an upstream scientific
repository.

## Source repositories

| Source | Owns | Frozen at |
| --- | --- | --- |
| [`topohl/pRoteomics`](https://github.com/topohl/pRoteomics) | preprocessing, QC, differential abundance, WGCNA, GSEA, spatial validation, spatial networks, all statistical models and canonical result tables | `6801edbce8a5d222f4af46e06b6db4e99f6a9761` (tag `pre-restructure-scientific-freeze-2026-09`) |
| [`topohl/MMMSociability`](https://github.com/topohl/MMMSociability) | behavioural acquisition and analysis, CombZ outcome scoring, behaviour prediction models | `30a2666198ca6f23795773da1b7d0ef34e78ac37` (Figure 1), `53bc7e91f3a837c1814babbf5d2ad19553b12457` (Extended Data 5 and 9) |

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
| `manuscript/` | prose: draft, legends, citations, front matter |
| `figures/main/`, `figures/extended_data/` | canonical rendered panels and assembled figures, by publication identity |
| `R/panels/` | panel implementation libraries reached only from figure renderers |
| `source_data/pRoteomics/` | frozen scientific source-data bundle + manifest |
| `source_data/MMMSociability/` | frozen behaviour bundle + manifest (byte-exact, `-text` in `.gitattributes`) |
| `provenance/` | claim provenance, publication registry, source manifests, AI assistance record |
| `export/` | journal submission bundles |
| `tests/` | publication integrity suite |
| `tools/` | bundle verification and rendering utilities |

## Publication identities

Figures are addressed by stable identity, never by renderer generation. The
historical generation names (`final_truth_v9`, `spatial_v6`, `ED1_FINAL_V9`,
`manuscript_figures_v2`) survive only inside provenance records.

| Identity | Status |
| --- | --- |
| `figure_01` | CANONICAL (behaviour, imported from MMMSociability) |
| `figure_02` | CANONICAL |
| `figure_03` | CANONICAL |
| `extended_data_01`, `_02`, `_03`, `_05`, `_06`, `_08`, `_09` | CANONICAL |
| `extended_data_04` | WITHHELD - scope and unsupported spatial labels; identity reserved |
| `extended_data_07` | WITHHELD - unused and archivable; identity reserved |

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
