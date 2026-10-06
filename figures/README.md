# Manuscript figure entry points

**New here? Read [FIGURE_INDEX.md](FIGURE_INDEX.md) first.** This directory
holds every figure generation the manuscript has passed through, and all of
them still run, so the file list alone does not say which is current. The index
names, per figure, the entry point to run and the canonical renderer that
produced the panels, and marks the remaining 36 files as superseded.

This directory is the explicit manuscript layer for Figures 2 and 3. The
scientific pipeline remains organized by analysis stage; these entry points
select and validate the exact downstream products promoted into the manuscript.

`figure_contract.yml` is the authority for panel identity, producer, inputs,
scientific contract, biological unit, hemisphere handling, and assembly
position. The scripts never select the newest matching file and never select a
result because it is significant.

## Commands

From the repository root:

```powershell
Rscript "figures/figure_02.R"
Rscript "figures/figure_03.R"
```

Validate without writing:

```powershell
Rscript "figures/figure_02.R" --check-only
Rscript "figures/figure_03.R" --check-only
```

Inspect pipeline availability without writing or failing on absent generated
artifacts:

```powershell
Rscript "figures/figure_02.R" --dry-run
Rscript "figures/figure_03.R" --dry-run
```

Render one panel:

```powershell
Rscript "figures/figure_03.R" --panel 3e
```

Use an isolated candidate root during review:

```powershell
Rscript "figures/figure_03.R" --output-root "C:\path\to\candidate"
```

Figure 2a, the anatomy schematic, is rendered (`f9_schematic`) and validated,
materialized and assembled like every other panel; it is no longer deferred to
Illustrator.

The authoring outputs intentionally remain separate from the journal export
package under `results/manuscript/`. See `docs/OUTPUT_CONTRACTS.md` and run
`Rscript "tools/audit_output_namespaces.R"` for the read-only namespace audit.

## Output contract

Each entry point writes four linked artifact families:

- `results/figures/manuscript/figure_02|03/panels/`: canonical panel SVGs.
- `results/figures/manuscript/figure_02|03/assembled/`: self-contained vector
  SVG, a 300-dpi PNG with each panel rasterised at the page resolution, and a
  PDF: the producing layer's vector page where the contract declares
  `assembled_pdf_source` (Figures 2 and 3, Extended Data 1, 3, 6 and 8),
  otherwise a raster-backed PDF of the same page (Extended Data 2, which omits
  the producer page's panel c).
- `results/source_data/manuscript/figure_02|03/`: exact displayed source-data
  snapshots.
- `results/reports/manuscript_figures/figure_02|03/`: panel and input manifests.
- `results/logs/manuscript_figures/figure_02|03/`: run manifest and session info.

The run manifest records every declared input using a repository-relative path,
resolved runtime path, size, timestamp, and SHA-256. Mapped-drive or UNC spelling
is therefore runtime context rather than artifact identity.

## Scientific boundary

The entry points do not refit differential-abundance, enrichment, or WGCNA
models and do not modify p-values, FDRs, module identities, or source results.
Most panels are promoted from their validated stage-level SVG and source table.
Figure 3 (panels a-m) is rendered here by `figures/final_truth_v9_figure_03.R`
from the frozen imports in `source_data/pRoteomics/`, grouped by the single
`manuscript_go_themes_v3` registry in `f9_atlas_themes()`, and promoted with
`tools/promote_manuscript_render.R figure_03`. (In the superseded v2 grid,
Figure 3e was a WGCNA_m12 filter of the three-module display source.)

Figure 2 and the proteomics Extended Data (1, 2, 3, 6 and 8) are rendered here
too, by `figures/final_truth_v9_figure_02.R` and
`figures/final_truth_v9_extended_data.R`, and published from `figures/main/`
and `figures/extended_data/`. Identities rendered at one commit are promoted
together, in one call:

```powershell
Rscript "tools/promote_manuscript_render.R" figure_02 figure_03 extended_data_01 extended_data_02 extended_data_03 extended_data_06 extended_data_08
```

The three inputs recorded as PROVENANCE_ONLY (above the import size ceiling)
are materialised by hash before rendering, with
`PROTEOMICS_ROOT=<pRoteomics checkout> Rscript tools/import_render_inputs.R --materialize-provenance-only`.
Extended Data 4 (WGCNA) also reads a WGCNA shortlist table this workspace does
not hold, so the Extended Data producer is run with
`--pages=ED1_FINAL_V9,ED2_FINAL_V9,ED3_FINAL_V9,ED6_FINAL_V9,ED7_FINAL_V9,ED8_FINAL_V9`;
its render record names the pages it drew, and the promotion refuses a page
whose panels that record does not cover.

The hemisphere contracts intentionally differ by panel and are declared in the
contract and panel manifest. Technical QC and exploratory PCA remain
sample-level; control-spatial validation remains animal-blocked and
hemisphere-adjusted; stress DA and WGCNA effect panels retain their canonical
animal-level aggregation contracts.

## Future repository cleanup

This layer is deliberately narrow. A later repository-structure task can
separate canonical, candidate, legacy, review, and diagnostic output namespaces
and normalize mapped-drive/UNC provenance without moving validated scientific
code during this implementation. Until then, manuscript promotion is governed
only by `figure_contract.yml`; broad directory scans are not authoritative.
