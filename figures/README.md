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

Figure 2a is intentionally deferred to Illustrator. It is retained in the
panel contract for provenance, but it is not a repository input and is excluded
from automated validation, materialization, and assembly. The automated Figure
2 output therefore contains panels 2b-2f and leaves the declared 2a layout slot
empty for final Illustrator composition.

The authoring outputs intentionally remain separate from the journal export
package under `results/manuscript/`. See `docs/OUTPUT_CONTRACTS.md` and run
`Rscript "tools/audit_output_namespaces.R"` for the read-only namespace audit.

## Output contract

Each entry point writes four linked artifact families:

- `results/figures/manuscript/figure_02|03/panels/`: canonical panel SVGs.
- `results/figures/manuscript/figure_02|03/assembled/`: self-contained vector
  SVG plus 300-dpi PNG and raster-backed PDF companions.
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
