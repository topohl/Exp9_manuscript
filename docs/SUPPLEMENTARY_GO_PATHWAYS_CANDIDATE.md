# Additional GO pathway examples: supplementary candidate

Historical four-term exploratory candidate. The comprehensive seven-theme
appendix is documented in `docs/FIGURE_03_GO_ATLAS_APPENDIX.md` and is the
current output for all FDR-supported SUS - RES atlas terms.

This is a candidate companion to main Figure 3 and Extended Data Figure 6. It
does not change their panel contracts, identities, source data or assembled
figures. It is not a numbered publication figure or a submission export.

## Proposed placement

- Keep Figure 3's three compartment examples and Extended Data 6's three
  contrast atlases in place.
- Put the four remaining exact-term curve/protein pairs on two supplementary
  pages, ordered by the manuscript GO registry: translation, chromatin
  organization, neuron projection development, and autophagy. A four-curve
  contact sheet is also emitted for comparison.
- Put the same four terms across all 18 spatial units and all three contrasts
  on a third supplementary page. The spatial order is fixed: ten neuropil
  region-layer units, four neuronal-soma regions, four microglia-enriched ROIs.
- Use the accompanying source tables for exact values and leading-edge gene
  lookup. No additional protein-level significance is implied.

## Selection and scientific scope

The fixed exact GO identifiers are GO:0006412, GO:0006325, GO:0031175, and
GO:0006914. They directly name the four previously unillustrated families;
GO:0006914 is a child of the registry's autophagy core anchor GO:0061919.
Within each exact term, the displayed SUS-RES spatial unit is the one with the
smallest stored, FDR-supported BH-adjusted p-value; ties break by dataset and
spatial-unit identifier. This is a *result-dependent display choice*, disclosed
in `selection.csv`, and is not an independent replication or a test of regional
specificity. All other spatial units remain visible on the regional page.

The curves reconstruct the running ES from the original collapsed ranked-gene
input and `org.Mm.eg.db` GO membership. Export stops unless the reconstructed
set size and ES match the stored canonical GSEA result and its NES/FDR match the
frozen pathway inventory. No enrichment test, model fit or FDR adjustment is
performed here. The three stress-group contrasts are related comparisons.
Term-level FDR dots do not mean the broader seven-family atlas has a theme-level
FDR. The microglia-enriched ROI is not a purified cell population.
The candidate calls `f9_gsea_curve_plot()` in
`R/panels/final_truth_v9_panels.R`, the drawing function the earlier a-i Figure 3
used for panels d-f through `f9_gsea_curve()` (the current curves, 3h-j, are
drawn by `f3a_curve()`). This shares that design's header, running ES, hit
ticks, three-contrast strip, palette, typography, and recorded NES colour
limit, which is now the manuscript's fixed NES limit (`config/manuscript_palette.yml`
`diverging_limits`, palette v3.2) shared with Figure 3 (Extended Data 6 follows
once it is re-rendered) rather than the frozen a-i Figure 3d value. It also calls `f9_protein_zoom_plot()`, the same drawing function
used by Figure 3 k-m through `f3a_proteins()`. The seven genes per term follow Figure 3's
leading-edge, absolute-ranked-statistic selection rule. Their log2 fold
changes and BH FDRs come from stored mapped protein-level DA files. None of
the 84 displayed protein/contrast rows has BH FDR below 0.05; the protein
panels are descriptive. Their four-panel x-axis limit is set from all four
appendix selections, because their values exceed Figure 3 k-m's three-panel
limit. The figure legend and numeric axes should be kept with both pages.

## Files and commands

The pRoteomics-side, downstream-only source exporters are
`tools/export_supplementary_go_pathways_candidate.R` and
`tools/export_supplementary_go_protein_zooms_candidate.R`. Their six CSV files
are under the two matching `exports/publication_source_data/` candidate
directories. They were imported byte-for-byte into this repository under the
matching `source_data/pRoteomics/` directories and entered in the publication
source-data manifest. The scientific source checkout commit recorded there is
the base commit; these candidate exporters remain uncommitted local changes
until explicitly reviewed and committed.

From the manuscript repository root, render with:

```powershell
Rscript --vanilla figures/figure_03_supplementary_go.R
```

By default the renderer writes only to the ignored candidate namespace
`results/figures/manuscript_candidates/supplementary_go_pathways_appendix_v2/`
(palette v3.2 with the darkened protein-contrast colours; the unsuffixed
`supplementary_go_pathways_appendix/` is the first palette v3.2 render).
Pass a new output directory as its sole argument for a separate review copy.
It refuses an existing output directory. Outputs are four individual curve
SVGs, four individual protein SVGs, two paired pages, a review contact sheet
of the four curves, the regional matrix, source-data sidecars, vector PDFs,
PNG previews, `display_selection.csv`, `regional_source_data.csv`, and
`selected_term_leading_edges.csv`. The last table is filtered from the already
frozen `supplementary_selection_inventories/leading_edge_protein_inventory.csv`.

To save a review copy below the scientific checkout's `results/figures`, run
from this manuscript repository root (using a fresh directory name):

```powershell
$parent = 'S:\Lab_Member\Tobi\Experiments\Exp9_Social-Stress\Analysis\proteomics\results\figures\manuscript_candidates'
$newOutput = Join-Path $parent ('supplementary_go_pathways_appendix_' + (Get-Date -Format yyyyMMdd_HHmmss))
Rscript --vanilla figures/figure_03_supplementary_go.R $newOutput
```

The validated review copy from this run is
`S:\Lab_Member\Tobi\Experiments\Exp9_Social-Stress\Analysis\proteomics\results\figures\manuscript_candidates\supplementary_go_pathways_appendix_v8`.
This is still a candidate namespace. It does not replace the canonical
`results/figures/manuscript/figure_03/panels` files.
