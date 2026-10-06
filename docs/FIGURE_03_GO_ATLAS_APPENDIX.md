# Figure 3 GO atlas appendix

This appendix extends the paired curve/protein presentation of Figure 3 (the
earlier a–i panels d–i; now 3h–m) to every GO term
with BH FDR < 0.05 in the **SUS - RES** contrast and at least one of the seven
claim-eligible Figure 3b atlas themes. It is a display of existing GSEA and
mapped protein results, with no new enrichment or differential analysis.

The theme order follows the Figure 3b registry: RNA processing, translation,
chromatin, mitochondrial respiration, synaptic signalling, neuron projection,
and autophagy. A term assigned to two themes appears in both theme folders;
the global index identifies each distinct GO ID and theme membership. Each
term uses the spatial unit with the smallest stored SUS - RES BH FDR; ties are
resolved by dataset and spatial-unit name. The three-contrast NES strip of
the earlier a–i Figure 3 curves and the leading-edge protein rule of Figure
3k–m are then shown for that unit. Up to seven leading-edge genes are selected by absolute ranked statistic.
Protein BH FDR < 0.05 is marked by a black ring.
No displayed leading-edge protein meets that threshold in this frozen export.

The full atlas has a wider NES range than the three main-figure exemplars.
The appendix uses the manuscript's fixed NES colour scale
(`config/manuscript_palette.yml` `diverging_limits`, palette v3.2: full colour
at |NES| 2), the one Figure 3 uses (Extended Data 6 follows once it is
re-rendered), so colour intensity compares directly with Figure 3e–g. A term
beyond the limit takes full colour;
every strip prints its NES, and the limit and the uncapped values are stored in
each panel's source data. Protein log2 fold-change axes use one symmetric limit
per theme and are recorded in the protein source data.

The renderer is `figures/figure_03_go_atlas_appendix.R`, adjacent to the
four-term exploratory renderer. It calls `f9_gsea_curve_plot()` and
`f9_protein_zoom_plot()` from `R/panels/final_truth_v9_panels.R`. The
protein function is also used by the current Figure 3k–m panels; the current
curves (3h–j) are drawn by `f3a_curve()` in
`R/panels/figure3_adaptation_panels.R`, so the appendix curves keep the
earlier a–i curve design.

The source exporter is
`pRoteomics/tools/export_figure_03_go_atlas_appendix.R`. It verifies each
reconstructed curve against the stored GSEA enrichment score, NES, and FDR.
The manuscript import is frozen at
`source_data/pRoteomics/figure_03_go_atlas_appendix/` and recorded in
`source_data/pRoteomics/manifest.csv`.
The manifest's source commit is the pRoteomics checkout base revision;
the new exporter and renderer are uncommitted working-tree changes in this
candidate and are not recoverable from that revision alone.

The output root is
`results/figures/manuscript_candidates/figure_03_go_atlas_appendix_v6/` (palette
v3.2, with the protein contrasts in darkened RES / SUS / grey20 and a typographic
minus on signed axes; `_v5` is the first palette v3.2 render and `_v4` the
earlier render on a data-maximum NES range of 3.4).
Its global `theme_term_index.csv` lists every displayed membership.
Each `theme_XX_...` folder has:

- `figures/`: a multipage PDF with one paired curve/protein column per
  term, plus editable SVG files for individual terms;
- `data/`: a theme index, regional exact-term inventory, and per-term
  running-curve, protein, and panel source CSVs.

The regional inventory contains the stored term–unit–contrast occurrences.
Two possible cells for GO:0006396 are absent from that inventory and are not
backfilled: CA2 SLM neuropil, SUS - CON; and CA1 SP soma, SUS - RES.

Run from the manuscript repository root:

```powershell
Rscript figures/figure_03_go_atlas_appendix.R
```

The output directory must not exist, so reruns require a new candidate name
or an explicit output-directory argument. This appendix is a candidate
manuscript supplement; it does not replace the approved Figure 3 panels.
