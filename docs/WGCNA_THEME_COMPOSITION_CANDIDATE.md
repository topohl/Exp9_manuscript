# WGCNA module wheel and theme composition: candidate

Candidate only. It does not change any panel contract, publication identity,
source-data release or assembled figure, and it is not promoted. The withheld WGCNA Extended Data
stays withheld in `provenance/publication_registry/canonical_publication_registry.csv`
(`WITHHELD_SCOPE_AND_UNSUPPORTED_SPATIAL_LABELS`); this candidate is a proposal
for how a re-scoped WGCNA Extended Data could look, not a replacement of that record.

## Question

Do the WGCNA co-abundance modules and supermodules of the three compartments
group proteins the way the curated GSEA themes (registry
`manuscript_go_themes_v3`, the seven Figure 3 themes) do? Most module group
effects are not FDR-supported, so the candidate uses the modules only as a
phenotype-blind structure: it asks whether a theme's proteins sit together in a
module, never whether any group differs. No group effect is drawn.

## Panels

- **a, module wheel.** Neuropil, soma and the microglia-enriched ROI in one
  wheel; supermodule arcs (compartment colour, lighter for a supermodule of one
  module) with their modules nested inside. Seven thin rings, one per theme in
  registry order from the outside in, are coloured with the Figure 3 theme colour
  where the module carries that theme. Inside the module name, in grey, the
  module's peak region; the two innermost rings are bilateral reproducibility
  and tau (concentration of the module's abundance across the spatial units).
  The structure rings are descriptive.
- **b, composition matrix.** Every module and every multi-module supermodule
  against the seven themes. Dot area: share of the theme's proteins in the
  module; fill: fold enrichment (white at or below 1x, full colour at or above
  8x); black outline: carried.
- **c, theme coherence.** Per theme and compartment, the class, the carrier
  modules, and the share of the theme's proteins they hold (fill). Five pips
  per tile say whether the top carrier is also carried in each sensitivity
  variant (filled: yes; open: no), in the order of the key.

## What "carried" means

All values come from the pRoteomics step
`analysis/integration/test_theme_module_composition.R`, exported by
`tools/export_wgcna_theme_composition_candidate.R` and imported byte for byte by
`tools/import_wgcna_theme_composition.R` (versions below). Nothing is
recomputed here. The numbers are those of v3 (the same as v2).

- Theme protein set: proteins annotated (`org.Mm.eg.db` 3.22.0, GOALL) to the
  theme's claim-eligible GO terms tested in that compartment, mapped through the
  compartment's GSEA-ranked genes.
- Background: proteins in WGCNA, in the GSEA ranking and annotated to a tested
  GO term (neuropil 4,681; soma 5,125; microglia-enriched ROI 4,849).
- Test: one-sided Fisher's exact test per theme x module (and per theme x
  multi-module supermodule), conditional-MLE odds ratio with exact 95% CI;
  Benjamini-Hochberg within compartment x level (105, 49 and 91 module cells;
  28, 21 and 28 supermodule cells).
- Carried: BH-FDR <= 0.05 and odds ratio >= 2. Modules: 9 neuropil, 6 soma,
  10 microglia cells; supermodules: 2, 2 and 4.
- Classes (share of the theme's proteins in its carrier modules): coherent >=
  50%, core 20-50%, minor < 20%, no carrier. Over the 21 theme x compartment
  tiles: 5 coherent, 8 core, 5 minor, 3 without a carrier.
- Sensitivities (pips): proteins of one theme only; FDR-supported leading edges
  (any contrast; SUS-RES only); all WGCNA proteins as the background; core
  members (|kME| >= 0.6) only.

Partition agreement between the theme labels and the module labels of the
single-theme proteins, against 1,000 label permutations (seed 20261012):
Cramer's V 0.27-0.38 (permutation maximum 0.11) and NMI 0.12-0.15
(permutation maximum 0.02), permutation p = 0.001 in every compartment and
at both levels. The agreement is real and partial, not a one-to-one mapping.

## Versions

- **v3** (current): the v2 composition rerun at pRoteomics fb852b4 and exported there
  (run and export at the same commit, which holds the label code and registries);
  frozen in `source_data/pRoteomics/wgcna_theme_composition_candidate_v3/`, rendered to
  `results/figures/manuscript_candidates/wgcna_theme_composition_v3/`. Every table
  equals v2 except the wheel table's `canonical_display_label` (not drawn, so the
  sheet is byte-identical to v2's): 23 modules now show the rule-based labels
  (all 15 neuropil modules, soma m02 and m04-m07, microglia m02, m04 and m09),
  decided on 2026-10-30.
- **v2**: primary pRoteomics tree (original centring,
  `Analysis/proteomics_original`, branch `original-centring-2026-10-24`),
  supermodule cut heights of 2026-10-26 (0.55 neuropil and soma, 0.45
  microglia); frozen in
  `source_data/pRoteomics/wgcna_theme_composition_candidate_v2/`, rendered to
  `results/figures/manuscript_candidates/wgcna_theme_composition_v2/`. The
  composition run and export commits are in the bundle's `input_manifest.csv`
  and manifest rows.
- **v1**: run f45209c, exported at 363e6f5, imported 2026-10-12 (b099591);
  frozen in `source_data/pRoteomics/wgcna_theme_composition_candidate/`,
  rendered to `.../wgcna_theme_composition_v1/`. Kept unchanged.

Module-level results, the theme summary (panel c) and the carried-cell counts
are unchanged in v2. The supermodule statistics change. Soma m01 + m03 and
microglia m09 + m13 now form multi-module supermodules (both SM04; neither
carries a theme), and the later microglia supermodules are renumbered down by
one (SM05-SM09 become SM04-SM08; m07 + m11 is now SM08). The supermodule test
families grow from 14 to 21 (soma) and from 21 to 28 (microglia) cells, so
their BH-FDR values rise (16 of the 63 cells with the same member set change,
6 of them carried; none crosses 0.05, and no cell changes its carried status). Supermodule
partition agreement: soma Cramer's V 0.33359 to 0.37876, microglia 0.27197 to
0.27050. The curated-mapping audit keeps its counts, with microglia SM09 read
as SM08. The wheel table's module labels (column `canonical_display_label`,
not drawn) follow the primary tree's current labels: neuropil m07 and m09
(relabelling of 2026-10-26) and the reviewed soma m01 and m03 labels of
2026-10-27.

Provenance of v2: the composition run and the export are stamped with commit
24c972b, but the run used label code and registries that were not yet
committed (the Stage 07 soma overrides and `config/wgcna_labels/neuron_soma.csv`,
reviewed 2026-10-27; committed on 2026-10-30 in b773a55). They reach only the soma m01 and m03 cells of
`canonical_display_label`; 24c972b alone reproduces every other value. The
composition step was rerun after the export, with byte-identical tables, so the
run manifest that `input_manifest.csv` hashes is no longer the one in the
pRoteomics tree. The current one differs only in its timestamp and in the md5
of the theme mapping, whose row-44 rationale text was corrected after the
export. The hashed copy is kept beside the export, as
`exports/publication_source_data/wgcna_theme_composition_candidate/run_manifest_at_export.yml`.

## Reading

- Mitochondrial respiration is coherent in every compartment: one module holds
  77% (neuropil m02), 55% (soma m06) and 76% (microglia m01) of the theme's
  proteins.
- RNA processing and chromatin share a carrier in every compartment (neuropil
  m12, soma m02, microglia m11), so the modules do not separate these two themes.
- Translation, synaptic signalling, neuron projection development and autophagy
  are core, minor or without a carrier depending on the compartment.
- The curated theme-module mapping (`config/gsea_wgcna_theme_module_mapping.csv`
  in pRoteomics) agrees on 23 rows, is weak (odds ratio below 2) on 6, not
  enriched on 7, and misses 10 supported pairs. That audit is exported but not
  drawn.

## Draft legend

**Co-abundance modules carry part of the GSEA theme structure.** **a**, WGCNA
modules of the neuropil, soma and microglia-enriched ROI, nested in their
supermodules (outer arcs; light, one-module supermodules). Coloured cells in the
seven theme rings mark a module that carries the theme: its proteins are
over-represented among the theme's GO-annotated proteins (one-sided Fisher's
exact test, Benjamini-Hochberg FDR <= 0.05 within compartment and level, odds
ratio >= 2). Grey text, peak region; inner rings, bilateral reproducibility and
tau, both descriptive. **b**, Share of each theme's proteins in each module and
multi-module supermodule (dot area) and fold enrichment (fill); outlined dots
are carried. **c**, Share of each theme's proteins held by its carrier modules,
with the class (coherent >= 50%, core 20-50%, minor < 20%); pips, whether the top
carrier is also carried in five sensitivity analyses. The analysis is
phenotype-blind: it uses no group label and implies no group difference. The
microglia-enriched ROI is not a purified cell population.

## Open decisions (Tobias)

1. Placement: re-scoped WGCNA Extended Data, or a supplementary page beside
   Figure 3.
2. The peak-region text in **a** restates a descriptive module attribute. If it
   reads as the spatial labels that withheld the WGCNA Extended Data, drop that text
   band (the structure rings stay).
3. Whether to draw the curated-mapping audit, or leave it in the source tables.

## Files and commands

    # pRoteomics primary tree (statistics, export; the export refuses an existing export)
    Rscript analysis/integration/test_theme_module_composition.R
    Rscript tools/export_wgcna_theme_composition_candidate.R

    # this repository (each version imported once, then rendered into a new directory)
    Rscript tools/import_wgcna_theme_composition.R <pRoteomics_root> v3
    Rscript figures/candidates/wgcna_theme_composition_candidate.R [v1|v2|v3] [new_output_directory]

Default output: `results/figures/manuscript_candidates/wgcna_theme_composition_<version>/`
(current version v3) with the sheet (SVG, PDF, PNG), per-panel source data,
`render_inputs.csv` and `render_manifest.csv`. The importer refuses an existing
version directory or manifest entry and an export it has already imported; the
renderer refuses an existing directory. A later version keeps the pRoteomics
path in `source_table` and names its own bundle in `exported_file`.
Tests: `tests/testthat/test-wgcna-theme-composition-candidate.R` (every version).
