# Conventional tests and physiology: Extended Data 10 to 12

Three pages, promoted as the identities `extended_data_10` to `_12` (the journal numbering is a separate decision):

- **Extended Data 10**: conventional behaviour tests (main readouts per sex; d of the main readouts and of every measure with a significant comparison) and the exploratory locomotion-by-sex result.
- **Extended Data 11**: physiology (main readouts; d of the main readouts and of every measure with a significant comparison) and the spread of CombZ after stress.
- **Extended Data 12**: the analysed automated (SLEAP) values against manual (BORIS) scoring of the 20 batch 1 males, from `Analysis/Behavior/Validation` (SISanalyzer `behaviour_tests/rebuild_validation.R`). The agreement table of the SLEAPanalyzer release describes an earlier detector and is not used; that folder's README says why.

Panels carry short labels only; the sample sizes and which readouts are CombZ components are in the legends, the full measure names in the legends and the per-panel source data.

Legends: `manuscript/legends/extended_data_10_legend.md` to `_12`. Cited in the suggested Results 1 wording (`manuscript/text/SUGGESTED_UPDATES_2026-10-15.md`): ED10a,b and ED11 from the paragraph on the heterogeneous response, ED10c,d from the exploratory locomotion paragraph. ED12 is for the text's "Supp. Fig. x" (the BORIS comparison).

## Where the numbers come from

Scientific ownership: SISanalyzer. The exporter `manuscript_bundle/export_bundle.R` writes the bundle to `Analysis/SIS_Analysis/bundles/<id>/`. It contains per-animal values, batch-adjusted group means, the contrasts as stored in the result workbooks, and the heterogeneity, locomotion and validation tables. The exporter refuses to write unless its recomputed contrasts reproduce the stored ones and the automated validation values equal the analysed ones. This repository imports the bundle unchanged and draws it; no statistic is computed here.

## Commands (from the repository root)

```sh
Rscript tools/import_conventional_bundle.R <Analysis>/SIS_Analysis/bundles <bundle_id>   # copy, pin, record hashes
Rscript tools/verify_source_bundles.R                                                    # includes the pinned bundle
Rscript figures/conventional_tests_candidate.R [new_output_directory]                    # preview; default results/figures/manuscript_candidates/conventional_tests_v1
Rscript figures/conventional_tests_candidate.R --publication                             # at a clean HEAD: the render that is promoted
Rscript tools/promote_manuscript_render.R extended_data_10 extended_data_11 extended_data_12
Rscript -e "testthat::test_file('tests/testthat/test-conventional-tests-candidate.R')"
```

The renderer refuses an existing output directory and writes `render_inputs.csv` and `render_manifest.csv` beside the figures.

## Why the heatmaps are a selection

The palette caps a page at 183 x 170 mm. A heatmap of all 87 behaviour measures needs about 145 mm at the minimum type size, so both heatmaps show the main readouts and every measure with a significant comparison (the same rule for physiology, so the two pages read alike). The full heatmaps belong in the Supplement: `Analysis/Behavior/All_tests/figures/heatmap` and the physiology results in `Analysis/Physiology`.

## Promotion

A new bundle means: import and pin it, set `sisanalyzer_bundle_id` of `ED_10` to `ED_12` in `figures/figure_contract.yml`, update the legend numbers, commit, render with `--publication` at that commit and promote. The promotion copies the pages and the per-panel source data to `figures/extended_data/` and records the commit and hashes in the registry. The tests check that the contract, the legends and the pin name the same bundle.
