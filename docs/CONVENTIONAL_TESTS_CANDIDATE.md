# Conventional tests and physiology: Extended Data candidates

Two pages, both candidates, nothing promoted:

- **Extended Data 10**: conventional behaviour tests (main readouts per sex; d of the main readouts and of every measure with a significant comparison) and the exploratory locomotion-by-sex result.
- **Extended Data 11**: physiology (main readouts marked as CombZ components; d of all 19 measures) and the spread of CombZ after stress.

Legends: `manuscript/candidates/conventional_tests_v1/legends.md`. Cited from Results 1, the sentence on the heterogeneous response.

## Where the numbers come from

Scientific ownership: SISanalyzer. Its exporter `manuscript_bundle/export_bundle.R` writes the bundle to `Analysis/SIS_Analysis/bundles/<id>/`. It contains per-animal values, batch-adjusted group means, the contrasts as stored in the result workbooks, and the heterogeneity and locomotion tables. The exporter refuses to write unless its recomputed contrasts reproduce the stored ones. This repository imports the bundle unchanged and draws it; no statistic is computed here.

## Commands (from the repository root)

```sh
Rscript tools/import_conventional_bundle.R <Analysis>/SIS_Analysis/bundles <bundle_id>   # copy, pin, record hashes
Rscript tools/verify_source_bundles.R                                                    # includes the pinned bundle
Rscript figures/conventional_tests_candidate.R [new_output_directory]                    # default results/figures/manuscript_candidates/conventional_tests_v1
Rscript -e "testthat::test_file('tests/testthat/test-conventional-tests-candidate.R')"
```

The renderer refuses an existing output directory and writes `render_inputs.csv` and `render_manifest.csv` beside the figures.

## Why the behaviour heatmap is a selection

The palette caps a page at 183 x 170 mm. A heatmap of all 87 measures needs about 145 mm at the minimum type size, so the candidate shows the main readouts and every measure with a significant comparison. The full heatmap belongs in the Supplement: `Analysis/Behavior/All_tests/figures/heatmap`.

## Promotion (not done)

Promotion needs: the identities `extended_data_10` and `extended_data_11` in the registry (the registry tests pin 10 canonical and 2 withheld identities); a promotion route in `tools/promote_manuscript_render.R` for an SISanalyzer-owned figure drawn from this bundle; the legends in `manuscript/legends/`. The packaging tool names Extended Data by identity, so the final journal numbering is a separate decision.
