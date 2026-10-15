# Add a candidate

A candidate is an analysis result that has been imported and rendered here but is not part of a canonical figure. Candidates change nothing that is promoted. The pattern below is what the existing candidates (the Stage 30 behaviour set `behaviour_v101_s30`, the CC4 sets, the WGCNA theme set) follow. Read one of them first: `docs/CC4_STATISTICS_CANDIDATE.md` is the shortest.

## The rules that always apply

- The statistics are computed upstream, in the repository that owns the analysis. Nothing here fits a model, runs a test or adjusts a p value. The renderers, `R/` and `figures/` are checked for this by `tests/testthat/test-no-analysis-in-manuscript.R`.
- An import is frozen and hashed. The importer refuses an existing destination and records the copy in a config file under `config/` and a manifest under `provenance/source_manifests/`.
- A renderer reads only from `source_data/`, never from a live results folder, and writes to a new directory under the gitignored `results/` workspace.
- Nothing becomes canonical by being rendered. Promotion is a separate, guarded step (`tools/promote_manuscript_render.R`) that needs a clean tree and a registry row.
- Results, legends and text that go with a candidate sit in `manuscript/candidates/<generation>/`, and a receipt of every file written sits in `provenance/candidates/<generation>_receipt.csv`.

## Steps

1. **Freeze upstream.** The producing repository writes a versioned, hash-frozen bundle with a manifest (for the 2026-10-15 result folders this step does not exist yet: they are plain output folders and need a bundle exporter in SISanalyzer first).
2. **Import by hand** with a script in `tools/` named `import_<thing>.R`, taking the upstream output folder and a bundle id (`<prefix>_v<version>_<date>_<commit>`). Copy `tools/import_stage30_bundle.R` as the template: it refuses unregistered or modified bundles and bundles built from a different pinned parent.
3. **Record the import** in `config/<thing>.yml` and in `provenance/source_manifests/<thing>_manifest.csv`; run `Rscript tools/verify_source_bundles.R`.
4. **Render** with `figures/<thing>_candidate.R <bundle id> <new output directory>`, using the palette in `config/manuscript_palette.yml` (`docs/MANUSCRIPT_PALETTE.md` explains it).
5. **Test** with `tests/testthat/test-<thing>-candidate.R`, modelled on `test-behaviour-v101-s30-candidates.R`: the imported manifest verifies and the receipt lists exactly the files written.
6. **Describe** it in `docs/<THING>_CANDIDATE.md`, with the import and render commands.

Run the candidate's own test file (the command is in the CC4 document) rather than the whole suite; the suite reads the gitignored `results/` workspace.

## Before a candidate goes into the text

Add its numbers to the claims table (`provenance/claims/`), state the figure panel each number belongs to, and check the sentence against the source value, not against an earlier draft. The older provenance tables are keyed to `manuscript/manuscript_draft.md`, which is no longer the master text (see `WHERE_THINGS_LIVE.md`).
