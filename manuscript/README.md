# manuscript/

Prose of the Exp9 manuscript.

## Which text is the master

The Word file `S:\Lab_Member\Tobi\Paper\E9SIS\manuscript_resiliencandsusceptibility.docx` is the master text. Nothing in this repository edits it.

`manuscript_draft.md` is an older markdown draft. It is **not** the master, but the claim tables in `provenance/claims/` (`results_statement_provenance.csv` and its siblings) are keyed to it and several tests read it (`test-figure1-behaviour-bridge.R`, `test-extended-data-behaviour.R`), so it stays unchanged as a frozen reference. Its Figure 1 numbers are the older bridge values.

## Folders

| Path | Contents |
|---|---|
| `text/` | Dated snapshots of the Word file, the text of each snapshot (one paragraph per line) and `SNAPSHOTS.csv` (date, path, SHA-256, size, time saved). Made with `tools/export_docx_text.R`. |
| `legends/` | Journal legends. The tests and the packaging tool enumerate this folder. |
| `candidates/` | Results text and legends that go with candidate figures (`behaviour_v101_s30`). |
| `citations/`, `front_matter/` | Citation needs, title options, abstract provenance, submission readiness. |

## Checking the numbers in the Word text

`provenance/claims/docx_claims.csv` has one row per claim of the Word text (snapshot of 2026-10-15; locations are lines of the snapshot's text file). Each row records the number in the text, the source and the status: `match`, `mismatch`, `partial`, `no_source`, `placeholder`, `orphan` or `no_text` (the source exists, the Word text is silent). Rows with `check = lookup` name a source table, a filter and a column.

```sh
Rscript tools/check_claims.R          # drift only
Rscript tools/check_claims.R --all    # every lookup row
```

The tool reads cells and compares them with the number in the text. It computes no statistics and writes nothing. Drift means a source changed after the register was written. If the Word file was saved after the newest snapshot, the tool says the register describes the snapshot, not the current text.

After the Word text changes, take a new snapshot (`Rscript tools/export_docx_text.R "<path to the .docx>"`), read the text again and update the register by hand. The line numbers of a new snapshot are not those of the old one.
