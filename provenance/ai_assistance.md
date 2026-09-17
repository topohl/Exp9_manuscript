# AI assistance record

This file records where AI assistance was used in producing the contents of
this repository, so that the record exists in one place and can be summarised
for a journal disclosure statement.

It is a provenance record, not a claim of authorship. All scientific claims,
interpretations and final wording are the responsibility of the human authors.

## Repository creation (2026-09-17)

| Item | Detail |
| --- | --- |
| Tool | Claude Code (Claude Opus 5) |
| Task | Phase 6B/6C repository architecture migration |
| Scope | Structural extraction of the manuscript publication layer out of `topohl/pRoteomics` into this repository |

What the assistant did:

- classified every tracked object in the source repository into one
  architectural role and assigned it a destination repository;
- copied the manuscript layer here byte-for-byte and verified every file by
  SHA-256 after copying;
- built the frozen source-data bundles and their manifests;
- wrote the bundle verifier and the publication integrity suite in this
  repository;
- wrote `README.md` and this file.

What the assistant did **not** do:

- it did not write, edit, rephrase or restructure any manuscript prose;
- it did not change any figure, panel, legend, title or abstract;
- it did not compute, recompute or alter any statistic or scientific value;
- it did not select candidates or reinterpret any result.

The extraction was verified against the pre-restructure scientific freeze
(`pre-restructure-scientific-freeze-2026-09`,
commit `6801edbce8a5d222f4af46e06b6db4e99f6a9761`) by
`tools/verify_restructure_equivalence.R` in the source repository: all 239
frozen manifest assertions over 200 distinct objects were accounted for, with
zero missing, zero duplicated and zero unexplained hash changes.

## Manuscript prose

Prose-level AI assistance, if any, is recorded per statement in
`provenance/claims/` alongside the statement provenance and red-team review
tables that were carried over from the source repository. **This section is a
placeholder for the authors to complete**: the assistant that created this
repository did not write any prose and therefore cannot attest to how earlier
drafts were produced.

## Disclosure

Any journal-facing AI disclosure statement should be written from this file plus
the per-statement records in `provenance/claims/`, and should be reviewed by the
corresponding author before submission.
