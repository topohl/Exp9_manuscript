# Manuscript palette

`config/manuscript_palette.yml` is the single colour source of every figure
(`palette_version: manuscript_palette_v3.2`).

- Groups: CON `#6B7296` (slate blue), RES `#BFBCB4` (warm grey), SUS `#C74C56`
  (red). RES is light and nearly grey, so group is never shown by colour alone:
  shape, line type or position carry it too.
- Diverging (signed effects): low `#6679D9`, white at zero, high `#F2CA4E`.
- Fixed colour limits by measure (`diverging_limits`): standardized mean
  difference 1, correlation 0.6, NES 2, z 2, set-mean z 1.

## How renderers use it

- Signed fills go through `nv_diverging(measure = ...)`
  (`R/panels/nature_v2_figure_utils.R`): the measure's limit, values beyond it at
  full colour, colour-bar ends reading ≤ / ≥. The sidecar of each panel records
  the limit drawn (`colour_limit`, `shared_NES_scale_limit`,
  `shared_NES_strip_limit`), and the source data keep every value uncapped.
- A panel with limits of its own (no measure) keeps ggplot's censor, so a value
  outside them is drawn in the missing-value colour rather than silently capped.
- Diverging tiles are outlined in grey85 (`nv_tile_border()`), so a tile at zero
  does not vanish into the page; signed axes use a typographic minus
  (`nv_minus_labels()`).
- MMMSociability pins this file in `Functions/manuscript_palette.R` (version,
  commit, git blob and sha256). Update the pin whenever the file changes.

## Checks

- `tests/testthat/test-manuscript-palette.R`: the contract, the limits and their
  labels, and every canonical renderer (including those in older modules,
  function by function) free of repeated or replaced colour literals.
- `figures/final_truth_v9_heatmap_scale_audit.R` and
  `figures/final_truth_v9_claim_audit.R`: no heatmap clips a value without
  disclosure, and a quantity drawn in more than one panel is drawn on one scale.
- `tools/palette_cvd_contact_sheet.R` writes
  `results/reports/manuscript_palette/cvd_<palette_version>/`: every canonical
  figure as drawn and under deuteranopia, protanopia, tritanopia and greyscale,
  the palette swatches in the same views, and the CIEDE2000 distance of every
  pair that must stay apart.

## What the colour-vision check shows (v3.2)

- The group colours stay apart under all three colour-vision simulations
  (CIEDE2000 23 or more), but CON and SUS have almost the same lightness: in
  greyscale they are the same grey (0.8). A greyscale print therefore relies on
  shape, line type or position for group, as RES already does in colour.
- The yellow arm of the diverging scale is the weaker one (28 from white, against
  42 for the blue arm) and is close to white in greyscale (11).
- Two evidence and claimability classes, not evaluable (`#D8D6D0`) and not
  audited (`#E6E4DF`), are 3 apart in every view and read as one colour.
