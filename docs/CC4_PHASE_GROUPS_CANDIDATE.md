# CC4 whole-phase behavioural candidate

Status: exploratory candidate, not assigned to a canonical publication figure.
Analysis owner: BehavioralDynamics/MMMSociability Stage 31b. This repository
only imports verified frozen tables and renders them. No candidate time scan
or inferred grid timestamp is an input.

Frozen source: `source_data/MMMSociability_candidates/cc4_phase_groups_20260929_v2/`, with
its own `manifest.csv`. This is separate from the existing Figure 1 bridge.
`tools/import_cc4_phase_groups.R` refuses overwrites and verifies all copied
bytes. The renderer re-verifies the complete local bundle before plotting.

Entry point: `figures/cc4_phase_groups_candidate.R`.
Panel library: `R/panels/cc4_phase_groups_candidate.R`.
Output: a new directory under `results/figures/manuscript_candidates/`, with
three figures in PNG/PDF/SVG and render input/output hashes.

The candidate uses the manuscript group palette, `config/manuscript_palette.yml`
(v3, 2026-10-06): CON slate blue #6B7296, RES warm grey #BFBCB4, SUS red
#C74C56. Every figure family reads it, and the upstream publication theme
(MMMSociability `Functions/manuscript_palette.R`) pins the same values. Until
v3 this candidate used the older behavioural trio of `R/vendor/plotting_nature.R`
(#3E3C6F / #C6C3BB / #E63A48) because the palette file then held conflicting
values; the vendored file stays byte-identical and is no longer read for group
colour. Shapes and line styles redundantly identify groups, and the light RES
symbols have dark edges.

1. `cc4_group_activity`: I2-I5 and A1-A5 absolute activity. Small symbols are
   batch estimates; large symbols are equal-batch means. Absolute panels show
   batch dispersion rather than unconstrained t limits that could be negative
   for a nonnegative activity measure. All intervals remain in the frozen table.
2. `cc4_group_changes`: I3-I5 minus I2 and A3-A5 minus A2. Small symbols are
   paired batch changes, large symbols their mean, bars pointwise 95% t intervals.
3. `cc4_group_differences`: RES-CON, SUS-CON and SUS-RES differences in those
   changes, paired within batch. Contrast marks use neutral charcoal.

Female and male estimates are separate, retaining all three batches per sex.
Scales are shared between sexes within each phase family. Inactive and active
rows use separate ranges. No significance stars, exposure ribbons or causal
grid-effect labels are used.

## Candidate legend

RFID position changes during complete inactive and active phases of CC4,
stratified by canonical CON, RES and SUS outcome groups. Analysis includes the
same 107 animals in 29 cages across I2-I5 and A1-A5: females CON 12, RES 28,
SUS 18; males CON 12, RES 22, SUS 15. B5 sys.1 is excluded because its recording
ends during I3. Rates are per animal-hour. Animal values are averaged within
cage/group, cages equally within batch/group, and batches equally within sex.
Intervals for changes and group contrasts are descriptive pointwise 95%
Student t intervals across three paired batch estimates per sex (df=2),
assuming independent, approximately normal batch estimates; they are not
multiplicity adjusted. There is one control cage per batch. Outcome groups are
not randomized, and sucrose preference contributes to classification.
Exact grid insertion/removal times were not recorded. I3-I5 denote the full
inactive phases containing grid sessions, and A3-A5 the subsequent active
phases. The figures cannot distinguish a timed grid effect from other changes
associated with the protocol or time since cage change.

## Reproduction (PowerShell, manuscript repository root)

```powershell
Rscript --vanilla 'tools/import_cc4_phase_groups.R' '<new Stage31b output>' 'cc4_phase_groups_<new version>'
Rscript --vanilla 'figures/cc4_phase_groups_candidate.R' 'cc4_phase_groups_<new version>' '<new candidate output directory>'
Rscript --vanilla -e "testthat::test_file('tests/testthat/test-cc4-phase-groups-candidate.R')"
```

Never replace the existing import or render directory. Upstream changes require
a new versioned scientific run and frozen import; styling changes require a
new candidate render directory but can reuse the same scientific bundle.
