# CC4 Nature-format candidate: presentation only

This candidate renders the unchanged `cc4_phase_groups_20260929_v2` frozen
scientific bundle. It does not fit the proposed formal model or add p-values.
The user confirmed on 29 September 2026 that CON cages also received grid
sessions. CON therefore is not an unexposed grid control.

## Source and output

- Renderer: `figures/cc4_phase_groups_nature_candidate.R`.
- Panel library: `R/panels/cc4_phase_groups_nature.R`.
- Test: `tests/testthat/test-cc4-nature-candidate.R`.
- Source bundle: `source_data/MMMSociability/cc4_phase_groups_20260929_v2/`.
- Reviewed output: `results/figures/manuscript_candidates/cc4_phase_groups_nature_20260929_v2/`.

All three figures are exported as editable vector PDF/SVG and 300 dpi preview
PNG. Activity and change figures are 183 x 120 mm; the contrast figure is
183 x 145 mm. Cairo rounds PDF dimensions to whole points. Arial text is
5.5-7 pt at final size, including bold lower-case panel letters. The established
CON/RES/SUS colours, shapes and line styles are preserved. Long descriptions
are carried by the legends below. Scales match between sexes within each phase
family. These choices follow the [Nature research figure guide](https://research-figure-guide.nature.com/figures/building-and-exporting-figure-panels/)
and its recommendations for sizing, editable text, accessible keys and compact
panel arrangement. This is formatting guidance, not a claim of journal approval.

## Legend: activity

**Whole-phase RFID activity during CC4.** Inactive-phase activity in females
(a) and males (b), and active-phase activity in females (c) and males (d),
stratified by CON, RES and SUS outcome group. Large symbols show equal-batch
means; small symbols show the three batch estimates per sex. Rates are recorded
RFID position changes per animal-hour. Within each batch, cage/group means
receive equal weight. The same 107 animals in 29 complete cages are included
throughout: females CON 12, RES 28, SUS 18; males CON 12, RES 22, SUS 15.
Inactive phases span 06:30-18:30 and active phases 18:30-06:30 on the logger
clock. I2 and A2 are the respective references; A1 provides earlier context.
I3-I5 contain the grid sessions and A3-A5 follow those session days. CON cages
also received grid sessions. Exact insertion/removal times were not recorded;
the displayed periods do not represent continuous grid exposure. B5 sys.1 is
excluded because its recording ends during I3. These are descriptive
associations with protocol day, not a causal grid-effect estimate.

## Legend: paired changes

**Whole-phase changes from the reference day or night.** Changes in inactive
activity from I2 in females (a) and males (b), and changes in active activity
from A2 in females (c) and males (d). Small symbols show changes paired within
batch; large symbols show the equal-batch mean. Error bars are descriptive,
pointwise 95% Student t intervals across three independent batch estimates per
sex (df=2), assuming approximately normal batch estimates. They are not
multiplicity adjusted. Population, clock windows and group definitions are
identical to the activity figure. These intervals are an existing descriptive
summary, not results of the proposed hierarchical count model.

## Legend: group contrasts

**Outcome-group differences in whole-phase change.** Each point is a difference
between group changes, paired within batch before averaging across batches.
The first-named group minus the second-named group defines the sign. Panels
show female inactive (a), male inactive (b), female active (c), and male active
(d) comparisons. Error bars are pointwise 95% t intervals with three batches
per sex; all 36 displayed intervals include zero. No multiplicity-adjusted
p-values or equivalence claims are made. RES/SUS classification is outcome
defined and includes sucrose preference, so these are exploratory phenotype
associations.

## Reproduce into a new directory

```powershell
Rscript --vanilla figures/cc4_phase_groups_nature_candidate.R 'cc4_phase_groups_20260929_v2' '<new candidate directory>'
Rscript --vanilla -e "testthat::test_file('tests/testthat/test-cc4-nature-candidate.R', reporter='summary', stop_on_failure=TRUE)"
```

All rendering is downstream of the frozen bundle. Formal modelling belongs
upstream in BehavioralDynamics and requires a new scientific output and frozen
import; the existing source bundle and its descriptive intervals stay intact.
