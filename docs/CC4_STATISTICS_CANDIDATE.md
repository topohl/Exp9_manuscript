# CC4 count-model figure candidate

Scientific ownership: MMMSociability Stage31c. This repository imports and renders
frozen estimates only; it does not fit models, choose phases, calculate confidence
intervals, or adjust P values. Existing descriptive CON/RES/SUS figures remain
separate and retain their blue-purple, grey and red palette.

The new forest plot reports four primary CON-versus-SIS comparisons: females and
males, each in inactive and active phases. Its charcoal symbols represent a
condition contrast, not a phenotype. Each row shows the ratio of rate ratios,
pointwise95% bootstrap interval and upstream Holm-adjusted P value. All conditions
received grid sessions. SIS pools stressed animals; it is not synonymous with SUS.
Whole-phase changes do not isolate the unrecorded two-hour insertion interval.

The renderer requires a verified source manifest and sufficient successful
bootstrap evidence for every row labelled PASS. Withheld rows receive text and
no interval or point. Validation-only runs cannot be rendered as inference.

Import and render from this repository using PowerShell:

```powershell
Rscript --vanilla 'tools/import_cc4_phase_statistics.R' '<Stage31c output>' 'cc4_phase_statistics_20260929_v1'
Rscript --vanilla 'figures/cc4_phase_statistics_candidate.R' 'cc4_phase_statistics_20260929_v1' '<new candidate directory>'
Rscript --vanilla -e "testthat::test_file('tests/testthat/test-cc4-phase-statistics-candidate.R', reporter='summary', stop_on_failure=TRUE)"
```

Both import and rendering refuse existing destinations. Import preserves the
complete upstream manifest, including saved models and simulation checkpoints.
Exports:183x85mm PDF with embedded Arial, SVG, and300dpi PNG. The render manifest
and input hashes record the exact scientific bundle and presentation sources.
This is a candidate; canonical manuscript figures and publication registries
are not promoted or changed.
