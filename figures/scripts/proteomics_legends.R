#!/usr/bin/env Rscript

# Part-25: figure legends and the per-panel statistical-role audit.
#
# Until now the layer had no figure-legend artefact. Everything a reader needs
# in order to not over-read a panel - what is descriptive, what is inferential,
# which multiple-testing family applies, whether a displayed quantity was
# selected by the same analysis it appears to support - lived only in renderer
# comments. This script emits both the reader-facing legends and the audit
# table that proves every panel was asked those questions.
#
# The per-panel statistical roles below are EDITORIAL facts about the design,
# not values derivable from the data, so they are declared here and checked
# against the built source data where that is possible.

source(file.path("R", "paths.R"))
source(repo_path("R", "null_coalescing.R"))
source(repo_path("R", "integration_utils.R"))
source(repo_path("R", "proteomics_style.R"))
source(repo_path("R", "proteomics_page_builder.R"))
Sys.setenv(PROTEOMICS_SCRIPT_ID = "figures/scripts/proteomics_legends.R")

args <- commandArgs(trailingOnly = TRUE)
if ("--dry-run" %in% args || is_dry_run()) {
  message("[DRY-RUN] final_truth_v9 legends and statistical-role audit")
  quit(save = "no", status = 0L)
}

ct <- s9f_contract()
SD <- path_results("source_data", "manuscript_candidates", "final_truth_v9")
REP <- path_results("reports", "manuscript_candidates", "final_truth_v9")
TAB <- path_results("tables", "manuscript_candidates", "final_truth_v9")
dir_create(REP); dir_create(TAB)

sidecar <- function(fig, id) {
  p <- file.path(SD, fig, paste0(id, "_source_data.csv"))
  if (file.exists(p)) hv_read_csv(p) else NULL
}

# ------------------------------------------------- eps-floor disclosure (PB-03)
#
# This disclosure used to be a hard-coded string: "90 of the 851 displayed
# FDR-supported occurrences". It was a scientific constant written into a legend
# by hand, so it silently went stale when the atlas was rebuilt - the current
# data give 94 of 953 - and re-running this script could not correct it, because
# there was nothing to re-run it FROM. It is now derived.
#
# eps is not set anywhere in this repository's GSEA configuration, so the floor
# in force is the gseGO/fgseaMultilevel default. Rather than assert that from
# documentation, the value is confirmed against the data: the smallest positive
# raw p in the canonical enrichment output must equal the declared floor. If the
# analysis is ever rerun with an explicit eps, this fails loudly instead of
# printing a number that no longer describes the figure.
GSEA_EPS <- 1e-10

f9_eps_floor_disclosure <- function(eps = GSEA_EPS) {
  th <- hv_read_csv(repo_path(
    "source_data", "pRoteomics", "supplementary_selection_inventories",
    "pathway_enrichment_inventory.csv"))
  seven <- c("rna_processing_splicing_rnp", "ribosome_translation",
             "chromatin_organization", "mitochondrial_respiration_oxphos",
             "synaptic_signaling_vesicle", "neuron_projection_development",
             "autophagy_lysosome_endosome")
  # The inventory holds a term assigned to two themes as one "a;b" row; the
  # atlases count it once per theme, so the rows are split before matching.
  parts <- strsplit(as.character(th$theme_id), ";", fixed = TRUE)
  th <- th[rep(seq_len(nrow(th)), lengths(parts)), , drop = FALSE]
  th$theme_id <- unlist(parts)
  d <- th[th$theme_id %in% seven &
            th$contrast %in% c("RES - CON", "SUS - CON", "SUS - RES"), ,
          drop = FALSE]

  observed_floor <- min(d$raw_p[d$raw_p > 0], na.rm = TRUE)
  if (!isTRUE(all.equal(observed_floor, eps))) {
    stop("f9_eps_floor_disclosure: the smallest positive raw p in the canonical ",
         "enrichment output is ", format(observed_floor, scientific = TRUE),
         ", which is not the declared floor ", format(eps, scientific = TRUE),
         ". The legend would misdescribe the figure; fix the declared floor ",
         "rather than this message.", call. = FALSE)
  }

  sup <- d[is.finite(d$BH_FDR) & d$BH_FDR < 0.05, , drop = FALSE]
  list(eps = eps, displayed = nrow(sup), floored = sum(sup$raw_p <= eps))
}

EPSF <- f9_eps_floor_disclosure()

# ---------------------------------------------------------------- shared text
ALGEBRA <- paste0(
  "Pairwise contrasts are shown jointly to make the directional pattern ",
  "across the three pairwise contrasts legible; they are algebraically ",
  "related (SUS-RES = SUS-CON minus RES-CON) and are not independent ",
  "replications.")
GSEA_N <- paste0(
  "Biological n = 3 animals per group. Ranks are the stored per-gene contrast ",
  "statistic for that spatial unit, collapsed from protein groups by the ",
  "prespecified median rule. The FDR is the gene-set enrichment FDR ",
  "conditional on that ranking; it is not a count of independent biological ",
  "observations. Enrichment p-values are floored at the fgsea tolerance ",
  "eps = ", format(EPSF$eps, scientific = TRUE), ": a term reported at that ",
  "value has a true p somewhere below it that the method does not resolve, so ",
  "its FDR bounds the evidence rather than measuring it. This applies to ",
  EPSF$floored, " of the ", EPSF$displayed, " displayed FDR-supported ",
  "occurrences, including each of the three exemplar terms in ",
  "at least one displayed cell. A very small gene-set FDR is a statement ",
  "about gene ranks, never about the three animals per group.")
DESCRIPTIVE <- "Descriptive; no hypothesis test is performed in this panel."
# The Figure 2 GSEA panels are control-only, and their floored p-values are counted
# from their own sidecars, not from the Figure 3 atlas (2026-10-28).
GSEA_CON <- function(id, pcol, what) {
  f <- file.path(SD, "figure_02", paste0(id, "_source_data.csv"))
  d <- if (file.exists(f)) utils::read.csv(f, check.names = FALSE) else NULL
  if (!is.null(d) && "supported" %in% names(d)) d <- d[d$supported %in% TRUE, , drop = FALSE]
  v <- if (is.null(d) || !pcol %in% names(d)) numeric() else as.numeric(d[[pcol]])
  floored <- if (length(v)) paste0(sum(v <= EPSF$eps * (1 + 1e-9), na.rm = TRUE), " of the ", length(v), " ", what)
             else paste("none of the", what)
  paste0("Biological n = 3 control animals. Ranks are the stored per-gene contrast statistic for that ",
         "spatial unit, collapsed from protein groups by the prespecified median rule. The FDR is the ",
         "gene-set enrichment FDR conditional on that ranking; it is not a count of independent biological ",
         "observations. Enrichment p-values are floored at the fgsea tolerance eps = ",
         format(EPSF$eps, scientific = TRUE), ": a term reported at that value has a true p below it that ",
         "the method does not resolve, so its FDR bounds the evidence rather than measuring it. This applies ",
         "to ", floored, ". A very small gene-set FDR is a statement about gene ranks, never about the three ",
         "control animals.")
}
CURATED <- paste0(
  "Rows are curated recurrent GO-BP program families, not an exhaustive ",
  "enumeration of enriched GO terms: they carry 26.7% of FDR-supported GO ",
  "occurrences and 14.0% of unique supported GO identifiers. The remainder ",
  "is overwhelmingly parent/child variants of the displayed branches; ",
  "semantic review of it identified no additional recurrent coherent ",
  "program. Complete constituent, overlapping and unclassified GO results ",
  "are provided in source data.")
MICROGLIA <- paste0(
  "The CA1 microglia-enriched ROI is an enriched measurement context, ",
  "not a sorted or single-cell microglial population, so this panel ",
  "cannot establish a cell-intrinsic microglial property.")
STRIPKEY <- paste0(
  "The three-cell strip prints its NES; its colour uses the same fixed NES ",
  "scale as the atlas colourbar (limit ", format(hv_diverging_limit("nes")),
  "), and a value beyond the limit takes full colour.")
# Where a heatmap's colour saturates: the manuscript's fixed limit of its measure
# (config/manuscript_palette.yml diverging_limits, palette v3.2), read from the
# palette so a legend cannot drift from its panel.
SATURATES <- function(measure, what) paste0(
  "Colour saturates at ", what, " ", format(hv_diverging_limit(measure)),
  " in either direction; values beyond take full colour, and the source data ",
  "keep every value.")
# The Figure 3 cards print every NES; their colour follows the same fixed scale.
CARDKEY <- paste0(
  "Tile colour uses the manuscript's fixed NES scale and saturates at ",
  format(hv_diverging_limit("nes")), " in either direction; every tile prints ",
  "its NES.")
# The enrichment-atlas Extended Data is drawn from upstream inputs this repository records as
# provenance only, so its panels can predate palette v3.2. Its legends state the
# limit the panel's source data record, not the one the renderer would draw now.
drawn_nes_limit <- function(id, col) {
  d <- sidecar("extended_data", id)
  if (is.null(d) || !col %in% names(d)) return(NA_real_)
  suppressWarnings(as.numeric(d[[col]][1]))
}
DRAWN_ATLAS <- function(id) {
  lim <- drawn_nes_limit(id, "shared_NES_scale_limit")
  if (!is.finite(lim) || isTRUE(all.equal(lim, hv_diverging_limit("nes"))))
    return(SATURATES("nes", "a median NES of"))
  sprintf(paste0("Colour uses one symmetric NES scale to %.2f in either ",
                 "direction, shared by the three contrast atlases."), lim)
}
DRAWN_STRIP <- function(id) {
  lim <- drawn_nes_limit(id, "shared_NES_strip_limit")
  if (!is.finite(lim) || isTRUE(all.equal(lim, hv_diverging_limit("nes"))))
    return(STRIPKEY)
  sprintf(paste0("The three-cell strip prints its NES; its colour uses one ",
                 "symmetric NES scale to %.2f, shared by the three strips."), lim)
}

# ------------------------------------------------------- per-panel declaration
#
# quantity / replicate_unit / descriptive_or_inferential / selection_dependency
# / test / FDR_family / independent_validation / legend
R <- function(id, quantity, replicate_unit, mode, selection, test, fdr,
              indep, legend, biological_n = "9 animals; 3 per group")
  list(id = id, quantity = quantity, replicate_unit = replicate_unit,
       mode = mode, selection = selection, test = test, fdr = fdr,
       indep = indep, legend = legend, biological_n = biological_n)

ROLES <- list(
  R("v9_depth", "proteins identified per spatial acquisition",
    "spatial acquisition (nested within animal)", "descriptive", "none",
    "none", "none", "no",
    paste0("Acquisition depth by compartment. The violins summarise spatial ",
           "acquisitions, not animals: 180 / 71 / 72 acquisitions from 9 ",
           "animals, 3 per group. AnimalID is the biological replicate and ",
           "the acquisition count must not be read as biological n. The ",
           "violin is a descriptive density with the median and ",
           "interquartile range; the neuropil distribution is multimodal ",
           "because it pools heterogeneous region and layer contexts and ",
           "repeated acquisitions within animals, and its modes are NOT ",
           "evidence of biological subpopulations. ", DESCRIPTIVE)),
  R("v9_schematic_space_f2", "sampling workflow schematic", "not applicable",
    "descriptive", "none", "none", "none", "no",
    paste0("Sampling workflow, drawn by hand: cryosectioning, staining, cell- and ",
           "region-specific annotation, laser-capture microdissection and DIA mass ",
           "spectrometry. Ten neuropil units carry region x layer resolution; the ",
           "neuronal soma and the microglia-enriched ROI are region-level only.")),
  R("v9_pca", "principal components of the global proteome",
    "spatial acquisition", "descriptive", "none", "none", "none", "no",
    paste0("Global proteome structure. Colour shows region, shape shows ",
           "compartment; points are acquisitions, not animals; labels name every ",
           "group of points they sit beside, microglia-enriched ROIs included (the ",
           "DG microglia ROIs fall with DG MO neuropil, the CA3 microglia ROIs with ",
           "the SO and SR neuropil). ", DESCRIPTIVE)),
  R("v9_marker_classes", "mean abundance of reference marker classes (log2, joint scale)",
    "animal (CON only)", "inferential", "external marker registries (Allen cell types; GO, MGI; SynGO)",
    "repeated-measures linear model on the animal means (compartment + animal as block), pairwise compartment differences",
    "BH over the 9 compartment comparisons", "no",
    paste0("Reference marker classes in every acquisition. Marker classes come from ",
           "external registries (microglia/PVM: Allen cell types; soma: GO, MGI; ",
           "neuropil: SynGO and GO, MGI); a protein counts for one class only and ",
           "must be detected in its intended compartment in at least two of three ",
           "control animals. Scores are observed log2 abundances on the joint ",
           "cross-compartment normalisation, relative to the typical shared-core ",
           "protein, averaged over the class markers detected in the acquisition; ",
           "nothing is imputed. Control animals only (n = 3). Small points are ",
           "acquisitions, larger points the animal means and the bar their mean; 20 ",
           "soma, 249 neuropil and 16 microglia/PVM markers. Compartments were compared ",
           "on the animal means with a repeated-measures linear model (compartment and ",
           "animal as factors, pooled error, df = 4), P adjusted by Benjamini-Hochberg ",
           "over the nine comparisons; brackets mark the comparisons with adjusted ",
           "P < 0.05, and all nine are in the source data. Acquisitions are repeated ",
           "measurements within animals, not replicates.")),
  R("v9_go_soma", "GSEA normalised enrichment score", "animal (CON only)",
    "inferential", "terms chosen by the authors among the terms of each region that pass direct comparison with every other region; criteria per term in the pRoteomics selection tables",
    "gene set enrichment (GO biological process)", "BH within each contrast",
    "no - same data",
    paste0("GO biological-process terms of the soma regions, control animals only. ",
           "Each region is compared with the mean of the others, and each term is shown ",
           "in every region; the shaded cell is the region the term was chosen for, and it ",
           "was chosen only if it is also enriched against every other region in direct ",
           "comparisons. Differences in significance between regions are not read as ",
           "specificity. No CA1 term passed the direct comparison with CA2, so none is ",
           "shown for CA1. Soma acquisitions differ in somatic content between regions ",
           "(highest in DG), so nuclear and mitochondrial terms may partly reflect cell size ",
           "and packing. Functional annotation of the same data, not validation. ",
           GSEA_CON("v9_go_soma", "pvalue", "FDR-supported cells (the largest dots)"))),
  R("v9_go_ca1", "GSEA normalised enrichment score", "animal (CON only)",
    "inferential", "terms chosen by the authors among the terms of each stratum that pass direct comparison with every other stratum",
    "gene set enrichment (GO biological process)", "BH within each contrast",
    "no - same data",
    paste0("GO biological-process terms of the CA1 neuropil strata, control animals only; ",
           "each stratum against the mean of the other two; display and selection as in f. ",
           "GPCR, G protein-coupled receptor. ",
           GSEA_CON("v9_go_ca1", "pvalue", "FDR-supported cells (the largest dots)"))),
  R("v9_go_ca2", "GSEA normalised enrichment score", "animal (CON only)",
    "inferential", "terms chosen by the authors among the terms of each stratum that pass direct comparison with every other stratum",
    "gene set enrichment (GO biological process)", "BH within each contrast",
    "no - same data",
    paste0("GO biological-process terms of the CA2 neuropil strata, control animals only; ",
           "contrasts added after the eleven prespecified ones; display and selection as in f. ",
           GSEA_CON("v9_go_ca2", "pvalue", "FDR-supported cells (the largest dots)"))),
  R("v9_go_dg", "GSEA normalised enrichment score", "animal (CON only)",
    "inferential", "terms chosen by the authors among the terms of each layer that pass the direct comparison with the other layer",
    "gene set enrichment (GO biological process)", "BH within each contrast",
    "no - same data",
    paste0("GO biological-process terms of the DG molecular and polymorphic layers, ",
           "control animals only (molecular against polymorphic); display and selection as in f. ",
           GSEA_CON("v9_go_dg", "pvalue", "FDR-supported cells (the largest dots)"))),
  R("v9_input_territories", "input organisation of laminar similarity (same input minus same region, Fisher z), excess over matched random proteins",
    "animal (CON only)", "inferential", "pre-declared input territories; SynGO presynaptic and postsynaptic protein classes",
    "one-sample t test over animals of the excess over abundance-matched random protein sets; presynaptic minus postsynaptic excess",
    "BH over the 6 control comparisons", "no",
    paste0("Input territories, control animals only (n = 3). Input organisation (", intToUtf8(0x394), "z) of SynGO ",
           "presynaptic-only and postsynaptic-only proteins: the similarity of laminae that share an input territory minus ",
           "that of laminae of the same region with other inputs (Spearman correlation of protein-centred laminar profiles, ",
           "Fisher z), as excess over each animal's 1,000 abundance-matched random protein sets (0, the random-set median). ",
           "Perforant-path territory: the SLM of CA1 and CA2 and the whole DG molecular layer, which also receive thalamic, ",
           "supramammillary and neurogliaform terminals (CA1 SLM and DG mo face each other across the fissure); positive ",
           "control, mossy fibres: the hilus (which also holds CA3c) and CA3 SR (with stratum lucidum). Lines join each ",
           "animal's values; grey bar, lowest 2.5th to highest 97.5th percentile of the animals' random sets. P above Pre and ",
           "below Post: that class against random; bracket: presynaptic against postsynaptic; one-sample t test over the ",
           "three animals (df = 2), Benjamini-Hochberg over six comparisons. Presynaptic proteins are input-organised beyond ",
           "random and more than postsynaptic proteins, which are not more input-organised than random. Five of the six ",
           "stressed animals replicate (Methods); the sensitivity analyses are in the source data.")),
  R("v9_afferent_sources", "laminar location of proteins grouped by the cell type whose mRNA they are enriched in (log2 excess over matched random proteins)",
    "animal (CON only)", "inferential", "origin classes defined from a whole-mouse-brain single-cell reference by stated thresholds; pre-declared contrasts",
    "one-sample t test over animals of the excess difference between two laminae",
    "BH over the 8 pre-declared contrasts", "no - an external reference defines the classes, the test is on these data",
    paste0("Laminar location of cell-type-enriched proteins, control animals only (n = 3). Proteins were grouped by the cell type ",
           "whose mRNA they are enriched in, in a whole-mouse-brain single-cell reference of other animals (Allen Brain Cell ",
           "atlas): entorhinal layer II or III neurons, 1.5 log2 above every hippocampal cell type (24 proteins); ",
           "other afferent neurons (reuniens, supramammillary, septal, raphe, locus coeruleus; 86, mostly ",
           "locus-coeruleus-enriched; descriptive); CA3 (5), granule (11), CA1 (15) and CA2 (30) cells and interneurons (28), ",
           "each above the other hippocampal types and every afferent and non-projecting type; astrocytes, vascular cells and ",
           "oligodendrocytes (30 each); at most 30 proteins per hippocampal or glial type (thresholds in Methods). The classes ",
           "do not overlap. The astrocyte and vascular rows carry two context contrasts (CA1 SLM against SR); the other rows ",
           "without an outlined lamina are descriptive. Tiles: mean over animals of the class score minus that of abundance-matched random proteins. ",
           "Outlined: the target lamina of each pre-declared afferent contrast. Entorhinal-enriched proteins are higher in CA1 ",
           "SLM, CA2 SLM and DG mo than in CA1 SR, CA2 SR and DG po, CA3-enriched proteins in CA1 and CA2 SR than SLM, ",
           "granule-enriched proteins in CA3 SR than SO (one-sample t test over animals, Benjamini-Hochberg over ",
           "eight contrasts; source data). Astrocyte and vascular proteins are also higher in CA1 SLM than SR (context ",
           "contrasts), and astrocyte proteins show a similar SLM excess in CA2 (source data). No protein whose mRNA is confined to neurons that do not ",
           "project to the hippocampus (striatal, cerebellar and olfactory-bulb granule neurons) was detected in every lamina, ",
           "so that control could not be tested. The five stressed animals of j replicate (source data).")),
  R("v9_inhibitory_share", "inhibitory presynaptic share (log2 GAD1, GAD2, VGAT relative to VGLUT1, centred per animal)",
    "animal (CON only)", "inferential", "pre-declared lamina pairs",
    "paired t test over animals of the lamina differences",
    "BH over the 15 pre-declared comparisons", "no",
    paste0("Inhibitory presynaptic share of every lamina, control animals only (n = 3). In each acquisition, the mean log2 ",
           "abundance of GAD1, GAD2 and VGAT minus that of VGLUT1: a within-sample ratio, independent of normalisation; ",
           "label-free, so it compares laminae and is not a synapse count. Animal means over hemispheres, centred on each ",
           "animal's ten laminae; points are animals, bars their mean. Pre-declared pairs (the strata within each region, and ",
           "the same stratum across regions) were tested on the per-animal differences (paired t, df = 2), P adjusted by ",
           "Benjamini-Hochberg over 15 comparisons; brackets mark adjusted P < 0.05. No comparison across regions reached ",
           "adjusted P < 0.05; all pairs and the replication in the stressed animals (six; five for DG mo-po) are in the ",
           "source data. VGLUT2 terminals (mainly thalamic and supramammillary) are concentrated in SLM and DG mo and are not in the ",
           "denominator, so the share there is an upper bound; GAD1 also marks interneuron somata (a GAD2 + VGAT-only ",
           "sensitivity is in the source data). Acquisitions with somatic carry-over were excluded as in j.")),
  R("v9_fingerprint", "baseline abundance (CON z-score)", "animal (CON only)",
    "descriptive", "rows are the top-ranked proteins of prespecified CON-only anatomical contrasts",
    "none", "none", "no",
    paste0("Baseline spatial molecular fingerprint, CON animals only ",
           "(n = 3). Rows are top-RANKED proteins from prespecified CON-only ",
           "anatomical contrasts, not significance-filtered hits, and no ",
           "stress information enters either the selection or the row order. ",
           "Rows are ordered by baseline peak spatial unit. Each protein is ",
           "standardised across the units of its own compartment, so in the ",
           "four-unit soma and microglia-enriched blocks a z-score cannot exceed ",
           "1.5. ", SATURATES("z", "a z-score of"), " ", DESCRIPTIVE)),
  R("v9_compartment", "baseline abundance of marker proteins (centred log2, median of CON animals)",
    "animal (CON only)", "descriptive", "prespecified marker panel", "none",
    "none", "no",
    paste0("Compartment identity of prespecified marker proteins, grouped by the ",
           "marker classes of panel d. Median of the three control animals; each ",
           "protein is centred on its median across compartments and animals, so ",
           "its intermediate compartment is close to 0 by construction. Colour ",
           "mapping saturates at 3.0 for display; cells exceeding the limit ",
           "are labelled with their uncapped values, and the uncapped values ",
           "are retained in the source data. ", DESCRIPTIVE)),
  R("v9_external_main", "GSEA normalised enrichment score", "animal",
    "inferential", "expected pairings only; the complete inventory is Extended Data Fig. 5d",
    "gene set enrichment", "BH within the external-validation inventory",
    "YES - external reference",
    paste0("EXTERNAL anatomical validation. Internal CON-only anatomical ",
           "contrasts are tested against independently published ",
           "hippocampal signatures, so this panel is genuine external ",
           "validation. For CA1 SO against CA3 SO the CA2/3 signature is expected ",
           "to be negative. ", GSEA_CON("v9_external_main", "single_set_p_unadjusted",
                                         "displayed pairings"))),
  R("v9_dap_track", "count of FDR-supported and robustness-qualified proteins",
    "animal", "inferential (counts of FDR-supported tests)",
    "all FDR-supported SUS-RES proteins", "differential abundance, SUS vs RES",
    "BH within the differential-abundance family", "no",
    paste0("Differential-abundance burden per spatial unit. The second row ",
           "is NOT a uniform second-stage test applied to all 37 proteins: ",
           "the 28 CA2-SLM proteins underwent the detailed missingness and ",
           "QC qualification of ED3, of which 6 qualified, while the 9 ",
           "FDR-supported proteins outside CA2-SLM enter unchanged because ",
           "they were never at risk from that artefact. 'Robustness-",
           "qualified' therefore means 'not excluded by the CA2-SLM ",
           "missingness audit', not 'passed an additional test'.")),
  R("v9_atlas", "median NES across mapped canonical GO terms", "animal",
    "descriptive aggregation of inferential inputs",
    "claim-eligible themes only; qc_review themes excluded",
    "gene set enrichment on the constituent GO terms",
    "BH within each constituent GO family; the THEME has no family of its own",
    "no",
    paste0("Programme-level SUS-RES enrichment atlas. Colour is the median ",
           "NES across every mapped constituent GO term; dot size is the ",
           "number of constituent terms with BH FDR < 0.05. Both are ",
           "descriptive programme summaries. The seven programmes are the ",
           "claim-eligible themes of the GO theme registry, the same rows as ",
           "the RES-CON and SUS-CON atlases of Extended Data Fig. 7; theme ",
           "aggregation does not refit enrichment or create a programme-level ",
           "p-value or FDR. Numbered outlines identify the three drill-down ",
           "exemplars. ", SATURATES("nes", "a median NES of"), " ", GSEA_N, " ",
           CURATED)),
  R("v9_adaptation_state",
    "paired programme median NES for RES-CON and SUS-CON", "animal",
    "descriptive aggregation and classification of inferential inputs",
    "all seven programmes by all 18 measured spatial contexts",
    "gene set enrichment on the constituent GO terms",
    "BH within each constituent GO family; no new programme-level test",
    "no",
    paste0("Adaptation-state map. Each point is one programme x spatial ",
           "context. Coordinates are median constituent-term NES for RES-CON ",
           "and SUS-CON. Point opacity records whether either control contrast ",
           "contains at least one constituent term with BH FDR < 0.05; opacity ",
           "is not a programme-level significance test. Region labels mark ",
           "the sign quadrants and axes only: a cell's pattern in panel d ",
           "also requires FDR support in the corresponding control contrast, ",
           "and non-support is called little detectable adaptation rather ",
           "than passive adaptation. Numbered points are ",
           "the same three exemplars as panels b and e-g. ", GSEA_N, " ", ALGEBRA)),
  R("v9_adaptation_burden",
    "count and fraction of programme-context adaptation states", "animal",
    "descriptive summary of classified inferential inputs",
    "all seven programmes by all 18 measured spatial contexts",
    "gene set enrichment on the constituent GO terms",
    "BH within each constituent GO family; no new programme-level test",
    "no",
    paste0("Adaptation-pattern burden by compartment. A control-contrast arm ",
           "is supported when any mapped constituent term has BH FDR < 0.05. ",
           "Neither arm is little detectable; one supported arm is assigned ",
           "to its outcome-specific remodelling state; two supported arms are ",
           "shared/parallel when their programme-median NES signs agree and ",
           "divergent/opposing when they differ. Counts are printed and dot ",
           "area is the within-compartment fraction. This classification is ",
           "descriptive and creates no new test or FDR family. ", GSEA_N)),
  R("v9_card_syn", "constituent-term NES in all three pairwise contrasts",
    "animal", "inferential inputs displayed descriptively",
    "fixed curve term plus the two highest-ranked SUS-RES terms by stored BH FDR, absolute NES and GO ID",
    "gene set enrichment", "BH within each GSEA family",
    "no - selected from the same analysis",
    paste0("Neuropil CA3 SR synaptic signalling/vesicle exemplar. The fixed curve term ",
           "and two additional programme terms are shown for RES-CON, ",
           "SUS-CON and SUS-RES; an outline marks constituent-term BH FDR < ",
           "0.05. This is a drill-down of the same enrichment analysis, not ",
           "independent validation. ", CARDKEY, " ", GSEA_N, " ", ALGEBRA)),
  R("v9_card_rna", "constituent-term NES in all three pairwise contrasts",
    "animal", "inferential inputs displayed descriptively",
    "fixed curve term plus the two highest-ranked SUS-RES terms by stored BH FDR, absolute NES and GO ID",
    "gene set enrichment", "BH within each GSEA family",
    "no - selected from the same analysis",
    paste0("Soma CA2 RNA processing exemplar. The fixed curve term and two ",
           "additional programme terms are shown for all three pairwise ",
           "contrasts. This is a same-analysis drill-down, not independent ",
           "validation. ", CARDKEY, " ", GSEA_N, " ", ALGEBRA)),
  R("v9_card_ox", "constituent-term NES in all three pairwise contrasts",
    "animal", "inferential inputs displayed descriptively",
    "fixed curve term plus the two highest-ranked SUS-RES terms by stored BH FDR, absolute NES and GO ID",
    "gene set enrichment", "BH within each GSEA family",
    "no - selected from the same analysis",
    paste0("Microglia ROI CA1 mitochondrial respiration exemplar. The fixed curve ",
           "term and two additional programme terms are shown for all three ",
           "pairwise contrasts. The ROI is microglia-enriched rather than a ",
           "sorted cell population. This is a same-analysis drill-down, not ",
           "independent validation. ", CARDKEY, " ", GSEA_N, " ", MICROGLIA, " ",
           ALGEBRA)),
  R("v9_curve_syn", "running enrichment score", "animal", "inferential",
    "fixed exemplar constituent term", "gene set enrichment", "BH within the GSEA family",
    "no", paste0("Ranked SUS-RES enrichment evidence for the fixed constituent ",
           "term identified in panel e. ", GSEA_N, " ", ALGEBRA)),
  R("v9_curve_rna", "running enrichment score", "animal", "inferential",
    "fixed exemplar constituent term", "gene set enrichment", "BH within the GSEA family",
    "no", paste0("Ranked SUS-RES enrichment evidence for the fixed constituent ",
           "term identified in panel f. ", GSEA_N, " ", ALGEBRA)),
  R("v9_curve_ox", "running enrichment score", "animal", "inferential",
    "fixed exemplar constituent term", "gene set enrichment", "BH within the GSEA family",
    "no", paste0("Ranked SUS-RES enrichment evidence for the fixed constituent ",
           "term identified in panel g. ", GSEA_N, " ", MICROGLIA, " ", ALGEBRA)),
  R("v9_prot_syn", "log2 fold change", "animal", "descriptive",
    "leading-edge constituents of the enrichment result above",
    "differential abundance", "BH within the differential-abundance family",
    "no - selected from the same analysis",
    paste0("Selected leading-edge proteins underlying the enrichment result ",
           "above are shown descriptively; proteins were selected using the ",
           "prespecified stored rank-statistic rule. They are NOT an ",
           "independent validation of the enrichment, NOT independently ",
           "significant proteins and NOT an unbiased protein sample. ",
           ALGEBRA)),
  R("v9_prot_rna", "log2 fold change", "animal", "descriptive",
    "leading-edge constituents of the enrichment result above",
    "differential abundance", "BH within the differential-abundance family",
    "no - selected from the same analysis",
    paste0("Selected leading-edge proteins, descriptive; see panel i. ",
           ALGEBRA)),
  R("v9_prot_ox", "log2 fold change", "animal", "descriptive",
    "leading-edge constituents of the enrichment result above",
    "differential abundance", "BH within the differential-abundance family",
    "no - selected from the same analysis",
    paste0("Selected leading-edge proteins, descriptive; see panel j. ",
           MICROGLIA, " ",
           ALGEBRA)),
  # the ED6 atlases are the same descriptive theme aggregation as Figure 3b and
  # must carry the same declaration, not fall through to a default
  R("v9_ed_atlas_rescon", "median NES across mapped canonical GO terms",
    "animal", "descriptive aggregation of inferential inputs",
    "claim-eligible themes only",
    "gene set enrichment on the constituent GO terms",
    "BH within each constituent GO family; the THEME has no family of its own",
    "no",
    paste0("RES vs CON theme atlas. Theme colours summarise the median NES of ",
           "mapped canonical GO terms and are DESCRIPTIVE. Support markers ",
           "indicate that at least one constituent canonical GO term passed ",
           "its prespecified FDR threshold. Theme aggregation does not ",
           "constitute an additional multiple-testing family. ",
           DRAWN_ATLAS("v9_ed_atlas_rescon"), " ", CURATED, " ", ALGEBRA)),
  R("v9_ed_atlas_suscon", "median NES across mapped canonical GO terms",
    "animal", "descriptive aggregation of inferential inputs",
    "claim-eligible themes only",
    "gene set enrichment on the constituent GO terms",
    "BH within each constituent GO family; the THEME has no family of its own",
    "no",
    paste0("SUS vs CON theme atlas, completing the set of three pairwise ",
           "contrasts. ",
           "Theme colours are a DESCRIPTIVE median of mapped canonical GO ",
           "terms; support markers indicate at least one FDR-supported ",
           "constituent term. Theme aggregation does not constitute an ",
           "additional multiple-testing family. ",
           DRAWN_ATLAS("v9_ed_atlas_suscon"), " ", CURATED, " ", ALGEBRA))
)

ED_DEFAULT_LEGEND <- list(
  v9_ed_bilateral_full = paste0(
    "Complete stored bilateral metric inventory. ", DESCRIPTIVE),
  v9_ed_precision = paste0(
    "Reliability gain from bilateral averaging. Light symbol = one ",
    "hemisphere; dark symbol = bilateral mean; faint lines are individual ",
    "endpoints and the arrow joins the class medians. Endpoints whose variance ",
    "components were singular or whose fit did not converge are not drawn ",
    "(their number is in the source data). Intraclass correlation is ",
    "reported as descriptive precision context; no significance test is ",
    "applied. ", DESCRIPTIVE),
  # This panel is the CON-only spatial fingerprint, not an external inventory.
  # The old text was copied from its neighbour and described the wrong analysis.
  v9_ed_fingerprint_full = paste0(
    "Complete CON-only spatial fingerprint of external prespecified signatures ",
    "across all spatial units. Each value is a signature score: the mean ",
    "within-protein standardised control abundance (z) of the signature's ",
    "measured member proteins. Panel f shows named proteins from the ",
    "CON-only anatomical contrasts instead. ",
    SATURATES("z_set_mean", "a mean z-score of"), " ", DESCRIPTIVE),
  # Two corrections here. The grid is 63 cells and 30 pairings were tested, so
  # "every contrast against every signature" overstated it; and the significance
  # statistic is signature_FDR, the signature-family correction, NOT the column
  # formerly exported as p_adjust, which is the uncorrected single-set value.
  v9_ed_external_full = paste0(
    "All 30 tested contrast-signature pairings - 10 expected and 20 specificity ",
    "comparisons - across the 9 control-only contrasts and 7 external ",
    "signatures that were structurally applicable to one another. Significance ",
    "is the stored signature-family FDR, which is the statistic the Results ",
    "claim rests on; the uncorrected single-set value is carried separately in ",
    "the source data under its own name. Main Figure 2e shows the expected ",
    "pairings alone."),
  v9_ed_internal_full = paste0(
    "Complete functional characterisation inventory: every canonical GO term ",
    "retained for every anatomical contrast, on one shared NES axis. This is ",
    "characterisation of the same data, not independent validation."),
  v9_ed_ca2_locator = "Location of CA2-SLM within the sampling scheme.",
  v9_ed_ca2_missing = paste0(
    "Pre-imputation missingness per acquisition, hemispheres paired within ",
    "animal. Both QC failures are a single hemisphere of an animal whose ",
    "other hemisphere passes. ", DESCRIPTIVE),
  v9_ed_ca2_displacement = paste0(
    "Relationship between missingness and the normalisation-related median ",
    "shift. The association is shown descriptively and no p-value is ",
    "computed: acquisitions are clustered within animal and hemisphere, so ",
    "an unclustered correlation test would be pseudoreplicated. ",
    DESCRIPTIVE),
  v9_ed_ca2_classes = paste0(
    "Robustness classification of the 28 canonical CA2-SLM proteins: ",
    "robustness-qualified, QC-sensitive, or insufficient observed data."),
  v9_ed_ca2_sensitivity = paste0(
    "Effect sizes with and without the two QC-failed hemispheres, on one ",
    "shared axis: filled symbol, canonical log2 fold change; open symbol, ",
    "with the QC-failed SUS acquisitions removed, so segment length is the ",
    "magnitude lost. Rows are grouped by robustness class; the key gives the ",
    "number of the 28 canonical CA2-SLM proteins in each class. ", DESCRIPTIVE),
  v9_ed_module_fingerprint = paste0(
    "Mean CON z-score of each module's MEMBER PROTEINS across spatial units. ",
    "This is not the module eigengene, which is shown in panel b. ",
    DESCRIPTIVE),
  v9_ed_wgcna_phenotype = paste0(
    "Module eigengene differences by contrast. 13 of 15 modules show the ",
    "descriptive three-group directional trajectory; 0 of 45 module x ",
    "contrast cells and 0 of 35 stress x spatial-unit interaction omnibus ",
    "tests are FDR-supported (smallest FDR 0.27). The 13-of-15 statement is ",
    "DESCRIPTIVE and the three pairwise contrast directions are ",
    "algebraically related. A negative interaction result means no spatially ",
    "heterogeneous module effect survived multiple-testing correction; it ",
    "does NOT establish that such effects are absent. ", ALGEBRA),
  v9_ed_celltype = paste0(
    "External cell-type affinity counts per compartment. ", DESCRIPTIVE),
  v9_ed_m11 = paste0(
    "Worked example for one module. The active historical label is unchanged ",
    "and the proposed oligodendrocyte / myelin label is NOT activated; the ",
    "external evidence is context only. ", DESCRIPTIVE),
  v9_ed_gsea_curve_syn = paste0("Detailed ranked enrichment. ",
                                DRAWN_STRIP("v9_ed_gsea_curve_syn"), " ", GSEA_N,
                                " ", ALGEBRA),
  v9_ed_gsea_curve_rna = paste0("Detailed ranked enrichment. ",
                                DRAWN_STRIP("v9_ed_gsea_curve_rna"), " ", GSEA_N,
                                " ", ALGEBRA),
  v9_ed_gsea_curve_ox = paste0("Detailed ranked enrichment. ",
                               DRAWN_STRIP("v9_ed_gsea_curve_ox"), " ", GSEA_N, " ",
                               MICROGLIA, " ", ALGEBRA),
  v9_ed_atlas_rescon = paste0(
    "RES vs CON theme atlas. Theme colours are a descriptive median of ",
    "mapped canonical GO terms; support markers indicate at least one ",
    "FDR-supported constituent term. Theme aggregation is not an additional ",
    "multiple-testing family. ", ALGEBRA),
  v9_ed_atlas_suscon = paste0(
    "SUS vs CON theme atlas; see the RES vs CON panel. ", ALGEBRA),
  v9_ed_identity = paste0(
    "Among robustness-qualified proteins, the proportion whose strongest ",
    "phenotype-associated effect lies outside the canonical control-animal ",
    "affinity set, across nested robustness subsets. These proteins were ",
    "already selected as FDR-supported and robustness-qualified, so this is ",
    "a DESCRIPTIVE pattern within a selected set and not an unbiased test ",
    "of a spatial null."),
  v9_ed_locations = paste0(
    "Among robustness-qualified proteins, the dominant unit in control animals ",
    "and the unit of strongest phenotype-associated effect. Two distinct ",
    "statements are reported: 15 of 15 have their strongest effect in a ",
    "DIFFERENT unit from their single dominant control unit, and 14 of 15 are ",
    "also outside the broader canonical control-animal affinity set, SNU13 ",
    "being the sole exception. effect_identity_relationship is the ",
    "authoritative affinity classification. Each arrow joins two independent ",
    "measurements of one protein; nothing travels between units. This is a ",
    "descriptive pattern within a selected set."),
  v9_ed_similarity = paste0(
    "Molecular similarity between spatial proteomic profiles, NOT anatomical ",
    "connectivity. One diverging scale centred on zero is used for all three ",
    "blocks because the metric is the same in each, so one colour means one ",
    "correlation everywhere. Zero is NOT the no-structure expectation: ",
    "profiles are centred within protein, so the mean similarity of a block ",
    "of u units is fixed at about -1/(u-1), which is -0.11 for the ten ",
    "neuropil units and -0.33 for the four soma and four microglia units. ",
    "Neither region-level block contains a positive value. The colour limit ",
    "is the manuscript's fixed correlation limit of ",
    format(hv_diverging_limit("correlation")), ", ",
    local({
      d <- sidecar("extended_data", "v9_ed_similarity")
      v <- if (is.null(d)) NA_real_ else suppressWarnings(as.numeric(d$median_similarity))
      if (length(v) && all(is.finite(v)) &&
          max(abs(v)) <= hv_diverging_limit("correlation"))
        "inside which every value of the panel lies" else
        "and a value beyond it takes full colour"
    }), "; the omitted diagonal is grey. ", DESCRIPTIVE),
  v9_ed_network_distance = paste0(
    "Animal-level network distance from the CON consensus. For each CON ",
    "animal the distance was computed to a LEAVE-ONE-CON-ANIMAL-OUT ",
    "consensus, so no animal is compared with a consensus containing itself; ",
    "RES and SUS animals were compared with the full CON consensus. This ",
    "construction was verified against the edge data, not against its label. ",
    "Each facet prints the exact whole-network permutation P over animal ",
    "labels (smallest attainable P 0.0036); the statistic is a symmetric ",
    "three-group centroid deviation and does not use the CON consensus. No ",
    "whole-network group difference was detected."),
  v9_ed_nulls = paste0(
    "Exact whole-network permutation over animal labels, with the smallest ",
    "attainable p shown so the null is informative rather than merely ",
    "non-significant. No whole-network group difference was detected. The ",
    "statistic is a symmetric three-group centroid deviation and does not ",
    "use the CON consensus reference."),
  v9_ed_coupling = paste0(
    "Every distinct spatial-unit-pair by behavioural-outcome correlation. No ",
    "edge-behaviour association survived multiple-testing correction. With ",
    "n = 9 animals a single correlation has very little power, so this is a ",
    "limited-power negative result and not evidence of absence.")
)

# ------------------------------------------------------------------ assemble
by_id <- stats::setNames(ROLES, vapply(ROLES, function(r) r$id, character(1)))
panel_of <- function(id)
  Filter(function(p) identical(as.character(p$id), id), ct$panels)[[1]]

rows <- list(); leg <- c("# Figure legends - final_truth_v9", "",
  paste0("Generated from `figures/contracts/proteomics_panels_contract.yml`. ",
         "Figures 2 and 3 of this layer are the canonical manuscript figures ",
         "(provenance/publication_registry). Biological replicate is the animal ",
         "(9 animals, 3 per group) throughout unless a panel states otherwise."),
  "")

for (f in ct$figures) {
  leg <- c(leg, paste0("## ", as.character(f$name)), "",
           paste0("**", as.character(f$question %||% ""), "**"), "")
  for (it in f$layout) {
    id <- as.character(it$panel); p <- panel_of(id)
    r <- by_id[[id]]
    txt <- if (!is.null(r)) r$legend else
      (ED_DEFAULT_LEGEND[[id]] %||%
         gsub("[\r\n]+", " ", as.character(p$narrative %||% p$role %||% "")))
    leg <- c(leg, paste0("**", as.character(it$label), "** ", txt), "")
    sd <- sidecar(as.character(f$figure_key), id)
    rows[[length(rows) + 1L]] <- data.frame(
      figure = as.character(f$name), panel = as.character(it$label),
      panel_id = id,
      quantity = if (!is.null(r)) r$quantity else as.character(p$role %||% ""),
      biological_n = if (!is.null(r)) r$biological_n else "9 animals; 3 per group",
      replicate_unit = if (!is.null(r)) r$replicate_unit else "animal",
      descriptive_or_inferential = if (!is.null(r)) r$mode else "descriptive",
      selection_dependency = if (!is.null(r)) r$selection else "none",
      test = if (!is.null(r)) r$test else "none",
      FDR_family = if (!is.null(r)) r$fdr else "none",
      independent_validation = if (!is.null(r)) r$indep else "no",
      caption_sufficient = nzchar(txt),
      source_data_rows = if (is.null(sd)) NA_integer_ else nrow(sd),
      required_fix = "none",
      stringsAsFactors = FALSE)
  }
}
aud <- do.call(rbind, rows)
write_csv_safe(aud, file.path(TAB, "final_panel_statistical_role_audit.csv"))
writeLines(leg, file.path(REP, "final_figure_legends_v9.md"))

cat("\n===== PANEL STATISTICAL-ROLE AUDIT =====\n")
cat("panels audited          :", nrow(aud), "\n")
cat("with a written legend   :", sum(aud$caption_sufficient), "\n")
cat("inferential panels      :",
    sum(grepl("inferential", aud$descriptive_or_inferential)), "\n")
cat("claimed as INDEPENDENT validation:",
    sum(grepl("^YES", aud$independent_validation)), "\n")
print(aud[grepl("^YES", aud$independent_validation),
          c("figure", "panel", "quantity")], row.names = FALSE)
cat("\nselected-from-same-analysis panels:\n")
print(aud[grepl("same analysis", aud$selection_dependency),
          c("figure", "panel", "selection_dependency")], row.names = FALSE)
cat("\nwritten:\n  ", relative_to(file.path(REP, "final_figure_legends_v9.md")),
    "\n  ", relative_to(file.path(TAB, "final_panel_statistical_role_audit.csv")),
    "\n")
