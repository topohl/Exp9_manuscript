# WGCNA module wheel and theme x module composition (manuscript candidate).
#
# Downstream only. Reads the frozen pRoteomics export wgcna_theme_composition_candidate
# (imported by tools/import_wgcna_theme_composition.R) and draws it. Every value shown is a
# value of that export; the only things computed here are geometry (angles, radii), display
# order and the colour each value is drawn in.
#
# The composition is phenotype-blind: it asks whether the co-abundance modules group the
# proteins of the curated GSEA themes, not whether any group differs, so no group effect is
# drawn. A module "carries" a theme when the export marks the cell composition_supported.
#
# Needs R/panels/nature_v2_figure_utils.R, nature_final_v7_panels.R, final_truth_v9_panels.R
# and figure3_adaptation_panels.R (theme labels and the Figure 3 theme colours).

WTC_ID <- "wgcna_theme_composition_candidate"
WTC_FILES <- c("theme_entity_composition_long.csv", "theme_composition_summary.csv",
               "theme_composition_summary_all_variants.csv", "theme_partition_agreement.csv",
               "curated_mapping_composition_audit.csv", "wgcna_module_wheel_structure.csv",
               "composition_universe_audit.csv", "theme_index.csv", "input_manifest.csv")
WTC_DATASETS <- c("neuron_neuropil", "neuron_soma", "microglia")
# the five sensitivity variants the export checks each top carrier against, in pip order
WTC_SENSITIVITIES <- c(
  go_membership_single_theme__wgcna_ranked_annotated__all_members = "proteins of one theme only",
  leading_edge_fdr__wgcna_ranked_annotated__all_members = "FDR-supported leading edges",
  leading_edge_fdr_sus_res__wgcna_ranked_annotated__all_members = "leading edges, SUS\u2212RES",
  go_membership__wgcna_all__all_members = "all WGCNA proteins as background",
  go_membership__wgcna_ranked_annotated__core_kME_0.6 = "core members only")

# ------------------------------------------------------------------ inputs

wtc_read <- function(src = repo_path("source_data", "pRoteomics", WTC_ID),
                     manifest = repo_path("source_data", "pRoteomics", "manifest.csv")) {
  mf <- utils::read.csv(manifest, stringsAsFactors = FALSE)
  mf <- mf[mf$publication_id == WTC_ID, , drop = FALSE]
  for (f in WTC_FILES) {
    row <- mf[basename(mf$exported_file) == f, , drop = FALSE]
    p <- file.path(src, f)
    if (!file.exists(p) || nrow(row) != 1L ||
        !identical(unname(tools::sha256sum(p)), row$sha256[[1]]))
      stop("Frozen composition source missing or hash mismatch: ", f, call. = FALSE)
  }
  rd <- function(f) utils::read.csv(file.path(src, f), stringsAsFactors = FALSE, check.names = FALSE)
  x <- list(long = rd("theme_entity_composition_long.csv"),
            summary = rd("theme_composition_summary.csv"),
            wheel = rd("wgcna_module_wheel_structure.csv"),
            themes = rd("theme_index.csv"),
            partition = rd("theme_partition_agreement.csv"))
  if (!setequal(unique(x$wheel$dataset), WTC_DATASETS) ||
      anyDuplicated(x$wheel[c("dataset", "module_id")]))
    stop("The wheel table must list each module of the three compartments once.", call. = FALSE)
  x
}

# Theme order from the export's registry index, labels and colours from Figure 3, so a theme
# has the same name and colour in both figures.
wtc_themes <- function(theme_index) {
  p <- f3a_programmes()
  idx <- theme_index[order(theme_index$display_order), , drop = FALSE]
  if (!identical(idx$theme_id, p$source_theme_id))
    stop("Theme registry order differs from the Figure 3 programmes.", call. = FALSE)
  data.frame(theme_id = p$source_theme_id, label = p$programme_label,
             colour = p$colour, order = seq_len(nrow(p)), stringsAsFactors = FALSE)
}

wtc_short_module <- function(x) sub("^WGCNA_", "", x)
wtc_split <- function(x) if (is.na(x) || !nzchar(x)) character() else strsplit(x, ";", fixed = TRUE)[[1]]
wtc_id_number <- function(x) as.integer(gsub("\\D", "", x))
wtc_compartment_label <- function(x) unname(nv_dataset_label(x))

# ------------------------------------------------------------ wheel layout

# Modules sit side by side inside their supermodule, supermodules inside their compartment.
# One module is one angular unit; gaps separate supermodules and compartments. Angles run
# clockwise from the top, in degrees.
wtc_layout <- function(wheel, gap_sm = 0.4, gap_comp = 2.4) {
  w <- wheel[order(match(wheel$dataset, WTC_DATASETS), wtc_id_number(wheel$supermodule_id),
                   wtc_id_number(wheel$module_id)), , drop = FALSE]
  start <- numeric(nrow(w))
  at <- gap_comp / 2
  for (i in seq_len(nrow(w))) {
    if (i > 1L) {
      if (w$dataset[[i]] != w$dataset[[i - 1L]]) at <- at + gap_comp
      else if (w$supermodule_id[[i]] != w$supermodule_id[[i - 1L]]) at <- at + gap_sm
    }
    start[[i]] <- at
    at <- at + 1
  }
  total <- at + gap_comp / 2
  w$t0 <- start / total * 360
  w$t1 <- (start + 1) / total * 360
  w$tm <- (w$t0 + w$t1) / 2
  w$module_label <- wtc_short_module(w$module_id)
  w$wheel_order <- seq_len(nrow(w))
  rownames(w) <- NULL
  w
}

wtc_sector <- function(t0, t1, r0, r1, key, step = 1) {
  th <- seq(t0, t1, length.out = max(2L, ceiling((t1 - t0) / step) + 1L)) * pi / 180
  data.frame(key = key, x = c(r1 * sin(th), r0 * sin(rev(th))),
             y = c(r1 * cos(th), r0 * cos(rev(th))), stringsAsFactors = FALSE)
}

wtc_sectors <- function(d, r0, r1, ring) {
  out <- lapply(seq_len(nrow(d)), function(i) {
    s <- wtc_sector(d$t0[[i]], d$t1[[i]], r0, r1, paste(ring, i))
    s$fill <- d$fill[[i]]
    s
  })
  do.call(rbind, out)
}

wtc_xy <- function(t, r) data.frame(x = r * sin(t * pi / 180), y = r * cos(t * pi / 180))
# text reading outwards along the radius, turned on the left half so it is never upside down
wtc_radial_angle <- function(t) ifelse(t > 180, 270 - t, 90 - t)
# hjust that pins radial text to the outer (or inner) end of its band on either half
wtc_radial_hjust <- function(t, end = c("outer", "inner")) {
  outer <- match.arg(end) == "outer"
  ifelse(t > 180, as.numeric(!outer), as.numeric(outer))
}
# text along the circle, turned on the lower half
wtc_tangent_angle <- function(t) ifelse(t > 90 & t < 270, 180 - t, -t)

# Colour-only rings sit inside, where the circumference is small; the text band (peak region at its
# inner end, module name at its outer end) sits outside, where a module has room for both labels.
WTC_R <- list(bilateral = c(10.4, 11.8), tau = c(12.1, 13.5),
              theme0 = 13.9, theme_w = 1.65, theme_gap = 0.2,
              peak = c(27.1, 38.3), sm = c(38.9, 40.5), sm_label = 42.5,
              comp_line = 45.3, comp_label = 47.8)

wtc_lighten <- function(col, amount = 0.6) grDevices::colorRampPalette(c(col, "white"))(101)[round(amount * 100) + 1L]
wtc_ramp <- function(v, high, limits = c(0, 1)) {
  z <- pmin(pmax((v - limits[[1]]) / diff(limits), 0), 1)
  grDevices::rgb(grDevices::colorRamp(c("white", high))(z), maxColorValue = 255)
}
WTC_TAU_HIGH <- "grey25"
WTC_EMPTY <- "grey95"

# One row per module and ring, with the value drawn: the wheel's source data.
wtc_wheel_cells <- function(x) {
  themes <- wtc_themes(x$themes)
  w <- wtc_layout(x$wheel)
  ev <- nv_evidence_colours()
  rows <- list()
  for (k in seq_len(nrow(themes))) {
    carried <- vapply(w$carried_themes, function(s) themes$theme_id[[k]] %in% wtc_split(s), logical(1))
    rows[[k]] <- data.frame(w[c("dataset", "supermodule_id", "module_id", "module_label", "wheel_order",
                                "t0", "t1", "tm")],
                            ring = paste0("theme_", k), theme_id = themes$theme_id[[k]],
                            value = as.character(carried),
                            fill = ifelse(carried, themes$colour[[k]], WTC_EMPTY),
                            r0 = WTC_R$theme0 + (nrow(themes) - k) * (WTC_R$theme_w + WTC_R$theme_gap),
                            stringsAsFactors = FALSE, row.names = NULL)
    rows[[k]]$r1 <- rows[[k]]$r0 + WTC_R$theme_w
  }
  base <- w[c("dataset", "supermodule_id", "module_id", "module_label", "wheel_order", "t0", "t1", "tm")]
  bil <- c(reproducible_level_and_pattern = ev[["supported"]],
           poor_bilateral_reproducibility = ev[["qc_context"]])
  rows$tau <- data.frame(base, ring = "spatial_tau", theme_id = NA_character_,
                         value = as.character(w$spatial_tau), fill = wtc_ramp(w$spatial_tau, WTC_TAU_HIGH),
                         r0 = WTC_R$tau[[1]], r1 = WTC_R$tau[[2]], stringsAsFactors = FALSE)
  rows$bil <- data.frame(base, ring = "bilateral_reproducibility", theme_id = NA_character_,
                         value = w$bilateral_reproducibility_class,
                         fill = ifelse(w$bilateral_reproducibility_class %in% names(bil),
                                       bil[w$bilateral_reproducibility_class], ev[["not_evaluable"]]),
                         r0 = WTC_R$bilateral[[1]], r1 = WTC_R$bilateral[[2]], stringsAsFactors = FALSE)
  rows$peak <- data.frame(base, ring = "peak_unit", theme_id = NA_character_, value = w$peak_unit,
                          fill = NA_character_, r0 = WTC_R$peak[[1]], r1 = WTC_R$peak[[2]],
                          stringsAsFactors = FALSE)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

wtc_wheel_plot <- function(x) {
  fam <- nf_fam()
  cells <- wtc_wheel_cells(x)
  w <- wtc_layout(x$wheel)
  dcol <- nv_dataset_colours()
  tiles <- cells[!is.na(cells$fill), , drop = FALSE]
  poly <- do.call(rbind, lapply(seq_len(nrow(tiles)), function(i) {
    s <- wtc_sector(tiles$t0[[i]], tiles$t1[[i]], tiles$r0[[i]], tiles$r1[[i]], paste("cell", i))
    s$fill <- tiles$fill[[i]]
    s
  }))
  # supermodule arcs: compartment colour, lighter for a supermodule of one module
  sm <- do.call(rbind, lapply(split(w, paste(w$dataset, w$supermodule_id)), function(g) {
    data.frame(dataset = g$dataset[[1]], supermodule_id = g$supermodule_id[[1]],
               t0 = min(g$t0), t1 = max(g$t1), n = nrow(g),
               multi = isTRUE(g$is_multi_module_supermodule[[1]]), stringsAsFactors = FALSE)
  }))
  sm$fill <- ifelse(sm$multi, dcol[sm$dataset], vapply(dcol[sm$dataset], wtc_lighten, character(1)))
  sm$tm <- (sm$t0 + sm$t1) / 2
  sm_poly <- wtc_sectors(sm, WTC_R$sm[[1]], WTC_R$sm[[2]], "sm")
  sm_lab <- cbind(sm, wtc_xy(sm$tm, WTC_R$sm_label))
  comp <- do.call(rbind, lapply(split(w, w$dataset), function(g)
    data.frame(dataset = g$dataset[[1]], t0 = min(g$t0), t1 = max(g$t1), stringsAsFactors = FALSE)))
  comp$tm <- (comp$t0 + comp$t1) / 2
  comp_line <- do.call(rbind, lapply(seq_len(nrow(comp)), function(i) {
    th <- seq(comp$t0[[i]], comp$t1[[i]], length.out = 120)
    data.frame(dataset = comp$dataset[[i]], wtc_xy(th, WTC_R$comp_line))
  }))
  comp_lab <- cbind(comp, wtc_xy(comp$tm, WTC_R$comp_label))
  comp_lab$label <- wtc_compartment_label(comp_lab$dataset)
  mod_lab <- cbind(w, wtc_xy(w$tm, WTC_R$peak[[2]]))
  peak <- cells[cells$ring == "peak_unit", , drop = FALSE]
  peak <- cbind(peak, wtc_xy(peak$tm, WTC_R$peak[[1]] + 0.3))
  peak$label <- toupper(sub("_.*$", "", peak$value))
  ggplot2::ggplot() +
    ggplot2::geom_polygon(data = poly, ggplot2::aes(x, y, group = key, fill = fill),
                          colour = "white", linewidth = nv_lw("tile_border_pt")) +
    ggplot2::geom_polygon(data = sm_poly, ggplot2::aes(x, y, group = key, fill = fill),
                          colour = "white", linewidth = nv_lw("tile_border_pt")) +
    ggplot2::geom_path(data = comp_line, ggplot2::aes(x, y, group = dataset),
                       colour = "grey20", linewidth = nv_lw("axis_pt")) +
    ggplot2::geom_text(data = mod_lab, ggplot2::aes(x, y, label = module_label, angle = wtc_radial_angle(tm),
                                                    hjust = wtc_radial_hjust(tm, "outer")),
                       family = fam, size = nv_size(nv_pt("annotation_pt")), colour = "grey10") +
    ggplot2::geom_text(data = peak, ggplot2::aes(x, y, label = label, angle = wtc_radial_angle(tm),
                                                 hjust = wtc_radial_hjust(tm, "inner")),
                       family = fam, size = nv_size(nv_pt("annotation_pt")), colour = "grey45") +
    ggplot2::geom_text(data = sm_lab, ggplot2::aes(x, y, label = supermodule_id,
                                                   angle = wtc_tangent_angle(tm),
                                                   colour = ifelse(multi, "grey10", "grey55")),
                       family = fam, size = nv_size(nv_pt("annotation_pt"))) +
    ggplot2::geom_text(data = comp_lab, ggplot2::aes(x, y, label = label, angle = wtc_tangent_angle(tm)),
                       family = fam, fontface = "bold", size = nv_size(nv_pt("axis_title_pt"))) +
    ggplot2::scale_fill_identity() + ggplot2::scale_colour_identity() +
    ggplot2::coord_fixed(xlim = c(-50.5, 50.5), ylim = c(-50.5, 50.5), expand = FALSE, clip = "off") +
    ggplot2::theme_void(base_family = fam)
}

# The wheel's key, drawn as its own small panel so the wheel keeps its full width.
wtc_wheel_key <- function(x) {
  fam <- nf_fam()
  themes <- wtc_themes(x$themes)
  ev <- nv_evidence_colours()
  dcol <- nv_dataset_colours()
  ts <- nv_size(nv_pt("legend_text_pt"))
  hs <- nv_size(nv_pt("legend_title_pt"))
  heads <- list(); boxes <- list(); texts <- list()
  pos <- c(x = 0, y = 0)
  head <- function(label) {
    heads[[length(heads) + 1L]] <<- data.frame(x = pos[["x"]], y = pos[["y"]], label = label)
    pos[["y"]] <<- pos[["y"]] - 1
  }
  item <- function(fill, label, border = "white") {
    boxes[[length(boxes) + 1L]] <<- data.frame(x = pos[["x"]] + 0.45, y = pos[["y"]], fill = fill, border = border)
    texts[[length(texts) + 1L]] <<- data.frame(x = pos[["x"]] + 1.2, y = pos[["y"]], label = label)
    pos[["y"]] <<- pos[["y"]] - 1
  }
  line <- function(label) {
    texts[[length(texts) + 1L]] <<- data.frame(x = pos[["x"]] + 0.1, y = pos[["y"]], label = label)
    pos[["y"]] <<- pos[["y"]] - 1
  }
  # left column: what the rings and arcs are
  head("Theme rings, outside \u2192 inside")
  for (k in seq_len(nrow(themes))) item(themes$colour[[k]], themes$label[[k]])
  item(WTC_EMPTY, "theme not carried", border = "grey80")
  pos[["y"]] <- pos[["y"]] - 0.4
  head("Supermodule arc")
  item(dcol[["neuron_neuropil"]], "several modules")
  item(wtc_lighten(dcol[["neuron_neuropil"]]), "one module")
  # right column: inner structure rings and the pips of c
  pos <- c(x = 15, y = 0)
  head("Structure rings, descriptive")
  line("CA1 \u2026 DG  peak region (grey text)")
  line("innermost ring: bilateral; next: \u03c4")
  ramp_y <- pos[["y"]]
  pos[["y"]] <- pos[["y"]] - 1.7
  item(ev[["supported"]], "reproducible across hemispheres")
  item(ev[["qc_context"]], "poor bilateral reproducibility")
  pos[["y"]] <- pos[["y"]] - 0.4
  head("Pips in c: top carrier also carried with")
  for (j in seq_along(WTC_SENSITIVITIES)) line(paste0(j, "  ", WTC_SENSITIVITIES[[j]]))
  ramp <- data.frame(x = 15.05 + seq(0, 3.6, length.out = 40), y = ramp_y)
  ramp$fill <- wtc_ramp(seq(0, 1, length.out = 40), WTC_TAU_HIGH)
  ticks <- data.frame(x = 15.05 + c(0, 1.8, 3.6), y = ramp_y - 0.55, label = c("0", "0.5", "1"))
  heads <- do.call(rbind, heads); boxes <- do.call(rbind, boxes); texts <- do.call(rbind, texts)
  ggplot2::ggplot() +
    ggplot2::geom_tile(data = boxes, ggplot2::aes(x, y, fill = fill, colour = border),
                       width = 0.8, height = 0.72, linewidth = nv_lw("tile_border_pt")) +
    ggplot2::geom_tile(data = ramp, ggplot2::aes(x, y, fill = fill), width = 0.1, height = 0.72) +
    ggplot2::geom_text(data = ticks, ggplot2::aes(x, y, label = label), family = fam, size = ts, vjust = 1) +
    ggplot2::annotate("text", x = 19.1, y = ramp_y, label = "\u03c4, concentration across units",
                      family = fam, size = ts, hjust = 0) +
    ggplot2::geom_text(data = texts, ggplot2::aes(x, y, label = label), family = fam, size = ts, hjust = 0) +
    ggplot2::geom_text(data = heads, ggplot2::aes(x, y, label = label), family = fam, size = hs,
                       fontface = "bold", hjust = 0) +
    ggplot2::scale_fill_identity() + ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(limits = c(-0.1, 31), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::theme_void(base_family = fam)
}

# ------------------------------------------------- composition dot matrix

# Primary-variant rows for every module and every multi-module supermodule, in wheel order.
wtc_matrix_data <- function(x) {
  themes <- wtc_themes(x$themes)
  w <- wtc_layout(x$wheel)
  d <- x$long[x$long$is_primary_variant %in% TRUE, , drop = FALSE]
  d <- d[d$level == "module" | grepl(";", d$member_modules, fixed = TRUE), , drop = FALSE]
  key <- paste(d$dataset, d$entity_id)
  is_mod <- d$level == "module"
  d$column_order <- ifelse(is_mod, match(key, paste(w$dataset, w$module_id)),
                           1000L + wtc_id_number(d$entity_id))
  if (anyNA(d$column_order)) stop("A module of the composition table is not on the wheel.", call. = FALSE)
  d$entity_label <- ifelse(is_mod, wtc_short_module(d$entity_id), d$entity_id)
  d$facet <- factor(paste(d$dataset, ifelse(is_mod, "module", "supermodule")),
                    levels = as.vector(t(outer(WTC_DATASETS, c("module", "supermodule"), paste))))
  d$column <- factor(paste(d$dataset, d$entity_id),
                     levels = unique(paste(d$dataset, d$entity_id)[order(match(d$dataset, WTC_DATASETS),
                                                                         d$column_order)]))
  d$theme_label <- factor(themes$label[match(d$theme_id, themes$theme_id)], levels = rev(themes$label))
  d[order(d$facet, d$column, d$theme_label), , drop = FALSE]
}

wtc_matrix_plot <- function(x) {
  fam <- nf_fam()
  d <- wtc_matrix_data(x)
  d <- d[d$n_overlap > 0, , drop = FALSE]
  strip <- function(v) {
    ds <- sub(" .*$", "", v)
    ifelse(grepl("supermodule$", v), "SM", wtc_compartment_label(ds))
  }
  labs_x <- stats::setNames(as.character(d$entity_label), as.character(d$column))
  navy <- nv_evidence_colours()[["supported"]]
  ggplot2::ggplot(d, ggplot2::aes(column, theme_label)) +
    ggplot2::geom_point(ggplot2::aes(size = frac_theme_in_entity, fill = fold_enrichment,
                                     colour = composition_supported,
                                     stroke = ifelse(composition_supported, 0.45, 0.2)), shape = 21) +
    ggplot2::scale_size_area(max_size = 3.3, limits = c(0, 1), breaks = c(0.1, 0.25, 0.5, 0.75),
                             labels = c("10%", "25%", "50%", "75%"),
                             name = "Share of the theme's\nproteins it holds") +
    ggplot2::scale_fill_gradient(low = "white", high = navy, transform = "log2", limits = c(1, 8),
                                 breaks = c(1, 2, 4, 8),
                                 labels = c("\u22641\u00d7", "2\u00d7", "4\u00d7", "\u22658\u00d7"),
                                 oob = scales::squish, name = "Fold enrichment") +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "black", `FALSE` = "grey72"),
                                 labels = c(`TRUE` = "carried", `FALSE` = "not carried"),
                                 breaks = c("TRUE", "FALSE"), name = NULL) +
    ggplot2::scale_x_discrete(labels = labs_x) +
    ggplot2::facet_grid(. ~ facet, scales = "free_x", space = "free_x",
                        labeller = ggplot2::as_labeller(strip)) +
    ggplot2::labs(x = NULL, y = NULL) +
    nf_theme_tile() +
    ggplot2::theme(
      text = ggplot2::element_text(family = fam),
      axis.text.x = ggplot2::element_text(size = nv_pt("axis_text_pt"), angle = 90, vjust = 0.5, hjust = 1),
      axis.text.y = ggplot2::element_text(size = nv_pt("axis_text_pt")),
      strip.text = ggplot2::element_text(size = nv_pt("axis_title_pt")),
      panel.grid.major = ggplot2::element_line(colour = "grey93", linewidth = nv_lw("reference_pt")),
      panel.spacing.x = grid::unit(1.2, "mm"),
      legend.position = "right", legend.box = "vertical",
      legend.text = ggplot2::element_text(size = nv_pt("legend_text_pt")),
      legend.title = ggplot2::element_text(size = nv_pt("legend_title_pt")),
      legend.key.size = grid::unit(2.6, "mm"))
}

# ------------------------------------------------------ theme coherence

# Per theme and compartment: the class, the carrier modules, and whether the top carrier is
# also supported in each of the five sensitivity variants (pips, in WTC_SENSITIVITIES order).
wtc_coherence_data <- function(x) {
  themes <- wtc_themes(x$themes)
  s <- x$summary
  cols <- paste0("top_carrier_supported_in__", names(WTC_SENSITIVITIES))
  if (!all(cols %in% names(s))) stop("Summary lacks the sensitivity columns.", call. = FALSE)
  wrap <- function(v) sub("-enriched ", "-\nenriched ", v, fixed = TRUE)   # keeps the column narrow
  s$compartment <- factor(wrap(wtc_compartment_label(s$dataset)),
                          levels = wrap(wtc_compartment_label(WTC_DATASETS)))
  s$theme_label <- factor(themes$label[match(s$theme_id, themes$theme_id)], levels = rev(themes$label))
  s$carrier_label <- vapply(s$carriers, function(v) paste(wtc_short_module(wtc_split(v)), collapse = " "),
                            character(1))
  s$class_label <- ifelse(s$n_carriers > 0, s$composition_class, "no carrier")
  pips <- do.call(rbind, lapply(seq_along(cols), function(j)
    data.frame(dataset = s$dataset, theme_id = s$theme_id, compartment = s$compartment,
               theme_label = s$theme_label, sensitivity = unname(WTC_SENSITIVITIES[[j]]), pip = j,
               holds = as.logical(s[[cols[[j]]]]), share = s$share_captured_by_carriers,
               stringsAsFactors = FALSE)))
  list(tiles = s, pips = pips[!is.na(pips$holds), , drop = FALSE])
}

wtc_coherence_plot <- function(x) {
  fam <- nf_fam()
  cd <- wtc_coherence_data(x)
  s <- cd$tiles
  p <- cd$pips
  navy <- nv_evidence_colours()[["supported"]]
  dark <- function(v) v >= 0.5
  p$x <- as.integer(p$compartment) + (p$pip - 3) * 0.13
  p$y <- as.integer(p$theme_label) - 0.29
  p$col <- ifelse(dark(p$share), "white", navy)
  p$shape <- ifelse(p$holds, 15L, 0L)   # filled square: also carried; open: not
  ggplot2::ggplot(s, ggplot2::aes(compartment, theme_label)) +
    ggplot2::geom_tile(ggplot2::aes(fill = share_captured_by_carriers), colour = nv_tile_border(),
                       linewidth = nv_lw("tile_border_pt")) +
    ggplot2::geom_text(ggplot2::aes(label = class_label, colour = ifelse(dark(share_captured_by_carriers),
                                                                         "white", "grey10")),
                       nudge_y = 0.24, family = fam, size = nv_size(nv_pt("annotation_pt")), fontface = "bold") +
    ggplot2::geom_text(ggplot2::aes(label = carrier_label, colour = ifelse(dark(share_captured_by_carriers),
                                                                           "white", "grey25")),
                       nudge_y = -0.01, family = fam, size = nv_size(nv_pt("annotation_pt"))) +
    ggplot2::geom_point(data = p, ggplot2::aes(x, y, colour = col, shape = shape), size = 0.75,
                        stroke = 0.3, inherit.aes = FALSE) +
    ggplot2::scale_shape_identity() +
    ggplot2::scale_fill_gradient(low = "white", high = navy, limits = c(0, 1), breaks = c(0, 0.5, 1),
                                 labels = c("0%", "50%", "100%"), na.value = WTC_EMPTY,
                                 name = "Share of the theme's\nproteins in carriers") +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(x = NULL, y = NULL) +
    nf_theme_tile() +
    ggplot2::theme(text = ggplot2::element_text(family = fam),
                   axis.text = ggplot2::element_text(size = nv_pt("axis_text_pt")),
                   legend.position = "bottom",
                   legend.text = ggplot2::element_text(size = nv_pt("legend_text_pt")),
                   legend.title = ggplot2::element_text(size = nv_pt("legend_title_pt")),
                   legend.key.height = grid::unit(2, "mm"), legend.key.width = grid::unit(6, "mm"))
}
