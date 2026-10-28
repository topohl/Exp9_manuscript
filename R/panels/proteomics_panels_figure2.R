# House final v7 Figure-2 panels.
#
# Every renderer here obeys a hard 5 pt floor at final size: hf_pt() clamps, and
# the build re-measures the emitted SVG and fails if anything slipped below.
#
# DOWNSTREAM ONLY. These reshape and label canonical values. No model, no test,
# no p-value, no new inference.

HF_MIN_PT <- 5.0

# clamp any requested point size to the floor
hf_pt <- function(x) pmax(as.numeric(x), HF_MIN_PT)
# ggplot size units from a clamped point size
hf_sz <- function(x) hv_size(hf_pt(x))

hf_fam <- function() hv_palette()$typography$family

# base theme with the floor applied to every text element
hf_theme <- function(grid = "none") {
  fam <- hf_fam()
  hv_theme(grid = grid) +
    ggplot2::theme(
      text = ggplot2::element_text(family = fam, size = HF_MIN_PT),
      axis.text = ggplot2::element_text(size = HF_MIN_PT, colour = "grey20"),
      axis.title = ggplot2::element_text(size = hf_pt(5.6)),
      legend.text = ggplot2::element_text(size = HF_MIN_PT),
      legend.title = ggplot2::element_text(size = hf_pt(5.4)),
      strip.text = ggplot2::element_text(size = hf_pt(5.4)),
      plot.caption = ggplot2::element_text(size = HF_MIN_PT, colour = "grey30",
                                           hjust = 0, lineheight = 1.2))
}

hf_theme_tile <- function() {
  hf_theme() + ggplot2::theme(axis.line = ggplot2::element_blank(),
                              axis.ticks = ggplot2::element_blank())
}

# ==========================================================================
# a. Anatomy and sampling, redesigned to TEACH the spatial grammar
# ==========================================================================
#
# Part 22 found the previous schematic encoded COMPARTMENT (by colour) but left
# REGION and LAYER purely textual, so it could not act as the key to the
# fingerprint in panel d. This version pairs the anatomical drawing with the
# 18-unit grid IN FINGERPRINT COLUMN ORDER, using the same region headers, the
# same layer abbreviations and the same separator weights. A reader can read a
# location off the drawing and find the matching column block in panel d.
#
# Compartment = glyph style (dashed band / solid band / diamond marker).
# Region      = shared header-and-separator system, not a rainbow.
# Layer       = anatomical position plus an explicit short label.
hf_schematic <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  dcol <- hv_dataset_colours()
  np <- unname(dcol[["neuron_neuropil"]])
  so_col <- unname(dcol[["neuron_soma"]])
  mg_col <- unname(dcol[["microglia"]])

  arcdf <- function(cx, cy, r, t0, t1, n = 160) {
    t <- seq(t0, t1, length.out = n)
    data.frame(x = cx + r * cos(t), y = cy + r * sin(t))
  }
  t0 <- pi * 1.06; t1 <- pi * 2.32; span <- t1 - t0
  slm_end <- t0 + 0.66 * span   # SLM exists in CA1 and CA2 only

  gap <- data.frame(x = NA_real_, y = NA_real_)
  ca  <- arcdf(0, 0, 1.00, t0, t1)
  slm <- arcdf(0, 0, 1.24, pi * 1.08, slm_end)
  sr  <- arcdf(0, 0, 1.12, pi * 1.08, pi * 2.30)
  so  <- arcdf(0, 0, 0.88, pi * 1.08, pi * 2.30)
  dgg <- rbind(arcdf(0.44, -0.24, 0.26, pi * 1.22, pi * 1.98), gap,
               arcdf(0.44, -0.24, 0.26, pi * 0.30, pi * 0.86))
  dgm <- rbind(arcdf(0.44, -0.24, 0.38, pi * 1.22, pi * 1.98), gap,
               arcdf(0.44, -0.24, 0.38, pi * 0.30, pi * 0.86))
  dgp <- arcdf(0.44, -0.24, 0.12, pi * 1.30, pi * 1.90)

  at <- function(frac, r) {
    t <- t0 + frac * span
    c(x = r * cos(t), y = r * sin(t))
  }
  # region brackets drawn ON the arc, the same device as the fingerprint headers
  reg <- data.frame(
    region = c("CA1", "CA2", "CA3"),
    f0 = c(0.00, 0.30, 0.64), f1 = c(0.28, 0.62, 1.00),
    stringsAsFactors = FALSE)
  brk <- do.call(rbind, lapply(seq_len(nrow(reg)), function(i) {
    a <- arcdf(0, 0, 1.42, t0 + reg$f0[i] * span, t0 + reg$f1[i] * span, 40)
    a$region <- reg$region[i]; a
  }))
  rlab <- do.call(rbind, lapply(seq_len(nrow(reg)), function(i) {
    v <- at(mean(c(reg$f0[i], reg$f1[i])), 1.52)
    data.frame(x = v[["x"]], y = v[["y"]], l = reg$region[i], stringsAsFactors = FALSE)
  }))

  p <- ggplot2::ggplot() +
    ggplot2::geom_path(data = slm, ggplot2::aes(x, y), colour = np,
                       linewidth = 0.42, linetype = "22", lineend = "round") +
    ggplot2::geom_path(data = sr, ggplot2::aes(x, y), colour = np,
                       linewidth = 0.42, linetype = "22", lineend = "round") +
    ggplot2::geom_path(data = so, ggplot2::aes(x, y), colour = np,
                       linewidth = 0.42, linetype = "22", lineend = "round") +
    ggplot2::geom_path(data = dgm, ggplot2::aes(x, y), colour = np,
                       linewidth = 0.42, linetype = "22", lineend = "round") +
    ggplot2::geom_path(data = dgp, ggplot2::aes(x, y), colour = np,
                       linewidth = 0.42, linetype = "22", lineend = "round") +
    ggplot2::geom_path(data = ca, ggplot2::aes(x, y), colour = so_col,
                       linewidth = 1.5, lineend = "round") +
    ggplot2::geom_path(data = dgg, ggplot2::aes(x, y), colour = so_col,
                       linewidth = 1.2, lineend = "round") +
    ggplot2::geom_path(data = brk, ggplot2::aes(x, y, group = region),
                       colour = "grey55", linewidth = 0.3) +
    ggplot2::geom_text(data = rlab, ggplot2::aes(x, y, label = l), family = fam,
                       size = hf_sz(5.8), fontface = "bold", colour = "grey12")

  # DG bracket and label
  dgb <- arcdf(0.44, -0.24, 0.56, pi * 1.18, pi * 0.30, 40)
  p <- p +
    ggplot2::geom_path(data = dgb, ggplot2::aes(x, y), colour = "grey55",
                       linewidth = 0.3) +
    ggplot2::annotate("text", x = 0.44, y = -0.95, label = "DG", family = fam,
                      size = hf_sz(5.8), fontface = "bold", colour = "grey12")

  # microglia-enriched ROI: one diamond per region
  mg <- do.call(rbind, lapply(c(0.12, 0.46, 0.84), function(f) {
    v <- at(f, 0.70); data.frame(x = v[["x"]], y = v[["y"]])
  }))
  mg <- rbind(mg, data.frame(x = 0.44, y = -0.58))
  p <- p + ggplot2::geom_point(data = mg, ggplot2::aes(x, y), shape = 18,
                               size = 1.15, colour = mg_col)

  # Layer abbreviations live in the key beside this drawing, so repeating them
  # on the arcs only creates collisions at 86 mm. The drawing carries REGION,
  # which is the anchor a reader needs to find a block in panel d.
  p <- p +
    ggplot2::annotate("text", x = 0, y = 1.88, label = "both hemispheres",
                      family = fam, size = hf_sz(5.0), colour = "grey30")

  drawing <- p +
    ggplot2::coord_equal(xlim = c(-1.95, 1.95), ylim = c(-1.70, 2.02),
                         expand = FALSE) +
    ggplot2::theme_void(base_family = fam)

  # ---- compartment / region / layer key --------------------------------
  #
  # An 18-cell grid cannot hold 5 pt tokens in 46 mm, and it would duplicate the
  # header strip panel d already carries. The key states the grammar in words
  # instead: one line per compartment, giving its glyph, its spatial resolution
  # and the exact abbreviations used everywhere else in the figure.
  kd <- data.frame(
    row = 3:1,
    style = c("neuropil", "soma", "microglia"),
    name = c("Neuropil", "Neuronal soma", "Microglia-enriched ROI"),
    res = c("region x layer", "region", "region"),
    detail = c("SO, SR, SLM in CA1 and CA2; SO, SR in CA3; MO, PO in DG",
               "SP in CA1, CA2, CA3; SG in DG",
               "CA1, CA2, CA3, DG"),
    n = c(10L, 4L, 4L), stringsAsFactors = FALSE)
  kcol <- c(neuropil = np, soma = so_col, microglia = mg_col)
  key <- ggplot2::ggplot(kd) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = 0.075, y = row, yend = row,
                                       colour = style),
                          linewidth = ifelse(kd$style == "soma", 1.4, 0.5),
                          linetype = ifelse(kd$style == "neuropil", "22", "solid"),
                          lineend = "round") +
    ggplot2::geom_point(data = kd[kd$style == "microglia", ],
                        ggplot2::aes(x = 0.037, y = row), shape = 18,
                        size = 1.3, colour = mg_col) +
    ggplot2::geom_text(ggplot2::aes(0.105, row + 0.24,
                                    label = sprintf("%s  -  %d units, %s", name, n, res)),
                       hjust = 0, family = fam, size = hf_sz(5.2),
                       fontface = "bold", colour = "grey12") +
    ggplot2::geom_text(ggplot2::aes(0.105, row - 0.18, label = detail),
                       hjust = 0, family = fam, size = hf_sz(5.0),
                       colour = "grey35") +
    ggplot2::scale_colour_manual(values = kcol, guide = "none") +
    ggplot2::coord_cartesian(xlim = c(-0.02, 1.62), ylim = c(0.25, 3.95),
                             expand = FALSE) +
    ggplot2::labs(caption = "abbreviations and order as in d") +
    ggplot2::theme_void(base_family = fam) +
    ggplot2::theme(plot.caption = ggplot2::element_text(size = HF_MIN_PT,
                                                        colour = "grey45",
                                                        hjust = 0),
                   plot.margin = ggplot2::margin(1, 1, 0, 1, "mm"))

  pl <- patchwork::wrap_plots(drawing, key, ncol = 2, widths = c(0.40, 0.60))

  out <- sg_order_for()
  out$drawn_as <- ifelse(out$dataset == "neuron_neuropil", "dashed neuropil band",
                  ifelse(out$dataset == "neuron_soma", "solid somatic band",
                         "ROI diamond"))
  out$note <- paste0(
    "declared vector schematic, no histology fabricated. Compartment is glyph ",
    "style, region is the shared bracket/header system, layer is anatomical ",
    "position plus a short label. DG MO and DG PO are neuropil. SLM exists in ",
    "CA1 and CA2 only; there is no CA3 SLM unit.")
  write_csv_safe(out, csv_path)
  hv_save_panel(pl, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# ==========================================================================
# b. Proteome depth - the least dominant main panel
# ==========================================================================
hf_depth_compact <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  d <- hv_read_csv(repo_path(panel$primary_source))
  cn <- intersect(c("Proteins.Identified", "n_proteins", "proteins_identified"),
                  names(d))[1]
  if (is.na(cn)) stop("hf_depth_compact: no protein-count column", call. = FALSE)
  d$val <- as.numeric(d[[cn]])
  # this source names the compartment in celltype_layer / qc_compartment, not
  # in a `dataset` column; excluded runs must not count toward depth
  ccol <- intersect(c("celltype_layer", "qc_compartment", "dataset"), names(d))[1]
  if (is.na(ccol)) stop("hf_depth_compact: no compartment column", call. = FALSE)
  map <- c(neuron_neuropil = "neuron_neuropil", neuropil = "neuron_neuropil",
           neuron_soma = "neuron_soma", soma = "neuron_soma",
           microglia = "microglia")
  d$dataset <- unname(map[as.character(d[[ccol]])])
  if ("exclude" %in% names(d)) d <- d[!(d$exclude %in% TRUE), , drop = FALSE]
  d <- d[!is.na(d$dataset), , drop = FALSE]
  d <- d[is.finite(d$val), , drop = FALSE]
  d$lab <- factor(sg_compartment_label(d$dataset),
                  levels = sg_compartments()$short)
  d <- d[!is.na(d$lab), , drop = FALSE]
  d <- d[order(d$lab, d$val), , drop = FALSE]
  d$off <- (stats::ave(seq_len(nrow(d)), d$lab, FUN = seq_along) %% 5 - 2) * 0.07

  p <- ggplot2::ggplot(d, ggplot2::aes(as.integer(lab) + off, val)) +
    ggplot2::stat_summary(ggplot2::aes(x = as.integer(lab)), fun = stats::median,
                          geom = "crossbar", width = 0.55, linewidth = 0.22,
                          colour = "grey45") +
    ggplot2::geom_point(ggplot2::aes(colour = lab), size = 0.5, alpha = 0.8) +
    ggplot2::scale_colour_manual(
      values = stats::setNames(unname(hv_dataset_colours()[sg_compartment_levels()]),
                               sg_compartments()$short), guide = "none") +
    ggplot2::scale_x_continuous(breaks = 1:3, labels = sg_compartments()$short,
                                limits = c(0.5, 3.5)) +
    ggplot2::labs(x = NULL, y = "proteins identified") +
    hf_theme(grid = "y") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  write_csv_safe(d[, c("dataset", "val")], csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# ==========================================================================
# c. PCA, simplified for the main figure
# ==========================================================================
#
# Part 22 measured two defects in the previous main PCA: a 20 x 15 mm block of
# exactly 0.0% ink, and a legend line terminating exactly on the panel boundary.
# Both came from a right-hand legend competing with the data for a 42 mm box.
# 2026-10-27 (reviewer: colour by subgroup): region is colour (palette region
# block) and compartment is shape, so each region x compartment cluster can be
# named; the box was widened for the two keys.
hf_pca_compact <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  d <- hv_read_csv(repo_path(panel$primary_source))
  ve <- hv_read_csv(repo_path(as.character(unlist(panel$input_dependencies))[1]),
                    required = FALSE)
  pc <- function(k) {
    if (is.null(ve)) return(sprintf("PC%d", k))
    v <- ve$variance_explained[ve$PC == paste0("PC", k)]
    if (!length(v)) return(sprintf("PC%d", k))
    sprintf("PC%d (%.0f%%)", k, 100 * v[1])
  }
  ds <- sg_compartment_levels()
  d <- d[d$dataset %in% ds, , drop = FALSE]
  d$lab <- factor(sg_compartment_label(d$dataset), levels = sg_compartments()$short)
  regions <- sg_region_levels()
  d$region_lab <- factor(toupper(as.character(d$region)), levels = toupper(regions))
  if (anyNA(d$region_lab)) stop("hf_pca_compact: unknown region in ", panel$primary_source, call. = FALSE)
  d <- d[order(d$lab, d$region_lab, d$Sample), , drop = FALSE]
  shapes <- stats::setNames(c(16, 17, 15)[seq_along(ds)], sg_compartments()$short)
  # 2026-10-28: the anatomical groups the two axes separate are named in the
  # panel, as in the hand-edited figure. A label sits beside its group, just
  # outside the 3rd-97th percentile extent of its members on the given side
  # (side: r, l = right, left; t, b = above, below; br = below, right-aligned; rt = right, at the top).
  lay <- tolower(as.character(d$layer))
  grp <- list(
    list(label = "DG soma", keep = d$dataset == "neuron_soma" & d$region_lab == "DG", side = "br"),
    list(label = "CA soma", keep = d$dataset == "neuron_soma" & d$region_lab != "DG", side = "t"),
    list(label = "DG MO,\nmicroglia ROI", keep = d$region_lab == "DG" & (lay == "mo" | d$dataset == "microglia"),
         side = "r"),
    list(label = "SLM", keep = lay == "slm", side = "t"),
    list(label = "DG PO", keep = lay == "po", side = "r"),
    list(label = "Microglia ROI", keep = d$dataset == "microglia" & d$region_lab %in% c("CA1", "CA2"),
         side = "rt"),
    list(label = "SO, SR", keep = lay %in% c("so", "sr") | (d$dataset == "microglia" & d$region_lab == "CA3"),
         side = "r"))
  rx <- diff(range(d$PC1)); ry <- diff(range(d$PC2))
  q <- function(v, p) stats::quantile(v, p, names = FALSE)
  glab <- do.call(rbind, lapply(grp, function(g) {
    if (!any(g$keep)) return(NULL)
    x <- d$PC1[g$keep]; y <- d$PC2[g$keep]
    switch(g$side,
           r = data.frame(label = g$label, x = q(x, 0.97) + 0.04 * rx, y = stats::median(y), hj = 0, vj = 0.5),
           l = data.frame(label = g$label, x = q(x, 0.03) - 0.05 * rx, y = stats::median(y), hj = 1, vj = 0.5),
           t = data.frame(label = g$label, x = stats::median(x), y = q(y, 0.97) + 0.04 * ry, hj = 0.5, vj = 0),
           b = data.frame(label = g$label, x = stats::median(x), y = q(y, 0.03) - 0.04 * ry, hj = 0.5, vj = 1),
           br = data.frame(label = g$label, x = q(x, 0.97), y = q(y, 0.03) - 0.04 * ry, hj = 1, vj = 1),
           rt = data.frame(label = g$label, x = q(x, 0.97) + 0.04 * rx, y = q(y, 0.97), hj = 0, vj = 0.5))
  }))

  p <- ggplot2::ggplot(d, ggplot2::aes(PC1, PC2)) +
    # symbols large enough that compartment (shape) reads at print size; the region key is a swatch
    # from a second, invisible layer, so no key glyph repeats a compartment shape
    ggplot2::geom_point(ggplot2::aes(colour = region_lab, shape = lab), size = 0.85,
                        stroke = 0, alpha = 0.75, show.legend = c(colour = FALSE, shape = TRUE)) +
    ggplot2::geom_point(ggplot2::aes(colour = region_lab), alpha = 0, key_glyph = "rect",
                        show.legend = c(colour = TRUE, shape = FALSE)) +
    ggplot2::geom_text(data = glab, ggplot2::aes(x, y, label = label, hjust = hj, vjust = vj), family = fam,
                       size = hf_sz(HF_MIN_PT), colour = "black", lineheight = 0.9) +
    ggplot2::scale_colour_manual(values = hv_region_colours()[levels(d$region_lab)], name = "Region") +
    ggplot2::scale_shape_manual(values = shapes, name = "Compartment") +
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1, order = 1, override.aes = list(alpha = 1)),
                    shape = ggplot2::guide_legend(ncol = 1, order = 2, override.aes = list(size = 1.1, colour = "grey30",
                                                                                         alpha = 1))) +
    # typographic minus on both PC axes (2026-10-07), as on every v9 axis
    ggplot2::scale_x_continuous(labels = hv_minus_labels) +
    ggplot2::scale_y_continuous(labels = hv_minus_labels) +
    ggplot2::labs(x = pc(1), y = pc(2)) +
    hf_theme() +
    ggplot2::theme(
      # a scatter is drawn square (house style); the key goes to the side so
      # the square can use the box's height
      aspect.ratio = 1,
      axis.text = ggplot2::element_text(colour = "black"),
      legend.position = "right",
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(0, 0, 0, -1),
      legend.key.size = ggplot2::unit(2.2, "mm"),
      legend.title = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
      legend.text = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
      plot.margin = ggplot2::margin(1, 1, 1, 1, "mm"))
  write_csv_safe(d[, intersect(c("Sample", "PC1", "PC2", "dataset", "region", "layer", "region_layer",
                                 "animal_id"), names(d))], csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# ==========================================================================
# d. Baseline spatial molecular fingerprint - the dominant Figure-2 panel
# ==========================================================================
#
# Same phenotype-blind selection as Part 21: rows are the top genes of each
# prespecified CON-only anatomical contrast. Values are within-protein CON z.
# Gene labels are held at the 5 pt floor.
hf_fingerprint <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  z <- hv_read_csv(repo_path(panel$primary_source))
  ord <- unique(z[, c("gene", "contrast")])
  ord <- ord[order(ord$contrast, ord$gene), , drop = FALSE]
  ord <- ord[!duplicated(ord$gene), , drop = FALSE]
  lev <- ord$gene
  z$sg_unit <- sg_resolve_unit(z$spatial_unit, z$dataset)
  b <- sg_blocks(z$sg_unit, z$dataset)
  o <- b$order
  z$xpos <- match(paste(z$dataset, z$sg_unit), paste(o$dataset, o$unit))
  z$ypos <- match(z$gene, rev(lev))
  n <- nrow(o); ny <- length(lev)
  y_comp <- ny + 2.3; y_reg <- ny + 1.05
  lim <- max(abs(z$con_z), na.rm = TRUE) * c(-1, 1)

  p <- ggplot2::ggplot(z, ggplot2::aes(xpos, ypos)) +
    ggplot2::geom_tile(ggplot2::aes(fill = con_z), colour = "white",
                       linewidth = 0.1) +
    hv_diverging(limits = lim, name = "CON z") +
    ggplot2::scale_x_continuous(breaks = seq_len(n),
                                labels = sg_axis_labels(b),
                                limits = c(0.5, n + 0.5), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = seq_len(ny), labels = rev(lev),
                                limits = c(0.5, y_comp + 0.9), expand = c(0, 0)) +
    ggplot2::annotate("segment", x = b$compartment$start - 0.5,
                      xend = b$compartment$end + 0.5,
                      y = y_comp - 0.32, yend = y_comp - 0.32,
                      linewidth = 0.45, colour = "grey25") +
    ggplot2::annotate("text", x = b$compartment$mid, y = y_comp,
                      label = b$compartment$label, family = fam,
                      size = hf_sz(5.4), fontface = "bold", colour = "grey12") +
    ggplot2::annotate("text", x = b$region$mid, y = y_reg, label = b$region$label,
                      family = fam, size = hf_sz(5.0), colour = "grey30") +
    ggplot2::labs(x = NULL, y = NULL,
                  caption = paste0(
                    "CON animals only (n = 3). Genes are the top hits of each ",
                    "prespecified CON-only anatomical contrast; no stress ",
                    "information enters selection. Each gene is standardised ",
                    "across its own compartment.")) +
    hf_theme_tile() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = HF_MIN_PT, colour = "grey25"),
      axis.text.y = ggplot2::element_text(size = HF_MIN_PT, face = "italic"),
      legend.position = "right",
      legend.key.width = ggplot2::unit(1.6, "mm"),
      legend.key.height = ggplot2::unit(3.6, "mm"))
  cend <- utils::head(b$compartment$end, -1)
  rend <- setdiff(utils::head(b$region$end, -1), cend)
  if (length(cend)) p <- p + ggplot2::annotate(
    "segment", x = cend + 0.5, xend = cend + 0.5, y = 0.5, yend = y_comp - 0.32,
    linewidth = 0.42, colour = "grey25")
  if (length(rend)) p <- p + ggplot2::annotate(
    "segment", x = rend + 0.5, xend = rend + 0.5, y = 0.5, yend = y_reg + 0.4,
    linewidth = 0.18, colour = "grey72")
  write_csv_safe(z, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# ==========================================================================
# e. Compartment identity - given real width this time
# ==========================================================================
#
# Part 22 found the previous version had rotated axis labels escaping the panel
# box and a legend consuming a quarter of 39 mm. Here the compartment labels are
# horizontal, the legend sits at the bottom, and the box is 78 mm.
hf_compartment <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  d <- hv_read_csv(repo_path(panel$primary_source))
  mcol <- intersect(c("marker_label", "marker_gene", "marker", "GeneSymbol",
                      "gene"), names(d))[1]
  ccol <- intersect(c("compartment", "dataset", "celltype_layer",
                      "qc_compartment"), names(d))[1]
  vcol <- intersect(c("displayed_centered_log2", "median_centered_log2",
                      "z", "value"), names(d))[1]
  if (any(is.na(c(mcol, ccol, vcol)))) {
    stop("hf_compartment: cannot locate marker/compartment/value columns; have: ",
         paste(names(d), collapse = ", "), call. = FALSE)
  }
  map <- c(neuron_neuropil = "neuron_neuropil", neuropil = "neuron_neuropil",
           neuron_soma = "neuron_soma", soma = "neuron_soma",
           microglia = "microglia")
  d$ds <- unname(map[as.character(d[[ccol]])])
  d <- d[!is.na(d$ds), , drop = FALSE]
  d$marker <- as.character(d[[mcol]])
  d$val <- as.numeric(d[[vcol]])
  d$comp <- factor(sg_compartment_label(d$ds), levels = sg_compartments()$short)
  mk <- unique(d$marker)
  d$ypos <- match(d$marker, rev(mk))
  d$xpos <- as.integer(d$comp)
  lim <- max(abs(d$val), na.rm = TRUE) * c(-1, 1)

  p <- ggplot2::ggplot(d, ggplot2::aes(xpos, ypos)) +
    ggplot2::geom_tile(ggplot2::aes(fill = val), colour = "white",
                       linewidth = 0.15) +
    hv_diverging(limits = lim, name = NULL) +
    ggplot2::scale_x_continuous(breaks = 1:3, labels = sg_compartments()$short,
                                limits = c(0.5, 3.5), expand = c(0, 0),
                                position = "top") +
    ggplot2::scale_y_continuous(breaks = seq_along(mk), labels = rev(mk),
                                limits = c(0.5, length(mk) + 0.5),
                                expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL) +
    hf_theme_tile() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = HF_MIN_PT, colour = "grey15",
                                          face = "bold"),
      axis.text.y = ggplot2::element_text(size = HF_MIN_PT, face = "italic"),
      legend.position = "bottom",
      legend.key.height = ggplot2::unit(1.6, "mm"),
      legend.key.width = ggplot2::unit(5, "mm"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(-2, 0, 0, 0))
  write_csv_safe(d[, c("marker", "ds", "val")], csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# ==========================================================================
# f. Bilateral reproducibility - MAIN version
# ==========================================================================
#
# Main carries the story: coarse regional identity reproduces strongly, fine
# laminar identity less so, with an explicit block separator between the two
# anatomical levels. The complete metric inventory (Pearson, Spearman, sign
# agreement, ICC, precision) stays in ED1 - this panel is NOT that panel.
hf_bilateral_main <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  su <- hv_read_csv(repo_path(as.character(unlist(panel$input_dependencies))[1]))
  rc <- intersect(c("pearson_r", "r", "correlation", "bilateral_pearson"),
                  names(su))[1]
  cc <- intersect(c("contrast", "anatomical_contrast", "comparison"), names(su))[1]
  if (any(is.na(c(rc, cc)))) {
    stop("hf_bilateral_main: cannot locate r/contrast columns; have: ",
         paste(names(su), collapse = ", "), call. = FALSE)
  }
  su$r <- as.numeric(su[[rc]])
  su$contrast <- as.character(su[[cc]])
  su <- su[is.finite(su$r), , drop = FALSE]
  # anatomical level from the stored domain where available, else the contrast
  dcol <- intersect(c("validation_domain", "anatomical_level", "level"),
                    names(su))[1]
  su$level <- if (!is.na(dcol)) {
    ifelse(grepl("strat|lamin|layer", su[[dcol]], ignore.case = TRUE),
           "Fine / laminar identity", "Regional identity")
  } else {
    ifelse(grepl("_strata$|_DG_layers$", su$contrast),
           "Fine / laminar identity", "Regional identity")
  }
  su$level <- factor(su$level,
                     levels = c("Regional identity", "Fine / laminar identity"))
  su <- su[order(su$level, -su$r), , drop = FALSE]
  su$lab <- gsub("_", " ", su$contrast)
  su$lab <- sub(" vs mean other", " vs rest", su$lab)
  su$ypos <- rev(seq_len(nrow(su)))

  p <- ggplot2::ggplot(su, ggplot2::aes(r, ypos)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = r, yend = ypos,
                                       colour = level),
                          linewidth = hv_lw("reference_pt")) +
    ggplot2::geom_point(ggplot2::aes(colour = level), size = 1) +
    ggplot2::scale_colour_manual(
      values = c("Regional identity" = "#1F3D52",
                 "Fine / laminar identity" = "#C2A878"), name = NULL) +
    ggplot2::scale_y_continuous(breaks = su$ypos, labels = su$lab,
                                expand = ggplot2::expansion(add = 0.8)) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    ggplot2::labs(x = "left-right correlation of the anatomical effect",
                  y = NULL) +
    hf_theme(grid = "x") +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = HF_MIN_PT),
                   legend.position = "bottom",
                   legend.key.size = ggplot2::unit(2.4, "mm"),
                   legend.margin = ggplot2::margin(0, 0, 0, 0),
                   legend.box.margin = ggplot2::margin(-2, 0, 0, 0))
  # explicit rule between the two anatomical levels
  nreg <- sum(su$level == "Regional identity")
  if (nreg > 0 && nreg < nrow(su)) {
    yb <- rev(seq_len(nrow(su)))[nreg] - 0.5
    p <- p + ggplot2::annotate("segment", x = -Inf, xend = Inf, y = yb, yend = yb,
                               linewidth = 0.3, colour = "grey55")
  }
  write_csv_safe(su[, c("contrast", "level", "r")], csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# ==========================================================================
# g. External validation - MAIN version (expected pairings only)
# ==========================================================================
#
# Main shows only the story-driving expected pairings, blocked by anatomical
# level. The complete specificity inventory - every off-target comparison -
# stays in ED2, so the two are not the same graphic.
hf_external_main <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  k <- hv_read_csv(repo_path(panel$primary_source))
  k <- k[k$expected_match %in% TRUE, , drop = FALSE]
  k$level <- ifelse(grepl("strata", k$validation_domain),
                    "CA1 laminar identity", "Regional identity")
  k$level <- factor(k$level, levels = c("Regional identity", "CA1 laminar identity"))
  k$sig <- is.finite(k$p_adjust) & k$p_adjust < 0.05
  k$lab <- paste0(gsub("_", " ", k$internal_contrast), "  ->  ", k$external_signature)
  k <- k[order(k$level, k$NES), , drop = FALSE]
  k$ypos <- seq_len(nrow(k))

  p <- ggplot2::ggplot(k, ggplot2::aes(NES, ypos)) +
    ggplot2::geom_vline(xintercept = 0, linewidth = hv_lw("reference_pt"),
                        colour = "grey65") +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = NES, yend = ypos,
                                       colour = level),
                          linewidth = hv_lw("reference_pt")) +
    ggplot2::geom_point(ggplot2::aes(colour = level, shape = sig), size = 1.1,
                        fill = "white", stroke = 0.4) +
    ggplot2::scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 21),
                                guide = "none") +
    ggplot2::scale_colour_manual(
      values = c("Regional identity" = "#1F3D52",
                 "CA1 laminar identity" = "#C2A878"), name = NULL) +
    ggplot2::scale_y_continuous(breaks = k$ypos, labels = k$lab,
                                expand = ggplot2::expansion(add = 0.8)) +
    ggplot2::labs(x = "NES vs external hippocampal signature", y = NULL,
                  caption = "expected pairings only; filled = FDR < 0.05. Full specificity inventory in ED2.") +
    hf_theme(grid = "x") +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = HF_MIN_PT),
                   legend.position = "none")
  nreg <- sum(k$level == "Regional identity")
  if (nreg > 0 && nreg < nrow(k)) {
    p <- p + ggplot2::annotate("segment", x = -Inf, xend = Inf,
                               y = nreg + 0.5, yend = nreg + 0.5,
                               linewidth = 0.3, colour = "grey55")
  }
  write_csv_safe(k[, c("internal_contrast", "external_signature", "level",
                       "NES", "p_adjust")], csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# ==========================================================================
# h. Internal anatomical program validation - MAIN version
# ==========================================================================
#
# Main shows the single strongest canonical term per contrast so the panel fits
# at >=5 pt. The complete term inventory stays in ED2.
hf_internal_main <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  g <- hv_read_csv(repo_path(panel$primary_source))
  g$level <- ifelse(grepl("CA1_strata", g$contrast),
                    "CA1 laminar identity", "Regional identity")
  g$level <- factor(g$level, levels = c("Regional identity", "CA1 laminar identity"))
  # one term per contrast: the largest |NES|, deterministic on ties
  g <- g[order(g$contrast, -abs(g$NES), g$Description), , drop = FALSE]
  g <- g[!duplicated(g$contrast), , drop = FALSE]
  g <- g[order(g$level, g$NES), , drop = FALSE]
  g$ypos <- seq_len(nrow(g))
  g$lab <- paste0(gsub("_", " ", g$contrast), "  ->  ", g$Description)
  g$sig <- is.finite(g$p_adjust) & g$p_adjust < 0.05

  p <- ggplot2::ggplot(g, ggplot2::aes(NES, ypos)) +
    ggplot2::geom_vline(xintercept = 0, linewidth = hv_lw("reference_pt"),
                        colour = "grey65") +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = NES, yend = ypos,
                                       colour = level),
                          linewidth = hv_lw("reference_pt")) +
    ggplot2::geom_point(ggplot2::aes(colour = level, size = setSize,
                                     shape = sig), fill = "white", stroke = 0.4) +
    ggplot2::scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 21),
                                guide = "none") +
    ggplot2::scale_size_continuous(range = c(0.6, 1.8), guide = "none") +
    ggplot2::scale_colour_manual(
      values = c("Regional identity" = "#1F3D52",
                 "CA1 laminar identity" = "#C2A878"), name = NULL) +
    ggplot2::scale_y_continuous(breaks = g$ypos, labels = g$lab,
                                expand = ggplot2::expansion(add = 0.8)) +
    ggplot2::labs(x = "NES, internal anatomical GO program", y = NULL,
                  caption = "strongest canonical term per contrast; point size = set size. Full term inventory in ED2.") +
    hf_theme(grid = "x") +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = HF_MIN_PT),
                   legend.position = "none")
  nreg <- sum(g$level == "Regional identity")
  if (nreg > 0 && nreg < nrow(g)) {
    p <- p + ggplot2::annotate("segment", x = -Inf, xend = Inf,
                               y = nreg + 0.5, yend = nreg + 0.5,
                               linewidth = 0.3, colour = "grey55")
  }
  write_csv_safe(g[, c("contrast", "Description", "level", "NES", "p_adjust",
                       "setSize")], csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# ==========================================================================
# a. Space for the sampling schematic, drawn by hand (2026-10-27)
# ==========================================================================
#
# The author draws the workflow schematic in Illustrator; the figure keeps an
# empty box of the declared size for it, as Figure 3c does.
hf_schematic_space <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  p <- ggplot2::ggplot() + ggplot2::theme_void()
  write_csv_safe(data.frame(panel = panel$id,
                            note = "space for a schematic drawn by hand; no data"), csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# ==========================================================================
# d. Reference marker classes in every acquisition (2026-10-27; control
#    animals and compartment comparisons 2026-10-28)
# ==========================================================================
#
# Marker classes from external references (Allen cell types; GO, MGI; SynGO),
# scored per acquisition on the joint cross-compartment scale by pRoteomics
# (score_compartment_markers). Only the animals of panel$groups (control) are
# drawn. Points are acquisitions, the larger marks the animal means, the bar
# their mean. The brackets show the significant compartment comparisons
# pRoteomics computed on the same animal means (repeated-measures linear model,
# BH); this renderer only draws them.
hf_marker_classes <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  deps <- as.character(unlist(panel$input_dependencies))
  acq <- hv_read_csv(repo_path(panel$primary_source))
  ani <- hv_read_csv(repo_path(deps[1]))
  ct <- hv_read_csv(repo_path(deps[2]))
  grp <- as.character(unlist(panel$groups %||% "CON"))
  acq <- acq[acq$group %in% grp, , drop = FALSE]
  ani <- ani[ani$group %in% grp, , drop = FALSE]
  if (!nrow(acq) || !nrow(ani)) stop("hf_marker_classes: no acquisitions of ", paste(grp, collapse = ", "), call. = FALSE)
  if (!setequal(unique(ct$group), grp) || !setequal(unique(ct$n_animals), length(unique(ani$AnimalID))))
    stop("hf_marker_classes: the comparisons were not computed on the animals drawn", call. = FALSE)
  ds <- sg_compartment_levels()
  lab <- stats::setNames(sg_compartments()$short, ds)
  cls <- c("Soma markers", "Neuropil markers", "Microglia/PVM markers")
  # plain two-line headers (2026-10-28); the references and class sizes are in the legend
  nmk <- tapply(acq$n_markers_in_class, acq$marker_class, max)
  strip <- stats::setNames(sub(" markers$", "\nmarkers", cls), cls)
  if (!setequal(unique(acq$marker_class), cls) || !setequal(unique(ct$marker_class), cls))
    stop("hf_marker_classes: unexpected marker classes in ", panel$primary_source, call. = FALSE)
  for (nm in c("acq", "ani", "ct")) {
    x <- get(nm)
    x$cls <- factor(strip[as.character(x$marker_class)], levels = strip[cls])
    if ("dataset" %in% names(x)) x$comp <- factor(lab[as.character(x$dataset)], levels = lab)
    assign(nm, x)
  }
  acq <- acq[is.finite(acq$score_log2_vs_typical_protein), , drop = FALSE]
  # deterministic horizontal spread: rank within compartment x class, no RNG
  acq <- acq[order(acq$cls, acq$comp, acq$AnimalID, acq$Sample), , drop = FALSE]
  acq$k <- stats::ave(seq_len(nrow(acq)), acq$cls, acq$comp, FUN = seq_along)
  acq$n <- stats::ave(seq_len(nrow(acq)), acq$cls, acq$comp, FUN = length)
  acq$x <- as.numeric(acq$comp) + ((acq$k - 0.5) / acq$n - 0.5) * 0.55
  mm <- stats::aggregate(animal_mean_score ~ cls + comp, data = ani, FUN = mean)
  # the animal means side by side in a fixed animal order, inside the bar
  aid <- sort(unique(ani$AnimalID))
  ani$x <- as.numeric(ani$comp) + (match(ani$AnimalID, aid) - (length(aid) + 1) / 2) * 0.1
  cols <- stats::setNames(unname(hv_dataset_colours()[ds]), lab)
  dark <- vapply(cols, hv_darken, character(1), amount = 0.35)

  # brackets mark the significant comparisons only (adjusted P < 0.05, 2026-10-28); every comparison stays in the
  # sidecar. Within a marker class each bracket gets its own level, neighbouring pairs below the outer pair.
  ct$xa <- match(as.character(ct$compartment_a), ds); ct$xb <- match(as.character(ct$compartment_b), ds)
  ct$lo <- pmin(ct$xa, ct$xb); ct$hi <- pmax(ct$xa, ct$xb)
  sig <- ct[ct$p_adjust < 0.05, , drop = FALSE]
  sig <- sig[order(sig$cls, sig$hi - sig$lo, sig$lo), , drop = FALSE]
  sig$level <- stats::ave(seq_len(nrow(sig)), sig$cls, FUN = seq_along)
  rng <- diff(range(acq$score_log2_vs_typical_protein))
  # each level tall enough for its label (about 0.22 of the data range at this box size)
  step <- 0.22 * rng
  sig$y <- max(acq$score_log2_vs_typical_protein) + step * (sig$level - 0.45)
  sig$tick <- 0.03 * rng
  sig$label <- ifelse(sig$p_adjust < 0.001, "italic(P)*'<0.001'",
                      sprintf("italic(P)*'=%s'", formatC(signif(sig$p_adjust, 2), format = "fg", digits = 2)))
  brk <- rbind(data.frame(cls = sig$cls, x = sig$lo + 0.06, xend = sig$hi - 0.06, y = sig$y, yend = sig$y),
               data.frame(cls = sig$cls, x = sig$lo + 0.06, xend = sig$lo + 0.06, y = sig$y, yend = sig$y - sig$tick),
               data.frame(cls = sig$cls, x = sig$hi - 0.06, xend = sig$hi - 0.06, y = sig$y, yend = sig$y - sig$tick))
  top <- if (nrow(sig)) max(sig$y) + 0.2 * rng else max(acq$score_log2_vs_typical_protein)

  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, linewidth = hv_lw("reference_pt"), colour = "grey80") +
    ggplot2::geom_point(data = acq, ggplot2::aes(x, score_log2_vs_typical_protein, colour = comp),
                        size = 0.35, stroke = 0, alpha = 0.45) +
    ggplot2::geom_point(data = ani, ggplot2::aes(x, animal_mean_score),
                        colour = unname(dark[as.character(ani$comp)]), size = 0.8, stroke = 0) +
    ggplot2::geom_errorbar(data = mm, ggplot2::aes(as.numeric(comp), ymin = animal_mean_score,
                                                   ymax = animal_mean_score),
                           width = 0.5, linewidth = hv_lw("data_pt"), colour = "black") +
    ggplot2::geom_segment(data = brk, ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          linewidth = hv_lw("reference_pt"), colour = "black") +
    ggplot2::geom_text(data = sig, ggplot2::aes((lo + hi) / 2, y + 0.02 * rng, label = label), parse = TRUE,
                       vjust = 0, family = fam, size = hf_sz(HF_MIN_PT), colour = "black") +
    # headroom for the top label, which is drawn above its anchor
    ggplot2::geom_blank(data = data.frame(cls = ct$cls, y = top), ggplot2::aes(2, y)) +
    ggplot2::scale_colour_manual(values = cols, guide = "none") +
    ggplot2::scale_x_continuous(breaks = seq_along(lab), labels = lab,
                                expand = ggplot2::expansion(add = 0.45)) +
    # ticks only over the data, not over the brackets
    ggplot2::scale_y_continuous(breaks = function(l) {
      b <- pretty(range(acq$score_log2_vs_typical_protein))
      b[b >= min(acq$score_log2_vs_typical_protein) - 0.5 & b <= max(acq$score_log2_vs_typical_protein) + 0.5]
    }, labels = hv_minus_labels) +
    ggplot2::facet_wrap(~cls, nrow = 1) +
    ggplot2::labs(x = NULL, y = "Marker abundance (log2,\nrelative to typical protein)") +
    hf_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, colour = "black"),
                   axis.text.y = ggplot2::element_text(colour = "black"),
                   strip.text = ggplot2::element_text(size = hf_pt(5.4), lineheight = 0.95, vjust = 0),
                   panel.spacing = ggplot2::unit(1.2, "mm"),
                   plot.margin = ggplot2::margin(0.5, 0.5, 0.5, 0.5, "mm"))
  means <- merge(ani[, c("dataset", "marker_class", "AnimalID", "group", "n_acquisitions", "animal_mean_score")],
                 stats::setNames(mm, c("cls", "comp", "mean_of_animal_means")) |>
                   transform(marker_class = names(strip)[match(as.character(cls), strip)],
                             dataset = names(lab)[match(as.character(comp), lab)]) |>
                   subset(select = c("dataset", "marker_class", "mean_of_animal_means")),
                 by = c("dataset", "marker_class"))
  means$row_type <- "animal_mean"
  cmp <- ct[, intersect(c("marker_class", "compartment_a", "compartment_b", "mean_a", "mean_b", "estimate",
                          "std_error", "ci_low", "ci_high", "df", "t_value", "p_value", "p_adjust", "n_animals",
                          "group", "method"), names(ct))]
  cmp$label_a <- unname(lab[as.character(cmp$compartment_a)])
  cmp$label_b <- unname(lab[as.character(cmp$compartment_b)])
  cmp$row_type <- "compartment_comparison"
  all_cols <- union(names(means), names(cmp))
  means[setdiff(all_cols, names(means))] <- NA
  cmp[setdiff(all_cols, names(cmp))] <- NA
  out <- rbind(means[, all_cols], cmp[, all_cols])
  out$legend_text_moved <- paste0(
    "control animals only (n = ", length(aid), "); points: acquisitions (", nrow(acq), "); larger marks: animal means; ",
    "bar: mean of the animal means; brackets: compartments that differ (BH-adjusted P < 0.05 over the ", nrow(ct),
    " comparisons, all of them in this table), compared on the animal means with a repeated-measures linear model ",
    "(compartment + animal as block, pooled error, df = ", unique(ct$df), "); joint cross-compartment scale, observed values only; marker references: soma GO, MGI; ",
    "neuropil SynGO, GO; microglia/PVM Allen cell types; markers per class: ",
    paste(sprintf("%s %d", tolower(sub(" markers$", "", cls)), as.integer(nmk[cls])), collapse = ", "))
  write_csv_safe(out, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# ==========================================================================
# f-i. GO biological-process programmes of regions and layers (2026-10-28)
# ==========================================================================
#
# One family of sibling units per panel (panel$families; several would sit side
# by side), under a family header over a thin rule. Rows are the displayed terms,
# grouped by the unit each was chosen for, and that cell is shaded. Every unit of
# the term's family is drawn, so its enrichment elsewhere stays visible (a term
# is never shown as absent where it is enriched): colour is NES on the
# manuscript's fixed NES scale, size is -log10 q, and q >= 0.05 is a small grey
# dot. Terms were chosen in pRoteomics (map_anatomical_programmes) on direct
# contrasts against every sibling unit, which the panel does not redraw.
#
# Everything is placed in millimetres inside the panel's box, so the panels of
# one row share one dot pitch (HF_GO_PITCH_MM) and their headers and rows line
# up: the dot grid sits at the box's right edge, a term name ends just left of
# it and takes a second line only when one line does not fit the box. The
# legend (panel$legend) is drawn by hand below the grid, on the same scales. A
# row is tall enough to keep two two-line names apart.
HF_GO_PITCH_MM <- c(col = 4.6, row = 4.4)

hf_programme_dots <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  d <- hv_read_csv(repo_path(panel$primary_source))
  sel <- as.character(panel$selection %||% "curated")
  fams <- as.character(unlist(panel$families))
  heads <- as.character(unlist(panel$family_labels))
  if (!length(fams) || length(heads) != length(fams))
    stop("hf_programme_dots: panel needs families and one family_label each", call. = FALSE)
  d <- d[d$selection == sel & d$family %in% fams, , drop = FALSE]
  miss <- setdiff(fams, d$family)
  if (length(miss)) stop("hf_programme_dots: no '", sel, "' rows for ", paste(miss, collapse = ", "), call. = FALSE)
  pc <- HF_GO_PITCH_MM[["col"]]
  pr <- HF_GO_PITCH_MM[["row"]]
  # width in mm of the widest line, measured with the font svglite writes
  tw <- function(x, pt = HF_MIN_PT) vapply(strsplit(x, "\n", fixed = TRUE), function(l)
    max(systemfonts::string_width(l, family = fam, size = pt, res = 72)) / 72 * 25.4, numeric(1))
  cols <- do.call(rbind, lapply(seq_along(fams), function(i) {
    f <- d[d$family == fams[i], , drop = FALSE]
    data.frame(family = fams[i], unit = unique(f$unit[order(f$unit_order)]), k = i, stringsAsFactors = FALSE)
  }))
  # column index, 0.8 of a column between families
  cols$j <- seq_len(nrow(cols)) + (cols$k - 1) * 0.8
  hd <- do.call(rbind, lapply(seq_along(fams), function(i) {
    j <- cols$j[cols$k == i]
    data.frame(j0 = min(j) - 0.4, j1 = max(j) + 0.4, jm = mean(range(j)), label = heads[i])
  }))
  # the grid ends 0.4 mm inside the box's right edge, or further in where the last family header is wider
  # than its columns
  last <- nrow(hd)
  x_end <- min(w_mm - 0.4, w_mm - 0.2 - tw(hd$label[last], 5.6) / 2 + (max(cols$j) - hd$jm[last] + 0.5) * pc)
  xj <- function(j) x_end - (max(cols$j) - j + 0.5) * pc
  cols$x <- xj(cols$j)
  hd$x0 <- xj(hd$j0); hd$x1 <- xj(hd$j1); hd$xm <- xj(hd$jm)
  d$x <- cols$x[match(paste(d$family, d$unit), paste(cols$family, cols$unit))]
  home <- d[d$unit == d$selected_for_unit, , drop = FALSE]
  home <- home[order(match(home$family, fams), home$selected_for_order, home$display_rank), , drop = FALSE]
  ids <- unique(home$ID)
  # from the top of the box (y is negative downwards): family header, rule, unit names, then the rows
  y_head <- -0.2; y_rule <- -2.5; y_unit <- -4.2; y_grid <- -4.7
  rows <- data.frame(ID = ids, y = y_grid - (seq_along(ids) - 0.5) * pr, stringsAsFactors = FALSE)
  y_low <- y_grid - length(ids) * pr
  if (-y_low > h_mm - 0.3)
    stop("hf_programme_dots: ", length(ids), " rows need ", round(-y_low + 0.3, 1), " mm; the box is ", h_mm,
         " mm high", call. = FALSE)
  # GPCR is defined in the legend; every other term keeps its GO name. A name takes a second line, split
  # where its wider line is narrowest, only when one line does not fit left of the grid.
  short <- function(x) gsub("G protein-coupled receptor", "GPCR", x, fixed = TRUE)
  x_lab <- min(cols$x) - pc / 2 - 0.8
  fit <- function(x) {
    if (tw(x) <= x_lab) return(x)
    sp <- gregexpr(" ", x, fixed = TRUE)[[1]]
    two <- if (sp[1] > 0)
      vapply(sp, function(s) paste0(substr(x, 1, s - 1), "\n", substr(x, s + 1, nchar(x))), character(1)) else x
    best <- two[which.min(tw(two))]
    if (tw(best) > x_lab)
      stop("hf_programme_dots: '", x, "' needs ", round(tw(best), 1), " mm in two lines; the box leaves ",
           round(x_lab, 1), " mm left of the grid", call. = FALSE)
    best
  }
  rows$label <- vapply(short(home$Description[match(ids, home$ID)]), fit, character(1), USE.NAMES = FALSE)
  d$y <- rows$y[match(d$ID, rows$ID)]
  # one cell per term x unit (a term chosen for two units of a family is listed twice in the display table)
  cell <- d[!duplicated(paste(d$ID, d$family, d$unit)), , drop = FALSE]
  cell$home <- paste(cell$ID, cell$family, cell$unit) %in% paste(home$ID, home$family, home$unit)
  cell$supported <- is.finite(cell$p.adjust) & cell$p.adjust < 0.05
  cell$mlq <- pmin(-log10(cell$p.adjust), 8)
  sup <- cell[cell$supported, , drop = FALSE]
  ns <- cell[!cell$supported, , drop = FALSE]
  hm <- cell[cell$home, c("x", "y"), drop = FALSE]
  lim <- hv_diverging_limit("nes")
  # hairline guides (about 0.25 pt) through the cells of each family's own rows only, so no guide crosses
  # a cell that was not tested
  fy <- stats::aggregate(y ~ family, data = cell, FUN = range)
  vg <- merge(cols, data.frame(family = fy$family, y0 = fy$y[, 1] - pr / 2, y1 = fy$y[, 2] + pr / 2), by = "family")
  hg <- merge(unique(cell[, c("family", "y")]),
              data.frame(family = fams, x0 = xj(hd$j0 + 0.1), x1 = xj(hd$j1 - 0.1)), by = "family")
  hair <- 0.12
  tile <- 0.92 * min(pc, pr)
  ge <- paste0(intToUtf8(0x2265), " 0.05")

  p <- ggplot2::ggplot() +
    # the shaded cell is square (house style), centred on its dot
    ggplot2::geom_tile(data = hm, ggplot2::aes(x, y), width = tile, height = tile, fill = "grey90", colour = NA) +
    ggplot2::annotate("segment", x = vg$x, xend = vg$x, y = vg$y0, yend = vg$y1,
                      linewidth = hair, colour = "grey85") +
    ggplot2::annotate("segment", x = hg$x0, xend = hg$x1, y = hg$y, yend = hg$y,
                      linewidth = hair, colour = "grey85") +
    ggplot2::geom_point(data = ns, ggplot2::aes(x, y), size = 0.45, colour = "grey60", stroke = 0) +
    # colour "transparent" (not NA, which ggplot2 4 drops as missing): svglite writes stroke: none
    ggplot2::geom_point(data = sup, ggplot2::aes(x, y, fill = NES, size = mlq), shape = 21,
                        colour = "transparent", stroke = 0) +
    hv_diverging(measure = "nes", name = "NES", breaks = c(-lim, 0, lim)) +
    ggplot2::scale_size_area(max_size = as.numeric(panel$max_point_mm %||% 2.3), limits = c(0, 8)) +
    ggplot2::annotate("text", x = x_lab, y = rows$y, label = rows$label, family = fam,
                      size = hf_sz(HF_MIN_PT), colour = "black", hjust = 1, vjust = 0.5, lineheight = 0.85) +
    ggplot2::annotate("text", x = cols$x, y = y_unit, label = cols$unit, family = fam,
                      size = hf_sz(HF_MIN_PT), colour = "black", vjust = 0) +
    ggplot2::annotate("segment", x = hd$x0, xend = hd$x1, y = y_rule, yend = y_rule,
                      linewidth = hv_lw("axis_pt"), colour = "black") +
    ggplot2::annotate("text", x = hd$xm, y = y_head, label = hd$label, family = fam,
                      size = hf_sz(5.6), colour = "black", vjust = 1)

  if (isTRUE(panel$legend)) {
    # bottom-left of the box: NES colourbar (vector rectangles, never a raster), -log10 q sizes and the grey
    # dot in one row, the shaded cell below; drawn through the panel's own fill and size scales
    lab3 <- hv_diverging_label_fun(lim)(c(-lim, 0, lim))
    x0 <- 0.3
    y_top <- -h_mm + 8.8
    if (y_top > y_low - 1.5)
      stop("hf_programme_dots: the legend needs 8.8 mm below the grid; the box leaves ",
           round(h_mm + y_low - 1.5, 1), " mm", call. = FALSE)
    y_bar <- c(y_top - 2.3, y_top - 3.6)
    bx <- x0 + tw(lab3[1]) / 2 + c(0, 9)
    nb <- 40
    bar <- data.frame(xmin = bx[1] + diff(bx) * (seq_len(nb) - 1) / nb, xmax = bx[1] + diff(bx) * seq_len(nb) / nb,
                      NES = seq(-lim, lim, length.out = nb))
    sx <- bx[2] + tw(lab3[3]) / 2 + 2.5
    keys <- data.frame(x = sx + 1 + (0:2) * 3, y = mean(y_bar), mlq = c(2, 4, 8),
                       label = c("2", "4", paste0(intToUtf8(0x2265), "8")))
    gx <- max(keys$x) + 1.2 + 2.5
    y_cell <- y_top - 3.9 - 1.8 - 1.0 - 0.9
    p <- p +
      ggplot2::geom_rect(data = bar, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = y_bar[2], ymax = y_bar[1],
                                                  fill = NES), colour = NA) +
      ggplot2::annotate("text", x = c(x0, sx), y = y_top, label = c("NES", paste0(intToUtf8(0x2212), "log10 q")),
                        family = fam, size = hf_sz(HF_MIN_PT), colour = "black", hjust = 0, vjust = 1) +
      ggplot2::annotate("text", x = c(bx[1], mean(bx), bx[2]), y = y_bar[2] - 0.3, label = lab3, family = fam,
                        size = hf_sz(HF_MIN_PT), colour = "black", vjust = 1) +
      ggplot2::geom_point(data = keys, ggplot2::aes(x, y, size = mlq), shape = 21, fill = "grey40",
                          colour = "transparent", stroke = 0) +
      ggplot2::annotate("text", x = keys$x, y = y_bar[2] - 0.3, label = keys$label, family = fam,
                        size = hf_sz(HF_MIN_PT), colour = "black", vjust = 1) +
      ggplot2::annotate("point", x = gx, y = mean(y_bar), size = 0.45, colour = "grey60", stroke = 0) +
      ggplot2::annotate("text", x = gx + 0.9, y = mean(y_bar), label = paste("q", ge), family = fam,
                        size = hf_sz(HF_MIN_PT), colour = "black", hjust = 0, vjust = 0.5) +
      ggplot2::annotate("tile", x = x0 + 0.9, y = y_cell, width = 1.8, height = 1.8, fill = "grey90") +
      ggplot2::annotate("text", x = x0 + 2.6, y = y_cell, label = "unit the term was chosen for", family = fam,
                        size = hf_sz(HF_MIN_PT), colour = "black", hjust = 0, vjust = 0.5)
    if (gx + 0.9 + tw(paste("q", ge)) > w_mm)
      stop("hf_programme_dots: the legend is wider than the box", call. = FALSE)
  }

  # one data unit is one millimetre: the panel fills the box, nothing outside it takes space
  p <- p +
    ggplot2::coord_cartesian(xlim = c(0, w_mm), ylim = c(-h_mm, 0), expand = FALSE, clip = "off") +
    ggplot2::labs(x = NULL, y = NULL) +
    hf_theme() +
    ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(), axis.ticks.length = ggplot2::unit(0, "pt"),
                   legend.position = "none", plot.margin = ggplot2::margin(0, 0, 0, 0))
  cell$legend_text_moved <- paste0(
    "terms: ", d$display_selection_rule[1], "; shaded: the unit the term was chosen for; every unit of the ",
    "family drawn; colour: NES (fixed scale, full colour beyond ", intToUtf8(0xb1), lim, "); size: ",
    intToUtf8(0x2212), "log10 q; grey dot: q ", ge, "; GPCR, G protein-coupled receptor; ",
    "control animals only (n = 3)")
  cell$colour_limit <- lim
  keep <- intersect(c("family", "selected_for_unit", "display_rank", "ID", "Description", "unit", "contrast",
                      "NES", "pvalue", "p.adjust", "setSize", "supported", "home", "colour_limit",
                      "analytical_source", "legend_text_moved"), names(cell))
  cell <- cell[order(match(cell$family, fams), -cell$y, cell$x), , drop = FALSE]
  write_csv_safe(cell[, keep], csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# ==========================================================================
# j. Input territories (2026-10-28)
# ==========================================================================
#
# Laminae that receive the same afferent pathway resemble each other across
# subfields in their presynaptic proteins, not in their postsynaptic ones.
# pRoteomics map_input_territories computes, per animal, d = mean similarity
# (Spearman, Fisher z, protein-centred laminar profiles) of the same-input
# pairs minus that of the same-region pairs, for SynGO presynaptic and
# postsynaptic proteins, and its excess over abundance-matched random protein
# sets (each animal against its own random sets), and tests across the control
# animals. This renderer only draws: one line per animal from its presynaptic
# to its postsynaptic excess; 0 is the random-set median and the grey bar runs
# from the lowest 2.5% to the highest 97.5% quantile of the animals' random
# sets; P values (BH, control animals; drawn if < 0.05) above the Pre column and
# below the Post column test that class against random, the bracket pre vs post.
# The source data also carry the replication, the post hoc sensitivity analyses
# and the per-protein territory contrasts computed there.
hf_input_territories <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  grp <- as.character(unlist(panel$groups %||% "CON"))
  deps <- unlist(panel$input_dependencies)
  dep <- function(name) {   # one declared input per table, read by name
    f <- grep(name, deps, value = TRUE, fixed = TRUE)
    if (length(f) != 1L) stop("hf_input_territories: input ", name, " not declared once", call. = FALSE)
    hv_read_csv(repo_path(f))
  }
  sc <- hv_read_csv(repo_path(panel$primary_source))
  ts <- dep("input_territory_tests")
  sv <- dep("input_territory_sensitivity")
  pc <- dep("input_territory_protein_contrasts")
  sc <- sc[sc$group %in% grp & sc$protein_class %in% c("presynaptic", "postsynaptic"), , drop = FALSE]
  ts_all <- ts
  ts <- ts[ts$animal_set == paste(grp, collapse = "+"), , drop = FALSE]
  if (!nrow(sc) || !nrow(ts)) stop("hf_input_territories: no rows for ", paste(grp, collapse = "+"), call. = FALSE)
  n_an <- length(unique(sc$AnimalID))
  if (any(ts$n_animals != n_an)) stop("hf_input_territories: tests and scores disagree on the animals", call. = FALSE)
  terr <- c(perforant_path = "Perforant path", mossy_fibre = "Mossy fibre\n(positive control)")
  sc$tf <- factor(unname(terr[sc$territory]), levels = terr)
  ts$tf <- factor(unname(terr[ts$territory]), levels = terr)
  sc$x <- ifelse(sc$protein_class == "presynaptic", 1, 2)
  sc$y <- sc$excess_over_random
  nul <- merge(stats::aggregate(null_q025_excess ~ tf + x, data = sc, FUN = min),
               stats::aggregate(null_q975_excess ~ tf + x, data = sc, FUN = max), by = c("tf", "x"))
  top <- max(c(sc$y, nul$null_q975_excess))
  low <- min(c(sc$y, nul$null_q025_excess))
  rng <- top - low
  plab <- function(p) ifelse(p < 0.001, "italic(P)*'<0.001'",
                             sprintf("italic(P)*'=%s'", formatC(signif(p, 2), format = "fg", digits = 2, flag = "#")))
  # vs matched random: above the Pre column, below the Post column; pre vs post as a bracket above both
  vr <- ts[grepl("vs matched random", ts$comparison) & ts$p_adjust < 0.05, , drop = FALSE]
  vr$x <- ifelse(grepl("^presynaptic", vr$comparison), 1, 2)
  vr$y <- ifelse(vr$x == 1, top + 0.08 * rng, low - 0.1 * rng)
  vr$vj <- ifelse(vr$x == 1, 0, 1)
  vr$label <- plab(vr$p_adjust)
  pp <- ts[ts$comparison == "presynaptic vs postsynaptic" & ts$p_adjust < 0.05, , drop = FALSE]
  pp$y <- top + 0.32 * rng
  pp$label <- plab(pp$p_adjust)
  tick <- 0.03 * rng
  brk <- rbind(data.frame(tf = pp$tf, x = 1, xend = 2, y = pp$y, yend = pp$y),
               data.frame(tf = pp$tf, x = 1, xend = 1, y = pp$y, yend = pp$y - tick),
               data.frame(tf = pp$tf, x = 2, xend = 2, y = pp$y, yend = pp$y - tick))
  p <- ggplot2::ggplot(sc, ggplot2::aes(x, y)) +
    ggplot2::geom_rect(data = nul, ggplot2::aes(xmin = x - 0.17, xmax = x + 0.17, ymin = null_q025_excess,
                                                ymax = null_q975_excess), inherit.aes = FALSE, fill = "grey88", colour = NA) +
    ggplot2::geom_hline(yintercept = 0, linewidth = hv_lw("reference_pt"), colour = "grey45") +
    ggplot2::geom_line(ggplot2::aes(group = AnimalID), linewidth = hv_lw("reference_pt"), colour = "grey55") +
    # borderless animal marks
    ggplot2::geom_point(size = 1.3, colour = "black", stroke = 0) +
    ggplot2::geom_segment(data = brk, ggplot2::aes(x = x, xend = xend, y = y, yend = yend), inherit.aes = FALSE,
                          linewidth = hv_lw("reference_pt"), colour = "black") +
    ggplot2::geom_text(data = pp, ggplot2::aes(x = 1.5, y = y + 0.02 * rng, label = label), parse = TRUE, vjust = 0,
                       family = fam, size = hf_sz(HF_MIN_PT), colour = "black") +
    ggplot2::geom_text(data = vr, ggplot2::aes(x = x, y = y, label = label, vjust = vj), parse = TRUE,
                       family = fam, size = hf_sz(HF_MIN_PT), colour = "black") +
    ggplot2::geom_blank(data = data.frame(tf = factor(terr, levels = terr), x = 1.5, y = c(top + 0.5 * rng, low - 0.3 * rng)),
                        ggplot2::aes(x, y)) +
    ggplot2::facet_grid(. ~ tf) +
    ggplot2::scale_x_continuous(breaks = c(1, 2), labels = c("Pre", "Post"), limits = c(0.55, 2.45)) +
    ggplot2::scale_y_continuous(labels = hv_minus_labels, breaks = scales::breaks_pretty(4)) +
    ggplot2::labs(x = "SynGO proteins",
                  y = paste0("Input organisation (", intToUtf8(0x394), "z)\nexcess over matched random proteins")) +
    hf_theme() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
                   axis.title = ggplot2::element_text(size = HF_MIN_PT, colour = "black", lineheight = 0.9),
                   strip.text = ggplot2::element_text(size = HF_MIN_PT, colour = "black", lineheight = 0.9,
                                                      margin = ggplot2::margin(0, 0, 1, 0, "mm")),
                   strip.background = ggplot2::element_blank(), panel.spacing.x = ggplot2::unit(2, "mm"),
                   legend.position = "none", plot.margin = ggplot2::margin(1, 1, 0.5, 0.5, "mm"))
  out <- sc[, c("AnimalID", "group", "territory", "territory_label", "protein_class", "n_proteins", "same_input_z",
                "same_region_z", "input_organisation_d", "null_median_d", "null_q025_d", "null_q975_d",
                "excess_over_random", "null_q025_excess", "null_q975_excess", "p_greater_than_random", "p_less_than_random",
                "p_presynaptic_minus_postsynaptic")]
  out$row_type <- "animal"
  # the stressed animals' replication travels with the control comparisons (never pooled, never plotted)
  tt <- ts_all[, c("animal_set", "territory", "territory_label", "comparison", "estimate", "std_error", "ci_low", "ci_high",
                   "df", "t_value", "p_value", "p_adjust", "n_animals", "animals_positive", "animals", "method")]
  tt$row_type <- ifelse(tt$animal_set == paste(grp, collapse = "+"), "comparison", "replication_comparison")
  tt$group <- tt$animal_set
  tt$animal_set <- NULL
  # post hoc sensitivity analyses and per-protein territory contrasts, as computed in pRoteomics (never drawn)
  sv$row_type <- "sensitivity"
  sv$territory_label <- ts_all$territory_label[match(sv$territory, ts_all$territory)]
  sv$group <- sv$animal_set
  sv$animal_set <- NULL
  pc$row_type <- "protein_contrast"
  pc$group <- paste(grp, collapse = "+")
  blocks <- list(out, tt, sv, pc)
  all_cols <- Reduce(union, lapply(blocks, names))
  out <- do.call(rbind, lapply(blocks, function(b) { b[setdiff(all_cols, names(b))] <- NA; b[, all_cols] }))
  n_rep <- unique(ts_all$n_animals[ts_all$animal_set != paste(grp, collapse = "+")])
  out$legend_text_moved <- paste0(
    "control animals only (n = ", n_an, "); d per animal: mean Fisher z (Spearman, protein-centred laminar profiles) of the ",
    "same-input lamina pairs minus the same-region pairs (perforant path: CA1 SLM-CA2 SLM, CA1 SLM-DG mo, CA2 SLM-DG mo vs ",
    "SLM-SR and SLM-SO in CA1 and CA2 and DG mo-po; mossy fibre: DG po-CA3 SR vs DG po-mo, CA3 SR-SO); SynGO ",
    "presynaptic-only / postsynaptic-only proteins (annotated to both: neither) without glial, vascular and mitochondrial ",
    "genes, observed in every retained acquisition of all ten laminae; animal rows, excess_over_random: d minus the ",
    "animal's own abundance-matched random-set median (1000 sets); grey bar: lowest 2.5% to highest 97.5% quantile of the ",
    "animals' random sets (null_q025_excess, null_q975_excess); P above Pre and below Post: that class vs random, bracket: ",
    "presynaptic vs postsynaptic excess (comparison rows: one-sample t test over animals, BH over the 6 control ",
    "comparisons); the per-animal P columns are descriptive (rank of d among the animal's random sets; class-label ",
    "permutation of d(pre) - d(post)); replication_comparison rows: stressed animals with a complete ten-lamina profile ",
    "(n = ", paste(n_rep, collapse = "/"), "), BH separately; sensitivity rows: post hoc analyses (lamina-label permutation, ",
    "cross-host pairs, without the shared DG pair, laminar-SD-matched null, without GABAergic-terminal proteins, left-out ",
    "animal re-entered); protein_contrast rows: control-animal mean territory contrast of every synaptic protein ",
    "(descriptive)")
  write_csv_safe(out, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# ==========================================================================
# k. Where the proteins of each cell type sit (2026-10-28)
# ==========================================================================
#
# Proteins grouped by the cell type whose mRNA they are enriched in (whole-
# mouse-brain 10x reference of other animals; pRoteomics map_afferent_sources):
# entorhinal neurons, other afferent neurons, CA3, granule, CA1 or CA2 cells,
# interneurons, glia and vascular cells. Square tiles: control-animal mean of
# the class score minus that of abundance-matched random proteins, per lamina
# (log2, laminar profiles centred within animal). Outlined: the target lamina
# of each pre-declared afferent contrast; statistics in the source data.
hf_afferent_sources <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  grp <- as.character(unlist(panel$groups %||% "CON"))
  deps <- unlist(panel$input_dependencies)
  dep <- function(name) {   # one declared input per table, read by name
    f <- grep(name, deps, value = TRUE, fixed = TRUE)
    if (length(f) != 1L) stop("hf_afferent_sources: input ", name, " not declared once", call. = FALSE)
    hv_read_csv(repo_path(f))
  }
  sc <- hv_read_csv(repo_path(panel$primary_source))
  ct <- dep("afferent_source_contrasts")
  members <- dep("afferent_source_sets")
  sc <- sc[sc$group %in% grp, , drop = FALSE]
  ct_all <- ct
  ct <- ct[ct$animal_set == paste(grp, collapse = "+"), , drop = FALSE]
  n_an <- length(unique(sc$AnimalID))
  if (!nrow(sc) || any(ct$n_animals != n_an)) stop("hf_afferent_sources: scores and contrasts disagree", call. = FALSE)
  sc$excess <- sc$score - sc$null_mean
  tl <- stats::aggregate(excess ~ origin_class + lamina, data = sc, FUN = mean)
  rows <- c(entorhinal = "Entorhinal neurons", other_afferent = "Other afferent neurons", ca3 = "CA3 pyramidal cells",
            granule = "Granule cells", ca1 = "CA1 pyramidal cells", ca2 = "CA2 pyramidal cells",
            interneuron = "Interneurons", astrocyte = "Astrocytes", vascular = "Vascular cells",
            oligodendrocyte = "Oligodendrocytes")
  if (!all(names(rows) %in% tl$origin_class)) stop("hf_afferent_sources: origin class missing", call. = FALSE)
  tl <- tl[tl$origin_class %in% names(rows), , drop = FALSE]
  lam <- c("CA1_so", "CA1_sr", "CA1_slm", "CA2_so", "CA2_sr", "CA2_slm", "CA3_so", "CA3_sr", "DG_mo", "DG_po")
  reg <- sub("_.*", "", lam)
  xpos <- seq_along(lam) + (match(reg, unique(reg)) - 1) * 0.4
  names(xpos) <- lam
  ypos <- stats::setNames(rev(seq_along(rows)), names(rows))
  tl$x <- xpos[tl$lamina]
  tl$y <- ypos[tl$origin_class]
  # display limit 0.5: stronger values take full colour (disclosed on the colourbar; uncapped in the source data)
  lim <- 0.5
  # outlined: the target lamina of every pre-declared afferent contrast (and only those)
  pred <- ct[ct$expectation %in% c("perforant path", "Schaffer collaterals", "mossy fibres") & is.na(ct$minus_class),
             c("origin_class", "lamina_a"), drop = FALSE]
  names(pred) <- c("origin_class", "lamina")
  pred$x <- xpos[pred$lamina]
  pred$y <- ypos[pred$origin_class]
  hd <- do.call(rbind, lapply(unique(reg), function(r) data.frame(region = r, x0 = min(xpos[reg == r]) - 0.45,
                                                                    x1 = max(xpos[reg == r]) + 0.45)))
  top <- length(rows) + 0.5
  p <- ggplot2::ggplot(tl, ggplot2::aes(x, y, fill = excess)) +
    ggplot2::geom_tile(width = 1, height = 1, colour = hv_tile_border(), linewidth = hv_lw("tile_border_pt")) +
    ggplot2::geom_tile(data = pred, ggplot2::aes(x, y), inherit.aes = FALSE, width = 0.94, height = 0.94, fill = "transparent",
                       colour = "black", linewidth = hv_lw("axis_pt")) +
    hv_diverging(limits = c(-lim, lim), name = "log2 vs\nmatched\nproteins", breaks = c(-lim, 0, lim),
                 labels = hv_diverging_label_fun(lim), oob = scales::squish) +
    ggplot2::annotate("segment", x = hd$x0, xend = hd$x1, y = top + 0.35, yend = top + 0.35,
                      linewidth = hv_lw("axis_pt"), colour = "black") +
    ggplot2::annotate("text", x = (hd$x0 + hd$x1) / 2, y = top + 0.55, label = hd$region, vjust = 0, family = fam,
                      size = hf_sz(HF_MIN_PT), colour = "black") +
    ggplot2::annotate("text", x = xpos, y = 0.35, label = toupper(sub(".*_", "", lam)), angle = 90, hjust = 1,
                      family = fam, size = hf_sz(HF_MIN_PT), colour = "black") +
    ggplot2::annotate("text", x = 0.25, y = ypos, label = unname(rows), hjust = 1, family = fam,
                      size = hf_sz(HF_MIN_PT), colour = "black") +
    ggplot2::annotate("text", x = 0.25, y = top + 0.55, label = "mRNA enriched in", hjust = 1, vjust = 0, family = fam,
                      size = hf_sz(HF_MIN_PT), colour = "black") +
    ggplot2::scale_x_continuous(breaks = NULL) +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::coord_fixed(ratio = 1, clip = "off", expand = FALSE, xlim = c(0.5, max(xpos) + 0.5),
                         ylim = c(0.5, top)) +
    ggplot2::guides(fill = ggplot2::guide_colourbar(display = "rectangles", nbin = 40, title.position = "top",
                                                    theme = ggplot2::theme(legend.key.width = ggplot2::unit(1.4, "mm"),
                                                                           legend.key.height = ggplot2::unit(7, "mm")))) +
    ggplot2::labs(x = NULL, y = NULL) +
    hf_theme() +
    ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                   legend.title = ggplot2::element_text(size = HF_MIN_PT, colour = "black", lineheight = 0.9),
                   legend.text = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
                   legend.margin = ggplot2::margin(0, 0, 0, 1, "mm"),
                   plot.margin = ggplot2::margin(3.5, 0.5, 4, 20, "mm"))
  tl$origin_label <- unname(rows[tl$origin_class])
  tl$predicted_target <- paste(tl$origin_class, tl$lamina) %in% paste(pred$origin_class, pred$lamina)
  tl$row_type <- "tile"
  cc <- ct_all[, c("animal_set", "origin_class", "minus_class", "origin_label", "lamina_a", "lamina_b", "expectation", "estimate", "std_error",
                   "ci_low", "ci_high", "df", "t_value", "p_value", "p_adjust", "n_animals", "animals_positive", "n_proteins",
                   "animals", "method")]
  cc$row_type <- ifelse(cc$animal_set == paste(grp, collapse = "+"), "contrast", "replication_contrast")
  out <- tl[, c("origin_class", "origin_label", "lamina", "excess", "predicted_target", "row_type")]
  # the values behind each tile (control animals) and the class members, as computed in pRoteomics
  at <- sc[, c("AnimalID", "origin_class", "origin_label", "lamina", "n_proteins", "score", "null_mean", "excess")]
  at$row_type <- "animal_tile"
  members$row_type <- "class_member"
  blocks <- list(out, at, cc, members)
  all_cols <- Reduce(union, lapply(blocks, names))
  out <- do.call(rbind, lapply(blocks, function(b) { b[setdiff(all_cols, names(b))] <- NA; b[, all_cols] }))
  out$group <- ifelse(is.na(out$animal_set), paste(grp, collapse = "+"), out$animal_set)
  out$group[out$row_type == "class_member"] <- NA
  out$animal_set <- NULL
  n_rep <- unique(ct_all$n_animals[ct_all$animal_set != paste(grp, collapse = "+")])
  out$legend_text_moved <- paste0(
    "control animals only (n = ", n_an, "); proteins grouped by the cell type whose mRNA they are enriched in (whole-mouse-brain ",
    "10x reference, other animals; class_member rows, thresholds in Methods); tile: mean over animals of the class score ",
    "minus the abundance-matched random-set score (animal_tile rows: the control animals' values); outlined: the target ",
    "lamina of each pre-declared afferent contrast (perforant path, Schaffer collaterals, mossy fibres); contrast rows: ",
    "one-sample t test over animals, BH over the 8 pre-declared contrasts; replication_contrast rows: stressed animals with ",
    "a complete ten-lamina profile (n = ", paste(n_rep, collapse = "/"), "), BH separately; full colour beyond ",
    intToUtf8(0xb1), lim, " (uncapped values here)")
  write_csv_safe(out, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}

# ==========================================================================
# l. Inhibitory presynaptic share of every lamina (2026-10-28)
# ==========================================================================
#
# GAD1, GAD2 and VGAT relative to VGLUT1 in every acquisition (a within-sample
# log2 ratio), each animal's ten laminae centred on their mean, computed with
# the lamina comparisons in pRoteomics compare_lamina_synapses. Points:
# control animals; bar: their mean; brackets: the comparisons with BH-adjusted
# P < 0.05 (paired over the control animals; all pre-declared pairs, the
# stressed-animal replication and the sensitivities in the source data).
hf_inhibitory_share <- function(panel, svg_path, csv_path, w_mm, h_mm) {
  fam <- hf_fam()
  grp <- as.character(unlist(panel$groups %||% "CON"))
  an <- hv_read_csv(repo_path(panel$primary_source))
  f <- grep("inhibitory_share_contrasts", unlist(panel$input_dependencies), value = TRUE, fixed = TRUE)
  if (length(f) != 1L) stop("hf_inhibitory_share: input inhibitory_share_contrasts not declared once", call. = FALSE)
  ct <- hv_read_csv(repo_path(f))
  an <- an[an$group %in% grp, , drop = FALSE]
  ct_all <- ct
  ct <- ct[ct$group %in% paste(grp, collapse = "+"), , drop = FALSE]
  if (any(ct$n_animals != length(unique(an$AnimalID)))) stop("hf_inhibitory_share: animals disagree", call. = FALSE)
  lam <- c("CA1_so", "CA1_sr", "CA1_slm", "CA2_so", "CA2_sr", "CA2_slm", "CA3_so", "CA3_sr", "DG_mo", "DG_po")
  reg <- sub("_.*", "", lam)
  xpos <- seq_along(lam) + (match(reg, unique(reg)) - 1) * 0.6
  names(xpos) <- lam
  an$x <- xpos[an$lamina]
  an$region <- sub("_.*", "", an$lamina)
  k <- stats::ave(seq_len(nrow(an)), an$lamina, FUN = seq_along)
  an$xj <- an$x + (k - 2) * 0.16
  mn <- stats::aggregate(presynaptic_share_centred ~ lamina + region + x, data = an, FUN = mean)
  col <- unlist(hv_palette()$region)
  y <- an$presynaptic_share_centred
  rng <- diff(range(y))
  sig <- ct[ct$p_adjust < 0.05, , drop = FALSE]
  sig$xa <- xpos[sig$lamina_a]
  sig$xb <- xpos[sig$lamina_b]
  sig$lo <- pmin(sig$xa, sig$xb)
  sig$hi <- pmax(sig$xa, sig$xb)
  sig <- sig[order(sig$hi - sig$lo), , drop = FALSE]
  sig$y <- max(y) + 0.13 * rng * seq_len(nrow(sig))
  sig$label <- ifelse(sig$p_adjust < 0.001, "italic(P)*'<0.001'",
                      sprintf("italic(P)*'=%s'", formatC(signif(sig$p_adjust, 2), format = "fg", digits = 2, flag = "#")))
  tick <- 0.025 * rng
  brk <- rbind(data.frame(x = sig$lo, xend = sig$hi, y = sig$y, yend = sig$y),
               data.frame(x = sig$lo, xend = sig$lo, y = sig$y, yend = sig$y - tick),
               data.frame(x = sig$hi, xend = sig$hi, y = sig$y, yend = sig$y - tick))
  # stratum names on the axis, the region name as a second line under the middle of its strata
  lab <- toupper(sub(".*_", "", lam))
  rc <- tapply(xpos, reg, mean)
  brk_x <- c(xpos, rc)
  brk_lab <- c(lab, paste0("\n", names(rc)))
  p <- ggplot2::ggplot(an, ggplot2::aes(xj, presynaptic_share_centred)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = hv_lw("reference_pt"), colour = "grey70") +
    ggplot2::geom_point(ggplot2::aes(colour = region), size = 0.9, alpha = 0.8, stroke = 0) +
    ggplot2::geom_segment(data = mn, ggplot2::aes(x = x - 0.3, xend = x + 0.3, y = presynaptic_share_centred,
                                                  yend = presynaptic_share_centred), inherit.aes = FALSE,
                          linewidth = 0.45, colour = "black") +
    ggplot2::geom_segment(data = brk, ggplot2::aes(x = x, xend = xend, y = y, yend = yend), inherit.aes = FALSE,
                          linewidth = hv_lw("reference_pt"), colour = "black") +
    ggplot2::geom_text(data = sig, ggplot2::aes(x = (lo + hi) / 2, y = y + 0.02 * rng, label = label), inherit.aes = FALSE,
                       parse = TRUE, vjust = 0, family = fam, size = hf_sz(HF_MIN_PT), colour = "black") +
    ggplot2::scale_colour_manual(values = col, guide = "none") +
    ggplot2::scale_x_continuous(breaks = unname(brk_x), labels = brk_lab, expand = ggplot2::expansion(add = 0.5)) +
    ggplot2::scale_y_continuous(labels = hv_minus_labels, breaks = scales::breaks_pretty(4)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = NULL, y = "Inhibitory share, centred\n(log2 GAD, VGAT : VGLUT1)") +
    hf_theme() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = HF_MIN_PT, colour = "black"),
                   axis.title.y = ggplot2::element_text(size = HF_MIN_PT, colour = "black", lineheight = 0.9),
                   axis.ticks.x = ggplot2::element_blank(),
                   legend.position = "none", plot.margin = ggplot2::margin(1, 1, 0.5, 0.5, "mm"))
  out <- an[, c("AnimalID", "group", "lamina", "n_acquisitions", "inhibitory_presynaptic_share_log2",
                "presynaptic_share_centred", "inhibitory_postsynaptic_share_log2", "postsynaptic_share_centred",
                "gad2_vgat_share_log2", "vglut2_minus_vglut1_log2", "vglut2_acquisitions_observed")]
  out$row_type <- "animal_lamina"
  cc <- ct_all[, intersect(c("group", "lamina_a", "lamina_b", "comparison_type", "estimate", "std_error", "ci_low",
                             "ci_high", "df", "t_value", "p_value", "p_adjust", "n_animals", "animals_positive",
                             "estimate_gad2_vgat_only", "pooled_std_error", "pooled_df", "pooled_p_value",
                             "pooled_p_adjust", "animals", "method"), names(ct_all))]
  cc$row_type <- ifelse(cc$group == paste(grp, collapse = "+"), "lamina_comparison", "replication_lamina_comparison")
  all_cols <- union(names(out), names(cc))
  out[setdiff(all_cols, names(out))] <- NA
  cc[setdiff(all_cols, names(cc))] <- NA
  out <- rbind(out[, all_cols], cc[, all_cols])
  out$legend_text_moved <- paste0(
    "control animals only (n = ", length(unique(an$AnimalID)), "); per acquisition, mean log2 GAD1, GAD2, VGAT minus log2 ",
    "VGLUT1 (label-free relative share, not a synapse count), animal mean over hemispheres, centred on the animal's ",
    "ten laminae; bar: mean; brackets: comparisons with BH-adjusted P < 0.05 (paired t over the control animals, BH over ",
    "the ", nrow(ct), " pre-declared pairs); all pairs, the stressed-animal replication (every stressed animal with both ",
    "laminae of a pair; animals named), a pooled-error sensitivity, GAD2 + VGAT only and VGLUT2 relative to VGLUT1 ",
    "(gad2_vgat_share_log2, vglut2_minus_vglut1_log2; animal rows) in the source data")
  write_csv_safe(out, csv_path)
  hv_save_panel(p, svg_path, w_mm, h_mm)
  invisible(list(status = "ok"))
}
