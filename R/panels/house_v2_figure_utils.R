# =====================================================================
# House-style v2 candidate figure engine. (Named nature_v2_figure_utils.R until 2026-10-19.)
#
# A THIRD parallel layer. It does not touch the canonical manuscript contract
# and it does not touch the Part-16 candidate layer: different contract,
# different engine, different output root (manuscript_candidates/nature_v2).
#
# THE ONE STRUCTURAL DIFFERENCE FROM THE PART-16 ENGINE
#
# The Part-16 assembler scaled whole panel SVGs into uniform grid cells with
# preserveAspectRatio, so a panel authored at 183 mm that landed in an 85 mm
# cell had all of its text shrunk by more than half. That is why every label
# in the Part-16 PDFs is illegible at print size.
#
# Here each panel declares its EXACT final box in millimetres, is rendered at
# that box, and is placed at 1:1. Scale is never applied, so 6 pt text on the
# canvas really is 6 pt on the page. hv_verify_scale() asserts it.
#
# Renderers remain downstream only: no model fit, no enrichment, no FDR.
#
# 2026-10-07: the nature_v2 candidate generation (its contract, entry scripts and the hv_build()
# driver) was retired. What remains are the shared palette, theme, I/O and guard helpers that the
# final_truth_v9 producers and the current candidates use.
# =====================================================================

hv_palette_path <- function() repo_path("config", "manuscript_palette.yml")

# ------------------------------------------------------------- palette

hv_palette <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) cache <<- yaml::read_yaml(hv_palette_path())
    cache
  }
})

hv_group_colours <- function() unlist(hv_palette()$group)
# A darker shade of a palette colour (mixed with black), for marks too small to
# carry the light original, e.g. RES points.
hv_darken <- function(col, amount = 0.35)
  grDevices::colorRampPalette(c(col, "black"))(101)[round(amount * 100) + 1]
# The outline of a tile in a white-midpoint heatmap (palette v3): light grey, so
# a tile at zero (white) still reads as a tile and not as a gap in the page.
hv_tile_border <- function() "grey85"
hv_dataset_colours <- function() unlist(hv_palette()$dataset)
hv_dataset_label <- function(x) {
  l <- unlist(hv_palette()$dataset_label)
  out <- unname(l[as.character(x)])
  ifelse(is.na(out), as.character(x), out)
}
hv_claim_colours <- function() unlist(hv_palette()$claimability)
hv_evidence_colours <- function() unlist(hv_palette()$evidence)
hv_pt <- function(key) as.numeric(hv_palette()$typography[[key]])
hv_lw <- function(key) as.numeric(hv_palette()$line[[key]]) * 0.75  # pt -> ggplot linewidth

# ggplot2 sizes are in mm for text; 1 pt = 0.3527 mm
hv_size <- function(pt) pt * 0.3527777

# The fixed colour limit of a signed measure (config/manuscript_palette.yml
# diverging_limits): every figure of a measure uses it, so one colour means one
# value everywhere.
hv_diverging_limit <- function(measure) {
  l <- hv_palette()$diverging_limits[[measure]]
  if (is.null(l)) stop("no fixed diverging limit for measure '", measure, "' in ",
                       hv_palette_path(), call. = FALSE)
  as.numeric(l)
}

# Colourbar breaks and labels of a fixed limit L. Values beyond the limit take
# full colour, so the ends read "<=-L" and ">=L" whether or not a value reaches
# them (the source data keep the uncapped values). Signs are the typographic
# minus. hv_diverging_label_fun() labels any breaks, so a caller may pass its own.
hv_minus <- function(x) sub("^-", "\u2212", x)
# Axis labels with the typographic minus: ggplot's own default formatting of the
# breaks, with U+2212 for a negative sign (2026-10-07: one minus glyph per page)
hv_minus_labels <- function(x) {
  out <- hv_minus(format(x, trim = TRUE, justify = "left"))
  out[is.na(x)] <- NA
  out
}
hv_diverging_breaks <- function(limit) c(-limit, -limit / 2, 0, limit / 2, limit)
hv_diverging_label_fun <- function(limit) function(b) {
  num <- function(v) sub("[.]?0+$", "", formatC(v, format = "f", digits = 2))
  s <- hv_minus(num(b))
  s[is.finite(b) & abs(b) < 1e-12] <- "0"
  s[is.finite(b) & abs(b - limit) < 1e-9] <- paste0("\u2265", num(limit))
  s[is.finite(b) & abs(b + limit) < 1e-9] <- paste0("\u2264\u2212", num(limit))
  s
}
hv_diverging_labels <- function(limit)
  hv_diverging_label_fun(limit)(hv_diverging_breaks(limit))

# The manuscript's one diverging fill. With `measure`, the scale takes that
# measure's fixed limit, squishes values beyond it to full colour and labels the
# colourbar ends <= / >= (palette v3.1; the v3.2 measures alike). Without it, `limits` are the caller's
# own (a declared display cap, or a measure that no other figure shares) and
# ggplot's default oob = censor stays, so a value outside them shows as a grey50
# tile instead of passing silently. na.value stays grey50: never white, which
# means zero.
hv_diverging <- function(limits = NULL, name = "NES", measure = NULL, ...) {
  d <- hv_palette()$diverging
  if (is.null(measure))
    return(ggplot2::scale_fill_gradient2(low = d$low, mid = d$mid, high = d$high,
                                         midpoint = 0, limits = limits, name = name, ...))
  if (!is.null(limits))
    stop("hv_diverging(): give `limits` or `measure`, not both", call. = FALSE)
  lim <- hv_diverging_limit(measure)
  args <- list(...)
  if ("oob" %in% names(args))
    stop("hv_diverging(measure =) always squishes values beyond the fixed limit; ",
         "do not pass `oob`", call. = FALSE)
  if (!"breaks" %in% names(args)) args[["breaks"]] <- hv_diverging_breaks(lim)
  if (!"labels" %in% names(args)) args[["labels"]] <- hv_diverging_label_fun(lim)
  do.call(ggplot2::scale_fill_gradient2,
          c(list(low = d$low, mid = d$mid, high = d$high, midpoint = 0,
                 limits = c(-lim, lim), oob = scales::squish, name = name), args))
}

# --------------------------------------------------------------- theme
#
# House-style: no plot title, no subtitle, no gridlines by default, hairline
# axes, compact legend. Explanatory text belongs in the figure legend, not
# inside the artwork.
hv_theme <- function(grid = "none", base = hv_pt("axis_text_pt")) {
  fam <- hv_palette()$typography$family
  th <- ggplot2::theme_bw(base_size = base, base_family = fam) +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      plot.caption = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(1, 1, 1, 1, "mm"),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      axis.line = ggplot2::element_line(linewidth = hv_lw("axis_pt"), colour = "black"),
      axis.ticks = ggplot2::element_line(linewidth = hv_lw("axis_pt"), colour = "black"),
      axis.ticks.length = ggplot2::unit(0.6, "mm"),
      axis.text = ggplot2::element_text(size = hv_pt("axis_text_pt"), colour = "black"),
      axis.title = ggplot2::element_text(size = hv_pt("axis_title_pt"), colour = "black"),
      legend.title = ggplot2::element_text(size = hv_pt("legend_title_pt")),
      legend.text = ggplot2::element_text(size = hv_pt("legend_text_pt")),
      legend.key.size = ggplot2::unit(2.4, "mm"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.spacing = ggplot2::unit(1, "mm"),
      legend.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = hv_pt("axis_text_pt"), colour = "black",
                                         margin = ggplot2::margin(0.4, 0, 0.4, 0, "mm")),
      panel.grid = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(0.8, "mm")
    )
  if (identical(grid, "y")) {
    th <- th + ggplot2::theme(panel.grid.major.y =
      ggplot2::element_line(linewidth = hv_lw("reference_pt"), colour = "grey92"))
  } else if (identical(grid, "x")) {
    th <- th + ggplot2::theme(panel.grid.major.x =
      ggplot2::element_line(linewidth = hv_lw("reference_pt"), colour = "grey92"))
  }
  th
}

# Heatmap variant: no axis line, tight tiles
hv_theme_tile <- function() {
  hv_theme() + ggplot2::theme(
    axis.line = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank())
}

# ------------------------------------------------------------- guards

# Tokens are CALL forms. `p.adjust` and `p.adjust.method` are also COLUMN names
# in canonical GSEA tables, so matching the bare word would forbid reading the
# stored FDR - which is exactly what a renderer is supposed to do. Requiring the
# opening parenthesis distinguishes "call the function" from "read the column".
hv_forbidden_tokens <- function() {
  c("lmFit(", "eBayes(", "limma::", "lmer(", "lme4::", "glm(", "aov(",
    "t.test(", "wilcox.test(", "cor.test(", "p.adjust(", "fdrtool(", "qvalue(",
    "gseGO(", "gseKEGG(", "GSEA(", "enricher(", "enrichGO(",
    "bootstrap_enrichment_test(", "blockwiseModules(", "TOMsimilarity(", "WGCNA::")
}

hv_assert_no_model_fitting <- function(files) {
  bad <- character()
  for (f in files) {
    if (!file.exists(f)) next
    lines <- sub("#.*$", "", readLines(f, warn = FALSE))
    start <- grep("^hv_forbidden_tokens <- function", lines)
    if (length(start)) {
      close <- grep("^\\}", lines)
      stop_at <- close[close > start[1]][1]
      if (!is.na(stop_at)) lines <- lines[-seq.int(start[1], stop_at)]
    }
    txt <- paste(lines, collapse = "\n")
    for (tok in hv_forbidden_tokens()) {
      if (grepl(tok, txt, fixed = TRUE)) bad <- c(bad, paste0(basename(f), ": ", tok))
    }
  }
  if (length(bad)) {
    stop("house_v2 renderers must not create new inference. Found: ",
         paste(bad, collapse = "; "), call. = FALSE)
  }
  invisible(TRUE)
}

# ------------------------------------------------------------- helpers

# Some clusterProfiler audit paths exceed the Windows 260-character MAX_PATH,
# so file.exists() reports FALSE and the readers fail on files that are really
# present. Changing directory does not help, because R still resolves the
# relative name against the full working directory. The Windows
# extended-length prefix does work, but only with backslashes and a fully
# qualified path.
hv_long_path <- function(path) {
  if (.Platform$OS.type != "windows") return(path)
  if (nchar(path) < 250L || grepl("^\\\\\\\\[?]", path)) return(path)
  paste0("\\\\?\\", gsub("/", "\\\\", path))
}

hv_read_csv_longpath <- function(dir, file, reader = utils::read.csv, ...) {
  p <- file.path(dir, file)
  if (file.exists(p)) return(reader(p, ...))
  lp <- hv_long_path(p)
  if (!identical(lp, p) && file.exists(lp)) return(reader(lp, ...))
  stop("missing_required_input: ", p, call. = FALSE)
}

hv_read_csv <- function(path, required = TRUE) {
  if (!file.exists(path)) {
    if (required) stop("missing_required_input: ", path, call. = FALSE)
    return(NULL)
  }
  as.data.frame(readr::read_csv(path, show_col_types = FALSE, progress = FALSE,
                                guess_max = Inf))
}

# Render at the EXACT declared box. Nothing downstream rescales it.
hv_save_panel <- function(plot, svg_path, w_mm, h_mm) {
  dir_create(dirname(svg_path))
  ggplot2::ggsave(svg_path, plot, width = w_mm, height = h_mm, units = "mm",
                  device = svglite::svglite, bg = "white")
  invisible(svg_path)
}

hv_placeholder <- function(path, id, reason, w_mm, h_mm) {
  dir_create(dirname(path))
  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%smm" height="%smm" viewBox="0 0 %s %s">',
            w_mm, h_mm, w_mm, h_mm),
    '<rect width="100%" height="100%" fill="white"/>',
    sprintf('<rect x="0.3" y="0.3" width="%.2f" height="%.2f" fill="none" stroke="#B9B9B4" stroke-width="0.3" stroke-dasharray="1.5,1.2"/>',
            w_mm - 0.6, h_mm - 0.6),
    sprintf('<text x="%.2f" y="%.2f" font-family="Arial" font-size="6" fill="#555" text-anchor="middle">%s</text>',
            w_mm / 2, h_mm / 2 - 1, substr(reason, 1, 60)),
    sprintf('<text x="%.2f" y="%.2f" font-family="Arial" font-size="4.6" fill="#999" text-anchor="middle">%s</text>',
            w_mm / 2, h_mm / 2 + 4, id),
    '</svg>'), path, useBytes = TRUE)
  invisible(path)
}

# A panel authored at its declared box must be embedded at that same box.
hv_verify_scale <- function(figure, panel_paths, tol = 0.01) {
  bad <- character()
  for (it in figure$layout) {
    id <- as.character(it$panel)
    p <- panel_paths[[sprintf("%s@%gx%g", id, as.numeric(it$w), as.numeric(it$h))]]
    if (is.null(p) || !file.exists(p)) {
      stop("no panel rendered for ", id, " at its declared box ",
           it$w, "x", it$h, " mm", call. = FALSE)
    }
    hdr <- paste(readLines(p, n = 4, warn = FALSE), collapse = " ")
    # svglite writes single-quoted attributes; a double-quote-only pattern
    # silently fails to parse and would make this whole check a no-op
    grab <- function(attr) {
      m <- regmatches(hdr, regexpr(paste0(attr, "=['\"]([0-9.]+)pt"), hdr))
      if (!length(m)) return(NA_real_)
      suppressWarnings(as.numeric(sub(paste0(attr, "=['\"]"), "", m)))
    }
    wm <- grab("width"); hm <- grab("height")
    if (!is.finite(wm) || !is.finite(hm)) next        # placeholder svg, mm units
    w_mm <- wm / 72 * 25.4; h_mm <- hm / 72 * 25.4
    if (abs(w_mm - as.numeric(it$w)) > max(tol * as.numeric(it$w), 0.5) ||
        abs(h_mm - as.numeric(it$h)) > max(tol * as.numeric(it$h), 0.5)) {
      bad <- c(bad, sprintf("%s authored %.1fx%.1f mm but placed at %.1fx%.1f mm",
                            id, w_mm, h_mm, as.numeric(it$w), as.numeric(it$h)))
    }
  }
  if (length(bad)) {
    stop("house_v2 panels would be rescaled, which shrinks text below the ",
         "minimum point size: ", paste(bad, collapse = "; "), call. = FALSE)
  }
  invisible(TRUE)
}
