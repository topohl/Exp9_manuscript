# Shared house style for the behaviour figures (Figure 1 and its candidates).
#
# Extracted unchanged from figures/figure_01_panels.R so that the Figure 1
# renderer and any candidate or Extended Data behaviour panel draw with the same
# constants, theme, annotation resolver and SVG device. Nothing here computes a
# statistic: the annotation resolver returns exactly one stored bundle cell per
# printed number, formatted by the sprintf format the annotation map declares.
#
# Contents
#   GROUP_COL / GROUP_LEV / GROUP_SHAPE / SEX_SHAPE / CON_GREY   group and sex encoding
#   INK / MUTED / RULE / GREEN_* / NEUTRAL_BOX                    inks
#   BASE_PT / BODY_PT / NOTE_PT                                   type sizes (pt)
#   BH_NATURE, bh_style_nature()                                  the opt-in Nature style profile
#   theme_f1()                                                    the Figure 1 theme (style = "nature": the profile)
#   BH_FOREST, bh_forest_zero(), bh_forest_ci(), bh_forest_point(), bh_forest_int_point()
#                                                                 forest marks of the behaviour ED renderer
#                                                                 (interaction rows: white diamond)
#   bh_minus(), bh_sci_minus()                                    typographic minus (U+2212)
#   bh_annotation()                                               multi-bundle annotation-map resolver
#   bh_panel(), bh_save_svg()                                     panel box and SVG device
#   bh_save_page(), bh_pdf_mediabox()                             one vector page (SVG + PDF) of placed panels;
#                                                                 the PDF page at its exact (fractional) size

suppressPackageStartupMessages(library(ggplot2))

# ---------------------------------------------------------------- house style
# Group colours from R/plotting_nature.R (byte-identical to MMM_GROUP_COLOURS
# upstream). Shape carries group redundantly.
source(repo_path("R", "plotting_nature.R"))
GROUP_COL <- NATURE_SEMANTIC_PALETTES$group[c("CON", "RES", "SUS")]
GROUP_LEV <- c("CON", "RES", "SUS")
GROUP_SHAPE <- c(CON = 21L, RES = 24L, SUS = 22L)
# Circles for every group: the candidate figures draw group by colour alone (user directive 2026-09-30).
# Opt-in; the canonical Figure 1 keeps GROUP_SHAPE.
GROUP_CIRCLE <- c(CON = 21L, RES = 21L, SUS = 21L)
SEX_SHAPE <- c(Female = 21L, Male = 24L)
CON_GREY <- "grey45"   # CON is descriptive: hollow grey marks and dashed lines (RES is the light group ink)

INK <- "#2B2B2B"; MUTED <- "#6E6E6E"; RULE <- "#B5B5B5"
GREEN_DARK <- "#2F6F62"; GREEN_MID <- "#6FA79B"; GREEN_LIGHT <- "#A8D5CF"; NEUTRAL_BOX <- "#EDEDED"
BASE_PT <- 7; BODY_PT <- 6.5; NOTE_PT <- 6

# ---------------------------------------------------------------- style profiles
# "figure1" is the Figure 1 house style above (the default everywhere). "nature" is the opt-in
# profile of the Nature-layout candidate (NATURE_REDESIGN_SPEC 1-3): two text sizes (5 pt for
# ticks, keys, direct labels and on-panel statistics; 5.5 pt for axis titles, facet strips and
# the few one-line headers; the 8-pt letters are the assembler's), 0.47-pt open L-axes with
# 1.5-pt outward ticks, and one animal-mark vocabulary. It changes sizes and weights only: every
# ink, fill and shape is one of the constants above, and nothing here carries a value.
# Line widths are ggplot2 linewidths (mm-based: 0.22 draws 0.47 pt); point sizes and strokes are
# ggplot2 size / stroke (0.95 with stroke 0.26 draws a 2.4-pt circle with a 0.37-pt outline).
BH_NATURE <- list(
  text_pt = 5, title_pt = 5.5,
  axis_lw = 0.22, tick_len_pt = 1.5,
  point_size = 0.95, point_stroke = 0.26, point_colour = "grey20",  # animal marks (b, c, e, f), opaque
  jitter_width = 0.14,                                              # the same jitter half-width in b and c
  mean_size = 1.4, mean_stroke = 0.3, mean_lw = 0.3,                # c: model mean and its capless 95% CI
  forest = list(point_size = 1.1, point_stroke = 0.25, int_size = 1.0, int_stroke = 0.25, ci_linewidth = 0.3),  # d: lighter than c
  rule_lw = 0.14, identity_lw = 0.235, bracket_lw = 0.23,           # CON-mean rule, identity line, a's brackets
  header_indent_mm = 3.2)                                           # a header on the letter's line starts here
BH_STYLES <- c("figure1", "nature")

#' TRUE for the Nature profile, FALSE for the Figure 1 house style (the default everywhere).
bh_style_nature <- function(style) {
  if (!is.character(style) || length(style) != 1L || !style %in% BH_STYLES)
    stop("style must be one of ", paste(sprintf('"%s"', BH_STYLES), collapse = ", "), ".", call. = FALSE)
  identical(style, "nature")
}

#' The Figure 1 theme. `style = "nature"` returns the same theme at the Nature profile's sizes and
#' weights (BH_NATURE); the default draws Figure 1 exactly as before.
theme_f1 <- function(base = BASE_PT, style = "figure1") {
  nature <- bh_style_nature(style)
  if (nature) base <- BH_NATURE$title_pt
  th <- theme_classic(base_size = base, base_family = "sans") +
    theme(
      axis.text = element_text(size = BODY_PT, colour = INK),
      axis.title = element_text(size = base, colour = INK),
      axis.line = element_line(linewidth = 0.3, colour = "black"),
      axis.ticks = element_line(linewidth = 0.3, colour = "black"),
      axis.ticks.length = unit(2, "pt"),
      strip.background = element_blank(),
      strip.text = element_text(size = base, colour = INK, face = "italic", margin = margin(b = 1.5, t = 1)),
      legend.position = "top", legend.justification = "left",
      legend.key.height = unit(7, "pt"), legend.key.width = unit(9, "pt"),
      legend.text = element_text(size = NOTE_PT), legend.title = element_blank(),
      legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(0, 0, -3.5, 0),
      panel.grid = element_blank(),
      plot.title = element_text(size = base, colour = INK, hjust = 0, face = "plain", margin = margin(b = 0.5)),
      plot.subtitle = element_text(size = NOTE_PT, colour = "grey25", margin = margin(b = 1.5)),
      plot.caption = element_text(size = NOTE_PT, colour = INK, hjust = 0, lineheight = 1.05, margin = margin(t = 1.5)),
      # 4 mm of top margin: the assembler draws the panel letter over the top-left corner.
      plot.margin = margin(11.5, 3, 3, 3))
  if (!nature) return(th)
  N <- BH_NATURE
  th + theme(
    axis.text = element_text(size = N$text_pt, colour = INK),
    axis.title = element_text(size = N$title_pt, colour = INK),
    axis.line = element_line(linewidth = N$axis_lw, colour = "black"),
    axis.ticks = element_line(linewidth = N$axis_lw, colour = "black"),
    axis.ticks.length = unit(N$tick_len_pt, "pt"),
    strip.text = element_text(size = N$title_pt, colour = INK, face = "plain", margin = margin(b = 1, t = 0)),
    legend.text = element_text(size = N$text_pt, margin = margin(l = 0.8)),
    legend.key.height = unit(5, "pt"), legend.key.width = unit(5, "pt"),
    legend.key.spacing.x = unit(3.5, "pt"),
    plot.title = element_text(size = N$title_pt, colour = INK, hjust = 0, face = "plain", margin = margin(b = 0.5)),
    plot.subtitle = element_text(size = N$text_pt, colour = "grey25", margin = margin(b = 1)),
    plot.caption = element_text(size = N$text_pt, colour = INK, hjust = 0, margin = margin(t = 1)),
    plot.margin = margin(0, 1, 1, 1))
}

# ---------------------------------------------------------------- forest marks
# The estimate / interval / zero-line marks of the behaviour Extended Data
# renderer (figures/extended_data_behaviour_panels.R): point shape 21 size 1.9
# stroke 0.3; interval linewidth 0.7 in MUTED with round ends; zero reference
# dashed "22", linewidth 0.3, RULE.
# Interaction rows (the Female - male difference) take their own shape, a white
# diamond (23), so that no fill state reads as a significance code: Figures 2
# and 3 draw FDR-supported points filled and the others open (white).
BH_FOREST <- list(
  point_shape = 21L, point_size = 1.9, point_stroke = 0.3, point_colour = "grey20", point_fill = "white",
  ci_linewidth = 0.7, ci_colour = MUTED, ci_lineend = "round",
  zero_linetype = "22", zero_linewidth = 0.3, zero_colour = RULE,
  int_shape = 23L, int_size = 1.7, int_stroke = 0.3, int_fill = "white")

bh_forest_zero <- function(xintercept = 0, horizontal = FALSE, marks = BH_FOREST) {
  if (horizontal)
    return(geom_hline(yintercept = xintercept, linetype = marks$zero_linetype,
                      linewidth = marks$zero_linewidth, colour = marks$zero_colour))
  geom_vline(xintercept = xintercept, linetype = marks$zero_linetype,
             linewidth = marks$zero_linewidth, colour = marks$zero_colour)
}
bh_forest_ci <- function(mapping, ..., marks = BH_FOREST) {
  geom_segment(mapping, linewidth = marks$ci_linewidth, colour = marks$ci_colour,
               lineend = marks$ci_lineend, ...)
}
bh_forest_point <- function(mapping, fill = BH_FOREST$point_fill, ..., marks = BH_FOREST) {
  geom_point(mapping, shape = marks$point_shape, size = marks$point_size,
             stroke = marks$point_stroke, colour = marks$point_colour, fill = fill, ...)
}
#' The interaction-row mark (Female - male): a white diamond, never a fill-coded circle.
bh_forest_int_point <- function(mapping, ..., marks = BH_FOREST) {
  geom_point(mapping, shape = marks$int_shape, size = marks$int_size,
             stroke = marks$int_stroke, colour = marks$point_colour, fill = marks$int_fill, ...)
}

# ---------------------------------------------------------------- typography
#' Replace a leading hyphen-minus with the typographic minus sign U+2212.
bh_minus <- function(x) sub("^-", "−", x)
#' The typographic minus in the exponent of an e-notation string, without its leading
#' zeros ("2.61e-05" -> "2.61e−5"). String handling only; opt-in (candidate typography).
bh_sci_minus <- function(x) sub("e-0*([0-9])", "e−\\1", x)

# ---------------------------------------------------------------- annotation map
#' Resolver for an annotation map: every printed number is one stored cell.
#'
#' `map_csv` is the path of an annotation map with columns
#' key, panel, [bundle,] table, column, filters, format, meaning.
#' `tables` is a named list of table getters, one per bundle label, each
#' function(table_name) returning that bundle table as read from its pinned copy
#' (e.g. list(ebb = <behaviour bundle getter>, s30b = <Stage 30 bundle getter>)).
#' A map without a `bundle` column (figures/figure_01_annotation_map.csv) resolves
#' every row against `default_bundle`, the first getter. `filters` is
#' "name=value;name=value" (value NA matches a missing cell); an empty filter
#' selects the whole table, which bundle_cell() accepts only if it has one row.
#' `cell` is the one-cell extractor, bundle_cell() from R/behaviour_bundle.R.
#'
#' Returns list(ann, fa, ci, map, row, resolved):
#'   ann(key)  the stored value (unformatted);
#'   fa(key)   the value formatted with the map's sprintf format ("%d" rounds to
#'             integer first), leading minus as U+2212;
#'   ci(k)     "est [lo, hi]" from keys k, k_lo, k_hi;
#'   resolved() the keys resolved so far, in first-use order.
bh_annotation <- function(map_csv, tables, cell = bundle_cell, default_bundle = names(tables)[1]) {
  if (!is.list(tables) || !length(tables) || is.null(names(tables)) || any(!nzchar(names(tables))))
    stop("bh_annotation: tables must be a named list of table getters.", call. = FALSE)
  amap <- utils::read.csv(map_csv, stringsAsFactors = FALSE)
  if (anyDuplicated(amap$key)) stop(basename(map_csv), " has duplicated keys.", call. = FALSE)
  if (!"bundle" %in% names(amap)) amap$bundle <- default_bundle
  unknown <- setdiff(unique(amap$bundle), names(tables))
  if (length(unknown)) stop("bh_annotation: map names bundle(s) with no table getter: ", paste(unknown, collapse = ", "), call. = FALSE)
  seen <- character(0)
  map_row <- function(key) {
    r <- amap[amap$key == key, , drop = FALSE]
    if (nrow(r) != 1L) stop("annotation key not declared: ", key, call. = FALSE)
    r
  }
  ann <- function(key) {
    r <- map_row(key)
    filters <- list()
    if (!is.na(r$filters) && nzchar(r$filters)) {
      kv <- strsplit(strsplit(r$filters, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)
      filters <- stats::setNames(lapply(kv, function(x) if (identical(x[[2]], "NA")) NA else x[[2]]),
                                 vapply(kv, `[[`, "", 1L))
    }
    if (!key %in% seen) seen <<- c(seen, key)
    do.call(cell, c(list(tables[[r$bundle]](r$table), r$column), filters))
  }
  fa <- function(key) {
    r <- map_row(key); v <- ann(key)
    if (grepl("%d", r$format, fixed = TRUE)) v <- as.integer(round(v))
    bh_minus(sprintf(r$format, v))
  }
  ci <- function(k) sprintf("%s [%s, %s]", fa(k), fa(paste0(k, "_lo")), fa(paste0(k, "_hi")))
  list(ann = ann, fa = fa, ci = ci, map = amap, row = map_row, resolved = function() seen)
}

# ---------------------------------------------------------------- panel box and device
#' A panel plot together with the box (mm) it is authored for.
bh_panel <- function(plot, w_mm, h_mm) {
  if (!is.numeric(w_mm) || !is.numeric(h_mm) || length(w_mm) != 1L || length(h_mm) != 1L || !(w_mm > 0) || !(h_mm > 0))
    stop("bh_panel: box width and height must be single positive numbers (mm).", call. = FALSE)
  structure(list(plot = plot, w_mm = w_mm, h_mm = h_mm), class = "bh_panel")
}

#' Write a panel as SVG at its exact box, so the absolute-layout assembler places it at scale 1.
#' `p` is a ggplot/patchwork object, or a bh_panel whose box is used when w_mm/h_mm are not given.
bh_save_svg <- function(p, path, w_mm = NULL, h_mm = NULL) {
  if (inherits(p, "bh_panel")) {
    if (is.null(w_mm)) w_mm <- p$w_mm
    if (is.null(h_mm)) h_mm <- p$h_mm
    p <- p$plot
  }
  if (is.null(w_mm) || is.null(h_mm)) stop("bh_save_svg: the panel box (w_mm, h_mm) is required.", call. = FALSE)
  svglite::svglite(path, width = w_mm / 25.4, height = h_mm / 25.4, bg = "white", fix_text_size = FALSE)
  print(p); grDevices::dev.off()
  path
}

#' Write placed panels as ONE vector page: each panel's plot drawn into a grid viewport at its box
#' (x, y from the page's top-left corner, w, h; mm) on one device, and each panel letter drawn over
#' its box's top-left corner as manuscript_figure_assemble_svg draws it in absolute mode (bold,
#' `letter_pt`, left edge at the box x, baseline 0.92 letter heights below the box top). The same
#' drawing goes to an SVG (svglite, fix_text_size = FALSE as bh_save_svg: live text, no embedded
#' images, deterministic) and to a PDF (cairo_pdf, family Arial: fonts embedded as TrueType subsets,
#' no raster images). `panels` is a list of list(panel = <bh_panel>, x, y, letter). Returns both paths.
bh_save_page <- function(panels, width_mm, height_mm, svg_path, pdf_path, letter_pt = 8) {
  lab_mm <- letter_pt * 25.4 / 72
  draw <- function() {
    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(col = NA, fill = "white"))
    for (p in panels) {
      grid::pushViewport(grid::viewport(x = grid::unit(p$x, "mm"), y = grid::unit(height_mm - p$y, "mm"),
                                        width = grid::unit(p$panel$w_mm, "mm"), height = grid::unit(p$panel$h_mm, "mm"),
                                        just = c("left", "top")))
      plot <- p$panel$plot
      grid::grid.draw(if (inherits(plot, "patchwork")) patchwork::patchworkGrob(plot) else ggplot2::ggplotGrob(plot))
      grid::popViewport()
    }
    for (p in panels) if (nzchar(p$letter))
      grid::grid.text(p$letter, x = grid::unit(p$x, "mm"), y = grid::unit(height_mm - p$y - lab_mm * 0.92, "mm"),
                      hjust = 0, vjust = 0, gp = grid::gpar(fontsize = letter_pt, fontface = "bold", fontfamily = "sans", col = "black"))
  }
  svglite::svglite(svg_path, width = width_mm / 25.4, height = height_mm / 25.4, bg = "white", fix_text_size = FALSE)
  tryCatch(draw(), finally = grDevices::dev.off())
  grDevices::cairo_pdf(pdf_path, width = width_mm / 25.4, height = height_mm / 25.4, family = "Arial", bg = "white")
  tryCatch(draw(), finally = grDevices::dev.off())
  bh_pdf_mediabox(pdf_path, width_mm / 25.4 * 72, height_mm / 25.4 * 72)
  c(svg = svg_path, pdf = pdf_path)
}

#' Give a one-page PDF its exact page size. R's cairo device authors the page at whole points (it
#' truncates the size: 183 x 160 mm = 518.74 x 453.54 pt is written as a 518 x 453 MediaBox), while
#' bh_save_page places every box from the page's bottom-left corner in mm, so only the top and right
#' edges lose the fraction. This appends one incremental update (ISO 32000-1, 7.5.6) that redefines
#' the page object with /MediaBox [0 0 width_pt height_pt]; the bytes cairo wrote are kept, and the
#' drawing is unchanged. Handles a page object stored plainly or in a compressed object stream, and
#' a cross-reference stream (cairo >= 1.17.6) or a classic cross-reference table. Layout only.
bh_pdf_mediabox <- function(path, width_pt, height_pt) {
  fail <- function(...) stop("bh_pdf_mediabox (", basename(path), "): ", ..., call. = FALSE)
  raw <- readBin(path, "raw", file.size(path))
  bytes_text <- function(r) { r[r == as.raw(0)] <- as.raw(32); s <- rawToChar(r); Encoding(s) <- "bytes"; s }
  txt <- bytes_text(raw)
  find <- function(pattern, x) { m <- regexpr(pattern, x, perl = TRUE, useBytes = TRUE); if (m < 0) NA_character_ else regmatches(x, m) }
  # the dictionary that opens at byte `at` (1-based; balanced << >>)
  dict_at <- function(s, at) {
    depth <- 0L; i <- at; n <- nchar(s, type = "bytes")
    while (i < n) {
      two <- substr(s, i, i + 1L)
      if (identical(two, "<<")) { depth <- depth + 1L; i <- i + 2L; next }
      if (identical(two, ">>")) { depth <- depth - 1L; i <- i + 2L; if (depth == 0L) return(substr(s, at, i - 1L)); next }
      i <- i + 1L
    }
    fail("an unterminated dictionary")
  }
  num <- function(pattern, s) as.numeric(sub(pattern, "\\1", find(pattern, s), perl = TRUE))
  # the last cross-reference section and its trailer
  prev <- num("(?s)startxref\\s+([0-9]+)\\s+%%EOF\\s*$", txt)
  if (is.na(prev)) fail("no startxref")
  sec <- substr(txt, prev + 1, nchar(txt, type = "bytes"))
  stream_xref <- !startsWith(sec, "xref")
  trailer <- if (stream_xref) dict_at(sec, regexpr("<<", sec, fixed = TRUE, useBytes = TRUE)) else
    dict_at(sec, regexpr("trailer\\s*<<", sec, perl = TRUE, useBytes = TRUE) + 7L)
  if (stream_xref && !grepl("/Type\\s*/XRef", trailer, useBytes = TRUE)) fail("the last section is neither a table nor a stream")
  size <- num("/Size\\s+([0-9]+)", trailer)
  keep <- na.omit(c(find("/Root\\s+[0-9]+\\s+[0-9]+\\s+R", trailer), find("/Info\\s+[0-9]+\\s+[0-9]+\\s+R", trailer),
                    find("/ID\\s*\\[[^]]*\\]", trailer)))
  # every object's dictionary, plain or in an object stream: the one page object
  objs <- list()
  starts <- gregexpr("(?<![0-9])[0-9]+\\s+[0-9]+\\s+obj\\s*<<", txt, perl = TRUE, useBytes = TRUE)[[1]]
  for (st in starts[starts > 0]) {
    head <- find("^[0-9]+\\s+[0-9]+\\s+obj", substr(txt, st, st + 40L))
    ids <- as.numeric(strsplit(trimws(sub("obj$", "", head)), "\\s+")[[1]])
    at <- st + regexpr("<<", substr(txt, st, st + 60L), fixed = TRUE, useBytes = TRUE) - 1L
    d <- dict_at(txt, at)
    objs[[length(objs) + 1L]] <- list(num = ids[1], gen = ids[2], dict = d)
    if (!grepl("/Type\\s*/ObjStm", d, useBytes = TRUE)) next
    len <- find("/Length\\s+[0-9]+(\\s+[0-9]+\\s+R)?", d)
    len <- as.numeric(strsplit(sub("\\s+R$", "", sub("^/Length\\s+", "", len)), "\\s+")[[1]])
    if (length(len) == 2L)   # an indirect length "n g R": the integer object n g
      len <- num(sprintf("(?<![0-9])%.0f\\s+%.0f\\s+obj\\s*([0-9]+)\\s*endobj", len[1], len[2]), txt)
    e <- at + nchar(d, type = "bytes")   # the byte after the dictionary
    kw <- regexpr("^\\s*stream\r?\n", substr(txt, e, e + 20L), perl = TRUE, useBytes = TRUE)
    if (kw < 0) fail("an object stream without its stream keyword")
    s0 <- e + attr(kw, "match.length")   # the first data byte (1-based)
    if (!grepl("/Filter\\s*/FlateDecode", d, useBytes = TRUE) || grepl("/DecodeParms", d, useBytes = TRUE))
      fail("an object stream that is not plain FlateDecode")
    dec <- memDecompress(raw[s0:(s0 + len - 1)], type = "gzip")
    first <- num("/First\\s+([0-9]+)", d); n_obj <- num("/N\\s+([0-9]+)", d)
    pairs <- as.numeric(strsplit(trimws(rawToChar(dec[seq_len(first)])), "\\s+")[[1]])
    if (length(pairs) != 2 * n_obj) fail("an object stream header that does not list /N objects")
    off <- c(pairs[c(FALSE, TRUE)], length(dec) - first)
    for (k in seq_len(n_obj)) {
      body <- bytes_text(dec[(first + off[k] + 1):(first + off[k + 1])])
      if (grepl("<<", body, fixed = TRUE))
        objs[[length(objs) + 1L]] <- list(num = pairs[2 * k - 1], gen = 0, dict = dict_at(body, regexpr("<<", body, fixed = TRUE, useBytes = TRUE)))
    }
  }
  page <- Filter(function(o) grepl("/Type\\s*/Page(?![a-zA-Z])", o$dict, perl = TRUE, useBytes = TRUE), objs)
  page <- page[!duplicated(vapply(page, function(o) o$num, 0))]
  if (length(page) != 1L) fail("expected one page object, found ", length(page))
  page <- page[[1]]
  box <- sprintf("/MediaBox [ 0 0 %s %s ]", formatC(width_pt, format = "f", digits = 4), formatC(height_pt, format = "f", digits = 4))
  dict <- if (grepl("/MediaBox", page$dict, fixed = TRUE)) sub("/MediaBox\\s*\\[[^]]*\\]", box, page$dict, perl = TRUE, useBytes = TRUE) else
    sub(">>$", paste0(" ", box, " >>"), page$dict, useBytes = TRUE)
  # the update: the page object, then its cross-reference section and trailer
  out <- raw
  if (out[length(out)] != as.raw(10)) out <- c(out, as.raw(10))
  add <- function(s) out <<- c(out, charToRaw(s))
  page_off <- length(out)
  add(sprintf("%d %d obj\n%s\nendobj\n", page$num, page$gen, dict))
  xref_off <- length(out)
  if (stream_xref) {
    be <- function(v, n) as.raw(rev(vapply(seq_len(n) - 1L, function(i) (v %/% 256^i) %% 256, 0)))
    data <- c(as.raw(1), be(page_off, 4), be(page$gen, 2), as.raw(1), be(xref_off, 4), be(0, 2))
    add(sprintf("%d 0 obj\n<< /Type /XRef /Size %d /Index [ %d 1 %d 1 ] /W [ 1 4 2 ] %s /Prev %.0f /Length %d >>\nstream\n",
                size, size + 1, page$num, size, paste(keep, collapse = " "), prev, length(data)))
    out <- c(out, data)
    add("\nendstream\nendobj\n")
  } else {
    add(sprintf("xref\n%d 1\n%010.0f %05.0f n \ntrailer\n<< /Size %d %s /Prev %.0f >>\n", page$num, page_off, page$gen,
                max(size, page$num + 1), paste(keep, collapse = " "), prev))
  }
  add(sprintf("startxref\n%.0f\n%%%%EOF\n", xref_off))
  writeBin(out, path)
  invisible(path)
}

#' The review PNG of a vector page: the page SVG rasterised by magick (librsvg) at `dpi`, on white.
#' Preview only; the SVG and the PDF are the deliverables.
bh_page_png <- function(svg_path, png_path, dpi = 300) {
  image <- magick::image_read(svg_path, density = dpi)
  image <- magick::image_background(image, "white", flatten = TRUE)
  magick::image_write(image, path = png_path, format = "png")
  png_path
}
