#!/usr/bin/env Rscript

# Colour-vision and greyscale check of the published figures (palette QA).
#
# Every canonical figure (provenance/publication_registry/canonical_publication_registry.csv)
# is shown as drawn and as it appears under deuteranopia, protanopia and
# tritanopia (colorspace simulations at full severity) and in greyscale, one
# sheet per figure. A swatch sheet shows the palette's group colours, its
# diverging ramp and its evidence and claimability classes under the same views,
# and palette_cvd_distances.csv gives the CIEDE2000 distance of every pair that
# must stay apart. It reads only the tracked renders and
# config/manuscript_palette.yml and writes only to
# results/reports/manuscript_palette/cvd_<palette_version>/, regenerated whole;
# nothing it writes is an input to a figure.
#
# Usage: Rscript tools/palette_cvd_contact_sheet.R

suppressWarnings(source(file.path("R", "paths.R")))
for (pkg in c("colorspace", "farver", "magick", "scales", "yaml"))
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("Required package unavailable: ", pkg, call. = FALSE)

pal <- yaml::read_yaml(repo_path("config", "manuscript_palette.yml"))
out <- path_results("reports", "manuscript_palette", paste0("cvd_", pal$palette_version))
unlink(out, recursive = TRUE)
dir_create(out)

VIEWS <- list(
  "as drawn" = function(h) h,
  "deuteranopia" = function(h) colorspace::deutan(h, severity = 1),
  "protanopia" = function(h) colorspace::protan(h, severity = 1),
  "tritanopia" = function(h) colorspace::tritan(h, severity = 1),
  "greyscale" = function(h) colorspace::desaturate(h))

# one view of a raster, computed on its distinct colours only
view_of <- function(img, fun) {
  m <- as.matrix(as.raster(img))
  u <- unique(as.vector(m))
  seen <- fun(toupper(substr(u, 1, 7)))
  magick::image_read(as.raster(matrix(seen[match(m, u)], nrow = nrow(m))))
}
labelled <- function(img, text, size = 30) {
  bar <- magick::image_blank(magick::image_info(img)$width, round(size * 1.8), "white")
  bar <- magick::image_annotate(bar, text, size = size, gravity = "center", font = "Arial",
                                color = "black")
  magick::image_append(c(bar, img), stack = TRUE)
}

# ------------------------------------------------------------- figure sheets
reg <- utils::read.csv(repo_path("provenance", "publication_registry",
                                 "canonical_publication_registry.csv"),
                       stringsAsFactors = FALSE, colClasses = "character")
reg <- reg[reg$status == "CANONICAL", , drop = FALSE]
sheets <- character()
for (i in seq_len(nrow(reg))) {
  pid <- reg$publication_id[i]
  png <- repo_path(sub("[.]svg$", ".png", reg$rendered_artifact[i]))
  if (!file.exists(png)) {
    message("no PNG beside the registered render of ", pid, "; skipped")
    next
  }
  img <- magick::image_background(magick::image_read(png), "white", flatten = TRUE)
  img <- magick::image_scale(img, "800")
  views <- lapply(names(VIEWS), function(v) labelled(view_of(img, VIEWS[[v]]), v))
  sheet <- labelled(magick::image_append(do.call(c, views)), pid, size = 36)
  f <- file.path(out, paste0(pid, "_cvd.png"))
  magick::image_write(sheet, f, format = "png")
  sheets <- c(sheets, basename(f))
}

# ------------------------------------------------------------- swatches
lim <- seq(-1, 1, by = 0.25)
SETS <- list(
  group = toupper(unlist(pal$group)),
  diverging = stats::setNames(toupper(scales::div_gradient_pal(
    pal$diverging$low, pal$diverging$mid, pal$diverging$high, "Lab")((lim + 1) / 2)),
    ifelse(lim == 0, "0", ifelse(lim < 0, paste0("-", abs(lim)), paste0("+", lim)))),
  evidence = toupper(unlist(pal$evidence)),
  claimability = toupper(unlist(pal$claimability)))
swatch_row <- function(cols, labels) {
  blocks <- lapply(seq_along(cols), function(k) {
    b <- magick::image_border(magick::image_blank(110, 70, cols[[k]]), "grey60", "1x1")
    text <- paste(strwrap(gsub("_", " ", labels[[k]]), width = 13), collapse = "\n")
    magick::image_append(c(b, magick::image_annotate(magick::image_blank(112, 56, "white"),
      text, size = 15, gravity = "north", font = "Arial", color = "black")), stack = TRUE)
  })
  magick::image_append(do.call(c, blocks))
}
# every view of every set in a cell of one width, so the views line up in columns
cell_w <- max(vapply(SETS, length, integer(1))) * 112L + 40L
cell <- function(img) magick::image_extent(img, sprintf("%dx%d", cell_w, magick::image_info(img)$height + 20L),
                                           gravity = "center", color = "white")
rows <- lapply(names(SETS), function(s) {
  cells <- lapply(names(VIEWS), function(v) cell(swatch_row(VIEWS[[v]](SETS[[s]]), names(SETS[[s]]))))
  labelled(magick::image_append(do.call(c, cells)),
           if (s == "diverging") "diverging ramp (fraction of the measure's limit)" else s, size = 24)
})
head_row <- magick::image_append(do.call(c, lapply(names(VIEWS), function(v)
  magick::image_annotate(magick::image_blank(cell_w, 50, "white"), v, size = 26, gravity = "center",
                         font = "Arial", color = "black"))))
swatches <- magick::image_background(magick::image_append(c(head_row, do.call(c, rows)), stack = TRUE),
                                     "white", flatten = TRUE)
magick::image_write(swatches, file.path(out, "palette_swatches_cvd.png"), format = "png")

# ------------------------------------------------------------- distances
de <- function(a, b) as.numeric(farver::compare_colour(
  farver::decode_colour(a), farver::decode_colour(b), from_space = "rgb", method = "cie2000"))
pairs <- list(c("group", "CON", "RES"), c("group", "CON", "SUS"), c("group", "RES", "SUS"))
dist <- list()
for (v in names(VIEWS)) {
  for (p in pairs) {
    set <- VIEWS[[v]](SETS[[p[1]]])
    names(set) <- names(SETS[[p[1]]])
    dist[[length(dist) + 1L]] <- data.frame(view = v, set = p[1], pair = paste(p[2], p[3], sep = "-"),
      delta_e_2000 = round(de(set[[p[2]]], set[[p[3]]]), 1), stringsAsFactors = FALSE)
  }
  d <- VIEWS[[v]](toupper(c(low = pal$diverging$low, mid = pal$diverging$mid, high = pal$diverging$high)))
  for (p in list(c(1, 2), c(3, 2), c(1, 3)))
    dist[[length(dist) + 1L]] <- data.frame(view = v, set = "diverging",
      pair = paste(c("low", "mid", "high")[p], collapse = "-"),
      delta_e_2000 = round(de(d[p[1]], d[p[2]]), 1), stringsAsFactors = FALSE)
  # the closest pair of each class set
  for (s in c("evidence", "claimability")) {
    t <- VIEWS[[v]](SETS[[s]])
    best <- c(Inf, NA, NA)
    for (a in seq_along(t)[-length(t)]) for (b in seq(a + 1L, length(t))) {
      x <- de(t[[a]], t[[b]])
      if (x < as.numeric(best[1])) best <- c(x, names(SETS[[s]])[a], names(SETS[[s]])[b])
    }
    dist[[length(dist) + 1L]] <- data.frame(view = v, set = s,
      pair = paste0("closest: ", best[2], "-", best[3]),
      delta_e_2000 = round(as.numeric(best[1]), 1), stringsAsFactors = FALSE)
  }
}
dist <- do.call(rbind, dist)
utils::write.csv(dist, file.path(out, "palette_cvd_distances.csv"), row.names = FALSE)

# ------------------------------------------------------------- README
keys <- unique(dist[, c("set", "pair")])
tab <- c(paste0("| set | pair | ", paste(names(VIEWS), collapse = " | "), " |"),
         paste0("|", paste(rep("---", length(VIEWS) + 2L), collapse = "|"), "|"))
for (k in seq_len(nrow(keys))) {
  v <- vapply(names(VIEWS), function(w) dist$delta_e_2000[dist$set == keys$set[k] &
    dist$pair == keys$pair[k] & dist$view == w], numeric(1))
  tab <- c(tab, paste0("| ", keys$set[k], " | ", keys$pair[k], " | ",
                       paste(format(v, nsmall = 1), collapse = " | "), " |"))
}
writeLines(c(
  paste0("# Colour-vision and greyscale check, ", pal$palette_version), "",
  "Generated by `tools/palette_cvd_contact_sheet.R` from the tracked renders of the",
  "canonical figures and `config/manuscript_palette.yml`; regenerated whole on every",
  "run. Nothing here is an input to a figure.", "",
  "- `<publication_id>_cvd.png`: the figure as drawn, then under deuteranopia,",
  "  protanopia and tritanopia (colorspace, severity 1) and in greyscale.",
  "- `palette_swatches_cvd.png`: the group colours, the diverging ramp from minus to",
  "  plus the measure's limit, and the evidence and claimability classes, in the",
  "  same views.",
  "- `palette_cvd_distances.csv`: CIEDE2000 distance of each pair that must stay",
  "  apart, per view (for the class sets, their closest pair).", "",
  "RES is the light, near-grey group by design, so group is never shown by colour",
  "alone: shape, line type or position carry it too.", "",
  paste0("Figures: ", length(sheets), " (", paste(sub("_cvd[.]png$", "", sheets), collapse = ", "), ")."), "",
  tab), file.path(out, "README.md"))
cat("written to", relative_to(out), "\n")
