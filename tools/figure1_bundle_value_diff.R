#!/usr/bin/env Rscript

# Figure 1 value diff between two imported behaviour bundles.
#
# Run by hand after a bundle re-pin, from the repository root:
#   Rscript tools/figure1_bundle_value_diff.R <old_bundle_id> <new_bundle_id> \
#     [--old-svg DIR --new-svg DIR] [--out provenance/source_manifests/<file>.csv]
#
# Both bundles must already be imported under source_data/MMMSociability/ and
# each is verified against its own manifest and the import manifest first. The
# tool computes nothing scientific: it resolves every Figure 1 annotation key in
# each bundle through the same resolver the renderer uses (bh_annotation +
# bundle_cell), extracts every plotted stored model-mean row and the plotted CON
# descriptive rows (row filters only), hashes the tables the renderer reads, and,
# if two rendered panel directories are given, lists the SVG text strings that
# differ per panel. It reports equality, nothing else.
#
# Output columns: kind, panel, key, table, column, filters, format, old_raw,
# new_raw, old_printed, new_printed, raw_changed, printed_changed.

suppressWarnings(source(file.path("R", "paths.R")))
source(repo_path("R", "behaviour_bundle.R"))
source(repo_path("R", "behaviour_figure_style.R"))
source(repo_path("R", "behaviour_figure1_panels.R"))

args <- commandArgs(trailingOnly = TRUE)
opt <- function(flag) { i <- match(flag, args); if (is.na(i)) NULL else args[[i + 1L]] }
pos <- args[!args %in% c("--old-svg", "--new-svg", "--out") & !seq_along(args) %in% (match(c("--old-svg", "--new-svg", "--out"), args) + 1L)]
if (length(pos) != 2L) stop("usage: Rscript tools/figure1_bundle_value_diff.R <old_bundle_id> <new_bundle_id> [--old-svg DIR --new-svg DIR] [--out FILE]", call. = FALSE)
OLD <- pos[[1]]; NEW <- pos[[2]]
short <- function(id) sub("^ebb_(v[0-9]+)_.*$", "\\1", id)
OUT <- opt("--out") %||% repo_path("provenance", "source_manifests", sprintf("figure1_%s_to_%s_value_diff.csv", short(OLD), short(NEW)))

# ---------------------------------------------------------------- verify both copies
imp <- utils::read.csv(repo_path("provenance", "source_manifests", "behaviour_bundle_manifest.csv"), stringsAsFactors = FALSE)
pin_of <- function(id) {
  m <- imp[imp$bundle_id == id & imp$file == "00_manifest.csv", , drop = FALSE]
  if (nrow(m) != 1L) stop("bundle not recorded in behaviour_bundle_manifest.csv: ", id, call. = FALSE)
  pin <- list(bundle_id = id, manifest_sha256 = m$sha256)
  behaviour_bundle_verify(pin)
  pin
}
PINS <- list(old = pin_of(OLD), new = pin_of(NEW))
getter <- function(pin) {
  cache <- new.env(parent = emptyenv())
  function(name) {
    if (!exists(name, envir = cache, inherits = FALSE)) assign(name, behaviour_bundle_table(name, pin), envir = cache)
    get(name, envir = cache, inherits = FALSE)
  }
}
TABS <- lapply(PINS, getter)
MAP <- repo_path("figures", "figure_01_annotation_map.csv")
AN <- lapply(TABS, function(t) bh_annotation(MAP, tables = list(ebb = t), cell = bundle_cell))
raw <- function(v) if (is.numeric(v)) sprintf("%.15g", v) else as.character(v)
rows <- list()
add <- function(...) rows[[length(rows) + 1L]] <<- data.frame(..., stringsAsFactors = FALSE)

# ---------------------------------------------------------------- annotation keys
amap <- AN$old$map
for (i in seq_len(nrow(amap))) {
  k <- amap$key[i]
  o <- raw(AN$old$ann(k)); n <- raw(AN$new$ann(k))
  op <- AN$old$fa(k); np <- AN$new$fa(k)
  add(kind = "annotation_key", panel = amap$panel[i], key = k, table = amap$table[i], column = amap$column[i],
      filters = amap$filters[i], format = amap$format[i], old_raw = o, new_raw = n, old_printed = op, new_printed = np,
      raw_changed = !identical(o, n), printed_changed = !identical(op, np))
}

# ---------------------------------------------------------------- plotted model means (C2, row filters only)
mm_rows <- function(tabf, prefix, construct) f1_model_means(tabf("C2_estimates"), prefix, construct)
PLOTTED <- list(list(panel = "1c", prefix = "^mean_(RES|SUS)_CC1$"), list(panel = "1d", prefix = "^mean_(RES|SUS)_TR_CC[1-4]$"))
for (pl in PLOTTED) for (k in c("crossing_rate", "shared_zone_use")) {
  o <- mm_rows(TABS$old, pl$prefix, k); n <- mm_rows(TABS$new, pl$prefix, k)
  if (pl$panel == "1c") { o <- o[o$sex %in% c("Female", "Male"), , drop = FALSE]; n <- n[n$sex %in% c("Female", "Male"), , drop = FALSE] }
  if (nrow(o) != nrow(n)) stop("plotted model-mean rows differ in number for ", k, " ", pl$panel, call. = FALSE)
  for (j in seq_len(nrow(o))) {
    m <- which(n$model_id == o$model_id[j] & n$estimand == o$estimand[j] & n$sex == o$sex[j])
    if (length(m) != 1L) stop("no unique v-new row for ", o$model_id[j], " ", o$estimand[j], call. = FALSE)
    for (col in c("estimate", "ci_low", "ci_high")) {
      ov <- raw(o[[col]][j]); nv <- raw(n[[col]][m])
      add(kind = "plotted_model_mean", panel = pl$panel, key = paste(o$model_id[j], o$estimand[j], sep = "|"), table = "C2_estimates",
          column = col, filters = sprintf("source=stage29;model_id=%s;estimand=%s;sex=%s", o$model_id[j], o$estimand[j], o$sex[j]),
          format = NA_character_, old_raw = ov, new_raw = nv, old_printed = NA_character_, new_printed = NA_character_,
          raw_changed = !identical(ov, nv), printed_changed = NA)
    }
  }
}

# ---------------------------------------------------------------- plotted CON descriptive rows (B2, panel d)
for (k in c("crossing_rate", "shared_zone_use")) {
  o <- TABS$old("B2_descriptive_summaries"); n <- TABS$new("B2_descriptive_summaries")
  o <- o[o$construct == k & o$Group == "CON", , drop = FALSE]
  for (j in seq_len(nrow(o))) {
    m <- which(n$construct == k & n$Group == "CON" & n$CC == o$CC[j] & n$Sex == o$Sex[j])
    if (length(m) != 1L) stop("no unique CON row in the new B2 for ", k, call. = FALSE)
    ov <- raw(o$mean[j]); nv <- raw(n$mean[m])
    add(kind = "plotted_con_descriptive", panel = "1d", key = paste(o$CC[j], o$Sex[j], k, "CON", sep = "|"), table = "B2_descriptive_summaries",
        column = "mean", filters = sprintf("construct=%s;Group=CON;CC=%s;Sex=%s", k, o$CC[j], o$Sex[j]), format = NA_character_,
        old_raw = ov, new_raw = nv, old_printed = NA_character_, new_printed = NA_character_, raw_changed = !identical(ov, nv), printed_changed = NA)
  }
}

# ---------------------------------------------------------------- tables the renderer reads (file hashes)
for (t in c("A0_design_timeline", "A1_animal_cc1", "A2_prediction_animals", "A2b_combz_thresholds", "A3_permutation_draws",
            "A4_combz_animals", "B2_descriptive_summaries", "C2_estimates", "C3_joint_tests", "E_multiplicity")) {
  h <- vapply(PINS, function(p) unname(tools::sha256sum(file.path(behaviour_bundle_dir(p), paste0(t, ".csv")))), "")
  add(kind = "renderer_table_sha256", panel = NA_character_, key = t, table = t, column = NA_character_, filters = NA_character_,
      format = NA_character_, old_raw = h[["old"]], new_raw = h[["new"]], old_printed = NA_character_, new_printed = NA_character_,
      raw_changed = !identical(h[["old"]], h[["new"]]), printed_changed = NA)
}

# ---------------------------------------------------------------- rendered SVG text (optional)
old_svg <- opt("--old-svg"); new_svg <- opt("--new-svg")
if (!is.null(old_svg) && !is.null(new_svg)) {
  svg_text <- function(p) {
    s <- paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "")
    m <- regmatches(s, gregexpr("<text[^>]*>[^<]*</text>", s))[[1]]
    sub("^<text[^>]*>(.*)</text>$", "\\1", m)
  }
  for (id in c("a", "b", "c", "d", "e", "f")) {
    f <- paste0("figure_01", id, ".svg")
    o <- svg_text(file.path(old_svg, f)); n <- svg_text(file.path(new_svg, f))
    gone <- o[!o %in% n]; came <- n[!n %in% o]
    add(kind = "rendered_svg_text", panel = paste0("1", id), key = f, table = NA_character_, column = "text elements not in the other render",
        filters = NA_character_, format = NA_character_,
        old_raw = unname(tools::sha256sum(file.path(old_svg, f))), new_raw = unname(tools::sha256sum(file.path(new_svg, f))),
        old_printed = paste(gone, collapse = " | "), new_printed = paste(came, collapse = " | "),
        raw_changed = !identical(unname(tools::sha256sum(file.path(old_svg, f))), unname(tools::sha256sum(file.path(new_svg, f)))),
        printed_changed = length(gone) > 0L || length(came) > 0L)
  }
}

d <- do.call(rbind, rows)
utils::write.csv(d, OUT, row.names = FALSE, na = "")
cat(sprintf("Figure 1 value diff %s -> %s: %d rows -> %s\n", OLD, NEW, nrow(d), OUT))
for (k in unique(d$kind)) {
  x <- d[d$kind == k, , drop = FALSE]
  cat(sprintf("  %-26s rows %3d  raw changed %3d  printed changed %s\n", k, nrow(x), sum(x$raw_changed),
              if (all(is.na(x$printed_changed))) "-" else as.character(sum(x$printed_changed, na.rm = TRUE))))
}
pc <- d[d$kind == "annotation_key" & d$printed_changed, , drop = FALSE]
for (i in seq_len(nrow(pc))) cat(sprintf("  printed: %-14s %s -> %s\n", pc$key[i], pc$old_printed[i], pc$new_printed[i]))
