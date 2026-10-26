# Excel workbooks for the paper folder (outside this repository, R/paths.R paper_path()):
#   source data   one workbook per published figure, one sheet per panel holding the values the panel plots
#                 (the promoted per-panel tables under figures/main|extended_data/source_data/<id>/), and an
#                 'about' sheet;
#   statistics    stats.xlsx, built by tools/build_stats.R from figures/contracts/stats_map.csv.
# Both are built from tracked, hash-checked files only and compute nothing. Needs openxlsx.

WB_BOLD <- function() openxlsx::createStyle(textDecoration = "bold")

#' Sheet name of a panel: "Fig. 1a" for a main figure, "Extended Data Fig. 4a" for Extended Data.
wb_panel_sheet <- function(pid, panel) {
  p <- gsub("_", " ", sub("^0+", "", panel))
  name <- if (startsWith(pid, "figure_")) paste0("Fig. ", p) else paste0("Extended Data Fig. ", p)
  if (nchar(name) > 31L || grepl("[]:*?/\\\\[]", name, perl = TRUE)) stop("not a valid sheet name: ", name, call. = FALSE)
  name
}

#' Write a table at `row`; a table with a column `table` is written as one titled block per part. Returns the
#' next free row.
wb_write_table <- function(wb, sheet, x, row = 1L) {
  parts <- if ("table" %in% names(x)) split(x[setdiff(names(x), "table")], factor(x$table, levels = unique(x$table)))
           else list(x)
  for (k in seq_along(parts)) {
    y <- parts[[k]]
    if (length(parts) > 1L || "table" %in% names(x)) {
      # drop the columns that belong to the other parts only
      y <- y[, vapply(y, function(v) !all(is.na(v) | v == ""), logical(1)) | !nrow(y), drop = FALSE]
      openxlsx::writeData(wb, sheet, gsub("_", " ", names(parts)[k]), startRow = row, startCol = 1)
      openxlsx::addStyle(wb, sheet, WB_BOLD(), rows = row, cols = 1)
      row <- row + 1L
    }
    openxlsx::writeData(wb, sheet, y, startRow = row, headerStyle = WB_BOLD())
    row <- row + nrow(y) + 2L
  }
  row
}

#' The promoted per-panel tables of one identity, checked against their manifest. Stops on any mismatch.
wb_panel_tables <- function(sd_dir) {
  man_file <- file.path(sd_dir, "00_manifest.csv")
  if (!file.exists(man_file)) stop("no source-data manifest: ", sd_dir, call. = FALSE)
  man <- utils::read.csv(man_file, stringsAsFactors = FALSE, colClasses = "character")
  files <- file.path(sd_dir, man$file)
  if (!all(file.exists(files))) stop("source data missing in ", sd_dir, call. = FALSE)
  if (any(unname(tools::sha256sum(files)) != man$sha256)) stop("source data differs from its manifest in ", sd_dir, call. = FALSE)
  tabs <- lapply(files, utils::read.csv, check.names = FALSE, stringsAsFactors = FALSE, na.strings = "")
  list(manifest = man, tables = stats::setNames(tabs, man$panel))
}

#' One source-data workbook. `info` holds title, analysis, source_commit and the panel descriptions (named by panel id);
#' `build` holds the repository commit and whether the tree was clean.
wb_source_data <- function(pid, sd_dir, out_file, info, build) {
  got <- wb_panel_tables(sd_dir)
  man <- got$manifest
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "about")
  head <- data.frame(
    item = c("Source data for", "Panels", "Analysis", "Analysis commit", "Rendered at commit", "Built from commit", "Built"),
    value = c(info$title, paste(man$panel, collapse = ", "), info$analysis, info$source_commit,
              paste(unique(man$render_git_commit), collapse = ", "),
              paste0(build$commit, if (isTRUE(build$clean)) "" else " (uncommitted changes)"),
              format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
    stringsAsFactors = FALSE)
  openxlsx::writeData(wb, "about", head, colNames = FALSE)
  openxlsx::addStyle(wb, "about", WB_BOLD(), rows = seq_len(nrow(head)), cols = 1)
  sheets <- vapply(man$panel, function(p) wb_panel_sheet(pid, p), "")
  contents <- data.frame(Sheet = sheets, Panel = man$panel,
                         `What it shows` = unname(vapply(man$panel, function(p) info$panels[[p]] %||% "", "")),
                         Rows = as.integer(man$rows), File = man$file, `SHA-256` = man$sha256,
                         check.names = FALSE, stringsAsFactors = FALSE)
  openxlsx::writeData(wb, "about", contents, startRow = nrow(head) + 2L, headerStyle = WB_BOLD())
  openxlsx::setColWidths(wb, "about", cols = 1:3, widths = c(22, 22, 90))
  for (k in seq_len(nrow(man))) {
    openxlsx::addWorksheet(wb, sheets[[k]])
    wb_write_table(wb, sheets[[k]], got$tables[[k]])
  }
  openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)
  invisible(out_file)
}

# ---------------------------------------------------------------- statistics overview (stats.xlsx)
# figures/contracts/stats_map.csv: one row per reported statistic, or per group of them. Columns:
#   sheet, panel, quantity   where it goes and what it is; quantity may use {column} placeholders
#   source                   a CSV in this repository; {ebb} {fsb} {s30b} {ctp} name the pinned bundles,
#                            {fig} {ed} the promoted per-panel source data of the main and Extended Data figures
#   join                     optional "<file in the same folder>:<key>", a left join on that key
#   filter                   "col=value;col!=value;col<number;col>number"
#   unique                   optional "col;col": one row per distinct combination (with count(), counted per group)
#   estimate ... note        a column name; "=text" (a literal, may use {column}); "@<csv>[<filter>]$<column>" (one
#                            cell of another table; {column} placeholders allowed); estimate "count()" or
#                            "count(<col>)" counts rows or distinct values
# A row whose quantity has no placeholder must match exactly one source row (count() aside).

STATS_FIELDS <- c(estimate = "Estimate", ci_low = "CI low", ci_high = "CI high", n = "n", test = "Test",
                  statistic = "Statistic", df = "df", p = "P", p_adj = "Adjusted P", family = "Correction", note = "Note")

stats_vars <- function() {
  pin <- function(f) yaml::read_yaml(repo_path("config", f))$bundle_id
  c(ebb = paste0("source_data/MMMSociability/", pin("behaviour_bundle.yml")),
    fsb = paste0("source_data/MMMSociability/", pin("figure_support_bundle.yml")),
    s30b = paste0("source_data/MMMSociability/", pin("stage30_bundle.yml")),
    ctp = paste0("source_data/SISanalyzer/", pin("conventional_bundle.yml")),
    fig = "figures/main/source_data", ed = "figures/extended_data/source_data")
}

stats_sub_vars <- function(x, vars) { for (k in names(vars)) x <- gsub(paste0("{", k, "}"), vars[[k]], x, fixed = TRUE); x }

stats_read <- local({
  cache <- list()
  function(path) {
    if (is.null(cache[[path]])) {
      f <- repo_path(path)
      if (!file.exists(f)) stop("stats map: no such table: ", path, call. = FALSE)
      cache[[path]] <<- utils::read.csv(f, check.names = FALSE, stringsAsFactors = FALSE, na.strings = c("", "NA"))
    }
    cache[[path]]
  }
})

stats_text <- function(v) {
  if (length(v) != 1L || is.na(v)) return("NA")
  if (is.numeric(v)) return(format(signif(v, 4), scientific = abs(v) < 1e-3 && v != 0, trim = TRUE))
  as.character(v)
}

#' Fill {column} placeholders from one row.
stats_fill <- function(x, r) {
  for (k in regmatches(x, gregexpr("\\{[^{}]+\\}", x))[[1]]) {
    col <- substr(k, 2L, nchar(k) - 1L)
    if (!col %in% names(r)) stop("stats map: no column '", col, "' for ", x, call. = FALSE)
    x <- sub(k, stats_text(r[[col]][1]), x, fixed = TRUE)
  }
  x
}

stats_filter <- function(tab, filter) {
  if (is.na(filter) || !nzchar(filter)) return(tab)
  for (cond in strsplit(filter, ";", fixed = TRUE)[[1]]) {
    m <- regmatches(cond, regexec("^(.+?)(!=|=|<|>)(.*)$", cond))[[1]]
    if (!length(m) || !m[2] %in% names(tab)) stop("stats map: bad filter '", cond, "'", call. = FALSE)
    v <- tab[[m[2]]]
    keep <- switch(m[3], "=" = as.character(v) == m[4], "!=" = as.character(v) != m[4],
                   "<" = suppressWarnings(as.numeric(v)) < as.numeric(m[4]),
                   ">" = suppressWarnings(as.numeric(v)) > as.numeric(m[4]))
    tab <- tab[!is.na(keep) & keep, , drop = FALSE]
  }
  tab
}

stats_value <- function(spec, r, vars) {
  if (is.na(spec) || !nzchar(spec)) return(NA)
  if (startsWith(spec, "=")) return(stats_fill(substring(spec, 2L), r))
  if (startsWith(spec, "@")) {
    m <- regmatches(spec, regexec("^@(.+?)\\[(.*)\\]\\$(.+)$", spec))[[1]]
    if (!length(m)) stop("stats map: bad cell reference ", spec, call. = FALSE)
    t <- stats_filter(stats_read(stats_sub_vars(m[2], vars)), stats_fill(m[3], r))
    if (nrow(t) != 1L) stop("stats map: ", spec, " matches ", nrow(t), " rows", call. = FALSE)
    return(t[[m[4]]][1])
  }
  if (!spec %in% names(r)) stop("stats map: no column '", spec, "'", call. = FALSE)
  r[[spec]][1]
}

#' The statistics rows of one map row.
stats_rows <- function(m, vars) {
  src <- stats_sub_vars(m$source, vars)
  tab <- stats_read(src)
  if (!is.na(m$join) && nzchar(m$join)) {
    j <- strsplit(m$join, ":", fixed = TRUE)[[1]]
    tab <- merge(tab, stats_read(file.path(dirname(src), j[1])), by = j[2], all.x = TRUE, sort = FALSE,
                 suffixes = c("", ".joined"))
  }
  tab <- stats_filter(tab, m$filter)
  by <- if (is.na(m$unique) || !nzchar(m$unique)) character() else strsplit(m$unique, ";", fixed = TRUE)[[1]]
  counting <- !is.na(m$estimate) && grepl("^count\\(", m$estimate)
  templated <- grepl("{", m$quantity, fixed = TRUE)
  if (!counting && !length(by) && !templated && nrow(tab) != 1L)
    stop("stats map: '", m$quantity, "' matches ", nrow(tab), " rows in ", src, call. = FALSE)
  if (!nrow(tab)) return(NULL)
  groups <- if (length(by)) unname(split(seq_len(nrow(tab)), interaction(tab[by], drop = TRUE, lex.order = TRUE)))
            else if (counting || !templated) list(seq_len(nrow(tab))) else as.list(seq_len(nrow(tab)))
  do.call(rbind, lapply(groups, function(g) {
    r <- tab[g[1], , drop = FALSE]
    vals <- lapply(names(STATS_FIELDS), function(f) {
      if (f == "estimate" && counting) {
        col <- sub("^count\\((.*)\\)$", "\\1", m$estimate)
        return(if (nzchar(col)) length(unique(tab[[col]][g])) else length(g))
      }
      v <- stats_value(m[[f]], r, vars)
      if (length(v) != 1L) NA else v
    })
    names(vals) <- unname(STATS_FIELDS)
    # A row has a 95% CI, or the map declares why it has none (ci_none): the CI cells then read "n/a" and the reason goes to Note.
    no_ci <- c(is.na(vals[["CI low"]]), is.na(vals[["CI high"]]))
    if (xor(no_ci[1], no_ci[2])) stop("stats map: '", m$quantity, "' has only one CI bound", call. = FALSE)
    why <- if (all(no_ci) && !is.null(m$ci_none) && !is.na(m$ci_none)) stats_fill(m$ci_none, r) else NA_character_
    if (all(no_ci) && is.na(why) && !counting)
      stop("stats map: '", stats_fill(m$quantity, r), "' has no CI and no declared reason (ci_none)", call. = FALSE)
    if (!is.na(why)) vals[["Note"]] <- paste0(if (!is.na(vals[["Note"]])) paste0(vals[["Note"]], "; ") else "", "no CI: ", why)
    data.frame(Panel = if (is.na(m$panel)) "" else m$panel, Quantity = stats_fill(m$quantity, r),
               as.data.frame(lapply(vals, function(v) if (is.numeric(v)) v else as.character(v)),
                             check.names = FALSE, stringsAsFactors = FALSE),
               Source = paste0(src, if (!is.na(m$filter) && nzchar(m$filter)) paste0(" [", m$filter, "]") else ""),
               .ci_na = !is.na(why), check.names = FALSE, stringsAsFactors = FALSE)
  }))
}

#' All statistics, one data frame per sheet in map order; a column whose values are all numbers is numeric.
stats_tables <- function(map, vars = stats_vars()) {
  map[] <- lapply(map, function(x) ifelse(is.na(x) | x == "", NA_character_, as.character(x)))
  rows <- lapply(seq_len(nrow(map)), function(i) {
    out <- tryCatch(stats_rows(map[i, , drop = FALSE], vars),
                    error = function(e) stop("stats map row ", i + 1L, " (", map$quantity[i], "): ", conditionMessage(e), call. = FALSE))
    if (!is.null(out)) out$sheet <- map$sheet[i]
    out
  })
  cols <- c("Panel", "Quantity", unname(STATS_FIELDS), "Source", ".ci_na", "sheet")
  # numbers stay numbers; in a column that also holds text (e.g. df "3, 190.3") they are printed to 4 digits
  all <- do.call(rbind, lapply(Filter(Negate(is.null), rows), function(x) {
    x[] <- lapply(x, function(v) if (is.numeric(v)) ifelse(is.na(v), NA_character_, format(v, digits = 15, trim = TRUE)) else as.character(v))
    x[cols] }))
  for (k in c(unname(STATS_FIELDS), "Panel")) {
    v <- all[[k]]
    num <- suppressWarnings(as.numeric(v))
    all[[k]] <- if (all(is.na(v) | !is.na(num))) num else
      ifelse(!is.na(num), vapply(num, stats_text, ""), v)
  }
  all$.ci_na <- all$.ci_na == "TRUE"   # rows with no CI by declaration: wb_stats writes "n/a" into their CI cells
  sheets <- unique(map$sheet)
  stats::setNames(lapply(sheets, function(s) {
    x <- all[all$sheet == s, setdiff(cols, "sheet"), drop = FALSE]; rownames(x) <- NULL; x }), sheets)
}

#' Write stats.xlsx: an 'about' sheet, then one sheet per map sheet.
wb_stats <- function(tables, out_file, build, vars = stats_vars()) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "about")
  about <- data.frame(item = c("What", "Built from commit", "Built", "Bundles", "Columns", "Full tables"),
    value = c(paste("Statistics of the Exp9 manuscript: one sheet per figure, then the numbers quoted only in the text,",
                    "sensitivity analyses, other registered and post hoc analyses, and group sizes.",
                    "Every value is a stored result; nothing is computed here."),
              paste0(build$commit, if (isTRUE(build$clean)) "" else " (uncommitted changes)"),
              format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
              paste(basename(vars[c("ebb", "fsb", "s30b", "ctp")]), collapse = "; "),
              paste("Estimate with its 95% CI (n/a where none is defined, the reason in Note); n; test; statistic and df; P (raw) and the adjusted P with its",
                    "correction family. Source names the file and filter each row was read from."),
              "The source data workbooks (source_data/<figure>.xlsx) hold every plotted value."),
    stringsAsFactors = FALSE)
  openxlsx::writeData(wb, "about", about, colNames = FALSE)
  openxlsx::addStyle(wb, "about", WB_BOLD(), rows = seq_len(nrow(about)), cols = 1)
  openxlsx::setColWidths(wb, "about", cols = 1:2, widths = c(20, 120))
  for (s in names(tables)) {
    openxlsx::addWorksheet(wb, s)
    x <- tables[[s]]; na <- if (is.null(x$.ci_na)) rep(FALSE, nrow(x)) else x$.ci_na; x$.ci_na <- NULL
    openxlsx::writeData(wb, s, x, headerStyle = WB_BOLD())
    for (k in match(c("CI low", "CI high"), names(x))) for (i in which(na))   # numbers stay numbers; "n/a" only where declared
      openxlsx::writeData(wb, s, "n/a", startCol = k, startRow = i + 1L)
    openxlsx::setColWidths(wb, s, cols = 2, widths = 60)
    openxlsx::freezePane(wb, s, firstRow = TRUE)
  }
  openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)
  invisible(out_file)
}
