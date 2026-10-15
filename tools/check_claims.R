#!/usr/bin/env Rscript

# Check the numbers quoted in the Word manuscript against the pinned source tables.
#
# provenance/claims/docx_claims.csv holds one row per claim of the Word text. A row with
# check = "lookup" names a source table, a row filter and a column; this tool reads that cell
# (or counts the filtered rows) and compares it with the number printed in the text, within
# the stated tolerance. It reads files and compares numbers. It fits nothing, tests nothing,
# adjusts nothing and writes nothing.
#
#   Rscript tools/check_claims.R          report drift only (exit status 1 on drift or a missing file)
#   Rscript tools/check_claims.R --all    list every lookup row with the text value and the source value
#
# Drift means the register no longer agrees with the sources: a source value changed since the
# register was written, a status flipped, or a file is missing. Rows with check = "manual" are
# not looked up; they record a reading of the text against a source that is not a single cell.
#
# The register itself only changes by hand, after a new reading of the text. If the Word file
# was saved after the newest snapshot in manuscript/text/SNAPSHOTS.csv, the tool says so.

.boot <- NULL
.dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
repeat {
  cand <- file.path(.dir, "R", "paths.R")
  if (file.exists(cand)) { .boot <- cand; break }
  parent <- dirname(.dir)
  if (identical(parent, .dir)) break
  .dir <- parent
}
if (is.null(.boot)) stop("Could not locate R/paths.R from ", getwd(), call. = FALSE)
source(.boot)
rm(.boot, .dir)

SHOW_ALL <- "--all" %in% commandArgs(trailingOnly = TRUE)
REGISTER <- repo_path("provenance", "claims", "docx_claims.csv")
SNAPSHOTS <- repo_path("manuscript", "text", "SNAPSHOTS.csv")

read_chr <- function(p) utils::read.csv(p, colClasses = "character", check.names = FALSE,
                                        na.strings = character(0), encoding = "UTF-8")

parse_filter <- function(s) {
  if (!nzchar(s)) return(list())
  parts <- strsplit(s, " && ", fixed = TRUE)[[1]]
  lapply(parts, function(p) {
    i <- regexpr("=", p, fixed = TRUE)
    list(key = trimws(substr(p, 1, i - 1)), value = trimws(substr(p, i + 1, nchar(p))))
  })
}

.tables <- new.env(parent = emptyenv())
table_at <- function(rel) {
  if (is.null(.tables[[rel]])) .tables[[rel]] <- read_chr(repo_path(rel))
  .tables[[rel]]
}

lookup_value <- function(file, filter, column) {
  d <- table_at(file)
  keep <- rep(TRUE, nrow(d))
  for (f in parse_filter(filter)) {
    if (!f$key %in% names(d)) stop("no column '", f$key, "' in ", file)
    keep <- keep & trimws(d[[f$key]]) == f$value
  }
  d <- d[keep, , drop = FALSE]
  if (identical(column, "count()")) return(as.numeric(nrow(d)))
  if (!column %in% names(d)) stop("no column '", column, "' in ", file)
  if (nrow(d) != 1L) stop(nrow(d), " rows match the filter in ", file, " (need exactly 1)")
  as.numeric(d[[column]])
}

within_tolerance <- function(a, b, tol) {
  kind <- sub(":.*", "", tol)
  size <- as.numeric(sub(".*:", "", tol))
  if (identical(kind, "abs")) return(abs(a - b) <= size)
  if (identical(kind, "rel")) return(abs(a - b) <= size * abs(b))
  stop("tolerance must be abs:<x> or rel:<x>, got '", tol, "'")
}

reg <- read_chr(REGISTER)
cat("Exp9_manuscript claim check\n===========================\n")
cat(sprintf("Register: %d claims (%d lookup, %d manual)\n\n", nrow(reg),
            sum(reg$check == "lookup"), sum(reg$check == "manual")))

drift <- character(0)
rows <- list()
for (i in which(reg$check == "lookup")) {
  r <- reg[i, ]
  now <- tryCatch(lookup_value(r$source_file, r$source_filter, r$source_column),
                  error = function(e) { drift <<- c(drift, sprintf("%s: %s", r$claim_id, conditionMessage(e))); NA_real_ })
  if (is.na(now)) next
  text_v <- as.numeric(r$value_in_text)
  status_now <- if (within_tolerance(text_v, now, r$tolerance)) "match" else "mismatch"
  if (!identical(status_now, r$status))
    drift <- c(drift, sprintf("%s: status is %s in the register but %s now", r$claim_id, r$status, status_now))
  recorded <- as.numeric(r$value_in_source)
  if (!isTRUE(abs(recorded - now) <= 1e-6 * max(1, abs(now))))
    drift <- c(drift, sprintf("%s: source value was %s in the register, is %s now", r$claim_id, r$value_in_source, format(now, digits = 7)))
  rows[[length(rows) + 1L]] <- data.frame(claim = r$claim_id, quantity = r$quantity, text = r$value_in_text,
                                          source = format(now, digits = 5), status = status_now, stringsAsFactors = FALSE)
}
if (length(rows)) {
  out <- do.call(rbind, rows)
  cat("Lookup rows by status:\n"); print(table(out$status))
  if (SHOW_ALL) { cat("\n"); print(out, row.names = FALSE) }
  else if (any(out$status == "mismatch")) {
    cat("\nNumbers in the text that differ from the source (use --all for every row):\n")
    print(out[out$status == "mismatch", ], row.names = FALSE)
  }
}

cat("\nManual rows by status:\n"); print(table(reg$status[reg$check == "manual"]))

# Has the Word file been saved since the newest snapshot?
if (file.exists(SNAPSHOTS)) {
  s <- read_chr(SNAPSHOTS)
  if (nrow(s)) {
    last <- s[nrow(s), ]
    live <- last$docx_path
    if (nzchar(live) && file.exists(live)) {
      sha_live <- unname(tools::sha256sum(live))
      if (!identical(tolower(sha_live), tolower(last$docx_sha256)))
        cat("\nNOTE: the Word file has changed since the snapshot of ", last$snapshot_date,
            ". The register describes the snapshot, not the current text.\n", sep = "")
    }
  }
}

if (length(drift)) {
  cat("\nDRIFT:\n"); cat(paste0("  - ", drift), sep = "\n")
  quit(status = 1L)
}
cat("\nNo drift: every lookup row agrees with its source.\n")
