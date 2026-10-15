#!/usr/bin/env Rscript

# Snapshot the Word manuscript and export its text.
#
# The Word file stays the master text and is never edited here. This tool copies it byte for byte
# to manuscript/text/, writes the text of the copy (one non-empty paragraph per line, in order) and
# appends a row to manuscript/text/SNAPSHOTS.csv. The claim register
# (provenance/claims/docx_claims.csv) refers to lines of that text file.
#
#   Rscript tools/export_docx_text.R "<path to the .docx>"
#
# Superscript runs are written ^{...} and subscript runs _{...}, so reference numbers stay visible.
# Text deleted under tracked changes is left out; inserted text is kept. Comments, footnotes and
# images are not exported. The tool refuses to overwrite an existing snapshot of the same day.

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

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("usage: Rscript tools/export_docx_text.R <path to .docx>", call. = FALSE)
src <- normalizePath(args[1], winslash = "/", mustWork = TRUE)
if (!grepl("[.]docx$", src, ignore.case = TRUE)) stop("not a .docx file: ", src, call. = FALSE)

stem <- tools::file_path_sans_ext(basename(src))
day <- format(Sys.Date(), "%Y-%m-%d")
out_dir <- repo_path("manuscript", "text")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
docx_out <- file.path(out_dir, sprintf("%s_%s.docx", stem, day))
text_out <- file.path(out_dir, sprintf("%s_%s.txt", stem, day))
index_out <- file.path(out_dir, "SNAPSHOTS.csv")
if (file.exists(docx_out) || file.exists(text_out))
  stop("a snapshot of ", day, " already exists in ", out_dir, "; remove it by hand if it should be replaced", call. = FALSE)

# ---- read the paragraphs ---------------------------------------------------------------------
tmp <- tempfile("docx_")
dir.create(tmp)
on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
zip::unzip(src, files = "word/document.xml", exdir = tmp)
doc <- xml2::read_xml(file.path(tmp, "word", "document.xml"))
ns <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")

run_text <- function(r) {
  kids <- xml2::xml_children(r)
  txt <- vapply(kids, function(k) {
    switch(xml2::xml_name(k),
           t = xml2::xml_text(k),
           tab = "\t",
           br = " ",
           noBreakHyphen = "-",
           "")
  }, character(1))
  txt <- paste(txt, collapse = "")
  if (!nzchar(txt)) return("")
  va <- xml2::xml_attr(xml2::xml_find_first(r, "./w:rPr/w:vertAlign", ns), "val", ns)
  if (identical(va, "superscript")) return(paste0("^{", txt, "}"))
  if (identical(va, "subscript")) return(paste0("_{", txt, "}"))
  txt
}

paras <- xml2::xml_find_all(doc, "//w:p", ns)
lines <- vapply(paras, function(p) {
  runs <- xml2::xml_find_all(p, ".//w:r[not(ancestor::w:del)]", ns)
  txt <- paste(vapply(runs, run_text, character(1)), collapse = "")
  txt <- gsub("\\}\\^\\{", "", txt)          # merge adjacent superscript runs
  txt <- gsub("[ \t]+$", "", gsub("^[ \t]+", "", txt))
  if (!nzchar(txt)) return(NA_character_)
  st <- xml2::xml_attr(xml2::xml_find_first(p, "./w:pPr/w:pStyle", ns), "val", ns)
  if (!is.na(st) && !st %in% c("Normal", "Standard")) txt <- paste0("[", st, "] ", txt)
  txt
}, character(1))
lines <- lines[!is.na(lines)]

# ---- write the snapshot, the text and the index row ------------------------------------------
if (!file.copy(src, docx_out, overwrite = FALSE)) stop("could not copy the docx", call. = FALSE)
con <- file(text_out, open = "wb")
writeBin(charToRaw(paste0(paste(enc2utf8(lines), collapse = "\n"), "\n")), con)
close(con)

sha <- function(p) unname(tools::sha256sum(p))
stopifnot(identical(sha(src), sha(docx_out)))
words <- length(strsplit(paste(lines, collapse = " "), "[[:space:]]+")[[1]])
row <- data.frame(
  snapshot_date = day,
  docx_path = src,
  docx_sha256 = sha(src),
  docx_bytes = file.size(src),
  docx_saved = format(file.mtime(src), "%Y-%m-%d %H:%M:%S"),
  snapshot_docx = file.path("manuscript", "text", basename(docx_out)),
  text_file = file.path("manuscript", "text", basename(text_out)),
  paragraphs = length(lines),
  words = words,
  stringsAsFactors = FALSE
)
# written as bytes so the index has LF line endings on every platform
csv <- utils::capture.output(utils::write.table(row, sep = ",", row.names = FALSE, qmethod = "double", quote = TRUE))
had_index <- file.exists(index_out)
if (had_index) csv <- csv[-1L]
con <- file(index_out, open = if (had_index) "ab" else "wb")
writeBin(charToRaw(paste0(paste(enc2utf8(csv), collapse = "\n"), "\n")), con)
close(con)
cat(sprintf("Snapshot: %s\nText:     %s (%d paragraphs, %d words)\nIndex:    %s\n",
            docx_out, text_out, length(lines), words, index_out))
