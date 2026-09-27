# Canonical behaviour bundle resolver.
#
# The behavioural results are produced and frozen in MMMSociability: Stage 29
# (characterisation) and Stage 09 (registered prediction), assembled by Stage 16b
# into a hash-frozen bundle. tools/import_behaviour_bundle.R copies one pinned
# bundle into source_data/MMMSociability/<bundle_id>/ and records the pin in
# config/behaviour_bundle.yml.
#
# This file only reads that pinned copy. It fits nothing and recomputes nothing:
# bundle_cell() returns exactly one stored value or fails, so a number that is not
# in the bundle cannot reach a figure.

behaviour_bundle_pin <- function() {
  p <- repo_path("config", "behaviour_bundle.yml")
  if (!file.exists(p))
    stop("No behaviour bundle is pinned (config/behaviour_bundle.yml). Run tools/import_behaviour_bundle.R.", call. = FALSE)
  yaml::read_yaml(p)
}

behaviour_bundle_dir <- function(pin = behaviour_bundle_pin()) {
  repo_path("source_data", "MMMSociability", pin$bundle_id)
}

#' Verify the pinned copy byte-for-byte against its own manifest and the pinned manifest hash.
behaviour_bundle_verify <- function(pin = behaviour_bundle_pin()) {
  d <- behaviour_bundle_dir(pin)
  man_path <- file.path(d, "00_manifest.csv")
  if (!file.exists(man_path)) stop("Pinned bundle is not imported: ", d, call. = FALSE)
  if (!identical(unname(tools::sha256sum(man_path)), pin$manifest_sha256))
    stop("00_manifest.csv does not match the pinned manifest hash.", call. = FALSE)
  man <- utils::read.csv(man_path, stringsAsFactors = FALSE)
  now <- unname(tools::sha256sum(file.path(d, man$file)))
  bad <- man$file[is.na(now) | now != man$sha256]
  if (length(bad)) stop("Bundle files differ from the manifest: ", paste(bad, collapse = ", "), call. = FALSE)
  extra <- setdiff(list.files(d), c(man$file, "00_manifest.csv"))
  if (length(extra)) stop("Files in the bundle copy that the manifest does not list: ", paste(extra, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

behaviour_bundle_table <- function(name, pin = behaviour_bundle_pin()) {
  p <- file.path(behaviour_bundle_dir(pin), paste0(name, ".csv"))
  if (!file.exists(p)) stop("Bundle table not found: ", name, call. = FALSE)
  utils::read.csv(p, stringsAsFactors = FALSE, na.strings = c("NA", ""))
}

#' Exactly one stored value: the rows of `tab` matching every name = value filter, column `column`.
bundle_cell <- function(tab, column, ...) {
  f <- list(...)
  keep <- rep(TRUE, nrow(tab))
  for (n in names(f)) {
    if (!n %in% names(tab)) stop("bundle_cell: no column '", n, "'", call. = FALSE)
    v <- tab[[n]]
    keep <- keep & if (is.na(f[[n]])) is.na(v) else (!is.na(v) & v == f[[n]])
  }
  if (sum(keep) != 1L)
    stop("bundle_cell resolves ", sum(keep), " rows for ", column, " | ",
         paste(sprintf("%s=%s", names(f), unlist(f)), collapse = ", "), call. = FALSE)
  tab[[column]][keep]
}
