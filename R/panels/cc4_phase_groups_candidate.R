# Render frozen Stage 31b estimates only. No model, interval or phenotype fitting.
cc4_verify_bundle <- function(src) {
  manifest_path <- file.path(src, "manifest.csv")
  if (!file.exists(manifest_path)) stop("CC4 frozen manifest missing.", call. = FALSE)
  m <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
  required <- c("animal_phase_activity.csv", "batch_group_phase_activity.csv", "group_phase_summary.csv",
    "group_phase_contrasts.csv", "population.csv", "input_manifest.csv", "README.txt")
  if (!all(c("File", "SHA256") %in% names(m)) || anyNA(m) || anyDuplicated(m$File) ||
      any(basename(m$File) != m$File) || !all(required %in% m$File)) stop("Invalid CC4 frozen manifest.", call. = FALSE)
  files <- file.path(src, m$File)
  if (!all(file.exists(files)) || !identical(unname(tools::sha256sum(files)), m$SHA256))
    stop("CC4 frozen source hash mismatch.", call. = FALSE)
  invisible(m)
}

cc4_read_bundle <- function(src) {
  cc4_verify_bundle(src)
  read <- function(name) utils::read.csv(file.path(src, paste0(name, ".csv")), stringsAsFactors = FALSE)
  data <- list(summary = read("group_phase_summary"), batch = read("batch_group_phase_activity"),
    contrasts = read("group_phase_contrasts"), population = read("population"))
  if (!all(c("Sex", "Group", "Phase", "PhaseLabel", "Measure", "Estimate", "Lower95", "Upper95", "Batches") %in% names(data$summary)))
    stop("CC4 summary schema mismatch.", call. = FALSE)
  if (anyDuplicated(data$summary[c("Sex", "Group", "PhaseLabel", "Measure")]) ||
      any(!data$summary$Group %in% c("CON", "RES", "SUS")) || any(data$summary$Batches != 3L) ||
      nrow(data$summary) != 108L || nrow(data$contrasts) != 108L || any(!is.finite(data$summary$Estimate)))
    stop("CC4 candidate requires the complete six-batch frozen design.", call. = FALSE)
  data
}

cc4_candidate_plots <- function(data) {
  palette <- nature_palette("group")
  expected <- c(CON = "#3E3C6F", RES = "#C6C3BB", SUS = "#E63A48")
  if (!identical(palette, expected)) stop("Behavioural group palette changed; review the CC4 contract.", call. = FALSE)
  shapes <- c(CON = 21, RES = 24, SUS = 22)
  lines <- c(CON = "solid", RES = "longdash", SUS = "dotdash")
  style <- function() theme_nature_manuscript_panel(base_size = 7, base_family = "Arial", publication_legible = TRUE) +
    ggplot2::theme(legend.position = "top", legend.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 8, face = "plain"),
      plot.subtitle = ggplot2::element_text(size = 6.5, colour = "grey30"),
      strip.text = ggplot2::element_text(size = 7), panel.spacing.x = grid::unit(6, "mm"),
      plot.margin = ggplot2::margin(3, 3, 3, 3, unit = "mm"))
  prep <- function(d) {
    d$Sex <- factor(d$Sex, c("Female", "Male")); d$Group <- factor(d$Group, names(palette))
    d$PhaseNumber <- as.integer(sub("^[IA]", "", d$PhaseLabel))
    d$DisplayX <- d$PhaseNumber + c(-.14, 0, .14)[as.integer(d$Group)]
    d
  }
  make <- function(phase, measure) {
    s <- prep(subset(data$summary, Phase == phase & Measure == measure))
    b <- prep(subset(data$batch, Phase == phase))
    if (measure == "Change") { s <- subset(s, PhaseNumber >= 3); b <- subset(b, PhaseNumber >= 3) }
    b$Value <- if (measure == "Rate") b$Rate else b$Change
    b$DisplayX <- b$DisplayX + c(B1=-.035,B2=0,B5=.035,B3=-.035,B4=0,B6=.035)[b$Batch]
    ref <- if (phase == "Inactive") "I2" else "A2"
    title <- if (phase == "Inactive") "Inactive phases | 06:30-18:30" else "Active phases | 18:30-06:30"
    subtitle <- if (measure == "Rate") {
      if (phase == "Inactive") "I2 reference; I3-I5 contain grid sessions" else "A1 earlier context; A2 reference; A3-A5 follow grid-session days"
    } else paste0("Paired changes from ", ref, "; pointwise 95% intervals across three batches")
    p <- ggplot2::ggplot(s, ggplot2::aes(DisplayX, Estimate, group = Group, colour = Group)) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey65", linewidth = .22, linetype = "dotted") +
      ggplot2::geom_point(data = b, ggplot2::aes(y = Value, fill = Group, shape = Group),
        colour = "grey45", size = 1.35, stroke = .25, alpha = .65, show.legend = FALSE)
    if (measure == "Change") p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = Lower95, ymax = Upper95),
      width = .09, linewidth = .45, show.legend = FALSE)
    p + ggplot2::geom_line(ggplot2::aes(linetype = Group), linewidth = .55) +
      ggplot2::geom_point(ggplot2::aes(fill = Group, shape = Group), colour = "grey20", size = 2.1, stroke = .32) +
      ggplot2::scale_colour_manual(values = palette, drop = FALSE) + ggplot2::scale_fill_manual(values = palette, drop = FALSE) +
      ggplot2::scale_shape_manual(values = shapes, drop = FALSE) + ggplot2::scale_linetype_manual(values = lines, drop = FALSE) +
      ggplot2::scale_x_continuous(breaks = unique(s$PhaseNumber), labels = unique(s$PhaseLabel), expand = ggplot2::expansion(add = .35)) +
      ggplot2::facet_wrap(~Sex, nrow = 1, scales = "fixed") +
      ggplot2::labs(title = title, subtitle = subtitle, x = NULL,
        y = if (measure == "Rate") "RFID position changes / animal-hour" else paste0("Change from ", ref, " / animal-hour")) + style()
  }
  assemble <- function(measure) {
    patchwork::wrap_plots(make("Inactive", measure), make("Active", measure), ncol = 1, guides = "collect") +
      patchwork::plot_annotation(tag_levels = "a", caption = if (measure == "Rate")
        "Small points: batch estimates. Large points: equal-batch means. Same complete cage roster throughout.\nExact grid times were not recorded. Whole-phase associations; exploratory candidate." else
        "Small points: batch changes. Large points: equal-batch means; pointwise 95% t intervals (n = 3, df = 2).\nIntervals are not multiplicity adjusted. Exact grid times unknown; no causal grid-effect claim.",
        theme = ggplot2::theme(plot.caption = ggplot2::element_text(family = "Arial", size = 6, hjust = 0),
          plot.tag = ggplot2::element_text(size = 9, face = "bold"))) & ggplot2::theme(legend.position = "top")
  }
  contrast_plot <- function(phase) {
    d <- subset(data$contrasts, Phase == phase & Measure == "ChangeDifference" & !PhaseLabel %in% c("I2", "A1", "A2"))
    d$Sex <- factor(d$Sex, c("Female", "Male"))
    d$Contrast <- factor(d$Contrast, c("RES-CON", "SUS-CON", "SUS-RES"))
    d$PhaseLabel <- factor(d$PhaseLabel, paste0(substr(phase,1,1),5:3))
    ggplot2::ggplot(d, ggplot2::aes(Estimate, PhaseLabel)) +
      ggplot2::geom_vline(xintercept=0, colour="grey60", linewidth=.25, linetype="dotted") +
      ggplot2::geom_errorbar(ggplot2::aes(xmin=Lower95,xmax=Upper95), orientation="y", width=.15, linewidth=.4) +
      ggplot2::geom_point(size=1.6, colour="#2B2B2B") +
      ggplot2::facet_grid(Contrast~Sex) +
      ggplot2::labs(title=paste(phase,"phases: group differences in change"),
        subtitle="Positive values indicate a greater change in the first-named group",
        x="Between-group difference in phase change / animal-hour", y=NULL) + style()
  }
  list(cc4_group_activity = assemble("Rate"), cc4_group_changes = assemble("Change"),
    cc4_group_differences = patchwork::wrap_plots(contrast_plot("Inactive"), contrast_plot("Active"), ncol=1) +
      patchwork::plot_annotation(tag_levels="a", caption="Contrasts paired within batch; pointwise 95% t intervals across three batches per sex.\nExploratory outcome-group associations; no p-values or multiplicity-adjusted claims.",
        theme=ggplot2::theme(plot.caption=ggplot2::element_text(family="Arial",size=6,hjust=0))))
}
