# ──────────────────────────────────────────────────────────────────────────────
# theme_ivo.R — shared figure conventions for R/ggplot2
#
# This file is the single source of truth for the R-figure rule in the global
# CLAUDE.md: Okabe-Ito colorblind-safe palette, group colours kept separate from
# phase/temporal colours, PNG-only export at 300 dpi, and no titles or notes
# baked into the figure (captions belong in Quarto).
#
# Usage:
#   source("theme_ivo.R")   # or a relative
#   source(here::here("scripts", "theme_ivo.R"))                 # project copy
#
# Requires ggplot2 (tested against 4.0.2). `ltc` is optional and used only by
# the colour-vision check at the foot of this file.
# ──────────────────────────────────────────────────────────────────────────────

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("theme_ivo.R requires ggplot2. install.packages('ggplot2')")
}

# ── Palette ───────────────────────────────────────────────────────────────────

#' The Okabe-Ito colourblind-safe palette (Nature/Science standard).
#' Named so call sites can be explicit: OKABE_ITO[["vermillion"]].
OKABE_ITO <- c(
  orange     = "#E69F00",
  skyblue    = "#56B4E9",
  green      = "#009E73",  # bluish green
  yellow     = "#F0E442",
  blue       = "#0072B2",
  vermillion = "#D55E00",
  purple     = "#CC79A7",  # reddish purple
  black      = "#000000",
  grey       = "#999999"
)

# Draw order for unordered groups: highest mutual contrast first, so a two-arm
# figure gets blue/vermillion rather than two neighbouring hues. Yellow
# (#F0E442) is deliberately excluded — it is close to invisible as a line or a
# small point on white, and Okabe-Ito intends it for large filled areas only.
GROUP_ORDER <- c("blue", "vermillion", "green", "purple", "skyblue", "orange", "black")

#' Discrete colours for unordered categories (treatment arms, groups, countries).
#' @param n number of levels, at most 7.
pal_group <- function(n) {
  if (n < 1) stop("pal_group(): n must be at least 1")
  if (n > length(GROUP_ORDER)) {
    stop(sprintf(
      paste0("pal_group(): asked for %d colours but only %d are safe. Okabe-Ito ",
             "yellow is excluded as a group colour. With more than %d groups, ",
             "facet, use direct labelling, or collapse categories rather than ",
             "adding hues no reader can separate."),
      n, length(GROUP_ORDER), length(GROUP_ORDER)
    ))
  }
  unname(OKABE_ITO[GROUP_ORDER[seq_len(n)]])
}

# Ordered scales are a different visual problem from group scales: the reader
# must recover a *rank*, not an identity. A light-to-dark ramp anchored on the
# Okabe-Ito blue does that and survives greyscale printing; a set of distinct
# hues does not.
# The light end stops well short of white: a near-white first level survives on
# screen but disappears as a 0.6pt line in print, which is where these figures
# actually land.
PHASE_ANCHORS <- c("#AECFE6", "#56B4E9", "#0072B2", "#00355A")

#' Sequential colours for ordered categories (waves, rounds, periods, phases).
#' @param n number of ordered levels.
pal_phase <- function(n) {
  if (n < 1) stop("pal_phase(): n must be at least 1")
  if (n == 1) return(unname(OKABE_ITO[["blue"]]))
  grDevices::colorRampPalette(PHASE_ANCHORS, space = "Lab")(n)
}

# ── ggplot2 scales ────────────────────────────────────────────────────────────

scale_colour_group <- function(...) {
  ggplot2::discrete_scale(aesthetics = "colour", palette = pal_group, ...)
}

scale_fill_group <- function(...) {
  ggplot2::discrete_scale(aesthetics = "fill", palette = pal_group, ...)
}

scale_colour_phase <- function(...) {
  ggplot2::discrete_scale(aesthetics = "colour", palette = pal_phase, ...)
}

scale_fill_phase <- function(...) {
  ggplot2::discrete_scale(aesthetics = "fill", palette = pal_phase, ...)
}

# American-spelling aliases, so either spelling works at the call site.
scale_color_group <- scale_colour_group
scale_color_phase <- scale_colour_phase

# ── Theme ─────────────────────────────────────────────────────────────────────

#' Publication base theme.
#'
#' Title, subtitle and caption are blanked rather than merely left unset. The
#' convention is that captions and notes live in Quarto, and a blanked element
#' makes that structural: a stray labs(title=) cannot silently reintroduce one.
#'
#' @param base_size base font size in points; 9 suits a 3.5-inch column.
#' @param base_family font family; "" keeps the device default.
theme_ivo <- function(base_size = 9, base_family = "") {
  ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title    = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      plot.caption  = ggplot2::element_blank(),
      axis.title    = ggplot2::element_text(size = base_size),
      axis.text     = ggplot2::element_text(size = base_size - 1, colour = "black"),
      axis.line     = ggplot2::element_line(colour = "black", linewidth = 0.3),
      axis.ticks    = ggplot2::element_line(colour = "black", linewidth = 0.3),
      legend.position   = "bottom",
      legend.title      = ggplot2::element_text(size = base_size),
      legend.text       = ggplot2::element_text(size = base_size - 1),
      legend.key.size   = grid::unit(0.8, "lines"),
      legend.background = ggplot2::element_blank(),
      strip.background  = ggplot2::element_blank(),
      strip.text        = ggplot2::element_text(size = base_size, face = "bold"),
      panel.spacing     = grid::unit(0.8, "lines"),
      plot.margin       = ggplot2::margin(4, 6, 4, 4)
    )
}

# ── Export ────────────────────────────────────────────────────────────────────

# Inches are the layout target: they fix how large 9pt type is relative to the
# panel once the journal prints the figure in that column. dpi is the print
# resolution, and pixel width simply follows (3.5 in -> 2100 px, 6.5 in ->
# 3900 px at 600 dpi).
#
# 600 dpi is the line-art standard; these are line drawings, not halftones.
# The older 1050/2100 pixel targets were a 300-dpi artifact and no longer apply.
#
# Note for refresh_figures.py: at 600 dpi a single-column figure is 2100 px, so
# the old "<=1100 px -> single column" heuristic misclassifies it as double.
# That script now reads the dpi written into the PNG and computes px/dpi, which
# is why nothing here should strip the pHYs metadata ggsave embeds.
FIG_WIDTH_IN <- c(single = 3.5, double = 6.5)
FIG_DPI      <- 600

#' Export a figure as PNG at journal column widths.
#'
#' PNG only, by convention: no PDF versions of figures are kept.
#'
#' @param filename output path, must end in .png.
#' @param plot ggplot object; defaults to the last plot drawn.
#' @param width "single" (3.5 in, 2100 px at 600 dpi) or "double" (6.5 in, 3900 px).
#' @param height height in inches; defaults to width / 1.6.
#' @param dpi print resolution; 600 unless a journal asks for something else.
ggsave_ivo <- function(filename,
                       plot   = ggplot2::last_plot(),
                       width  = c("single", "double"),
                       height = NULL,
                       dpi    = FIG_DPI) {
  if (!grepl("\\.png$", filename, ignore.case = TRUE)) {
    stop("ggsave_ivo(): figures are exported as PNG only; '", filename,
         "' does not end in .png")
  }
  if (!grepl("^fig_", basename(filename))) {
    warning("ggsave_ivo(): '", basename(filename),
            "' does not use the fig_ prefix; refresh_figures.py matches ",
            "bookmarks as FIGURE_<stem without fig_>")
  }
  width <- match.arg(width)
  w_in  <- unname(FIG_WIDTH_IN[[width]])
  h_in  <- if (is.null(height)) w_in / 1.6 else height

  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(filename, plot = plot, width = w_in, height = h_in,
                  dpi = dpi, units = "in", bg = "white")
  invisible(filename)
}

# ── Optional: colour-vision-deficiency check ──────────────────────────────────
#
# The `ltc` package (CRAN, loukesio/ltc-color-palettes) ships ltc_cvd(), which
# simulates deuteranopia, protanopia and tritanopia for a vector of colours.
# Only that check is borrowed; none of its 32 palettes are used here, since
# mixing them in would break the Okabe-Ito convention across figures.
#
# Install on demand with install.packages("ltc"), then:
#
#   if (requireNamespace("ltc", quietly = TRUE)) {
#     ltc::ltc_cvd(pal_group(5))
#     ltc::ltc_cvd(pal_phase(4))
#   }
#
# Okabe-Ito is CVD-safe by construction, so this is a check on *derived* colours
# — anything passed through adjust/desaturate, or a pal_phase() ramp with many
# levels, where adjacent steps can collapse under one of the deficiencies.
