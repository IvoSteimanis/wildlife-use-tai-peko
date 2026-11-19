# -----------------------------------------------------------------------------
# 04_figures.R -- the four figures, redrawn from the extracted data.
#
# Inputs  : ../processed/species.csv, localities.csv, seasonal.csv
# Outputs : ../results/figures/fig_conservation_status.png    (manuscript Fig. 3)
#           ../results/figures/fig_seasonal_consumption.png   (manuscript Fig. 4)
#           ../results/figures/fig_spatial_gradients.png      (manuscript Figs. 5-7)
#           ../results/figures/fig_citation_frequency.png     (new; the Peko pattern)
#           ../processed/computed_figures.csv                 <- read by 05_manifest.py
#
# Conventions: PNG only, 600 dpi, no titles, subtitles or notes inside the figure.
# Captions belong with the manuscript, not in the image. Okabe-Ito palette via the
# shared theme.
#
# Run from the scripts/ folder:  Rscript 04_figures.R
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(scales)
})

# The shared theme lives outside the package. Fall back to a local equivalent so the
# package runs on a reviewer's machine that does not have it.
# Resolve the shared theme without a hardcoded user path, so the package runs on a
# reviewer's machine. Local copy first, then the environment variable, then the
# author's path; each script already falls back to an inline equivalent if none exist.
theme_path <- Sys.getenv("THEME_IVO",
  unset = file.path("..", "code", "theme_ivo.R"))
if (!file.exists(theme_path))
  theme_path <- "theme_ivo.R"
if (file.exists(theme_path)) {
  source(theme_path)
} else {
  message("shared theme not found; using a local fallback")
  OKABE <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#000000")
  pal_group <- function(n) OKABE[seq_len(n)]
  pal_phase <- function(n) OKABE[seq_len(n)]
  theme_ivo <- function(base_size = 9, base_family = "") {
    theme_minimal(base_size = base_size, base_family = base_family) +
      theme(panel.grid.minor = element_blank(),
            strip.text = element_text(face = "bold"),
            plot.title = element_blank(), plot.subtitle = element_blank())
  }
  ggsave_ivo <- function(filename, plot = last_plot(),
                         width = c("single", "double"), height = NULL, dpi = 600) {
    width <- match.arg(width)
    w <- if (width == "single") 3.5 else 6.5
    h <- if (is.null(height)) w / 1.6 else height
    dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
    ggsave(filename, plot, width = w, height = h, dpi = dpi, units = "in", bg = "white")
    invisible(filename)
  }
}

setwd(dirname(normalizePath(sub("^--file=", "",
  grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1]))))

PROC <- "../data"
FIG  <- "../output/figures"
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)

# Keep every build of a figure under review, so successive versions can be compared
# instead of being silently overwritten.
save_version <- function(path) {
  stem <- sub("[.]png$", "", basename(path))
  n <- length(list.files(VER, pattern = paste0("^", stem, "_v[0-9]+[.]png$")))
  file.copy(path, file.path(VER, sprintf("%s_v%02d.png", stem, n + 1)))
  invisible(TRUE)
}

species    <- read.csv(file.path(PROC, "species.csv"), encoding = "UTF-8")
localities <- read.csv(file.path(PROC, "villages.csv"), encoding = "UTF-8")

park_lab <- c(TNP = "Taï NP", MPNP = "Mont Péko NP")

# Park colours are fixed here and used in every figure that distinguishes the two
# sites. Nothing else in this script may use them: a reader who learns that blue is
# Taï in one figure must not meet blue meaning "significant" in the next. Both are
# Okabe-Ito and neither appears in the IUCN Red List ramp used in Figure 3a.
PARK_COLS <- c("#0072B2", "#CC79A7")
names(PARK_COLS) <- unname(park_lab)
scale_park <- function(...) scale_colour_manual(values = PARK_COLS, name = NULL, ...)
computed <- list()   # quantities the figures assert, for the manifest

# -----------------------------------------------------------------------------
# Figures 5 to 7 equivalent: distance gradients, with the statistics on each panel
# -----------------------------------------------------------------------------
measures <- c(richness_all        = "All forms of use",
              richness_consumed   = "Consumed",
              richness_secondary  = "Secondary use",
              richness_threatened = "Threatened species")

fits <- expand.grid(park = c("TNP", "MPNP"), measure = names(measures),
                    stringsAsFactors = FALSE) |>
  rowwise() |>
  mutate(f = list({
    d <- localities[localities$park == park, ]
    # Unweighted by decision: see the note above p5 and 03_regressions_spatial.do.
    m <- lm(reformulate("distance_km", measure), data = d)
    s <- summary(m)
    tibble(n = nrow(d), slope = coef(m)[["distance_km"]],
           r2 = s$r.squared, p = s$coefficients["distance_km", "Pr(>|t|)"])
  })) |>
  unnest(f) |>
  ungroup()

plot_dat <- localities |>
  pivot_longer(all_of(names(measures)), names_to = "measure", values_to = "richness") |>
  left_join(fits[, c("park", "measure", "p")], by = c("park", "measure")) |>
  mutate(outcome = factor(unname(measures[measure]), levels = unname(measures)),
         park = factor(park_lab[park], levels = unname(park_lab)),
         sig = ifelse(p < 0.05, "p < 0.05", "not significant"))

lab <- fits |>
  mutate(outcome = factor(unname(measures[measure]), levels = unname(measures)),
         park = factor(park_lab[park], levels = unname(park_lab)),
         # The unit is the same in all eight panels and is recoverable from the two
         # axis titles, so it is printed once, in the top-left panel. Repeating it
         # eight times cost about a third of the annotation width for no information.
         unit = ifelse(park == levels(park)[1] & outcome == levels(outcome)[1],
                       " species/km", ""),
         txt = sprintf("b = %+.3f%s\nR\u00b2 = %.2f, p = %.3f", slope, unit, r2, p),
         sig = ifelse(p < 0.05, "p < 0.05", "not significant"))

# Point area encodes how many people were interviewed in the village, 6 to 41 against
# a median of 11. Species richness is effort-dependent by construction, so the reader
# should see that the points rest on very unequal amounts of information. The FIT is
# deliberately unweighted: the estimand is the average village, not the average person
# interviewed, and weighting hands about 40% of the answer to the eight settlements
# with 39 to 41 respondents. The sensitivity is tabulated in the SI instead.
p5 <- ggplot(plot_dat, aes(distance_km, richness)) +
  geom_point(aes(colour = park, size = n_respondents), alpha = 0.75,
             show.legend = FALSE) +
  geom_smooth(aes(linetype = sig, colour = park, fill = park), method = "lm",
              formula = y ~ x, linewidth = 0.55, alpha = 0.15) +
  scale_size_area(max_size = 3.2, guide = "none") +
  scale_colour_manual(values = PARK_COLS, guide = "none") +
  scale_fill_manual(values = PARK_COLS, guide = "none") +
  # Opaque backing rather than bare text. The Taï threatened panel has an observation
  # in its top-right corner, exactly where the annotation sits, and adding y headroom
  # does not help because the point moves with the scale.
  geom_label(data = lab, aes(x = Inf, y = Inf, label = txt), hjust = 1.02, vjust = 1.04,
             size = 3.2, lineheight = 1.05, inherit.aes = FALSE,
             fill = "white", alpha = 0.85, label.size = 0,
             label.padding = unit(0.9, "mm")) +
  # Free x as well as y: Tai's localities reach 27 km and Peko's 47, so a shared x
  # scale leaves the right third of every Tai panel empty. The slope comparison is
  # carried by the printed coefficients in species per km, not by the visual angle.
  facet_grid(outcome ~ park, scales = "free", switch = "y") +
  scale_linetype_manual(values = c("p < 0.05" = "solid",
                                   "not significant" = "22"), name = NULL) +
  labs(x = "Distance to park boundary (km)", y = "Reported species richness in locality") +
  # 11pt rather than the theme default of 9, which is calibrated for a 3.5-inch
  # column. Eight facets of double-column width left the tick labels and the in-panel
  # coefficients too small to read on the page.
  theme_ivo(base_size = 11) +
  theme(legend.position = "none", strip.placement = "outside")

# 5.8 -> 7.0 inches. The panels need the height twice over: to keep their aspect under
# the larger type, and because the rotated strip label "Threatened species" is set in
# 11pt bold and is clipped at both ends by any panel shorter than about 1.55 inches.
ggsave_ivo(file.path(FIG, "figure_5.png"), p5, width = "double", height = 7.0)