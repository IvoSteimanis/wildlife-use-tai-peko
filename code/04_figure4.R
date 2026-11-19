# 04e_figure4_timing.R -- Figure 4, publication build.
#
# Share of respondents naming the rainy season as a period of bushmeat abundance, by the
# month in which they were interviewed, separately for the two parks.
#
# The figure exists to show that the answer tracks WHEN people were asked. August 2024 is
# the only month in which both parks were surveyed and the two are indistinguishable
# there, so the apparent park difference in the pooled numbers is a survey-timing
# artefact rather than an ecological contrast.
#
# Sample: the 549 respondents who named at least one season. No interviews took place in
# September 2024 or January 2025, and the lines break across both gaps.
#
# Style variables are collected at the top so they can be synced with the other figures
# in this package. PARK_COLS matches 04_figures.R exactly; a hue must not carry two
# meanings across the paper's figures.
#
# Output is PNG only, per the project convention.
#
# Run:  Rscript 04e_figure4_timing.R

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(scales)
})

# strftime month names follow LC_TIME; on a German Windows "%b" yields "Okt" and "Mrz".
Sys.setlocale("LC_TIME", "C")

# ---- style variables, sync these with the other figures ------------------------------
PARK_COLS  <- c("Taï NP" = "#0072B2", "Mont Péko NP" = "#CC79A7")
FONT       <- "sans"
BASE_SIZE  <- 11
GRID_COL   <- "grey88"
LABEL_COL  <- "grey15"   # the italic n marking the sample-size row; grey30 read as faint
DODGE_DAYS <- 12
N_ROW      <- 0.045   # y position of the sample-size row, below every estimate
OUT_W_MM   <- 178
OUT_H_MM   <- 104

setwd(dirname(normalizePath(sub("^--file=", "",
  grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1]))))

DATA <- "../data"
FIG  <- "../output/figures"
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)

# ---- data ----------------------------------------------------------------------------
# Real respondent-level data when available; otherwise the fake block below, which
# reproduces the observed monthly counts exactly so the script runs standalone.
resp_path <- file.path(DATA, "respondents.csv")

if (file.exists(resp_path)) {
  raw <- read.csv(resp_path, encoding = "UTF-8", stringsAsFactors = FALSE)
  d <- raw %>%
    filter(season_dry == 1 | season_wet == 1) %>%
    transmute(month = as.Date(paste0(substr(interview_date, 1, 7), "-01")),
              park  = ifelse(park == "TNP", "Taï NP", "Mont Péko NP"),
              rainy = as.integer(season_wet == 1))
  message("using respondents.csv: ", nrow(d), " respondents")
} else {
  message("using the monthly counts in data/seasonality_by_month.csv")
  obs <- read.csv(file.path(DATA, "seasonality_by_month.csv"), stringsAsFactors = FALSE) %>%
    transmute(park = ifelse(park == "TNP", "Taï NP", "Mont Péko NP"),
              month = paste0(month, "-01"), k = n_rainy, n = n_answered)
  d <- obs %>%
    rowwise() %>%
    do(data.frame(park = .$park, month = as.Date(.$month),
                  rainy = c(rep(1L, .$k), rep(0L, .$n - .$k)))) %>%
    ungroup()
}

d$park <- factor(d$park, levels = names(PARK_COLS))

# ---- aggregate with a Wilson interval -------------------------------------------------
# binom::binom.wilson when the package is present; the closed form otherwise, which is
# the same quantity and keeps the script runnable without the dependency.
wilson <- function(k, n, conf = 0.95) {
  if (requireNamespace("binom", quietly = TRUE)) {
    w <- binom::binom.wilson(k, n, conf.level = conf)
    return(c(w$mean, w$lower, w$upper))
  }
  z <- qnorm(1 - (1 - conf) / 2)
  p <- k / n
  den <- 1 + z^2 / n
  c(p,
    (p + z^2 / (2 * n) - z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / den,
    (p + z^2 / (2 * n) + z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / den)
}

agg <- d %>%
  group_by(park, month) %>%
  summarise(k = sum(rainy), n = dplyr::n(), .groups = "drop") %>%
  rowwise() %>%
  mutate(est = wilson(k, n)[1], lo = wilson(k, n)[2], hi = wilson(k, n)[3]) %>%
  ungroup() %>%
  arrange(park, month)

# Run id per park: a new run starts wherever the previous surveyed month is more than one
# month back. This is what breaks the line across September 2024 and January 2025, and it
# leaves the Mont Péko August point unconnected from October.
agg <- agg %>%
  group_by(park) %>%
  mutate(gap = c(0, round(as.numeric(diff(month)) / 30.4)),
         run = cumsum(gap > 1)) %>%
  ungroup()

print(as.data.frame(agg))

# ---- figure ---------------------------------------------------------------------------
pd <- position_dodge(width = DODGE_DAYS)
brk <- seq(as.Date("2024-06-01"), as.Date("2025-03-01"), by = "month")

# Year labels sit outside the panel, centred under the months they cover.
yr <- data.frame(
  x = c(as.Date("2024-09-01"), as.Date("2025-02-01")),
  label = c("2024", "2025"))

p <- ggplot(agg, aes(x = month, y = est, colour = park, group = interaction(park, run))) +
  geom_line(position = pd, linewidth = 0.45) +
  geom_pointrange(aes(ymin = lo, ymax = hi), position = pd, size = 0.32,
                  linewidth = 0.5) +
  # The n labels sit in a fixed row near the axis, not beneath each interval. Anchoring
  # them to `lo` puts them wherever the lower confidence bound happens to fall, which
  # collided with the rising Taï line at July and with the Taï interval at August. A
  # fixed row cannot collide with anything, since no estimate falls below 40%.
  # Coloured by park so each number is still attributable, and dodged so the two August
  # values do not overlap each other.
  geom_text(aes(y = N_ROW, label = n, colour = park),
            position = position_dodge(width = DODGE_DAYS * 2.4),
            size = 3.2, show.legend = FALSE) +
  annotate("text", x = min(brk) - 6, y = N_ROW, label = "n",
           size = 3.2, colour = LABEL_COL, fontface = "italic", hjust = 1) +
  scale_colour_manual(values = PARK_COLS, name = NULL) +
  scale_x_date(breaks = brk, date_labels = "%b",
               expand = expansion(mult = c(0.075, 0.045))) +
  # The y range is set on the COORD, not the scale. scale_y_continuous(limits = ...)
  # drops any layer positioned outside the range, which silently deleted both year
  # labels at y = -0.155 and rendered the figure without them and without an error.
  scale_y_continuous(breaks = c(0, .25, .5, .75, 1),
                     labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.02, 0.03))) +
  labs(x = NULL, y = "Named the rainy season (%)") +
  coord_cartesian(ylim = c(0, 1), clip = "off") +
  theme_minimal(base_size = BASE_SIZE, base_family = FONT) +
  theme(
    panel.grid.major.y = element_line(colour = GRID_COL, linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(angle = 0, vjust = 1),
    axis.ticks         = element_blank(),
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    legend.title       = element_blank(),
    legend.key.size    = unit(0.49, "cm"),
    legend.box.margin  = margin(t = 12),
    plot.title         = element_blank(),
    plot.subtitle      = element_blank(),
    plot.margin        = margin(t = 4, r = 6, b = 6, l = 4))

# ---- panel b: observed rainfall -------------------------------------------------------
# The manuscript states that both areas have two rainy and two dry seasons (Lauginie
# 2007; Sidibe et al. 2020) but gives no months, and onset shifts from year to year, so a
# shaded "rainy season" band would be an unsourced assertion. This panel shows the
# observed rainfall for the survey window instead.
#
# Two independent products are used so the wet/dry reading does not rest on one model:
# ERA5-Land via the Open-Meteo archive, and NASA POWER PRECTOTCORR. The line is their
# mean and the band spans the two, so disagreement between products is visible rather
# than hidden. 13_fetch_rainfall.py writes the input.
#
# A single shaded band across both parks would have been wrong in any case: in July 2024
# Tai received about 68 mm against Mont Peko's 136 mm, Tai being in its short dry season
# while Mont Peko was still wet.
rain_path <- file.path(DATA, "rainfall_monthly.csv")
have_rain <- file.exists(rain_path)

if (have_rain) {
  rain <- read.csv(rain_path, encoding = "UTF-8", stringsAsFactors = FALSE) %>%
    mutate(park_f = factor(ifelse(park == "TNP", "Taï NP", "Mont Péko NP"),
                           levels = names(PARK_COLS)),
           mdate = as.Date(paste0(month, "-01"))) %>%
    filter(mdate >= min(brk), mdate <= max(brk)) %>%
    group_by(park_f, mdate) %>%
    summarise(mm = mean(precip_mm), lo = min(precip_mm), hi = max(precip_mm),
              .groups = "drop")

  cat("\n=== panel b: monthly rainfall, mean of two products ===\n")
  print(as.data.frame(rain))

  pb <- ggplot(rain, aes(x = mdate, y = mm, colour = park_f, fill = park_f)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.18, colour = NA) +
    geom_line(linewidth = 0.45) +
    scale_colour_manual(values = PARK_COLS, guide = "none") +
    scale_fill_manual(values = PARK_COLS, guide = "none") +
    scale_x_date(breaks = brk, date_labels = "%b",
                 expand = expansion(mult = c(0.075, 0.045))) +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
    labs(x = NULL, y = "Rainfall (mm)") +
    theme_minimal(base_size = BASE_SIZE, base_family = FONT) +
    theme(panel.grid.major.y = element_line(colour = GRID_COL, linewidth = 0.3),
          panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank(),
          axis.ticks         = element_blank())
}

# ---- variant: single panel, rainfall as shaded bars on a secondary axis ---------------
# Built alongside the two-panel version rather than replacing it, so the two can be
# compared directly. A secondary axis is the standard climate-diagram convention and is
# more compact, but the scaling of the second axis is arbitrary, which is the usual
# objection: it can make co-movement look stronger or weaker than it is. The two-panel
# version does not have that problem because each series keeps its own scale.
#
# Bars are monthly, matching the points they sit behind. Weekly bars were considered and
# rejected: the shares are monthly, so weekly rainfall would imply a resolution the
# survey data do not have.
if (have_rain) {
  RAIN_MAX <- ceiling(max(rain$mm) / 50) * 50

  p_ov <- ggplot() +
    geom_col(data = rain,
             aes(x = mdate, y = mm / RAIN_MAX, fill = park_f),
             position = position_dodge(width = 13), width = 12,
             alpha = 0.22, colour = NA, show.legend = FALSE) +
    geom_line(data = agg,
              aes(x = month, y = est, colour = park, group = interaction(park, run)),
              position = pd, linewidth = 0.45) +
    geom_pointrange(data = agg,
                    aes(x = month, y = est, ymin = lo, ymax = hi, colour = park,
                        group = interaction(park, run)),
                    position = pd, size = 0.32, linewidth = 0.5) +
    geom_text(data = agg,
              aes(x = month, y = N_ROW, label = n, colour = park),
              position = position_dodge(width = DODGE_DAYS * 2.4),
              size = 3.2, show.legend = FALSE) +
    annotate("text", x = min(brk) - 6, y = N_ROW, label = "n",
             size = 3.2, colour = LABEL_COL, fontface = "italic", hjust = 1) +
    annotate("text", x = yr$x, y = -0.135, label = yr$label,
             size = 3.5, colour = "grey25") +
    scale_colour_manual(values = PARK_COLS, name = NULL) +
    scale_fill_manual(values = PARK_COLS, guide = "none") +
    scale_x_date(breaks = brk, date_labels = "%b",
                 expand = expansion(mult = c(0.075, 0.045))) +
    scale_y_continuous(breaks = c(0, .25, .5, .75, 1),
                       labels = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0.02, 0.03)),
                       sec.axis = sec_axis(~ . * RAIN_MAX, name = "Rainfall (mm)")) +
    labs(x = NULL, y = "Named the rainy season (%)") +
    coord_cartesian(ylim = c(0, 1), clip = "off") +
    theme_minimal(base_size = BASE_SIZE, base_family = FONT) +
    theme(panel.grid.major.y = element_line(colour = GRID_COL, linewidth = 0.3),
          panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank(),
          axis.ticks         = element_blank(),
          legend.position    = "bottom",
          legend.direction   = "horizontal",
          legend.title       = element_blank(),
          legend.key.size    = unit(0.49, "cm"),
          legend.box.margin  = margin(t = 14),
          plot.title         = element_blank(),
          plot.margin        = margin(t = 4, r = 6, b = 8, l = 4))

  ov_path <- file.path(FIG, "figure_4_single_panel_variant.png")
  ggsave(ov_path, p_ov, width = OUT_W_MM, height = 110, units = "mm",
         dpi = 600, bg = "white")
  cat("wrote overlay variant:", ov_path, "\n")
}

png_path <- file.path(FIG, "figure_4.png")

if (have_rain && requireNamespace("patchwork", quietly = TRUE)) {
  suppressPackageStartupMessages(library(patchwork))
  # Month labels and year labels belong to the BOTTOM panel only. The year annotation is
  # therefore added to pb, in pb's own units, and panel a drops its x-axis text. Leaving
  # the years on panel a would print them in the gap between the two panels.
  # The offset must clear the month labels, which the axis draws immediately below the
  # panel. At -0.155 the years printed on top of "Sep" and "Feb". The value is a fraction
  # of panel height, so it has to be re-checked if the panel height ratio changes.
  ymax <- max(rain$hi)
  pb <- pb + annotate("text", x = yr$x, y = -0.34 * ymax, label = yr$label,
                      size = 3.5, colour = "grey25") +
    coord_cartesian(ylim = c(0, ymax), clip = "off") +
    theme(plot.margin = margin(t = 2, r = 6, b = 22, l = 4))
  pa <- p + theme(axis.text.x = element_blank(),
                  plot.margin = margin(t = 4, r = 6, b = 2, l = 4))
  combined <- (pa / pb) +
    plot_layout(heights = c(2.3, 1), guides = "collect") +
    plot_annotation(tag_levels = "a") &
    theme(legend.position = "bottom",
          plot.tag = element_text(size = 10, face = "bold"))
  ggsave(png_path, combined, width = OUT_W_MM, height = 136, units = "mm",
         dpi = 600, bg = "white")
  cat("\nwrote two-panel figure\n")
} else {
  if (!have_rain) message("rainfall_monthly.csv not found; writing the single panel")
  p1 <- p + annotate("text", x = yr$x, y = -0.112, label = yr$label,
                     size = 3.5, colour = "grey25")
  ggsave(png_path, p1, width = OUT_W_MM, height = OUT_H_MM, units = "mm",
         dpi = 600, bg = "white")
}

# PNG only, per the project convention. Remove any stale PDF of this figure so the two
# cannot drift apart and so nothing downstream picks up the wrong one.
pdf_path <- file.path(FIG, "fig_seasonality_timing.pdf")
if (file.exists(pdf_path)) {
  file.remove(pdf_path)
  message("removed stale ", basename(pdf_path))
}

cat("\nwrote", png_path, "\n")
cat("FIGURE4_DONE\n")
