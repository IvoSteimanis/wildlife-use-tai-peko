# -----------------------------------------------------------------------------
# 04b_figures_richness.R -- the restructured opening figure for the Results.
#
# The Results now open on species richness and taxonomic composition, and only then
# move to how those species are used. The title promises a contrast between the two
# parks, so the opening figure has to carry that contrast rather than a pooled
# inventory.
#
# What the contrast actually is, and is not
# -----------------------------------------
# Not taxonomic composition: the order profiles are near-identical (Artiodactyla
# 21.7% of species at Tai against 21.4% at Mont Peko, Primates 17.4 against 19.0,
# Rodentia 15.2 against 16.7, Carnivora 13.0 against 14.3).
#
# Not the pooled inventory: 46 against 42 species is an effort artefact, 333
# respondents in 21 villages against 316 in 15, and the manuscript has already
# retracted the reading of it as greater diversity at Tai.
#
# The contrast that survives is citation frequency, which is respondent-normalised:
# of the 41 species named at both parks, 25 are named more often around Mont Peko, by
# 4.5 percentage points on average. That is reported in the text rather than plotted.
#
# Outputs
# -------
#   fig_richness_contrast.png    main text: (a) species by order and park,
#                                (b) the 13 threatened species by name
#   fig_si_species_dumbbell.png  SI: all 47 species by name
#   fig_citation_scatter.png     replication package only, not in the manuscript
#   fig_status_counts.png        replication package only, not in the manuscript
#
# fig_conservation_status.png in 04_figures.R is superseded by this figure and should
# be retired once the arrangement is adopted. It is still embedded in the three .docx,
# and the copy in them predates the label fix of 2026-08-19.
#
# SI species labels are binomials only. Common names exist for 13 of the 47 species,
# exactly the threatened ones, because they were transcribed from the manuscript's own
# Results paragraph rather than supplied from memory. Printing common names for all 47
# needs 34 more names sourced and checked, which is a data task, not a plotting one.
#
# Conventions: PNG only, 600 dpi, no titles, subtitles or notes inside the figure.
#
# Run from the scripts/ folder:  Rscript 04b_figures_richness.R
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(patchwork)
})

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
    h <- if (is.null(height)) w * 0.75 else height
    ggsave(filename, plot, width = w, height = h, dpi = dpi, units = "in", bg = "white")
    invisible(filename)
  }
}

setwd(dirname(normalizePath(sub("^--file=", "",
  grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1]))))

PROC <- "../data"
FIG  <- "../output/figures"
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)

save_version <- function(path) {
  stem <- sub("[.]png$", "", basename(path))
  n <- length(list.files(VER, pattern = paste0("^", stem, "_v[0-9]+[.]png$")))
  file.copy(path, file.path(VER, sprintf("%s_v%02d.png", stem, n + 1)))
  invisible(TRUE)
}

species <- read.csv(file.path(PROC, "species.csv"), encoding = "UTF-8")
common  <- read.csv("../data/species_common_names.csv", stringsAsFactors = FALSE,
                    encoding = "UTF-8")

# Local null-coalescing helper, so the dumbbell() defaults can be "inherit from the
# base size" rather than a number that would have to be kept in sync by hand.
`%||%` <- function(a, b) if (is.null(a)) b else a

park_lab <- c(TNP = "Taï NP", MPNP = "Mont Péko NP")
PARK_COLS <- c("#0072B2", "#CC79A7")
names(PARK_COLS) <- unname(park_lab)

status_levels <- c("CR", "EN", "VU", "NT", "LC")
status_cols <- c(LC = "#60C659", NT = "#CCE226", VU = "#F9E814",
                 EN = "#FC7F3F", CR = "#D81E05")

computed <- list()

# Absent at a park is recorded as a blank cell in Table 1 and read as NA. For the
# comparison it means zero respondents named it, which is a value, not missingness.
sp <- species |>
  mutate(fc_tai  = ifelse(is.na(fc_tai), 0, fc_tai),
         fc_peko = ifelse(is.na(fc_peko), 0, fc_peko),
         at_tnp  = fc_tai > 0,
         at_mpnp = fc_peko > 0,
         diff    = fc_tai - fc_peko,
         mean_fc = (fc_tai + fc_peko) / 2,
         where   = case_when(
           at_tnp & at_mpnp & fc_peko > fc_tai ~ "Higher at Mont Péko",
           at_tnp & at_mpnp                    ~ "Higher at Taï",
           TRUE                                ~ "Cited at one park only"),
         where = factor(where, levels = c("Higher at Taï", "Higher at Mont Péko",
                                          "Cited at one park only")))

both <- sp |> filter(at_tnp, at_mpnp)
computed$X01 <- nrow(both)
computed$X02 <- sum(both$fc_peko > both$fc_tai)
computed$X03 <- mean(both$fc_peko - both$fc_tai)
computed$X04 <- sum(sp$at_tnp & !sp$at_mpnp)   # Tai-only
computed$X05 <- sum(sp$at_mpnp & !sp$at_tnp)   # Peko-only

# Abbreviated binomial, "P. capensis", for the handful of points worth naming on the
# scatter. Only the park-exclusive species are labelled: every one of them sits on an
# axis, where a reader cannot otherwise tell a true zero from a point near zero.
abbrev <- function(x) sub("^([A-Za-z])[a-z]+ ", "\\1. ", x)

excl <- sp |> filter(where == "Cited at one park only")

WHERE_COLS <- c("Higher at Taï" = unname(PARK_COLS[1]),
                "Higher at Mont Péko" = unname(PARK_COLS[2]),
                "Cited at one park only" = "grey45")

pa <- ggplot(sp, aes(fc_tai, fc_peko, colour = where)) +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.35, colour = "grey55",
              linetype = "22") +
  geom_point(size = 1.4, alpha = 0.9) +
  ggrepel::geom_text_repel(
    data = excl, aes(label = abbrev(scientific_name)),
    size = 2.0, fontface = "italic", seed = 1, min.segment.length = 0,
    segment.size = 0.25, segment.colour = "grey60",
    box.padding = 0.35, max.overlaps = Inf, show.legend = FALSE) +
  scale_colour_manual(values = WHERE_COLS, name = NULL,
                      guide = guide_legend(nrow = 1)) +
  coord_equal(xlim = c(0, 100), ylim = c(0, 100), clip = "off") +
  labs(x = "Citation frequency at Taï (%)",
       y = "Citation frequency at Mont Péko (%)") +
  theme_ivo() +
  theme(legend.position = "bottom", legend.key.size = unit(0.32, "cm"))

# (b) how many of those species are threatened, by park. Presence is citation
# frequency above zero, the same definition the manuscript uses for 46 and 42.
comp <- sp |>
  mutate(iucn_status = factor(trimws(iucn_status), levels = status_levels)) |>
  select(iucn_status, at_tnp, at_mpnp) |>
  pivot_longer(c(at_tnp, at_mpnp), names_to = "park", values_to = "present") |>
  filter(present) |>
  mutate(park = factor(ifelse(park == "at_tnp", "TNP", "MPNP"),
                       levels = c("TNP", "MPNP"), labels = unname(park_lab))) |>
  count(park, iucn_status, name = "n_species") |>
  # Label positions are computed here rather than left to position_stack(), which
  # gets them wrong in two different ways in this plot. Mapping a colour aesthetic
  # for the light-on-dark labels splits the rows into two groups, and position_stack
  # then restacks each group from zero, so the NT and VU labels land inside the LC
  # segment. Forcing group = park instead stacks the text in data order while
  # geom_col stacks in reverse factor order, which misplaces all five. geom_col puts
  # the LAST factor level at the origin, so reproduce that order explicitly and take
  # each running segment's midpoint. Verified below by verify_stack_labels().
  group_by(park) |>
  arrange(desc(as.integer(iucn_status)), .by_group = TRUE) |>
  mutate(x_end = cumsum(n_species), x_mid = x_end - n_species / 2) |>
  ungroup()

pb <- ggplot(comp, aes(n_species, park, fill = iucn_status)) +
  geom_col(width = 0.55, colour = "white", linewidth = 0.25) +
  geom_text(aes(x = x_mid, y = park, label = n_species,
                colour = ifelse(iucn_status %in% c("NT", "VU"), "grey20", "white")),
            inherit.aes = FALSE, size = 2.1, fontface = "bold") +
  scale_colour_identity() +
  scale_fill_manual(values = status_cols, name = NULL,
                    guide = guide_legend(nrow = 1, reverse = TRUE)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Species recorded in local use", y = NULL) +
  theme_ivo() +
  theme(legend.position = "bottom", legend.key.size = unit(0.32, "cm"),
        panel.grid.major.y = element_blank())

# A stacked-bar label that has drifted into the neighbouring segment renders perfectly
# and states something false, so it cannot be caught by looking at the figure. Check
# every label against the segment it actually falls in, from the built plot object.
verify_stack_labels <- function(p, bar_layer = 1, text_layer = 2) {
  b <- ggplot2::ggplot_build(p)
  seg <- b$data[[bar_layer]][, c("y", "xmin", "xmax")]
  txt <- b$data[[text_layer]][, c("y", "x", "label")]
  bad <- character(0)
  for (i in seq_len(nrow(txt))) {
    r <- seg[seg$y == txt$y[i] & seg$xmin < txt$x[i] & seg$xmax > txt$x[i], ]
    w <- if (nrow(r)) r$xmax[1] - r$xmin[1] else NA_real_
    if (is.na(w) || as.numeric(txt$label[i]) != w) {
      bad <- c(bad, sprintf("label %s at x=%.1f sits in a segment of width %s",
                            txt$label[i], txt$x[i], format(w)))
    }
  }
  if (length(bad)) stop("stacked-bar labels misplaced:\n  ", paste(bad, collapse = "\n  "))
  invisible(TRUE)
}
verify_stack_labels(pb)

# -----------------------------------------------------------------------------
# Species by taxonomic order and park
#
# This supports the opening Results sentences, which name the dominant orders and give
# their shares. Order rather than class: four of the five classes hold four species or
# fewer and no class differs between the parks by more than one species, so a class
# panel restates a sentence. The four orders the text names cover 66% of the inventory.
#
# Shares, not counts. The two parks hold different numbers of species (46 and 42), so
# raw counts confound composition with total richness and make Tai look larger in
# almost every order. The share of each park's own inventory is the normalisation that
# removes that, and it is the quantity the Results text reports.
#
# The cost is that many orders hold a single species, where one species is 2.2% at Tai
# and 2.4% at Mont Peko. The caption says the denominators are 46 and 42 so a reader
# can recover the counts, and Table S1 lists every species by order.
#
# No value labels. A dodged bar chart needs its labels dodged with exactly the same
# width, and a mismatch there silently pairs each number with the neighbouring bar.
# The axis carries the counts, so the labels are not worth that risk.
# -----------------------------------------------------------------------------
ord_raw <- sp |>
  select(order_corrected, at_tnp, at_mpnp) |>
  pivot_longer(c(at_tnp, at_mpnp), names_to = "park", values_to = "present") |>
  filter(present) |>
  mutate(park = factor(ifelse(park == "at_tnp", "TNP", "MPNP"),
                       levels = c("TNP", "MPNP"), labels = unname(park_lab))) |>
  count(park, order_corrected, name = "n_species")

# Eight of the sixteen orders hold a single species in the pooled inventory. Plotted
# individually they are eight rows of identical stubs, 2.2% at Tai against 2.4% at Mont
# Peko, which is one animal either side and nothing a reader can act on. They are
# collapsed into one labelled row, pinned to the bottom so it is read as a residual
# rather than ranked among the real orders.
# Counted from the pooled 47-species inventory, not by summing the two parks: an order
# holding one species that is recorded at both parks sums to 2 in the long format and
# would escape the filter, which is what left six singleton orders on the axis.
singletons <- sp |>
  count(order_corrected, name = "tot") |>
  filter(tot == 1) |>
  pull(order_corrected)

OTHER <- sprintf("Other (%d orders, 1 species each)", length(singletons))

ord <- ord_raw |>
  mutate(grp = ifelse(order_corrected %in% singletons, OTHER, order_corrected)) |>
  group_by(park, grp) |>
  summarise(n_species = sum(n_species), .groups = "drop") |>
  group_by(park) |>
  mutate(pct = 100 * n_species / sum(n_species)) |>
  ungroup()

named_levels <- ord |>
  filter(grp != OTHER) |>
  group_by(grp) |>
  summarise(tot = sum(n_species), .groups = "drop") |>
  arrange(tot, grp) |>
  pull(grp)

ord <- ord |> mutate(grp = factor(grp, levels = c(OTHER, named_levels)))

pcls <- ggplot(ord, aes(pct, grp)) +
  geom_line(aes(group = grp), colour = "grey70", linewidth = 0.4) +
  geom_point(aes(colour = park), size = 2.55) +
  # nrow and override.aes must match panel (b) exactly: plot_layout(guides =
  # "collect") merges two guides only when their specs are identical, and would
  # otherwise stack two legends rather than share one.
  scale_colour_manual(values = PARK_COLS, name = NULL,
                      guide = guide_legend(nrow = 1,
                                           override.aes = list(size = 3.2))) +
  # Anchored at zero: a share axis that starts at the smallest value exaggerates every
  # gap, and here the smallest is 2%, which would stretch a one-species difference
  # across a quarter of the panel.
  scale_x_continuous(limits = c(0, NA), breaks = seq(0, 25, 5),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  # Padding above the first row and below the last, so the top and bottom pairs do not
  # sit against the panel edge. Nine rows in a short panel read as a solid block
  # otherwise.
  scale_y_discrete(expand = expansion(add = 0.7)) +
  # No "(%)" in the title: the tick labels already carry the sign.
  labs(x = "Share of the park's recorded species", y = NULL) +
  # 11pt rather than the theme default of 9. theme_ivo()'s 9pt is calibrated for a
  # 3.5-inch column; this figure is double-column, so 9pt renders at roughly half the
  # relative height it was designed for and is hard to read on the page.
  theme_ivo(base_size = 11) +
  theme(legend.position = "bottom", legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 5)),
        panel.grid.major.y = element_blank())

# -----------------------------------------------------------------------------
# Uncertainty on the Tai minus Mont Peko difference
#
# Printed citation frequencies recover exact respondent counts against the park
# denominators, so the comparison needs no species crosswalk: FC x N is an integer.
#
# Intervals are Newcombe's hybrid-score interval for a difference of two independent
# proportions, which behaves near 0 and 100 where the normal approximation does not,
# and several species sit at zero at one park.
#
# They are NOT clustered. Respondents cluster within villages and species knowledge is
# exactly the sort of thing that clusters locally, so these intervals are too narrow.
# A village ICC of 0.05 would widen them by about a third and a value of 0.10 by about
# a half. The ICC cannot be estimated here: it needs respondent-by-species citations,
# and the response-option-to-species crosswalk resolves only 18 of 43 options. The
# caption must say the intervals assume independent respondents. No significance
# markers are printed, because whether a given species "differs" is decided by that
# unestimated ICC rather than by the data in this figure.
# -----------------------------------------------------------------------------
N_TNP <- 333
N_MPNP <- 316

wilson <- function(x, n) {
  z <- 1.959964; p <- x / n; d <- 1 + z^2 / n
  c((p + z^2 / (2 * n)) / d - z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / d,
    (p + z^2 / (2 * n)) / d + z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / d)
}
newcombe <- function(x1, n1, x2, n2) {
  a <- wilson(x1, n1); b <- wilson(x2, n2); p1 <- x1 / n1; p2 <- x2 / n2
  c((p1 - p2) - sqrt((p1 - a[1])^2 + (b[2] - p2)^2),
    (p1 - p2) + sqrt((a[2] - p1)^2 + (p2 - b[1])^2))
}

sp <- sp |>
  mutate(x_tai = round(fc_tai * N_TNP / 100),
         x_pek = round(fc_peko * N_MPNP / 100))
ci <- t(mapply(newcombe, sp$x_tai, N_TNP, sp$x_pek, N_MPNP))
sp$ci_lo <- 100 * ci[, 1]
sp$ci_hi <- 100 * ci[, 2]
# round() returns negative zero for a small negative value, and sprintf prints that
# as "-0", which reads as a real negative bound. Assigning 0 clears the sign bit.
z0 <- function(x) { v <- round(x); v[v == 0] <- 0; v }
sp$dlab <- sprintf("%+.0f [%.0f, %.0f]", z0(sp$diff), z0(sp$ci_lo), z0(sp$ci_hi))

# -----------------------------------------------------------------------------
# Dumbbell helper, used for panel (c) and for the SI figure
# -----------------------------------------------------------------------------
dumbbell <- function(dat, lab_col, facet = FALSE, label_fn = NULL,
                     lab_size = 5.6, italic = TRUE,
                     base = 9, pt_size = 1.4, ann_size = 1.9,
                     key_cm = 0.32, x_lab = 137, x_max = 168,
                     leg_size = NULL, key_pt = NULL) {
  long <- dat |>
    pivot_longer(c(fc_tai, fc_peko), names_to = "park", values_to = "fc") |>
    mutate(park = factor(ifelse(park == "fc_tai", "TNP", "MPNP"),
                         levels = c("TNP", "MPNP"), labels = unname(park_lab)))
  g <- ggplot(long, aes(fc, .data[[lab_col]])) +
    geom_line(aes(group = .data[[lab_col]]), colour = "grey70", linewidth = 0.4) +
    geom_point(aes(colour = park), size = pt_size) +
    geom_vline(xintercept = 106, colour = "grey85", linewidth = 0.3) +
    geom_text(data = dat, aes(x = x_lab, y = .data[[lab_col]], label = dlab),
              inherit.aes = FALSE, size = ann_size, hjust = 0.5, family = "mono") +
    scale_colour_manual(values = PARK_COLS, name = NULL,
                        guide = guide_legend(
                          nrow = 1,
                          override.aes = list(size = key_pt %||% pt_size))) +
    scale_x_continuous(limits = c(0, x_max), breaks = seq(0, 100, 25),
                       expand = expansion(mult = c(0.01, 0.01))) +
    coord_cartesian(clip = "off") +
    labs(x = "Citation frequency (%)", y = NULL) +
    theme_ivo(base_size = base) +
    theme(legend.position = "bottom", legend.key.size = unit(key_cm, "cm"),
          legend.text = element_text(size = leg_size %||% (base - 1)),
          panel.grid.major.y = element_blank())
  g <- if (is.null(label_fn)) {
    g + theme(axis.text.y = element_text(
      size = lab_size, face = if (italic) "italic" else "plain"))
  } else {
    g + scale_y_discrete(labels = label_fn) +
      theme(axis.text.y = element_text(size = lab_size))
  }
  if (facet) {
    g <- g + facet_grid(iucn_status ~ ., scales = "free_y", space = "free_y",
                        switch = "y") +
      theme(strip.placement = "outside")
  }
  g
}

# -----------------------------------------------------------------------------
# Panel (c): the 13 threatened species, by common name and binomial
#
# Common names exist for exactly these 13 species, because they were transcribed from
# the manuscript's own Results paragraph rather than supplied from memory. The Results
# enumerate all thirteen by common name, so this panel is what that prose points at.
# -----------------------------------------------------------------------------
thr <- sp |>
  filter(trimws(iucn_status) %in% c("VU", "EN", "CR")) |>
  left_join(common[, c("scientific_name", "common_name")], by = "scientific_name") |>
  mutate(iucn_status = factor(trimws(iucn_status), levels = c("VU", "EN", "CR")),
         sp_lab = reorder(scientific_name, mean_fc))
stopifnot(!any(is.na(thr$common_name)))

# Common name only. The binomial roughly doubles the label width, and the axis
# column it needs is taken straight out of the panel, which is what forced the
# labels down to 7pt. Figure S1 lists every species by scientific name, so the
# taxonomy is not lost, and the caption should point there.
lab_expr <- function(v) thr$common_name[match(as.character(v), thr$scientific_name)]

# ann_size is in millimetres: 3.2 mm is 9.1pt, the floor set for this figure.
pc <- dumbbell(thr, "sp_lab", facet = TRUE, label_fn = lab_expr, lab_size = 10,
               base = 11, pt_size = 2.2, ann_size = 3.2, key_cm = 0.5,
               leg_size = 12, key_pt = 3.2, x_lab = 148, x_max = 196)

# Two panels: what was recorded, then which of it is threatened.
#
# (a) species by taxonomic order and park, supporting the opening Results sentences.
# (b) the 13 threatened species by name, supporting the conservation paragraph.
#
# The citation-frequency scatter and the Red List count bars are deliberately NOT here.
# The average park difference in citation frequency is reported as text, and the Red
# List totals are given as absolute numbers in the text, so neither needs a panel. Both
# plots remain in the replication package.
#
# free() releases panel (a) from the left-edge alignment patchwork otherwise forces on
# both. Panel (b) carries long species labels, and without this panel (a) is pushed
# right to align with them, leaving a band of white space and detaching its axis from
# its bars.
# guides = "collect" gathers the two identical park legends into one for the whole
# plate. Both panels encode the same two colours, so a legend under each is pure
# duplication and costs vertical space the labels can use instead.
p <- patchwork::free(pcls, type = "panel", side = "l") + pc +
  plot_layout(ncol = 1, heights = c(2.4, 3.4), guides = "collect") +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(face = "bold", size = 11),
        legend.position = "bottom",
        legend.text = element_text(size = 12))

# Height raised from 7.6 to 8.7 inches alongside the 9 -> 11pt base. Holding the
# height fixed while enlarging the type collides the 13 species labels in panel (b).
ggsave_ivo(file.path(FIG, "figure_2.png"), p, width = "double", height = 8.7)

# Kept for the replication package: the park contrast across all species, and the Red
# List counts by park. Neither appears in the manuscript.

# -----------------------------------------------------------------------------
# SI: every species by name, ordered by mean citation frequency
# -----------------------------------------------------------------------------
all_sp <- sp |> mutate(sp_lab = reorder(scientific_name, mean_fc))
g_all <- dumbbell(all_sp, "sp_lab")
ggsave_ivo(file.path(FIG, "figure_S1.png"), g_all,
           width = "double", height = 8.4)

# -----------------------------------------------------------------------------
# SI table: the same 47 species as the dumbbell above, as a table
#
# The dumbbell needs about ten inches of height before 47 binomials are legible,
# and the SI places it at 5.76 inches, where its labels land at roughly 5pt. The
# content is 47 rows of paired numbers, which is what a table is for; the figure
# already printed the difference and its interval as text beside every row.
#
# Built from `sp` after the Newcombe intervals are attached, so the table and the
# figure format the same numbers rather than deriving them twice.
#
# Ordered by class, then family, then descending mean citation frequency within
# family, because the table is for looking a species up. The figure ordered
# purely by mean frequency; change the arrange() below to reproduce that order.
# -----------------------------------------------------------------------------
TAB <- "../output/tables"
dir.create(TAB, showWarnings = FALSE, recursive = TRUE)

ltx <- function(x) {
  x <- as.character(x)
  x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)
  for (ch in c("&", "%", "$", "#", "_", "{", "}"))
    x <- gsub(ch, paste0("\\", ch), x, fixed = TRUE)
  x
}

si_tab <- sp |>
  mutate(class_p  = ifelse(is.na(class) | class == "", "Unassigned", class),
         family_p = ifelse(is.na(family_corrected) | family_corrected == "",
                           "Unassigned", family_corrected),
         iucn_p   = ifelse(is.na(iucn_status) | trimws(iucn_status) == "",
                           "NE", trimws(iucn_status))) |>
  arrange(class_p, family_p, desc(mean_fc)) |>
  mutate(
    # One decimal throughout. The figure printed the difference as an integer;
    # these are the same quantities at the precision the source carries, so the
    # figure's labels are visibly roundings of these rather than other numbers.
    tai  = sprintf("%.1f", fc_tai),
    pek  = sprintf("%.1f", fc_peko),
    dcol = sprintf("%+.1f [%.1f, %.1f]", diff, ci_lo, ci_hi),
    # Blank a class or family that repeats the row above, as tab_localities.tex does.
    class_show  = ifelse(duplicated(class_p), "", class_p),
    family_show = ifelse(duplicated(paste(class_p, family_p)), "", family_p))

stopifnot(nrow(si_tab) == nrow(sp), !any(is.na(si_tab$ci_lo)))

body <- sprintf("%s & %s & \\textit{%s} & %s & %s & %s & %s \\\\",
                ltx(si_tab$class_show), ltx(si_tab$family_show),
                ltx(si_tab$scientific_name), ltx(si_tab$iucn_p),
                si_tab$tai, si_tab$pek, si_tab$dcol)

writeLines(c(
  "\\begin{tabular}{lllcrrr}",
  "\\toprule",
  paste("Class & Family & Species & IUCN & Ta\u00ef NP (\\%) &",
        "Mont P\u00e9ko NP (\\%) & Difference [95\\% CI] \\\\"),
  "\\midrule",
  body,
  "\\bottomrule",
  "\\end{tabular}"),
  file.path(TAB, "table_1_species.tex"), useBytes = TRUE)

write.csv(si_tab[, c("class_p", "family_p", "scientific_name", "iucn_p",
                     "fc_tai", "fc_peko", "diff", "ci_lo", "ci_hi")],
          file.path(TAB, "table_1_species.csv"), row.names = FALSE)

cat(sprintf("\nSI species table: %d rows, %d classes, %d families -> tab_si_species.tex\n",
            nrow(si_tab), dplyr::n_distinct(si_tab$class_p),
            dplyr::n_distinct(si_tab$family_p)))

# -----------------------------------------------------------------------------
# Numbered set for submission
#
# The pipeline keeps semantic names, because figure numbers move: the body-parts plate
# went from Figure 2 to Figure 3 when the Results were reordered to open on richness.
# ARPHA, though, needs an unambiguous upload order and gives no proofs, so a numbered
# copy is written here as a separate deliverable. The number-to-figure mapping lives in
# this one table; when the order changes, edit it here and nowhere else. The semantic
# stem is kept in the numbered name so a stale number is still self-describing.