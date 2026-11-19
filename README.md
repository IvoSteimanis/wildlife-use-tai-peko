# Replication data and code

**Multidimensional wildlife use in communities surrounding Taï and Mont Péko National Parks, Côte d'Ivoire**

Boris Gagbé Gagbe, Yao Célestin Kouakou, Yao Alban Fernand Tiécoura, Ange Edgar Habib Monket, Alex Ange Beda, Ivo Steimanis, Gilbert Fokou, Lorenzo Lagostina, Bjoern Vollan, Hjalmar S. Kühl

This package reproduces the distance-gradient regressions (Table S3), the robustness
checks (Table S13), and Figures 2, 4, 5 and S1 of the article, from village-level and
species-level data only.

## Data (`data/`)

| File | Content |
|---|---|
| `villages.csv` | One row per study village (21 around Taï, 15 around Mont Péko): park, village code, distance to the park boundary (km), reported species richness for all forms of use, consumption, secondary use and threatened species, and number of respondents. Villages are identified by codes only, numbered by distance within each park. |
| `species.csv` | The 47 species: taxonomy, IUCN status, citation frequency per park (%), and forms of use per park (Table 1). |
| `species_common_names.csv`, `taxonomy_corrections.csv` | English names; taxonomic corrections applied to the field records. |
| `seasonality_by_month.csv` | Per park and month of interview: respondents naming at least one season, and of those, naming the rainy season. |
| `seasonality_by_group.csv` | The same by respondent group (Table S6). |
| `clustered_estimates.csv` | Shares and village cluster-bootstrap intervals reported in Table S11. |
| `rainfall_monthly.csv` | Monthly precipitation at one point per park, June 2024 to March 2025, from ERA5-Land and NASA POWER (Fig. 4b). |

## Code (`code/`), run from `code/`

| Script | Produces |
|---|---|
| `01_regressions.do` (Stata 16+) | Distance-gradient regressions and sensitivity to survey effort (Tables S3, S8). |
| `02_robustness.do` (Stata 16+) | Robustness checks of the gradients (Table S13), `output/robustness_estimates.csv`. |
| `03_figure2_figureS1.R` | Figures 2 and S1. |
| `04_figure4.R` | Figure 4. |
| `05_figure5.R` | Figure 5. |

R packages: tidyverse, patchwork, scales (R 4.3 or later). `theme_ivo.R` holds the shared figure theme.

## What is not included, and why

The survey asked about hunting and the consumption of protected species, which can be
illegal. Several villages have fewer than ten respondents, so respondent-level records
(even without names) could identify individuals. They are therefore not published, and
neither are village names or coordinates. For this reason the package does not reproduce
Figure 1 (map of the study villages), Figure 3 (photographs), Figure S3 (species
accumulation) and the tests in Tables S9 and S12, which need respondent-level data;
the published estimates of Tables S5, S6 and S11 are provided as aggregates.

## Licence

Data and documentation: CC BY 4.0. Code: MIT.
