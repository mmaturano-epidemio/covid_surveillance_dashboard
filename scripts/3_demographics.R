# ==============================================================================
# ---- 3_demographics.R ----
# Assumes 1_setup.R and 0_load_data.R already ran. 
# Standalone fallback below only triggers if opened in isolation.
# ==============================================================================

if (!exists(".setup_done")) source(here::here("scripts", "1_setup.R"))
if (!exists(".data_loaded")) source(here::here("scripts", "1_setup.R"))

# ---- Load libraries, functions, palette and data ----

pacman::p_load(data.table, ggplot2, here, echarts4r)

# ---- Create indicators and graphs ----

# General indicators for valueboxes:
male_pct_general      <- pct(data, "gender", "M", 1) # 47.6
mean_age_general      <- round(data[, mean(age_at_dx, na.rm=T)], 1) # 41
male_pct_hospitalized <- pct(data[is_hospitalized == 1], "gender", "M", 1) # 45.9
mean_age_hospitalized <- round(data[is_hospitalized == 1, mean(age_at_dx, na.rm=T)], 1) # 52
male_pct_deceased     <- pct(data[deceased_post_covid == 1], "gender", "M", 1) # 56.9
mean_age_deceased     <- round(data[deceased_post_covid == 1, mean(age_at_dx, na.rm=T)], 1) # 71.9

# Race piechart
race <- create_piechart(data = data, group_var = "race_name", donut = T)

pyramid <- create_population_pyramid(data = data)
