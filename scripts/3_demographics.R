# ==============================================================================
# ---- 3_demographics.R ----
# Assumes 1_setup.R already ran. 
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



data[, .(Patients = .N), 
     keyby = .(`Severity level` = factor(
       fcase(severity_level == 0, "Outpatient",
             severity_level %in% 1:2, "Hospitalized without respiratory support",
             severity_level %in% 3:4, "Respiratory support / deceased"),
       levels = c("Outpatient", "Hospitalized without respiratory support", "Respiratory support / deceased")))][, `%` := round(Patients * 100 / sum(Patients), 1)][]

# 4  -- Deceased
# 3  -- Mechanical ventilation
# 2  -- Oxygen therapy without ventilation
# 1  -- Hospitalized without respiratory support
# 0  -- Outpatient/ER without admission


comorbidity_severity_chart <- create_comorbidity_severity_chart(
  data = data, 
  comorbidity_labels = comorbidity_labels,
  title = "Outcome severity by pre-existing comorbidity"
)

comorbidity_count_chart <- create_barchart(
  data = data, x_var = "comorbidity_count_grouped", 
  title = "Total patients by pre-existing comorbidity count"
)

comorbidity_doseresponse_chart <- create_grouped_severity_chart(
  data = data, group_var = "comorbidity_count_grouped",
  group_levels = c("0","1","2","3","4+"),
  title = "Severity by count of pre-existing selected comorbidities"
)

age_comorbidity_heatmap <- create_age_comorbidity_heatmap(cap = NULL,
  data = data, comorbidity_labels = comorbidity_labels,
  binning = "quantile", n_bins = 20, min_n = 5,
  title = "Critical outcome rate by age group and comorbidity"
)
