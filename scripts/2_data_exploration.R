# ==============================================================================
# ---- 2_data_exploration.R ----
# ==============================================================================

# ---- Getting libraries and data ----

pacman::p_load(data.table, ggplot2, here)

data <- readRDS(here("datasets", "tidy_data.rds"))

# ---- Exploring data ----

data |> str()

data[, age_at_dx |> hist()]  
data[is_hospitalized == 1, age_at_dx |> hist()] 
data[deceased_post_covid == 1, age_at_dx |> hist()] 

data[, as.numeric(deathdate - covid_date) |> summary()]
data[as.numeric(deathdate - covid_date) > 30, .N]
