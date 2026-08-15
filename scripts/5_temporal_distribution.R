# ==============================================================================
# ---- 5_temporal_distribution.R ----
# Assumes 1_setup.R already ran. 
# Standalone fallback below only triggers if opened in isolation.
# ==============================================================================
if (!exists(".setup_done")) source(here::here("scripts", "1_setup.R"))
if (!exists(".data_loaded")) source(here::here("scripts", "1_setup.R"))

epidemic_curve <- build_epidemic_curve(data)

fc_result <- forecast_epidemic_curve(epidemic_curve, cutoff_date = as.Date("2020-02-25"), horizon = 14)

forecast_chart <- create_forecast_chart(fc_result, title = "ARIMA backtest: naive extrapolation vs. actual outbreak curve")

temporal_series <- build_temporal_series(data)
temporal_chart <- create_temporal_chart(temporal_series, title = "COVID-19 timeline: cases, hospitalizations, deaths")
