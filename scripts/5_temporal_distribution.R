# ==============================================================================
# ---- 5_temporal_distribution.R ----
# Assumes 1_setup.R and 0_load_data.R already ran. 
# Standalone fallback below only triggers if opened in isolation.
# ==============================================================================
if (!exists(".setup_done")) source(here::here("scripts", "1_setup.R"))
if (!exists(".data_loaded")) source(here::here("scripts", "1_setup.R"))

epidemic_curve <- build_epidemic_curve(data)

fc_result <- forecast_epidemic_curve(epidemic_curve, cutoff_date = as.Date("2020-02-25"), horizon = 14)

forecast_chart <- create_forecast_chart(fc_result, title = "ARIMA backtest: naive extrapolation vs. actual outbreak curve")

# arima_note <- sprintf(
#   "Model: ARIMA(%d,%d,%d), no seasonal component (single outbreak wave, insufficient data to estimate a recurring cycle). 
#   This is a statistical extrapolation of the autocorrelation structure, not an epidemiological (SIR-type) model — 
#   it does not know the outbreak is past its peak, which is reflected in the widening uncertainty band rather than a false sense of precision.",
#   arima_order["p"], arima_order["d"], arima_order["q"]
# )

temporal_series <- build_temporal_series(data)
temporal_chart <- create_temporal_chart(temporal_series, title = "COVID-19 timeline: cases, hospitalizations, deaths")
