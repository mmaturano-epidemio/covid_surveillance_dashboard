# ==============================================================================
# ---- 1_setup.R ----
# Idempotent: safe to source multiple times, only runs once per session
# ==============================================================================
pacman::p_load(data.table, ggplot2, here, echarts4r, forecast)

data <- readRDS(here("datasets", "tidy_data.rds"))

.data_loaded <- TRUE

# ==============================================================================
# ---- Institutional palette ----
# ==============================================================================

# Sequential - for ordinal/continuous variables (severity, maps, gradients)
palette_blue <- c("#D6E8F5", "#8FC1E3", "#4C9FCB", "#1D6FA3", "#0B3C5D")

# Categorical - for nominal variables with no inherent order (gender, race, encounter class)
# Tone + luminosity variation within the same family, to keep it sober
palette_categorical <- c("#0B3C5D", "#1D6FA3", "#4C9FCB", "#8FC1E3", "#5B7B93", "#A9C4D6")

# Neutral - for "Unknown"/NA, kept outside the main palette so it doesn't compete visually
color_neutral <- "#B0B8BE"

# Clinical accent - EXCLUSIVE use for maximum severity / mortality. Do not reuse elsewhere.
color_alert <- "#C0392B"

# Severity-specific palette (levels 0 to 4), accent reserved for level 4 only
palette_severity <- c(
  "0" = "#D6E8F5",  # ambulatory
  "1" = "#8FC1E3",  # hospitalized, no respiratory support
  "2" = "#4C9FCB",  # oxygen therapy
  "3" = "#1D6FA3",  # mechanical ventilation
  "4" = color_alert  # deceased within 30-day window
)

dashboard_font <- "Manrope"  # adjust once the final CSS is defined

# ==============================================================================
# ---- Standardized chart functions ----
# ==============================================================================

# Shared theme: applies to any chart via e_common(), avoids repeating font/tooltip in every function
e_common(
  font_family = dashboard_font,
  theme = NULL  # transparent background, to integrate with dashboard cards (see mi_dashboard.css)
)

create_piechart <- function(data, group_var, title = NULL, palette = palette_categorical, donut = TRUE) {
  agg <- data[, .N, by = group_var]
  setnames(agg, group_var, "category")
  agg <- agg[order(-N)]
  
  agg |>
    e_charts(category) |>
    e_pie(
      N, 
      radius = if (donut) c("40%", "70%") else "70%",
      label = list(formatter = "{b}: {d}%", fontSize = 10)
    ) |>
    e_color(palette) |>
    e_tooltip(trigger = "item") |>
    e_legend(bottom = 0, textStyle = list(fontFamily = dashboard_font)) |>
    e_title(text = title, textStyle = list(fontFamily = dashboard_font, fontSize = 14))
}

create_barchart <- function(data, x_var, y_var = NULL, title = NULL, 
                            palette = palette_categorical, horizontal = FALSE, 
                            top_n = NULL, label_rotate = 45) {
  
  if (is.null(y_var)) {
    agg <- data[, .N, by = x_var]
    setnames(agg, c(x_var, "N"), c("category", "value"))
  } else {
    agg <- data[, .(value = sum(get(y_var), na.rm = TRUE)), by = x_var]
    setnames(agg, x_var, "category")
  }
  
  # Rank descending first — this is the "conceptual" order (largest = first)
  agg <- agg[order(-value)]
  if (!is.null(top_n)) agg <- head(agg, top_n)
  
  # echarts renders category axes bottom-to-top following array order.
  # For a flipped (horizontal) chart we want the largest value at the TOP,
  # so we reverse to ascending right before charting — vertical charts keep
  # left-to-right descending, which reads naturally without this correction.
  if (horizontal) agg <- agg[order(value)]
  
  bar_colors <- rep(palette, length.out = nrow(agg))
  agg[, color := bar_colors]
  
  chart <- agg |>
    e_charts(category) |>
    e_bar(value, legend = FALSE) |>
    e_add_nested("itemStyle", color) |>
    e_grid(containLabel = TRUE) |>  # auto-reserves margin so labels never get clipped
    e_tooltip(trigger = "axis") |>
    e_title(text = title, textStyle = list(fontFamily = dashboard_font, fontSize = 14))
  
  if (horizontal) {
    chart <- chart |> e_flip_coords()
  } else {
    chart <- chart |> e_x_axis(axisLabel = list(rotate = label_rotate, interval = 0))
  }
  
  chart
}
create_stacked_severity_barchart <- function(data, group_var, title = NULL) {
  # Stacked bars by severity level — meant to compare severity distribution across groups (e.g. age, region)
  agg <- data[, .N, by = .(group = get(group_var), severity_level)]
  agg[, severity_level := as.character(severity_level)]
  
  agg |>
    group_by(severity_level) |>
    e_charts(group) |>
    e_bar(N, stack = "severity") |>
    e_color(unname(palette_severity)) |>
    e_tooltip(trigger = "axis") |>
    e_legend(bottom = 0, textStyle = list(fontFamily = dashboard_font)) |>
    e_title(text = title, textStyle = list(fontFamily = dashboard_font, fontSize = 14))
}

pct <- function(dt, variable, category, decimals = 2){
  variable <- dt[[variable]]
  dt[, round(sum(variable == category) * 100 / .N, decimals)]
}

create_population_pyramid <- function(data, title = NULL, bin_width = 5, 
                                      colors = c("M" = "#0B3C5D", "F" = "#4C9FCB")) {
  
  max_age <- ceiling(max(data$age_at_dx, na.rm = TRUE) / bin_width) * bin_width
  breaks <- seq(0, max_age, by = bin_width)
  labels <- paste0(breaks[-length(breaks)], "-", breaks[-1] - 1)
  
  agg <- copy(data)
  agg[, age_group := cut(age_at_dx, breaks = breaks, labels = labels, right = FALSE, include.lowest = TRUE)]
  agg <- agg[!is.na(age_group), .N, by = .(age_group, gender)]
  
  agg_wide <- dcast(agg, age_group ~ gender, value.var = "N", fill = 0)
  for (g in names(colors)) {
    if (!g %in% names(agg_wide)) agg_wide[, (g) := 0]
  }
  
  agg_wide[, age_group := factor(age_group, levels = labels)]
  setorder(agg_wide, age_group)
  
  # Males negated so they render to the left; females stay positive, on the right
  agg_wide[, M_plot := -M]
  
  agg_wide |>
    e_charts(age_group) |>
    e_bar(M_plot, name = "Male", stack = "pyramid") |>
    e_bar(F, name = "Female", stack = "pyramid") |>
    e_color(c(colors[["M"]], colors[["F"]])) |>
    e_flip_coords() |>
    e_x_axis(
      axisLabel = list(formatter = htmlwidgets::JS("function(value) { return Math.abs(value); }"))
    ) |>
    e_tooltip(
      trigger = "axis",
      formatter = htmlwidgets::JS("
        function(params) {
          let s = params[0].name + '<br/>';
          params.forEach(p => { s += p.marker + p.seriesName + ': ' + Math.abs(p.value[0] ?? p.value) + '<br/>'; });
          return s;
        }
      ")
    ) |>
    e_legend(bottom = 0, textStyle = list(fontFamily = dashboard_font)) |>
    e_title(text = title, textStyle = list(fontFamily = dashboard_font, fontSize = 14))
}
# ==============================================================================
# ---- MA county population (2019 estimates, Donahue Institute / US Census) ----
# ==============================================================================

county_population <- data.table(
  county = c("Barnstable County", "Berkshire County", "Bristol County", "Dukes County",
             "Essex County", "Franklin County", "Hampden County", "Hampshire County",
             "Middlesex County", "Nantucket County", "Norfolk County", "Plymouth County",
             "Suffolk County", "Worcester County"),
  population = c(212990, 124944, 565217, 17332, 789034, 70180, 466372, 160830,
                 1611699, 11399, 706775, 521202, 803907, 830622)
)


# ==============================================================================
# ---- Table function (kableExtra, static — no DT) ----
# ==============================================================================
create_ranking_table <- function(data, cols, col_names = NULL, highlight_col = NULL, 
                                 palette = palette_blue, digits = 1, title = NULL) {
  
  tbl <- copy(data)[, ..cols]
  if (!is.null(col_names)) setnames(tbl, cols, col_names)
  
  numeric_cols <- names(tbl)[sapply(tbl, is.numeric)]
  tbl[, (numeric_cols) := lapply(.SD, round, digits), .SDcols = numeric_cols]
  
  kbl <- knitr::kable(tbl, format = "html", align = "l", caption = title)
  
  kbl <- kableExtra::kable_styling(
    kbl,
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 14,
    html_font = dashboard_font
  )
  
  kbl <- kableExtra::row_spec(kbl, 0, background = "#0B3C5D", color = "#D6E8F5", bold = TRUE)
  
  # Optional: gradient background on one column, using the same sequential blue scale as the map
  if (!is.null(highlight_col) && highlight_col %in% names(tbl)) {
    col_index <- which(names(tbl) == highlight_col)
    values <- tbl[[highlight_col]]
    
    color_fn <- scales::col_numeric(palette = palette, domain = range(values, na.rm = TRUE))
    bg <- color_fn(values)
    
    # Auto contrast: switch text to white once the background gets dark enough
    text_color <- ifelse(values > mean(range(values, na.rm = TRUE)), "#ffffff", "#0B3C5D")
    
    kbl <- kableExtra::column_spec(kbl, col_index, background = bg, color = text_color, bold = TRUE)
  }
  
  kbl
}
.setup_done <- TRUE

# ==============================================================================
# ---- Epidemic curve + backtest forecast ----
# ==============================================================================

build_epidemic_curve <- function(data, date_var = "covid_date") {
  daily <- data[, .N, by = date_var]
  setnames(daily, date_var, "date")
  full_range <- data.table(date = seq(min(daily$date), max(daily$date), by = "day"))
  daily <- merge(full_range, daily, by = "date", all.x = TRUE)
  daily[is.na(N), N := 0]
  daily[order(date)]
}

# cutoff_date lets you train on a partial window and compare against known future values
forecast_epidemic_curve <- function(daily, cutoff_date, horizon = 14) {
  train <- daily[date <= cutoff_date]
  actual_future <- daily[date > cutoff_date][seq_len(min(horizon, .N))]
  
  ts_data <- ts(train$N, frequency = 7)
  fit <- auto.arima(ts_data, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
  fc <- forecast(fit, h = horizon)
  
  future_dates <- seq(cutoff_date + 1, by = "day", length.out = horizon)
  
  list(
    model = fit,
    historical = train,
    actual_future = actual_future,  # what really happened — the point of the exercise
    forecast = data.table(
      date = future_dates,
      point = as.numeric(fc$mean),
      lower95 = as.numeric(fc$lower[, 2]),
      upper95 = as.numeric(fc$upper[, 2])
    )
  )
}

create_forecast_chart <- function(fc_result, title = NULL) {
  hist   <- fc_result$historical[, .(date, observed = N)]
  actual <- fc_result$actual_future[, .(date, actual = N)]
  fut    <- fc_result$forecast[, .(date, forecast = point, lower95, upper95)]
  
  bridge_date  <- hist[.N, date]
  bridge_value <- hist[.N, observed]
  fut <- rbind(
    data.table(date = bridge_date, forecast = bridge_value, lower95 = bridge_value, upper95 = bridge_value),
    fut
  )
  
  plot_data <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), list(hist, actual, fut))
  setorder(plot_data, date)
  plot_data[, band_width := upper95 - lower95]
  
  plot_data |>
    e_charts(date) |>
    e_line(observed, name = "Observed (training)", color = palette_blue[5], symbol = "none") |>
    e_line(actual, name = "Actual (held out)", color = color_alert, symbol = "circle", symbolSize = 5) |>
    e_line(lower95, name = "lower_helper", symbol = "none", legend = FALSE, lineStyle = list(opacity = 0), stack = "ci") |>
    e_line(band_width, name = "95% CI", symbol = "none", color = palette_blue[1],
           lineStyle = list(opacity = 0), areaStyle = list(opacity = 0.25, color = palette_blue[1]), stack = "ci") |>
    e_line(forecast, name = "Forecast", lineStyle = list(type = "dashed"), color = palette_blue[4], symbol = "none") |>
    e_tooltip(trigger = "axis") |>
    e_legend(bottom = 0, textStyle = list(fontFamily = dashboard_font)) |>
    e_title(text = title, textStyle = list(fontFamily = dashboard_font, fontSize = 14))
}

# ==============================================================================
# ---- Descriptive time series: cases, hospitalizations, deaths ----
# ==============================================================================

build_temporal_series <- function(data) {
  cases <- data[, .N, by = .(date = covid_date)]
  setnames(cases, "N", "cases")
  
  hosp <- data[is_hospitalized == 1 & !is.na(admission_date), 
               .N, by = .(date = as.Date(admission_date))]
  setnames(hosp, "N", "hospitalizations")
  
  deaths <- data[!is.na(deathdate), .N, by = .(date = deathdate)]
  setnames(deaths, "N", "deaths")
  
  # Union of date ranges — deaths can trail up to ~68 days past the last diagnosis,
  # so anchoring the range only to case dates would silently cut off the death tail
  all_dates <- c(cases$date, hosp$date, deaths$date)
  full_range <- data.table(date = seq(min(all_dates), max(all_dates), by = "day"))
  
  merged <- Reduce(function(x, y) merge(x, y, by = "date", all.x = TRUE),
                   list(full_range, cases, hosp, deaths))
  
  cols <- c("cases", "hospitalizations", "deaths")
  merged[, (cols) := lapply(.SD, function(x) fifelse(is.na(x), 0L, x)), .SDcols = cols]
  
  setorder(merged, date)
  merged
}

create_temporal_chart <- function(daily, title = NULL) {
  daily |>
    e_charts(date) |>
    e_line(cases, name = "Cases", color = palette_blue[3], symbol = "none") |>
    e_line(hospitalizations, name = "Hospitalizations", color = palette_blue[5], symbol = "none") |>
    e_line(deaths, name = "Deaths", color = color_alert, symbol = "none", y_index = 1) |>
    e_y_axis(index = 0, name = "Cases / Hospitalizations") |>
    e_y_axis(index = 1, name = "Deaths", position = "right") |>
    e_tooltip(trigger = "axis") |>
    e_legend(bottom = 0, textStyle = list(fontFamily = dashboard_font)) |>
    e_title(text = title, textStyle = list(fontFamily = dashboard_font, fontSize = 14))
}

create_lethality_barchart <- function(data, min_n = 20, title = "Average lethality by county") {
  agg <- data[, .(lethality = round(mean(deceased_post_covid, na.rm = TRUE), 2), n = .N), by = county]
  
  agg[, reliable := n >= min_n]
  agg <- agg[order(-lethality)]
  
  # Suppressed counties get a flat neutral color instead of the ranked palette,
  # so unreliable estimates don't visually compete with real signal
  agg[, color := fifelse(reliable, palette_blue[3], color_neutral)]
  agg[, label := fifelse(reliable, as.character(lethality), paste0(lethality, " (n=", n, ")"))]
  
  agg |>
    e_charts(county) |>
    e_bar(lethality, legend = FALSE) |>
    e_add_nested("itemStyle", color) |>
    e_grid(containLabel = TRUE) |>
    e_tooltip(
      trigger = "axis",
      formatter = htmlwidgets::JS("
        function(params) {
          return params[0].name + ': ' + params[0].value[1];
        }
      ")
    ) |>
    e_x_axis(axisLabel = list(rotate = 45, interval = 0)) |>
    e_title(text = title, subtext = "Grey bars: county n < 20, estimate unreliable",
            textStyle = list(fontFamily = dashboard_font, fontSize = 14))
}

.setup_done <- TRUE