# ==============================================================================
# ---- 4_demographics.R ----
# Assumes 1_setup.R and 0_load_data.R already ran. 
# Standalone fallback below only triggers if opened in isolation.
# ==============================================================================

if (!exists(".setup_done")) source(here::here("scripts", "1_setup.R"))
if (!exists(".data_loaded")) source(here::here("scripts", "1_setup.R"))

pacman::p_load(sf, leaflet, tigris, data.table)
options(tigris_use_cache = TRUE)
# ---- 1. County boundaries ----
ma_counties <- counties(state = "MA", cb = TRUE, year = 2022) |> st_as_sf()

# tigris' NAME field comes without "County" suffix (e.g. "Middlesex") — align keys before merging
ma_counties$county <- paste(ma_counties$NAME, "County")

# ---- 2. Case counts by county (patient residence) ----
cases_by_county <- data[, .(n_cases = .N), by = county]

# ---- 3. Merge counts + population, compute rate ----
county_summary <- merge(cases_by_county, county_population, by = "county", all.y = TRUE)
county_summary[is.na(n_cases), n_cases := 0]
county_summary[, rate_100k := round(n_cases / population * 100000, 1)]

map_counties <- merge(ma_counties, county_summary, by = "county", all.x = TRUE)

# ---- 4. Organization/center summary (diagnosis site, as proxy for network activity) ----
org_summary <- data[!is.na(dx_org_name), .(
  n_patients = .N,
  n_hospitalized = sum(is_hospitalized, na.rm = TRUE),
  n_severe = sum(severity_level >= 2, na.rm = TRUE),
  lat = first(dx_org_lat),
  lon = first(dx_org_lon)
), by = dx_org_name]

# ---- 5. Palettes ----
pal_rate <- colorNumeric(palette = palette_blue, domain = map_counties$rate_100k, na.color = color_neutral)
pal_org  <- colorNumeric(palette = c(palette_blue[2], color_alert), domain = org_summary$n_severe / org_summary$n_patients)

# ---- 6. Labels ----
county_labels <- sprintf(
  "<strong>%s</strong><br/>Cases: %d<br/>Rate /100k: %.1f",
  map_counties$county, map_counties$n_cases, map_counties$rate_100k
) |> lapply(htmltools::HTML)

org_labels <- sprintf(
  "<strong>%s</strong><br/>Patients diagnosed: %d<br/>Hospitalized: %d<br/>Severe/critical: %d",
  org_summary$dx_org_name, org_summary$n_patients, org_summary$n_hospitalized, org_summary$n_severe
) |> lapply(htmltools::HTML)

# ---- 7. Map ----
map <- leaflet(map_counties) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addMapPane("orgPane", zIndex = 650) |>  # markers pane sits above the default overlay pane (~400-500)
  addPolygons(
    fillColor = ~pal_rate(rate_100k),
    weight = 1.2,
    color = color_neutral,
    fillOpacity = 0.75,
    highlightOptions = highlightOptions(weight = 2.5, color = "#0B3C5D", bringToFront = TRUE),
    label = county_labels,
    labelOptions = labelOptions(style = list("font-family" = dashboard_font))
  ) |>
  addCircleMarkers(
    data = org_summary,
    lng = ~lon, lat = ~lat,
    radius = ~scales::rescale(n_patients, to = c(4, 16)),
    fillColor = ~pal_org(n_severe / n_patients),
    color = "#ffffff",
    weight = 1,
    fillOpacity = 0.9,
    options = pathOptions(pane = "orgPane"),  # <- forces markers to render above polygons, even during hover
    label = org_labels,
    labelOptions = labelOptions(style = list("font-family" = dashboard_font))
  ) |>
  addLegend(
    pal = pal_rate, values = ~rate_100k,
    title = "Cases / 100k", position = "bottomright", opacity = 0.8
  )

county_ranking <- county_summary[order(-rate_100k)]
county_ranking[, rank := .I]
setcolorder(county_ranking, c("rank", "county", "n_cases", "population", "rate_100k"))

county_table <- create_ranking_table(
  data = county_ranking,
  cols = c("rank", "county", "n_cases", "population", "rate_100k"),
  col_names = c("#", "County", "Cases", "Population", "Rate /100k"),
  highlight_col = "Rate /100k",
  title = "COVID-19 rates by county"
)


# ==============================================================================
#                   ---- Use of resources ---- 
# ==============================================================================
top_counties <- create_barchart(data = data[is_hospitalized == 1], 
                                x_var = "county", title = "Total hospitalizations by county", horizontal = T) 

county_average_days <- create_barchart(data = data[, 
                            .(stay = round(mean(length_of_stay, na.rm = T), 2)),
                            county
                            ],
                x_var = "county", y_var = "stay", title = "Average length of stay by county")

county_lethality <- create_lethality_barchart(data)

