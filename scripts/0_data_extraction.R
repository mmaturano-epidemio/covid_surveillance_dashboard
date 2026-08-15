# ==============================================================================
# ---- 0_data_extraction.R ----
# ==============================================================================

# ---- Getting libraries and data ----

pacman::p_load(data.table, DBI, RPostgres, stringr)

con <- dbConnect(
  RPostgres::Postgres(),
  host     = Sys.getenv("SUPABASE_HOST"),
  dbname   = Sys.getenv("SUPABASE_DBNAME"),
  port     = as.integer(Sys.getenv("SUPABASE_PORT")), 
  user     = Sys.getenv("SUPABASE_USER"),
  password = Sys.getenv("SUPABASE_PASSWORD")
)

# Quick connection test
dbGetQuery(con, "SELECT version();")

# Check tables
cols <- dbGetQuery(con, "SELECT column_name, data_type, table_name
FROM information_schema.columns
WHERE table_schema = 'public'
ORDER BY table_name;")

query <- "WITH covid_cohort AS (
  SELECT patient_id, MIN(start_date) AS covid_date
  FROM conditions
  WHERE condition_code = 840539006
  GROUP BY patient_id
),
first_covid_encounter AS (
  SELECT DISTINCT ON (e.patient_id) 
  e.patient_id, 
  e.id AS encounter_id,
  e.organization_id AS dx_org_id,
  e.encounterclass AS dx_encounter_class
  FROM encounters e
  INNER JOIN covid_cohort cc ON e.patient_id = cc.patient_id
  WHERE e.start_time::date >= cc.covid_date
  ORDER BY e.patient_id, e.start_time ASC
),
inpatient_admission AS (
  SELECT DISTINCT ON (e.patient_id)
  e.patient_id,
  e.id AS inpatient_encounter_id,
  e.organization_id AS inpatient_org_id,
  e.start_time AS admission_date,
  e.stop_time AS discharge_date
  FROM encounters e
  INNER JOIN covid_cohort cc ON e.patient_id = cc.patient_id
  WHERE e.encounterclass = 'inpatient'
  AND e.start_time::date >= cc.covid_date
  ORDER BY e.patient_id, e.start_time ASC
),
oxygen_therapy AS (
  SELECT DISTINCT p.patient_id, MIN(p.date) AS oxygen_date
  FROM procedures p
  INNER JOIN procedure_catalog pc ON p.procedure_code = pc.code
  INNER JOIN covid_cohort cc ON p.patient_id = cc.patient_id
  WHERE pc.description IN ('Oxygen administration by mask (procedure)', 'Oxygen Therapy')
  AND p.date >= cc.covid_date
  GROUP BY p.patient_id
),
mechanical_ventilation AS (
  SELECT DISTINCT p.patient_id, MIN(p.date) AS ventilation_date
  FROM procedures p
  INNER JOIN procedure_catalog pc ON p.procedure_code = pc.code
  INNER JOIN covid_cohort cc ON p.patient_id = cc.patient_id
  WHERE pc.description = 'Controlled ventilation procedure and therapy  initiation and management (procedure)'
  AND p.date >= cc.covid_date
  GROUP BY p.patient_id
)
SELECT 
p.id AS patient_id,
p.gender,
p.birthdate,
p.deathdate,
r.race_name,
EXTRACT(YEAR FROM AGE(cc.covid_date, p.birthdate)) AS age_at_dx,
p.lat, 
p.lon, 
p.city,
p.county,
cc.covid_date,
pe.dx_encounter_class,
org_dx.name AS dx_org_name,
org_dx.city AS dx_org_city,
org_dx.state AS dx_org_state,
org_dx.lat AS dx_org_lat,
org_dx.lon AS dx_org_lon,
CASE WHEN i.patient_id IS NOT NULL THEN 1 ELSE 0 END AS is_hospitalized,
i.admission_date,
i.discharge_date,
org_int.name AS inpatient_org_name,
org_int.city AS inpatient_org_city,
org_int.state AS inpatient_org_state,
org_int.lat AS inpatient_org_lat,
org_int.lon AS inpatient_org_lon,
ox.oxygen_date,
v.ventilation_date,
DATE_PART('day', i.discharge_date - i.admission_date) AS length_of_stay,
DATE_PART('day', v.ventilation_date - i.admission_date) AS days_to_ventilation,
CASE WHEN p.deathdate IS NOT NULL AND p.deathdate >= cc.covid_date THEN 1 ELSE 0 END AS deceased_post_covid,
-- Ordinal severity level, for Sankey and value boxes
CASE 
WHEN p.deathdate IS NOT NULL AND p.deathdate >= cc.covid_date THEN 4  -- Deceased
WHEN v.patient_id IS NOT NULL THEN 3                                  -- Mechanical ventilation
WHEN ox.patient_id IS NOT NULL THEN 2                                 -- Oxygen therapy without ventilation
WHEN i.patient_id IS NOT NULL THEN 1                                  -- Hospitalized without respiratory support
ELSE 0                                                                -- Outpatient/ER without admission
END AS severity_level
FROM patients p
INNER JOIN covid_cohort cc ON p.id = cc.patient_id
LEFT JOIN race_lookup r ON p.race_id = r.id
LEFT JOIN first_covid_encounter pe ON p.id = pe.patient_id
LEFT JOIN organizations org_dx ON pe.dx_org_id = org_dx.id
LEFT JOIN inpatient_admission i ON p.id = i.patient_id
LEFT JOIN organizations org_int ON i.inpatient_org_id = org_int.id
LEFT JOIN oxygen_therapy ox ON p.id = ox.patient_id
LEFT JOIN mechanical_ventilation v ON p.id = v.patient_id;
"

raw_data <- dbGetQuery(con, query) 

saveRDS(raw_data, here("datasets", "raw_data.rds"))

dbDisconnect(con)

# ==============================================================================
# ---- Data preparation ----
# ==============================================================================

# raw_data <- readRDS(here("datasets", "raw_data.rds"))

tidy_data <- as.data.table(raw_data)

tidy_data |> str()

factor_cols <- c("gender", "race_name", "city", "county", "dx_encounter_class",
                 names(tidy_data)[names(tidy_data) %like% "_org_name|_org_city|_org_state"])

tidy_data[, (factor_cols) := lapply(.SD, as.factor),
         .SDcols = factor_cols]

levels(tidy_data$race_name) <- stringr::str_to_title(levels(data$race_name))

tidy_data <- tidy_data[, -c("inpatient_org_state", "dx_org_state"), with = F] # Only one state

saveRDS(tidy_data, here("datasets", "tidy_data.rds"))
