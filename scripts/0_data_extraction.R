# ==============================================================================
# ---- 0_data_extraction.R ----
# ==============================================================================

# ---- Getting libraries and data ----

pacman::p_load(data.table, DBI, RPostgres, stringr, here)

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
obese_pre_covid AS (
    SELECT DISTINCT c.patient_id
    FROM conditions c
    INNER JOIN covid_cohort cc ON c.patient_id = cc.patient_id
    WHERE c.condition_code IN (162864005, 408512008)
      AND c.start_date < cc.covid_date
),
hta_pre_covid AS (
    SELECT DISTINCT c.patient_id
    FROM conditions c
    INNER JOIN covid_cohort cc ON c.patient_id = cc.patient_id
    WHERE c.condition_code = 59621000
      AND c.start_date < cc.covid_date
),
diabetes_pre_covid AS (
    SELECT DISTINCT c.patient_id
    FROM conditions c
    INNER JOIN covid_cohort cc ON c.patient_id = cc.patient_id
    WHERE c.condition_code IN (44054006) 
      AND c.start_date < cc.covid_date
),
ckd_pre_covid AS (
    SELECT DISTINCT c.patient_id
    FROM conditions c
    INNER JOIN covid_cohort cc ON c.patient_id = cc.patient_id
    WHERE c.condition_code IN (431855005, 431856006, 433144002)
      AND c.start_date < cc.covid_date
),
chd_pre_covid AS (
    SELECT DISTINCT c.patient_id
    FROM conditions c
    INNER JOIN covid_cohort cc ON c.patient_id = cc.patient_id
    WHERE c.condition_code = 53741008
      AND c.start_date < cc.covid_date
),
afib_pre_covid AS (
    SELECT DISTINCT c.patient_id
    FROM conditions c
    INNER JOIN covid_cohort cc ON c.patient_id = cc.patient_id
    WHERE c.condition_code = 49436004
      AND c.start_date < cc.covid_date
),
chronic_resp_pre_covid AS (
    SELECT DISTINCT c.patient_id
    FROM conditions c
    INNER JOIN covid_cohort cc ON c.patient_id = cc.patient_id
    WHERE c.condition_code IN (87433001, 185086009)  -- emphysema, COPD
      AND c.start_date < cc.covid_date
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
CASE WHEN obese_pre_covid.patient_id IS NOT NULL THEN 1 ELSE 0 END AS obese_pre_covid,
CASE WHEN hta_pre_covid.patient_id IS NOT NULL THEN 1 ELSE 0 END AS hta_pre_covid,
CASE WHEN diabetes_pre_covid.patient_id IS NOT NULL THEN 1 ELSE 0 END AS diabetes_pre_covid,
CASE WHEN ckd_pre_covid.patient_id IS NOT NULL THEN 1 ELSE 0 END AS ckd_pre_covid,
CASE WHEN chd_pre_covid.patient_id IS NOT NULL THEN 1 ELSE 0 END AS chd_pre_covid,
CASE WHEN afib_pre_covid.patient_id IS NOT NULL THEN 1 ELSE 0 END AS afib_pre_covid,
CASE WHEN chronic_resp_pre_covid.patient_id IS NOT NULL THEN 1 ELSE 0 END AS chronic_resp_pre_covid,
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
LEFT JOIN mechanical_ventilation v ON p.id = v.patient_id
LEFT JOIN obese_pre_covid ON p.id = obese_pre_covid.patient_id
LEFT JOIN hta_pre_covid ON p.id = hta_pre_covid.patient_id
LEFT JOIN diabetes_pre_covid ON p.id = diabetes_pre_covid.patient_id
LEFT JOIN ckd_pre_covid ON p.id = ckd_pre_covid.patient_id
LEFT JOIN chd_pre_covid ON p.id = chd_pre_covid.patient_id
LEFT JOIN afib_pre_covid ON p.id = afib_pre_covid.patient_id
LEFT JOIN chronic_resp_pre_covid  ON p.id = chronic_resp_pre_covid.patient_id;
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

tidy_data[, severity_level_broad := factor(
  fcase(severity_level == 0, "Outpatient",
        severity_level %in% 1:2, "Hospitalized without respiratory support",
        severity_level %in% 3:4, "Respiratory support / deceased"),
  levels = c("Outpatient", "Hospitalized without respiratory support", "Respiratory support / deceased"))]

morbidities <- names(data)[names(data) %ilike% "pre_"]
tidy_data[, comorbidities := Reduce(f = `+`, x = .SD), .SDcols = morbidities]
tidy_data[, comorbidity_count_grouped := fifelse(comorbidities >= 4, "4+", as.character(comorbidities))]
tidy_data[, comorbidity_count_grouped := factor(comorbidity_count_grouped, levels = c("0","1","2","3","4+"))]


saveRDS(tidy_data, here("datasets", "tidy_data.rds"))

