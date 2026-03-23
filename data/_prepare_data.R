# Data Sources:
# 1. FEMA Disaster Declarations Summaries: https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2
# 2. Zillow Home Value Index (ZHVI) - County, All Homes, Smoothed, Seasonally Adjusted: https://www.zillow.com/research/data/
# 3. US Counties Shapefile: https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html

# Setup
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(sf)
library(tigris)
library(rmapshaper)

# Load raw data
fema_raw <- read_csv("DisasterDeclarationsSummaries_raw.csv")
zillow_raw <- read_csv("County_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month_raw.csv")
us_counties_shapefile <- readRDS("us_counties_2020.rds")

# Process FEMA data
fema_clean <- fema_raw %>%
  filter(incidentType == "Hurricane", declarationType == "DR") %>%
  mutate(
    fipsStateCode = str_pad(fipsStateCode, width = 2, pad = "0"),
    fipsCountyCode = str_pad(fipsCountyCode, width = 3, pad = "0"),
    fips = paste0(fipsStateCode, fipsCountyCode),
    incident_date = as.Date(incidentBeginDate)
  ) %>%
  select(fips, incident_date, hurricane_name = declarationTitle)

fema_monthly <- fema_clean %>%
  mutate(incident_month = floor_date(incident_date, "month")) %>%
  group_by(fips, incident_month) %>%
  summarize(
    hurricane_names = str_c(unique(hurricane_name), collapse = "; "),
    n_hurricanes = n(),
    .groups = "drop"
  )

# Process Zillow data
zillow_clean <- zillow_raw %>%
  pivot_longer(
    cols = matches("^19|^20"),
    names_to = "date",
    values_to = "zhvi_value"
  ) %>%
  drop_na(zhvi_value) %>%
  mutate(
    StateCodeFIPS = str_pad(StateCodeFIPS, width = 2, pad = "0"),
    MunicipalCodeFIPS = str_pad(MunicipalCodeFIPS, width = 3, pad = "0"),
    fips = paste0(StateCodeFIPS, MunicipalCodeFIPS),
    date = ymd(date)
  ) %>%
  select(fips, RegionName, StateName, date, zhvi_value)

# Combine Zillow and FEMA data
data_hurricane_housing <- zillow_clean %>%
  mutate(date = floor_date(date, "month")) %>%
  left_join(fema_monthly, by = c("fips" = "fips", "date" = "incident_month")) %>%
  mutate(
    hurricane_flag = if_else(!is.na(hurricane_names), TRUE, FALSE)
  ) %>%
  select(fips, date, RegionName, StateName, zhvi_value, hurricane_flag, n_hurricanes, hurricane_names)


# Prepare data for map: counties with hurricanes
counties_with_hurricanes <- data_hurricane_housing %>%
  filter(hurricane_flag) %>%
  distinct(fips, RegionName, StateName) %>%
  mutate(fips = as.character(fips))

# Merge with shapefile

continental_state_fips <- c(
  "01", "04", "05", "06", "08", "09", "10", "12", "13", "16", "17", "18", "19", "20",
  "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34",
  "35", "36", "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48", "49",
  "50", "51", "53", "54", "55", "56"
)

us_counties_simplified <- us_counties_shapefile %>%
  filter(substr(GEOID, 1, 2) %in% continental_state_fips) %>%
  select(GEOID) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  ms_simplify(keep = 0.001, keep_shapes = TRUE)


# Merge with hurricane data
us_counties_processed <- us_counties_simplified %>%
  mutate(fips = GEOID) %>%
  left_join(counties_with_hurricanes, by = "fips")

# Save the processed data objects to an RDS file
saveRDS(
  list(
    data = data_hurricane_housing,
    us_counties = us_counties_processed
  ),
  file = "preprocessed_data.rds",
  compress = "gzip"
)

print("Data preprocessing complete.")
