# Data Sources:
# 1. FEMA Disaster Declarations Summaries: https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2
# 2. Zillow Home Value Index (ZHVI) - County, All Homes, Smoothed, Seasonally Adjusted: https://www.zillow.com/research/data/

# Setup
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(sf)
library(maps)

# Load raw data
fema_raw <- read_csv("DisasterDeclarationsSummaries_raw.csv")
zillow_raw <- read_csv("County_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month_raw.csv")

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

# Find counties with hurricanes since 2000
counties_with_hurricanes_since_2000 <- fema_clean %>%
  filter(incident_date >= as.Date("2000-01-01")) %>%
  distinct(fips)

# Process Zillow data, filtering for relevant counties and dates
zillow_filtered <- zillow_raw %>%
  mutate(
    StateCodeFIPS = str_pad(StateCodeFIPS, width = 2, pad = "0"),
    MunicipalCodeFIPS = str_pad(MunicipalCodeFIPS, width = 3, pad = "0"),
    fips = paste0(StateCodeFIPS, MunicipalCodeFIPS)
  ) %>%
  filter(fips %in% counties_with_hurricanes_since_2000$fips) %>%
  pivot_longer(
    cols = matches("^19|^20"),
    names_to = "date",
    values_to = "zhvi_value"
  ) %>%
  drop_na(zhvi_value) %>%
  mutate(date = ymd(date)) %>%
  filter(date >= as.Date("2000-01-01")) %>%
  select(fips, RegionName, StateName, date, zhvi_value)

# Combine filtered Zillow and FEMA data
data_hurricane_housing <- zillow_filtered %>%
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

# Merge with map
  # map to sf
  county_map_sf <- st_as_sf(map("county", plot = FALSE, fill = TRUE))

  # bring FIPS matching table
  data(county.fips, package = "maps")

  county_fips_clean <- county.fips %>%
  # Convert FIPS to "#####" text format
  mutate(fips = str_pad(fips, width = 5, pad = "0")) %>%
  # remove duplicate polyname
  distinct(polyname, .keep_all = TRUE)

  # Combine sf and FIPS
  us_counties_simplified <- county_map_sf %>%
    left_join(county_fips_clean, by = c("ID" = "polyname")) %>%
    filter(!is.na(fips))

  # combine map data with hurricane data
  us_counties_processed <- us_counties_simplified %>%
    left_join(counties_with_hurricanes, by = "fips")

# Save the processed data objects to an RDS file
# Remove name columns from the main data object before saving to reduce duplication
data_for_saving <- data_hurricane_housing %>%
  select(-RegionName, -StateName)

saveRDS(
  list(
    data = data_for_saving,
    us_counties = us_counties_processed
  ),
  file = "preprocessed_data.rds"
)

print("Data preprocessing complete.")
