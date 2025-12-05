# Load Packages
library(sf)
library(tidyverse)
library(tigris)
library(tidycensus)
library(scales)
library(patchwork)
library(here)
library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(stringr)
library(units)
library(stargazer)
library(car)
library(caret)
library(lmtest)

# Phase 1: Eviction Data
## Eviction Data - The Eviction Lab, Sourced from Princeton University
evictions <- st_read(here("data/tract_proprietary_valid_2000_2018_y2024m12.csv"))

phl_evictions <- evictions%>%
  filter(county == "Philadelphia County") %>%
  mutate(
    filings = as.numeric(filings),
    threatened = as.numeric(threatened),
    judgements = as.numeric(judgements)
  )

## Tract Shapes
tract_shapes <- tracts(
  state = "PA",
  county = "Philadelphia",
  year = 2016,
  cb = TRUE
)

## Join Tracts to Evictions
phl_evictions_sf <- tract_shapes %>%
  inner_join(phl_evictions, by = c("GEOID" = "fips"))

## Fillings Map
filings_map <- phl_evictions_sf%>%
  filter(year==2016)%>%
  mutate(
    filings = as.numeric(filings),   # <-- FIX HERE
    filings_bucket = cut(
      filings,
      breaks = c(0, 10, 25, 50, 100, Inf),
      labels = c("0–10", "11–25", "26–50", "51–100", "100+"),
      right = FALSE
    )
  ) %>%
  ggplot()+
  geom_sf(aes(fill=filings_bucket))+
  theme_void()

## Judgements Map
judgements_map <- phl_evictions_sf%>%
  filter(year==2016)%>%
  mutate(
    judgements = as.numeric(judgements),   # <-- FIX HERE
    judgements_bucket = cut(
      judgements,
      breaks = c(0, 10, 25, 50, 100, Inf),
      labels = c("0–10", "11–25", "26–50", "51–100", "100+"),
      right = FALSE
    )
  ) %>%
  ggplot()+
  geom_sf(aes(fill=judgements_bucket))+
  theme_void()

## Map Fillings and Judgements Side-by-Side
filings_map | judgements_map

# Phase 2: Property Data
## Property Data - Office of Property Assessment, Sourced from Dr. Elizabeth Delmelle
property_sales <- st_read("https://github.com/MUSA-5080-Fall-2025/MUSA-5080-Fall-2025/releases/download/v1.0.0/opa_properties_public.csv")

property_sales_filtered <- property_sales %>% 
  filter(sale_date >= as.Date("1900-01-01") & sale_date <= as.Date("2025-12-31") ) %>%
  filter(str_detect(category_code_description, "SINGLE FAMILY") | 
           str_detect(category_code_description, "MULTI FAMILY") |
           str_detect(category_code_description, "APARTMENTS > 4 UNITS") |
           str_detect(category_code_description, "MIXED USE"))

## Remove Unnecessary Columns to Make Easier to Read
property_sales_filtered <- property_sales_filtered %>%
  select(category_code_description, quality_grade, year_built, total_livable_area, shape)

## Convert to SF and Fix CRS
properties_sf <- property_sales_filtered %>%
  mutate(geometry = st_as_sfc(shape)) %>%
  st_as_sf()

st_crs(properties_sf) <- 2272
properties_sf <- st_transform(properties_sf, 4236)

# Phase 3: Census Data
variables <- c(
  total_population = "B01003_001",
  median_h_income = "B19013_001",
  families = "S1101_C01_002",
  white = "B02001_002",
  black = "B02001_003",
  total_pop = "B02001_001",
  rent_total = "B25070_001",
  rent_30_34 = "B25070_007",
  rent_35_39 = "B25070_008",
  rent_40_49 = "B25070_009",
  rent_50_plus = "B25070_010"
)

census_api_key("5a82e243438bea307ae1c04f150d539c4db5fa47", install = FALSE)
philadelphia <- get_acs(
  geography = "tract",
  county = "Philadelphia",
  state = "PA",
  variables = variables,
  survey = "acs5",
  year = 2023,
  output = "wide",
  geometry = TRUE
) %>%
  mutate (
    rent_burdened = rent_30_34E + rent_35_39E + rent_40_49E + rent_50_plusE,
    rent_burden_rate = rent_burdened / rent_totalE
  )

philadelphia <- st_transform(philadelphia, st_crs(properties_sf))

# Phase 4: Eviction Ratios
evictions_by_tract <- phl_evictions %>%
group_by(fips) %>%
  summarise(
    filings = sum(filings, na.rm = TRUE),
    threatened = sum(threatened, na.rm = TRUE),
    judgements = sum(judgements, na.rm = TRUE)
  ) %>%
  mutate(
    pct_judgements_filings = ifelse(filings > 0, judgements / filings, NA),
    pct_judgements_threats = ifelse(threatened > 0, judgements / threatened, NA)
  ) %>%
  rename(GEOID = fips) %>%
  select(GEOID, pct_judgements_filings, pct_judgements_threats)

philadelphia$GEOID <- as.character(philadelphia$GEOID)

tract_data <- philadelphia %>%
  left_join(st_drop_geometry(evictions_by_tract), by = "GEOID")

# Phase 5: Join Data to Property Data
properties_with_census <- properties_sf %>%
  st_join(tract_data %>%
    select(GEOID, pct_judgements_filings, pct_judgements_threats, rent_burden_rate, median_h_incomeE, blackE, total_popE, familiesE)
)

summary(properties_with_census$pct_judgements_filings)
sum(is.na(properties_with_census$pct_judgements_filings))

properties_with_tract <- st_join(properties_sf, tract_data %>% select(GEOID),
                                 join = st_within)

# PHASE 6: Predictive Models
## Filings > Judgements
structural_model <- lm(pct_judgements_filings ~ category_code_description + quality_grade + year_built + total_livable_area,
               data = properties_with_census)

census_model <- lm(pct_judgements_filings ~ category_code_description + quality_grade + year_built + total_livable_area +
                     rent_burden_rate + median_h_incomeE + familiesE + I(blackE/total_popE),
                   data = properties_with_census)

summary(structural_model)
summary(census_model)
