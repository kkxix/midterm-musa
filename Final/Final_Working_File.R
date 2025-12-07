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
evictions <- st_read("./data/tract_proprietary_valid_2000_2018_y2024m12.csv")

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
  year = 2016,
  output = "wide",
  geometry = TRUE
) %>%
  mutate (
    rent_burdened = rent_30_34E + rent_35_39E + rent_40_49E + rent_50_plusE,
    rent_burden_rate = rent_burdened / rent_totalE,
    black_percent = blackE / total_popE
  )

philadelphia <- st_transform(philadelphia, st_crs(properties_sf))

## Map Rent Burden Rate
rent_burden_map <- philadelphia %>%
  ggplot() +
  geom_sf(aes(fill = rent_burden_rate)) +
  scale_fill_viridis_c(option = "magma", labels = scales::percent) +
  theme_void() +
  labs(
    title = "Rent Burden by Tract",
    fill = "Rent Burdened Households",
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(10,20,10,10))

rent_burden_map

# Phase 4: Eviction Ratios
evictions_by_tract <- phl_evictions %>%
#group_by(fips) %>%
#  summarise(
#    filings = sum(filings, na.rm = TRUE),
#    threatened = sum(threatened, na.rm = TRUE),
#    judgements = sum(judgements, na.rm = TRUE)
#  ) %>%
  mutate(
    pct_judgements_filings = ifelse(filings > 0, judgements / filings, NA),
    pct_judgements_threats = ifelse(threatened > 0, judgements / threatened, NA)
  ) %>%
  rename(GEOID = fips) 
#%>%
#  select(GEOID, pct_judgements_filings, pct_judgements_threats)

philadelphia$GEOID <- as.character(philadelphia$GEOID)

tract_data <- philadelphia %>%
  left_join(st_drop_geometry(evictions_by_tract), by = "GEOID")

## Map Judgement and Filing Ratios
filings_ratio_map <- tract_data %>%
  filter(!is.na(pct_judgements_filings)) %>%
  filter(year=="2016")%>%
  ggplot() +
  geom_sf(aes(fill = pct_judgements_filings)) +
  scale_fill_viridis_c(option = "plasma", labels = scales::percent) +
  theme_void() +
  labs(
    title = "Eviction Judgement Ratio by Tract",
    subtitle = "Philadelphia (2016), The Eviction Lab and ACS",
    fill = "Judgements / Filings"
    ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = margin(10,20,10,10))

threats_ratio_map <- tract_data %>%
  filter(!is.na(pct_judgements_threats)) %>%
  filter(year=="2016")%>%
  ggplot() +
  geom_sf(aes(fill = pct_judgements_threats)) +
  scale_fill_viridis_c(option = "plasma", labels = scales::percent) +
  theme_void() +
  labs(
    title = "Eviction Threat Ratio by Tract",
    subtitle = "Philadelphia (2016), The Eviction Lab and ACS",
    fill = "Judgements / Threats"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = margin(10,20,10,10))

filings_ratio_map
threats_ratio_map

# Phase 5: Join Data to Property Data
properties_with_census <- properties_sf %>%
  st_join(tract_data %>%
    select(GEOID, 
           filings,
           threatened,
           judgements,
           year,
           pct_judgements_filings, 
           pct_judgements_threats, 
           rent_burden_rate, 
           median_h_incomeE, 
           blackE, 
           total_popE, 
           familiesE, 
           black_percent)
)

properties_with_tract <- st_join(properties_sf, tract_data %>% select(GEOID),
                                 join = st_within)

# PHASE 6: Predictive Models
## Filings > Judgements
properties_clean <- properties_with_census %>%
  st_drop_geometry() %>%
  mutate(
    total_livable_area = as.numeric(total_livable_area),
    year_built = as.numeric(year_built),
    rent_burden_rate = as.numeric(rent_burden_rate),
    median_h_incomeE = as.numeric(median_h_incomeE),
    familiesE = as.numeric(familiesE),
    black_percent = as.numeric(black_percent)
  )

model_data <- properties_clean %>%
  group_by(GEOID, year) %>%
  summarise(
    filings = first(filings),
    judgements = first(judgements),
    threatened = first(threatened),
    pct_judgements_filings = first(pct_judgements_filings),
    total_livable_area = median(total_livable_area, na.rm = TRUE),
    median_year_built = median(year_built, na.rm = TRUE),
    quality_grade = names(sort(table(quality_grade), decreasing = TRUE))[1],
    category_code_description = names(sort(table(category_code_description), decreasing = TRUE))[1],
    rent_burden_rate = first(rent_burden_rate),
    median_h_incomeE = first(median_h_incomeE),
    familiesE = first(familiesE),
    black_percent = first(black_percent),
    total_pop = first(total_popE)
  )

structural_model <- lm(pct_judgements_filings ~ category_code_description + quality_grade + median_year_built + total_livable_area + factor(year),
               data = model_data)

census_model <- lm(pct_judgements_filings ~ category_code_description + quality_grade + median_year_built + total_livable_area + factor(year) +
                     rent_burden_rate + median_h_incomeE + familiesE + black_percent,
                   data = model_data)
  
binomial_group_model <- glm(cbind(judgements, filings - judgements) ~ category_code_description + quality_grade + median_year_built + total_livable_area + factor(year) +
                              rent_burden_rate + median_h_incomeE + familiesE + black_percent + total_pop,
                         family = binomial,
                         data = model_data)

summary(structural_model)
summary(census_model)
summary(binomial_group_model)

stargazer(structural_model, census_model, type = "text",
          star.cutoffs = c(0.05, 0.01, 0.001))

stargazer(
  structural_model, census_model,
  title = "Structural and Census Model Results",
  type = "text",
  dep.var.labels = "Judgements/Filings",
  column.labels = c("Structural", "Census"),
  keep = c("category_code_description", "quality_grade", "total_livable_area", "year_built", "rent_burden_rate", "median_h_incomeE", "familiesE", "black_percent"),
  add.lines = list(
    c("Model Type", "Linear", "Linear")
  ),
  no.space = TRUE,
  digits = 3,
  omit.stat = c("f", "ser")
)

# Stargazer Findings
# Larger properties are less likely to have eviction judgments per filing
# Family density and race correlate with eviction
# Property characteristics like grade and type are not predictive

poission_model <- glm(log(pct_judgements_filings + 1) ~ category_code_description + quality_grade + median_year_built + total_livable_area + factor(year) +
                       rent_burden_rate + median_h_incomeE + familiesE + black_percent,
                     data = model_data)

summary(poission_model)

overdispersion_ratio <- sum(residuals(poission_model, type = "pearson")^2) / poission_model$df.residual
overdispersion_ratio

# Poission Findings
# 16% of variation is explained; 
# Family density and race correlate are strong predictors
# Type of housing, housing quality, rent burden rate, and income are not statistically significant

#^^ Not sure poisson makes a lot of sense here 


#CROSS VALIDATION####

train_data <- model_data%>%filter(year != "2016")
test_data <- model_data%>%filter(year == "2016")

#only took out quality grade as it was only variable not significant 
binomial_cv_model <- glm(cbind(judgements, filings - judgements) ~ category_code_description + median_year_built + total_livable_area +
                           rent_burden_rate + median_h_incomeE + familiesE + black_percent +total_pop,
                         family = binomial,
                         data = train_data)

test_data$predicted_prob_judgement <- predict(
  binomial_cv_model,
  newdata = test_data,
  type = "response"
)

test_data$predicted_judgements <- test_data$filings * test_data$predicted_prob_judgement
test_data$absolute_error <- abs(test_data$predicted_judgements - test_data$judgements)
mae = mean(test_data$absolute_error,  na.rm = TRUE)

mae

test_data_sf <- test_data %>%
  left_join(
    select(philadelphia, GEOID, geometry),  # keep only GEOID and geometry
    by = "GEOID"
  ) %>%
  st_as_sf()

absolute_error_map <- test_data_sf %>%
  filter(!is.na(absolute_error)) %>%
  ggplot() +
  geom_sf(aes(fill = absolute_error)) +
  scale_fill_viridis_c(option = "plasma") +
  theme_void() +
  labs(
    title = "Absolute Error by Census Tract",
    subtitle = "Philadelphia (2016), The Eviction Lab and ACS",
    fill = "Absolute Error"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = margin(10,20,10,10))

absolute_error_map

#remove two extreme outliers to better visually see distributiuon of error
absolute_error_map.2 <- test_data_sf %>%
  filter(!is.na(absolute_error)) %>%
  filter(absolute_error<100)%>%
  ggplot() +
  geom_sf(aes(fill = absolute_error)) +
  scale_fill_viridis_c(option = "plasma") +
  theme_void() +
  labs(
    title = "Absolute Error by Census Tract",
    subtitle = "Philadelphia (2016), The Eviction Lab and ACS",
    fill = "Absolute Error"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = margin(10,20,10,10))

absolute_error_map.2

#what are the characteristics of the tracts that are worst predicted?
test_data$absolute_error <- unname(test_data$absolute_error)

test_data %>%
  ungroup() %>%  # remove grouping
  slice_max(order_by = absolute_error, n = 10, na_rm = TRUE)%>%
  kable(
    caption = "Top 10 Tracts by Absolute Prediction Error",
    digits = 3,                       # format numeric columns
    align = "c"
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )

#from this, we can see that almost all the the tracts with high error are majority black,
#the top five also have higher than median census tract population, so this is not likely
#a result of low population areas. Rather, we see remarkably low actual percent judgement
#filings in the tracts with high error -- they are all in the upper quantile of filings, 
#so much higher than normal amount of filings, and mostly in the upper quantile or upper half
#of judgements. This could be a function of limitations on the number of court judgements
#that can be made a year. Across the four years, the judgement:filing ratio  
#hovers around 50%

#looking into how many filings top filers make (although from wrong year, can't find 2016)
hotspots_2024<-read_csv("./data/philadelphia_hotspots_media_report.csv")
filers<- hotspots_2024%>%
  group_by(xplaintiff)%>%
  summarise(count=sum(filings))

#save to rda file
save(philadelphia, file = "./data/philadelphia_census.rda")
