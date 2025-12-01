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

evictions <- st_read(here("data/tract_proprietary_valid_2000_2018_y2024m12.csv"))

phl_evictions <- evictions%>%
  filter(county=="Philadelphia County")

#map threats, filings, and judgements in 2016 (most recent year in dataset)
tract_shapes <- tracts(
  state  = "PA",       # or your state FIPS
  county = "Philadelphia",  # name OR county FIPS code
  year   = 2016,
  cb     = TRUE
)

phl_evictions_sf <- tract_shapes %>%
  inner_join(phl_evictions, by = c("GEOID" = "fips"))

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

filings_map | judgements_map













