library(sf)
library(tidyverse)
library(ggplot2)
library(tigris)
library(tidycensus)

data <- st_read("https://phl.carto.com/api/v2/sql?filename=opa_properties_public&format=geojson&skipfields=cartodb_id&q=SELECT+*+FROM+opa_properties_public")
          