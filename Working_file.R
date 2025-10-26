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


#PHASE 1: DATA PREPARATION

# Property / Sales Data
property_sales <- st_read("https://github.com/MUSA-5080-Fall-2025/MUSA-5080-Fall-2025/releases/download/v1.0.0/opa_properties_public.csv")

#Anything in property_sales$zoning starting with R or IRMX. 
property_sales_res <- property_sales %>% 
  filter(str_detect(zoning, "^R") | str_detect(zoning, "IRMX")) %>%
  filter(str_detect(category_code_description, "SINGLE FAMILY") | 
           str_detect(category_code_description, "MULTI FAMILY") |
           str_detect(category_code_description, "MIXED USE")) %>%
  mutate(sale_price_n = as.numeric(sale_price)) %>% 
  filter(sale_price_n < 10000000) %>% 
  filter(sale_date >= as.Date("2023-01-01") & sale_date <= as.Date("2024-12-31") )

property_sales_res <- property_sales_res %>%
  filter(sale_price_n<10000000 & sale_price_n > 5000)

ggplot(property_sales_res, aes(x = sale_date, y = sale_price_n)) +
  geom_point()

ggplot(property_sales_res, aes(x = number_of_bedrooms, y = sale_price_n)) +
  geom_point()

#Converting blank cells to NA
colSums(property_sales_res == "")


property_sales_res <- as.data.frame(lapply(property_sales_res, function(x) {
  x <- trimws(x)  # remove leading/trailing spaces
  x[x == ""] <- NA
  return(x)
}))

colSums(is.na(property_sales_res))

# Load Census Data
census_api_key("5a82e243438bea307ae1c04f150d539c4db5fa47", install = FALSE)
philadelphia <- get_acs(
  geography = "tract",
  county = "Philadelphia",
  state = "PA",
  variables = c(median_h_income = "B19013_001", total_population = "B01003_001"),
  survey = "acs5",
  year = 2023,
  output = "wide",
  geometry = TRUE
)

# Load Spatial Amenities Data
park_properties <- st_read("https://hub.arcgis.com/api/v3/datasets/d52445160ab14380a673e5849203eb64_0/downloads/data?format=geojson&spatialRefId=4326&where=1%3D1")
hospitals <- st_read("https://opendata.arcgis.com/datasets/df8dc18412494e5abbb021e2f33057b2_0.geojson")
farmers_markets <- st_read("https://hub.arcgis.com/api/v3/datasets/0707c1f31e2446e881d680b0a5ee54bc_0/downloads/data?format=geojson&spatialRefId=4326&where=1%3D1")
schools <- st_read("https://hub.arcgis.com/api/v3/datasets/d46a7e59e2c246c891fbee778759717e_0/downloads/data?format=geojson&spatialRefId=4326&where=1%3D1")
landmarks <- st_read("https://hub.arcgis.com/api/v3/datasets/68628278b86244469d110232f81ea8f9_0/downloads/data?format=geojson&spatialRefId=4326&where=1%3D1")
bike_network <- st_read("https://hub.arcgis.com/api/v3/datasets/b5f660b9f0f44ced915995b6d49f6385_0/downloads/data?format=geojson&spatialRefId=4326&where=1%3D1")

# Transform property sales data to sf object
st_crs(property_sales_res)
property_sales_res$geometry <- st_as_sfc(property_sales_res$shape)
res_properties_sf <- st_sf(property_sales_res)

# Transform CRS for Spatial Amenities - to match that of the property sales data
park_properties <- st_transform(park_properties, st_crs(res_properties_sf))
hospitals <- st_transform(hospitals, st_crs(res_properties_sf))
farmers_markets <- st_transform(farmers_markets, st_crs(res_properties_sf))
schools <- st_transform(schools, st_crs(res_properties_sf))
landmarks <- st_transform(landmarks, st_crs(res_properties_sf))
bike_network <- st_transform(bike_network, st_crs(res_properties_sf))

# Transform census data CRS - to match that of the property sales data
st_crs(philadelphia)
philadelphia <- st_transform(philadelphia, st_crs(res_properties_sf))

# Join census data to property sales data
philadelphia <- philadelphia %>%
  mutate(
    census_tract = as.numeric(str_extract(NAME, "(?<=Census Tract )\\d+(\\.\\d+)?"))
  )

res_properties_sf <- st_join(res_properties_sf, philadelphia %>% select(median_h_incomeE, total_populationE), left = TRUE)

# Join Spatial Amenities Data

# 1. Join parks data - What is the distance to the nearest park?
# Note that some of the parks are super tiny! It also includes playgrounds and rec centers, etc

# Calculate distance matrix (properties to parks)
dist_matrix_parks <- st_distance(res_properties_sf, park_properties)

# Function to get mean distance to k nearest neighbors
park_distance <- function(dist_matrix_parks, k) {
  apply(dist_matrix_parks, 1, function(distances) {
    # Sort and take first k, then average
    mean(as.numeric(sort(distances)[1:k]))
  })
}

# Add nearest park distance as a column
res_properties_sf <- res_properties_sf %>%
  mutate(
    nearest_park = park_distance(dist_matrix_parks, k = 1))

#2 Join hospitals data - What is the distance to the nearest hospital?

# Calculate distance matrix (properties to hospitals)
dist_matrix_hospitals <- st_distance(res_properties_sf, hospitals)

# Function to get mean distance to k nearest neighbors
hosp_distance <- function(dist_matrix_hospitals, k) {
  apply(dist_matrix_hospitals, 1, function(distances) {
    # Sort and take first k, then average
    mean(as.numeric(sort(distances)[1:k]))
  })
}

# Add nearest hospital distance as a column
res_properties_sf <- res_properties_sf %>%
  mutate(
    nearest_hospital = hosp_distance(dist_matrix_hospitals, k = 1))

#hospital distance in miles
res_properties_sf <- res_properties_sf %>%
  mutate(
    nearest_hospital_mi = nearest_hospital/5280
  )

#3 Join farmers market data - what is the distance to the nearest farmers market?

# Calculate distance matrix (properties to farmers markets)
dist_matrix_market <- st_distance(res_properties_sf, farmers_markets)

# Function to get nearest distance
market_distance <- function(dist_matrix_market, k) {
  apply(dist_matrix_market, 1, function(distances) {
    # Sort and take first k, then average
    mean(as.numeric(sort(distances)[1:k]))
  })
}

# Add nearest farmer's market distance as a column
res_properties_sf <- res_properties_sf %>%
  mutate(
    nearest_fmarket = market_distance(dist_matrix_market, k = 1))

# market distance in miles
res_properties_sf <- res_properties_sf %>%
  mutate(
    nearest_fmarket_mi = nearest_fmarket/5280
  )

#4 Join landmark data - what is the distance to the nearest landmark?

# Calculate distance matrix (properties to landmarks)
dist_matrix_landmark <- st_distance(res_properties_sf, landmarks)

# Function to get nearest distance
landmark_distance <- function(dist_matrix_landmark, k) {
  apply(dist_matrix_landmark, 1, function(distances) {
    # Sort and take first k, then average
    mean(as.numeric(sort(distances)[1:k]))
  })
}

# Add nearest landmark distance as a column
res_properties_sf <- res_properties_sf %>%
  mutate(
    nearest_landmark = landmark_distance(dist_matrix_landmark, k = 1))

# landmark distance in miles
res_properties_sf <- res_properties_sf %>%
  mutate(
    nearest_landmark_mi = nearest_landmark/5280
  )

#5 Join school data - what is the distance to the nearest school?

# Calculate distance matrix (properties to schools)
dist_matrix_school <- st_distance(res_properties_sf, schools)

# Function to get nearest distance
school_distance <- function(dist_matrix_school, k) {
  apply(dist_matrix_school, 1, function(distances) {
    # Sort and take first k, then average
    mean(as.numeric(sort(distances)[1:k]))
  })
}

# Add nearest farmer's market distance as a column
res_properties_sf <- res_properties_sf %>%
  mutate(
    nearest_school = school_distance(dist_matrix_school, k = 1))

# market distance in miles
res_properties_sf <- res_properties_sf %>%
  mutate(
    nearest_school_mi = nearest_school/5280
  )

#PHASE 2: EXPLORATORY DATA ANALYSIS - DATA VISUALIZATIONS

#1 Geographic Distribution (Map)

res_properties_sf <- res_properties_sf %>%
  mutate(sale_price_n = as.numeric(sale_price_n))

# Map of Sale Prices
ggplot(res_properties_sf) +
  geom_sf(aes(color = sale_price_n), size = 1, alpha = 0.7) +
  scale_color_viridis_c(option = "plasma", labels = scales::dollar) +
  labs(
    title = "Distribution of Home Prices in Philadelphia",
    color = "Home Price"
  ) +
  theme_minimal()

# Map of Sale Prices - Log Transformed
ggplot() +
  geom_sf(data = philadelphia, fill = "gray95", color = "white") +
  geom_sf(data = res_properties_sf, aes(color = log10(sale_price_n)), size = 1.5, alpha = 0.7) +
  scale_color_viridis_c(
    option = "plasma",
    labels = function(x) dollar(10^x),
    name = "Sale Price (log scale)"
  ) +
  labs(
    title = "Residential Property Sale Prices in Philadelphia",
    subtitle = "Log-scaled price visualization",
    caption = "Data: Property Sales from 2023, 2024"
  ) +
  theme_minimal()

#2 Distribution of Sale Prices (Histogram)

ggplot(res_properties_sf, aes(x = sale_price_n)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white", alpha = 0.8) +
  scale_x_continuous(labels = scales::dollar) +
  labs(
    title = "Distribution of Residential Sale Prices in Philadelphia",
    subtitle = "Data from 2023, 2024",
    x = "Sale Price (USD)",
    y = "Number of Properties"
  ) +
  theme_minimal()

# Sale Prices Histogram - Log Transformed
ggplot(res_properties_sf, aes(x = sale_price_n)) +
  geom_histogram(bins = 40, fill = "darkorange", color = "white", alpha = 0.8) +
  scale_x_log10(labels = dollar) +
  labs(
    title = "Residential Sale Prices in Philadelphia (Log Scale)",
    subtitle = "Data from 2023, 2024",
    x = "Sale Price (log10 USD)",
    y = "Number of Properties"
  ) +
  theme_minimal()

#3 Price vs. Structural Features - Scatter Plot
# Price vs. Number of Bedrooms
res_properties_sf <- res_properties_sf %>%
  mutate(number_of_bedrooms = as.numeric(number_of_bedrooms))

ggplot(
  data = res_properties_sf %>% filter(!is.na(number_of_bedrooms)),
  aes(x = number_of_bedrooms, y = sale_price_n)
) +
  geom_point(color = "steelblue", alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", color = "darkorange", se = TRUE) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Residential Sale Prices by Number of Bedrooms",
    subtitle = "Data from 2023, 2024 Philadelphia",
    x = "Number of Bedrooms",
    y = "Sale Price (USD)"
  ) +
  theme_minimal()

# 4 Price vs. Spatial Features - Scatter Plot
# Price vs. Distance to Nearest Farmer's Market

ggplot(
  data = res_properties_sf,
  aes(x = nearest_fmarket_mi, y = sale_price_n)
) +
  geom_point(color = "steelblue", alpha = 0.6, size = 2) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Residential Sale Prices by Nearest Market",
    subtitle = "Data from 2023, 2024 Philadelphia",
    x = "Distance to Nearest Farmer's Market, Miles",
    y = "Sale Price (USD)"
  ) +
  theme_minimal()


#PHASE 3: FEATURE ENGINEERING

# some of these aren't spatial - i was playing around with what i could do and didn't want to delete in case they would be helpful in the future

# price per bedroom
res_properties_sf <- res_properties_sf %>%
  mutate(
    price_per_bedroom = sale_price_n/number_of_bedrooms,
  )

# home costs relative to income
# lower ratio = affordable housing, higher ratio = expensive housing
res_properties_sf <- res_properties_sf %>%
    mutate(
      price_to_income = sale_price_n / median_h_incomeE
    )

# affordability measure
# lower score = housing is more affordable, higher score = housing is less affordable
res_properties_sf <- res_properties_sf %>%
  mutate(
    affordability = median_h_incomeE / sale_price_n
  )

# converting nearest_park to have miles
res_properties_sf <- res_properties_sf %>%
  mutate(
    nearest_park_mi = nearest_park / 5280
  )

# access index
# lower score = worse access, higher score = better access
res_properties_sf <- res_properties_sf %>%
  mutate(
    access_index = ( 1 / nearest_park_mi) + (1 / nearest_hospital_mi) + (1 / nearest_fmarket_mi) + (1/nearest_landmark_mi) + (1/nearest_school_mi)
  )

# buffers
property_buffers <- st_buffer(
  res_properties_sf, dist = 0.5 * 5280
)

res_properties_sf <- res_properties_sf %>%
  mutate(
    n_parks_near = lengths(st_intersects(property_buffers, park_properties)),
    n_schools_near = lengths(st_intersects(property_buffers, schools)),
    n_fmarkets_near = lengths(st_intersects(property_buffers, farmers_markets)),
    n_hospitals_near = lengths(st_intersects(property_buffers, hospitals)),
    n_landmarks_near = lengths(st_intersects(property_buffers, landmarks))
  )


#PHASE 4: MODEL BUILDING

# convert variables to numeric for later
res_properties_sf <- res_properties_sf %>%
  mutate(total_livable_area = as.numeric(total_livable_area),
         number_of_bathrooms = as.numeric(number_of_bathrooms),
         frontage = as.numeric(frontage),
         fireplaces = as.numeric(fireplaces),
         year_built = year_built
         )

#create categorical variables 
res_properties_sf$quality_grade <- as.factor(res_properties_sf$quality_grade)
res_properties_sf$category_code <- as.factor(res_properties_sf$category_code)

res_properties_sf <- res_properties_sf %>%

# log to reduce skew - compresses large values to make more symmetrical  
res_properties_sf <- res_properties_sf %>%
  mutate(log_price = log(sale_price_n))

#feature sets####
# structural features
structural_features <- res_properties_sf %>%
  select(log_price, number_of_bedrooms, number_of_bathrooms,total_livable_area)

# census variables features
census_features <- res_properties_sf %>%
  select(log_price, number_of_bedrooms, number_of_bathrooms,total_livable_area, 
         median_h_incomeE, total_populationE)

# spatial features
spatial_features <- res_properties_sf %>%
  select(log_price, number_of_bedrooms, number_of_bathrooms,total_livable_area, 
         median_h_incomeE, total_populationE, 
         nearest_park_mi, nearest_hospital_mi, nearest_fmarket_mi, nearest_landmark_mi, nearest_school_mi, 
         n_parks_near, n_schools_near, n_fmarkets_near, n_hospitals_near, n_landmarks_near, access_index)

#models####
# structural model
structural_model <- lm(log_price ~ number_of_bedrooms + number_of_bathrooms + total_livable_area
                      + fireplaces,
                       data = res_properties_sf)

# census model
census_model <- lm(log_price ~ number_of_bedrooms + number_of_bathrooms + total_livable_area + 
                     fireplaces + median_h_incomeE + total_populationE +price_to_income,
                   data = res_properties_sf)

# spatial model
spatial_model <- lm(log_price ~ number_of_bedrooms + number_of_bathrooms + total_livable_area + 
                      fireplaces + median_h_incomeE + total_populationE + price_to_income+
                    nearest_park_mi + nearest_hospital_mi + nearest_fmarket_mi + nearest_landmark_mi + nearest_school_mi + 
                    n_parks_near + n_schools_near + n_fmarkets_near + n_hospitals_near + n_landmarks_near + access_index,
                    data = res_properties_sf)

fixed_effects_interation_model <- lm(log_price ~ number_of_bedrooms + number_of_bathrooms + 
                                       total_livable_area * median_h_incomeE + 
                                       year_built*median_h_incomeE + 
                            fireplaces + total_populationE + price_to_income+
                            nearest_park_mi + nearest_hospital_mi + nearest_fmarket_mi + nearest_landmark_mi + nearest_school_mi + 
                            n_parks_near + n_schools_near + n_fmarkets_near + n_hospitals_near + n_landmarks_near + access_index +
                            category_code,
                          data = res_properties_sf)


summary(structural_model)
summary(census_model)
summary(spatial_model)
summary(fixed_effects_interation_model)


vif(fixed_effects_interation_model)

stargazer(structural_model, census_model, spatial_model, fixed_effects_interation_model, type = "text",
          star.cutoffs = c(0.05, 0.01, 0.001))

# 5 Creative Visualization

### Phase 5: Model Validation

train_control <- trainControl(method = "cv", number = 10)

cv.mod.1 <- train(log_price ~ number_of_bedrooms + number_of_bathrooms + total_livable_area
                  + fireplaces,
                  data = res_properties_sf,
                  method = "lm",
                  trControl = train_control,
                  na.action = na.omit)

cv.mod.2 <- train(log_price ~ number_of_bedrooms + number_of_bathrooms + total_livable_area + 
                    fireplaces + median_h_incomeE + total_populationE +price_to_income,
                  data = res_properties_sf,
                  method = "lm",
                  trControl = train_control,
                  na.action = na.omit)

cv.mod.3 <- train(log_price ~ number_of_bedrooms + number_of_bathrooms + total_livable_area + 
                    fireplaces + median_h_incomeE + total_populationE + price_to_income+
                    nearest_park_mi + nearest_hospital_mi + nearest_fmarket_mi + nearest_landmark_mi + nearest_school_mi + 
                    n_parks_near + n_schools_near + n_fmarkets_near + n_hospitals_near + n_landmarks_near + access_index,
                  data = res_properties_sf,
                  method = "lm",
                  trControl = train_control,
                  na.action = na.omit)

cv.mod.4 <- train(log_price ~ number_of_bedrooms + number_of_bathrooms + 
                    total_livable_area * median_h_incomeE + 
                    year_built*median_h_incomeE + 
                    fireplaces + total_populationE + price_to_income+
                    nearest_park_mi + nearest_hospital_mi + nearest_fmarket_mi + nearest_landmark_mi + nearest_school_mi + 
                    n_parks_near + n_schools_near + n_fmarkets_near + n_hospitals_near + n_landmarks_near + access_index +
                    category_code,
                  data = res_properties_sf,
                  method = "lm",
                  trControl = train_control,
                  na.action = na.omit)

cv.mod.1$results
cv.mod.2$results
cv.mod.3$results
cv.mod.4$results

#table of models' stats
metrics_mod1 <- cv.mod.1$results[1, c("RMSE", "Rsquared", "MAE")]
metrics_mod1$model <- "Structural Model"

metrics_mod2 <- cv.mod.2$results[1, c("RMSE", "Rsquared", "MAE")]
metrics_mod2$model <- "Structural + Census Model"

metrics_mod3 <- cv.mod.3$results[1, c("RMSE", "Rsquared", "MAE")]
metrics_mod3$model <- "Structural + Census + Spatial Model"

metrics_mod4 <- cv.mod.4$results[1, c("RMSE", "Rsquared", "MAE")]
metrics_mod4$model <- "Structural + Census + Spatial + Fixed Effects & Interactions Model"

metrics_table <- bind_rows(metrics_mod1, metrics_mod2, metrics_mod3, metrics_mod4)

kable(metrics_table, "html") %>%
  kable_styling(full_width = FALSE)

#plot fit vs residuals####
resid.1 <- data.frame(
  fitted = fitted(cv.mod.1$finalModel),
  residuals = resid(cv.mod.1$finalModel)
)

ggplot(resid.1, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residual Plot", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

resid.2 <- data.frame(
  fitted = fitted(cv.mod.2$finalModel),
  residuals = resid(cv.mod.2$finalModel)
)

ggplot(resid.2, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residual Plot", x = "Fitted Values", y = "Residuals") +
  theme_minimal()


resid.3 <- data.frame(
  fitted = fitted(cv.mod.3$finalModel),
  residuals = resid(cv.mod.3$finalModel)
)

ggplot(resid.3, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residual Plot", x = "Fitted Values", y = "Residuals") +
  theme_minimal()


resid.4 <- data.frame(
  fitted = fitted(cv.mod.4$finalModel),
  residuals = resid(cv.mod.4$finalModel)
)

ggplot(resid.4, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residual Plot", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

#plot fitted on map to see location of outliers

preds <- predict(cv.mod.4, newdata = res_properties_sf)
res_properties_sf$fitted <- NA_real_
res_properties_sf$residuals <- NA_real_
res_properties_sf$fitted[as.numeric(names(preds))] <- preds
res_properties_sf$residuals[as.numeric(names(preds))] <- res_properties_sf$log_price[as.numeric(names(preds))] - preds

res_properties_sf%>%
filter(!is.na(residuals))%>%
ggplot() +
  geom_sf(aes(color = residuals), alpha=.5)+
  scale_color_gradient2(
    low = "red", mid = "gray", high = "green", midpoint = 0,
    name = "Residual (log-price)"
  ) +
  theme_void()

