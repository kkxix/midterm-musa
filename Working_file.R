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

# DATA CLEANING SUMMARY
# Make Summary tables showing before and after data cleaning

before_summary <- tibble(
  Summary = "Raw data",
  Rows = nrow(property_sales),
  Columns = ncol(property_sales)
)

after_summary <- tibble(
  Summary = "Cleaned data",
  Rows = nrow(res_properties_sf),
  Columns = ncol(res_properties_sf)
)

# Combine into one summary table
summary_table <- bind_rows(before_summary, after_summary)

#Narrative: 
# The dataset initially had 583,776 properties. First we filtered properties based on their zoning code, only keeping those which were residential or 'IRMX,' meaning Industrial Residential Mixed Use. 
#Then to further ensure that we had only residential entries, we filtered for entries that had a category code of single family, multi family, or mixed use. 
# To remove any egregious outliers we removed properties whose sale price was less than $5000 or greater than $10000000. Finally, we only kept sales that occurred in 2023 or 2024. Blank cells were recoded as 'NA.'
# The cleaned dataset had 27357 lines.

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
  scale_color_viridis_c(option = "turbo", labels = scales::dollar) +
  labs(
    title = "Distribution of Sale Prices in Philadelphia",
    color = "Home Price"
  ) +
  theme_minimal()

###Interpretation: This map visualizes residential property sales across Philadelphia during 2023–2024, with each point representing a single sale and color indicating the sale price. The full range of values is very hard to see on this map because while there is a large range of values, going beyond $8 million, most of them fall on the much lower end of the spectrum. This causes the map to appear mostly one uniform shade of navy blue. A number of higher valued properties (shown in light blue and representing homes just under $2 million) can be seen clustered in northwest Philadelphia and around center city.  

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

### Interpretation: This map visualizes residential property sales across Philadelphia during 2023–2024, with each point representing a single sale and color indicating the sale price on a logarithmic scale. Using a log scale allows for clearer visualization of the wide range of property values, compressing extremely high prices while preserving overall spatial variation. The pattern reveals clear geographic differences in housing markets: higher sale prices (yellow to orange) are concentrated in central and northwestern neighborhoods—such as Center City, University City, and Chestnut Hill—while lower prices (purple) are more common in much of North and Southwest Philadelphia. These spatial patterns highlight the city’s pronounced housing value disparities, reflecting broader socioeconomic divides. They also show potential gentrification pressures in the areas where low- and mid-priced neighborhoods intersect.

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

### Interpretation: This histogram illustrates the distribution of residential property sale prices in Philadelphia for the years 2023–2024. The data are highly right-skewed, meaning most properties sold at relatively low prices, while a small number of transactions occurred at very high prices. The vast majority of sales cluster below about $500,000, with a sharp peak near the lower end of the price range. Only a few properties sold for prices above $1 million, indicating that such high-value transactions are rare.

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

### Interpretation: This histogram displays the distribution of residential sale prices in Philadelphia (2023–2024) on a logarithmic scale, which makes the skewed price data easier to interpret. The distribution appears roughly bell-shaped, indicating that the underlying price data follow a log-normal distribution. Most residential properties sold for between $100,000 and $1,000,000, forming the central peak of the distribution. Fewer homes sold at very low or very high prices, shown by the tapering tails on both sides.

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

###Interpretation: The scatter plot shows the relationship between the number of bedrooms in residential properties and their sale prices in Philadelphia for 2023 and 2024. The trend line does indicate a general upward trajectory, suggesting that sale prices tend to increase as the number of bedrooms increases. At the same time, the data shows a wide range of sale prices within each bedroom category, indicating that other factors such as neighborhood location, property condition, amenities, and lot size also have a large influence on overall value. In each bedroom category, most of the properties are valued below $2,000,000. ere There are also many high outliers in each category, meaning that for luxury properties the number of bedrooms is not highly associated with the value. Also, beyond 6 bedrooms, the sale prices begin to taper off, suggesting that at that point additional bedrooms have a diminishing return. 

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

### Interpretation: The scatter plot shows the relationship between the distance to the nearest farmers’ market and the sale price. The highest valued properties are clustered within 1 mile from a market. Beyond 2 miles, the values fall sharply. This suggests that proximity to farmers markets does to some degree play a role in sale value. However, it is almost important to note that there is a very wide range or properties within 1 mile. Overall this graphic is most revealing about the proximity of high end properties to farmers markets. 

#5 Creative Visualization
# Chloropleth Map of Median Prices and Median Income

tract_level_data <- res_properties_sf %>%
  st_drop_geometry() %>%  # remove geometry for calculation
  group_by(census_tract) %>%
  summarize(
    median_price = median(sale_price_n, na.rm = TRUE),
    median_income = median(median_h_incomeE, na.rm = TRUE) # ensures tract-level summary
  )

tract_level_data <- tract_level_data %>%
  mutate(census_tract = as.numeric(census_tract))

tracts_merged <- philadelphia %>%
  left_join(tract_level_data, by = c("census_tract"))

tracts_merged <- tracts_merged %>%
  mutate(
    income_cat = cut(median_income,
                     breaks = quantile(median_income, probs = seq(0, 1, 0.33), na.rm = TRUE),
                     include.lowest = TRUE, labels = c("Low Income", "Medium Income", "High Income")),
    price_cat = cut(median_price,
                    breaks = quantile(median_price, probs = seq(0, 1, 0.33), na.rm = TRUE),
                    include.lowest = TRUE, labels = c("Low Price", "Medium Price", "High Price")),
    bivariate_cat = interaction(income_cat, price_cat, sep = "-")
  )

# Define bivariate color palette
bivariate_colors <- c(
  "Low Income-Low Price"     = "#e8e8e8",
  "Medium Income-Low Price"  = "#ace4e4",
  "High Income-Low Price"    = "#5ac8c8",
  "Low Income-Medium Price"  = "#dfb0d6",
  "Medium Income-Medium Price" = "#a5add3",
  "High Income-Medium Price" = "#5698b9",
  "Low Income-High Price"    = "#be64ac",
  "Medium Income-High Price" = "#8c62aa",
  "High Income-High Price"   = "#3b4994"
)

ggplot(tracts_merged) +
  geom_sf(aes(fill = bivariate_cat), color = NA) +
  scale_fill_manual(values = bivariate_colors, name = "Income vs Price",
                    guide = guide_legend(title.position = "top", ncol = 1)) +
  labs(
    title = "Bivariate Choropleth Map of Property Values and Median Income",
    subtitle = "Census Tracts in Philadelphia (2023–2024)",
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    legend.position = "right"
  )

### Interpretation: This is a bivariate choropleth map of median home values and median income by census tract in Philadelphia. Median home values and median income were broken into tertiles. This shows that high-income, high-price areas are highly clustered in northwest Philadelphia, but also close to center city, university city, and in parts of the northeast. Low-income, low-price areas are found in the west and southwest, the north, and the northeast. These would indicate underfunded areas. The low-income, high price areas could represent areas that are quickly gentrifying. Low income residents here are at risked of getting pushed out of these neighborhoods. 

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

stargazer(
  model1, model2, model3,
  omit = c("year_built", "median_h_incomeE:year_built"),
  omit.labels = c("Year Built Fixed Effects", "Income-Year Interactions")
)

stargazer(
  structural_model, 
  census_model, 
  spatial_model, 
  fixed_effects_interation_model,
  type = "text",   # change to "latex" or "html" for Quarto/LaTeX output
  title = "Regression Results: Structural, Census, Spatial, and Full Interaction Models",
  dep.var.labels = "Log of Sale Price",
  column.labels = c("Structural", "Census", "Spatial", "Full w/FE & Interactions"),
  keep = c("number_of_bedrooms", "number_of_bathrooms", "total_livable_area", 
           "fireplaces", "median_h_incomeE", "total_populationE", "price_to_income",
           "nearest_park_mi", "nearest_hospital_mi", "nearest_fmarket_mi", 
           "nearest_landmark_mi", "nearest_school_mi", "n_parks_near", 
           "n_schools_near", "n_fmarkets_near", "n_hospitals_near", 
           "n_landmarks_near", "access_index"),
  omit = c("year_built", "median_h_incomeE:year_built", "category_code"),
  omit.words = c("year_built", "median_h_incomeE:year_built"),
  add.lines = list(
    c("Year Built Fixed Effects", "No", "No", "No", "Yes"),
    c("Category Fixed Effects", "No", "No", "No", "Yes"),
    c("Income Interactions", "No", "No", "No", "Yes")
  ),
  no.space = TRUE,
  digits = 3,
  omit.stat = c("f", "ser")  # optional to clean up extra stats
)

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

