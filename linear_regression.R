library(tidycensus)
library(leaflet) 
library(htmlwidgets) 
library(tigris)
library(tidyverse)
library(sf)
library(purrr)
source("us_census_api_key.R")

#setup to read in data
census_api_key(API_key, install=TRUE, overwrite=TRUE)
# v22 <- load_variables(year = 2022, dataset = "acs5", cache = TRUE)
# View(v22)
us <- unique(fips_codes$state)[1:51]
options(tigris_use_cache = TRUE)
# rappdirs::user_cache_dir("tigris")

#get built environment factors

#B25052_002        Estimate!!Total:!!Complete kitchen facilities       Kitchen Facilities for Occupied Housing Units       tract
#B25048_002     Estimate!!Total:!!      Complete plumbing facilities    Plumbing Facilities for Occupied Housing Units  tract
built_environ_data <- map_df(us, function(x) {get_acs(geography = "tract", variables = c(complete_kitchen="B25052_002", complete_plumbing="B25048_002"), state=x, year = 2022, survey = "acs5", geometry=TRUE, progress=FALSE)})

#B08141_016     Estimate!!Total:!!Public transportation (excluding taxicab):        Means of Transportation to Work by Vehicles Available       tract
#B08141_006     Estimate!!Total:!!Car, truck, or van - drove alone:     Means of Transportation to Work by Vehicles Available       tract
#B08141_011     Estimate!!Total:!!Car, truck, or van - carpooled:       Means of Transportation to Work by Vehicles Available       tract
#B08141_031     Estimate!!Total:!!Worked from home:     Means of Transportation to Work by Vehicles Available       tract
#B08141_026     Estimate!!Total:!!Taxicab, motorcycle, bicycle, or other means:     Means of Transportation to Work by Vehicles Available   tract
#B08141_021     Estimate!!Total:!!Walked:       Means of Transportation to Work by Vehicles Available   tract
transportation <- map_df(us, function(x) {get_acs(geography = "tract", variables = c(
    vehicle_alone="B08141_006", vehicle_carpool='B08141_011', public_trans="B08141_016", 
    walk='B08141_021', home='B08141_031', other='B08141_026'), state=x, year = 2022, survey = "acs5", geometry=TRUE, progress=FALSE)})


#get loneliness/social isolation factors

#B18107_001     Estimate!!Total:        Sex by Age by Independent Living Difficulty     tract
#NOT USING B09021_002     Estimate!!Total:!!      Lives alone                                     block group
#B18106_001     Estimate!!Total:        Sex by Age by Self-Care Difficulty              tract
#B17001_001     Estimate!!Total:        Poverty Status in the Past 12 Months by Sex by Age  tract
#B18101_001     Estimate!!Total:        Sex by Age by Disability Status     tract
#B06008_004        Estimate!!Total:!!Divorced      Place of Birth by Marital Status in the United States       tract
#B06008_006        Estimate!!Total:!!Widowed       Place of Birth by Marital Status in the United States       tract
#B06008_005        Estimate!!Total:!!Separated     Place of Birth by Marital Status in the United States       tract
loneliness_data <- map_df(us, function(x) {get_acs(geography = "tract", variables = c(
    ind_live_diff = "B18107_001", self_care_diff='B18106_001',
    disability='B18101_001', poverty = "B17001_001",
    divorced='B06008_004',widowed='B06008_006', separated='B06008_005'
    ), state=x, year = 2022, survey = "acs5", geometry=TRUE, progress=FALSE)})


#run linear regression

df <- data.frame(x=c(complete_kitchen,complete_plumbing), y=c(ind_live_diff,self_care_diff,poverty)) 

linear_model <- lm(formula = ind_live_diff ~ complete_kitchen+complete_plumbing, data=df) # fit linear model 
summary(linear_model) # view summary of linear model 

ind_live_diff$geometry <- ind_live_diff$geometry %>% st_transform(ind_live_diff$geometry, crs="ESRI:102010")



pal <- colorNumeric(
  palette = "magma",
  domain = ind_live_diff$estimate
)

pal(c(10, 20, 30, 40, 50))


primary_secondary_roads("DC")
roads()

map <- leaflet() %>% 
  setView(lng = -98.5833333333, lat = 39.8333333333, zoom = 5) %>% # center of USA
  addTiles() %>%
  # addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(data = transportation)

, 
    fillColor = ~pal(density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7)

  # addTiles() %>%
  # addPolygons(data = ind_live_diff)
              # color = ~pal(estimate),
              # weight = 0.5,
              # smoothFactor = 0.2,
              # fillOpacity = 0.5,
              # label = ~estimate) 

saveWidget(map, file="map.html") # Save the map as an HTML widget 