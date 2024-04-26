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
# df <- data.frame(x=c(complete_kitchen,complete_plumbing), y=c(ind_live_diff,self_care_diff,poverty)) 
# linear_model <- lm(formula = ind_live_diff ~ complete_kitchen+complete_plumbing, data=df) # fit linear model 
# summary(linear_model) # view summary of linear model 


p_s_roads <- map_df(us, function(x) {primary_secondary_roads(x)})
roads()


#labels
label_func <- function(name, data, units) {
  sprintf(
    paste("<strong>%s</strong><br/>%g ", units),
    name, data) %>% lapply(htmltools::HTML)
}

kitchen_labels <- label_func(built_environ_data$NAME, built_environ_data[built_environ_data$variable=='complete_kitchen',]$estimate, "kitchen facilities")
plumbing_labels <-  label_func(built_environ_data$NAME, built_environ_data[built_environ_data$variable=='complete_plumbing',]$estimate, "plumbing facilities")


# kitchen_labels <- sprintf(
#   "<strong>%s</strong><br/>%g kitchen facilities",
#   built_environ_data$NAME, built_environ_data[built_environ_data$variable=='complete_kitchen',]$estimate
# ) %>% lapply(htmltools::HTML)
trans_veh_labels <- label_func(transportation$NAME, transportation[transportation$variable=='vehicle_alone',]$estimate, "")
trans_carpool_labels <- label_func(transportation$NAME, transportation[transportation$variable=='vehicle_carpool',]$estimate, "")
trans_public_labels <- label_func(transportation$NAME, transportation[transportation$variable=='public_trans',]$estimate, "")
trans_walk_labels <- label_func(transportation$NAME, transportation[transportation$variable=='walk',]$estimate, "")
trans_home_labels <- label_func(transportation$NAME, transportation[transportation$variable=='home',]$estimate, "")
trans_other_labels <- label_func(transportation$NAME, transportation[transportation$variable=='other',]$estimate, "")
ind_live_diff_labels <- label_func(loneliness_data$NAME, loneliness_data[loneliness_data$variable=='ind_live_diff',]$estimate, "")
self_care_diff_labels <- label_func(loneliness_data$NAME, loneliness_data[loneliness_data$variable=='self_care_diff',]$estimate, "")
disability_labels <- label_func(loneliness_data$NAME, loneliness_data[loneliness_data$variable=='disability',]$estimate, "")
poverty_labels <- label_func(loneliness_data$NAME, loneliness_data[loneliness_data$variable=='poverty',]$estimate, "")


#color palettes
kitchen_pal <- colorQuantile("viridis", domain=built_environ_data[built_environ_data$variable=='complete_kitchen',]$estimate, n=4)
plumbing_pal <- colorQuantile("viridis", domain=built_environ_data[built_environ_data$variable=='complete_plumbing',]$estimate, n=4)
trans_veh_pal <- colorQuantile("viridis", domain=transportation[transportation$variable=='vehicle_alone',]$estimate, n=4)
trans_carpool_pal <- colorQuantile("viridis", domain=transportation[transportation$variable=='vehicle_carpool',]$estimate, n=4)
trans_public_pal <- colorQuantile("viridis", domain=unique(transportation[transportation$variable=='public_trans',]$estimate), n=4)
trans_walk_pal <- colorQuantile("viridis", domain=unique(transportation[transportation$variable=='walk',]$estimate), n=4)
trans_home_pal <- colorQuantile("viridis", domain=transportation[transportation$variable=='home',]$estimate, n=4)
trans_other_pal <- colorQuantile("viridis", domain=unique(transportation[transportation$variable=='other',]$estimate), n=4)
ind_live_diff_pal <- colorQuantile("viridis", domain=loneliness_data[loneliness_data$variable=='ind_live_diff',]$estimate, n=4)
self_care_diff_pal <- colorQuantile("viridis", domain=loneliness_data[loneliness_data$variable=='self_care_diff',]$estimate, n=4)
disability_pal <- colorQuantile("viridis", domain=loneliness_data[loneliness_data$variable=='disability',]$estimate, n=4)
poverty_pal <- colorQuantile("viridis", domain=loneliness_data[loneliness_data$variable=='poverty',]$estimate, n=4)

addToMap <- function(MAP, DATA, PALETTE, LAYER_NAME, LEGEND_NAME) {
  MAP %>%
  addPolygons(data = DATA, fillColor = ~PALETTE(estimate), weight = 0.01,  opacity = 0,  color = "white",  fillOpacity = 0.25,
              highlightOptions = highlightOptions(weight = 5, opacity=1, color = "white", fillOpacity = 0.7, bringToFront = FALSE, sendToBack = FALSE),
              label = kitchen_labels, labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto"),
              group=LAYER_NAME) %>%
  addLegend(pal=PALETTE, values = DATA$estimate, title=LEGEND_NAME, opacity = 0.7, position = "bottomright", group=LAYER_NAME)
}

map <- leaflet() %>% 
  setView(lng = -98.5833333333, lat = 39.8333333333, zoom = 5) %>% # center of USA
  addTiles(group="OSM") %>%
  addProviderTiles(provider = "CartoDB.Positron", group="CartoDB")
  # addPolygons(data = built_environ_data[built_environ_data$variable=='complete_kitchen',], fillColor = ~kitchen_pal(estimate), weight = 0.01,  opacity = 0,  color = "white",  fillOpacity = 0.25,
  #             highlightOptions = highlight_ops,
  #             label = kitchen_labels, labelOptions = label_ops,
  #             group="Kitchen Facilities") %>%
  # addLegend(pal=kitchen_pal, values = built_environ_data[built_environ_data$variable=='complete_kitchen',]$estimate, title="Complete Kitchen Facilities", opacity = 0.7, position = "bottomright", group="Kitchen Facilities") %>%
  # addPolygons(data = built_environ_data[built_environ_data$variable=='complete_plumbing',], fillColor = ~plumbing_pal(estimate), weight = 0.01,  opacity = 0,  color = "white",  fillOpacity = 0.25,
  #             highlightOptions = highlight_ops,
  #             label = plumbing_labels, labelOptions = label_ops,
  #             group="Plumbing Facilities") %>%
  # addLegend(pal=plumbing_pal, values = built_environ_data[built_environ_data$variable=='complete_plumbing',]$estimate, title="Complete Plumbing Facilities", opacity = 0.7, position = "bottomright", group="Plumbing Facilities") %>%
  

map <- addToMap(map, built_environ_data[built_environ_data$variable=='complete_kitchen',], kitchen_pal, "Kitchen Facilities", "Complete Kitchen Facilities")
map <- addToMap(map, built_environ_data[built_environ_data$variable=='complete_plumbing',], plumbing_pal, "Plumbing Facilities", "Complete Plumbing Facilities")
map <- addToMap(map, transportation[transportation$variable=='vehicle_alone',], trans_veh_pal, "Vehicles", "Drove Alone to Work")
map <- addToMap(map, transportation[transportation$variable=='vehicle_carpool',], trans_carpool_pal, "Carpool", "Carpooled to Work")
map <- addToMap(map, transportation[transportation$variable=='public_trans',], trans_public_pal, "Public Transportation", "Took Public Transportation to Work")
map <- addToMap(map, transportation[transportation$variable=='walk',], trans_walk_pal, "Walk", "Walked to Work")
map <- addToMap(map, transportation[transportation$variable=='home',], trans_home_pal, "Home", "Worked at Home")
map <- addToMap(map, transportation[transportation$variable=='other',], trans_other_pal, "Other Transportation", "Took Other Transportation to Work")
map <- addToMap(map, loneliness_data[loneliness_data$variable=='ind_live_diff',], ind_live_diff_pal, "Independent Living Difficulty", "Independent Living Difficulty")
map <- addToMap(map, loneliness_data[loneliness_data$variable=='self_care_diff',], self_care_diff_pal, "Self Care Difficulty", "Self Care Difficulty")
map <- addToMap(map, loneliness_data[loneliness_data$variable=='disability',], disability_pal, "Disability Status", "Disability Status")
map <- addToMap(map, loneliness_data[loneliness_data$variable=='poverty',], poverty_pal, "Poverty Status", "Poverty Status")

map <- map %>% addLayersControl(
  baseGroups = c("OSM", "CartoDB"),
  overlayGroups = c("Kitchen Facilities", "Plumbing Facilities", "Vehicles", "Carpool", "Public Transportation", "Walk", "Home", "Other Transportation", "Independent Living Difficulty", "Self Care Difficulty", "Disability Status", "Poverty Status"),
  options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("Plumbing Facilities", "Vehicles", "Carpool", "Public Transportation", "Walk", "Home", "Other Transportation", "Independent Living Difficulty", "Self Care Difficulty", "Disability Status", "Poverty Status"))

saveWidget(map, file="map.html") # Save the map as an HTML widget 