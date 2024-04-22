library(tidycensus)
library(leaflet) 
library(htmlwidgets) 
source("us_census_api_key.R")


#read in us census data
#get map geometries
#perform regression 

#read in ACS 5-year data
census_api_key(API_key, install=TRUE, overwrite=TRUE)
v22 <- load_variables(year = 2022, dataset = "acs5", cache = TRUE)
View(v22)

#get built environment factors

#B17020_001 Poverty Status in the Past 12 Months by Age

#then search for Estimate!!Total:!!Income in the past 12 months below poverty level:
#and find that the variable is B17001_002

#BB25052_002        Estimate!!Total:!!Complete kitchen facilities       Kitchen Facilities for Occupied Housing Units       tract
#B25048_002     Estimate!!Total:!!      Complete plumbing facilities    Plumbing Facilities for Occupied Housing Units  tract
built_environ_data <- get_acs(geography = "tract", variables = c(complete_kitchen="BB25052_002", complete_plumbing='B25048_002'), year = 2022, geometry=TRUE, progress=FALSE)


#B08141_016     Estimate!!Total:!!Public transportation (excluding taxicab):        Means of Transportation to Work by Vehicles Available       tract
#B08141_006     Estimate!!Total:!!Car, truck, or van - drove alone:     Means of Transportation to Work by Vehicles Available       tract
#B08141_011     Estimate!!Total:!!Car, truck, or van - carpooled:       Means of Transportation to Work by Vehicles Available       tract
#B08141_031     Estimate!!Total:!!Worked from home:     Means of Transportation to Work by Vehicles Available       tract
#B08141_026     Estimate!!Total:!!Taxicab, motorcycle, bicycle, or other means:     Means of Transportation to Work by Vehicles Available   tract
#B08141_021     Estimate!!Total:!!Walked:       Means of Transportation to Work by Vehicles Available   tract
transportation <- get_acs(geography = "tract", variables = c(
    vehicle_alone="B08141_006", vehicle_carpool='B08141_011', public_trans="B08141_016", 
    walk='B08141_021', home='B08141_031', other='B08141_026'), year = 2022, geometry=TRUE, progress=FALSE)


#get loneliness/social isolation factors

#B18107_001     Estimate!!Total:        Sex by Age by Independent Living Difficulty     tract
#B09021_002     Estimate!!Total:!!      Lives alone                                     block group
#B18106_001     Estimate!!Total:        Sex by Age by Self-Care Difficulty              tract


# B06008_004        Estimate!!Total:!!Divorced      Place of Birth by Marital Status in the United States       tract
poverty = ""
# poverty; living alone; divorced, separated or widowed; never married; disability; 
loneliness_data <- get_acs(geography = "tract", variables = c(ind_live_diff = "B18107_001", self_care_diff='B18106_001'), year = 2022, geometry=TRUE, progress=FALSE)



#run linear regression


df <- data.frame( x= c(1,2,3,4,5), 
                  y= c(1,5,8,15,26)) 

linear_model <- lm(y ~ x, data=df) # fit linear model 
summary(linear_model) # view summary of linear model 




usa <- c(39.8333333333, -98.5833333333) # center of USA
map <- leaflet() %>% 
  setView(lng = usa[2], lat = usa[1], 
          zoom = 5) %>% 
  addTiles() 


saveWidget(map, file="map.html") # Save the map as an HTML widget 