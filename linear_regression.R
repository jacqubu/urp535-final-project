library(tidycensus)

census_api_key("5b2979d9a21fa6439568ee0cc13cf19cb1c52797", install=TRUE)

v22 <- load_variables(year = 2022, dataset = "acs5", cache = TRUE)
View(v22)
#then search for Estimate!!Total:!!Income in the past 12 months below poverty level:
#and find that the variable is B17001_002
get_acs(geography = "state", variables = c(poverty = "B17001B_002"), state="MI", year = 2022, progress = FALSE)

get_acs(geography = "state", variables = c(men_edu = "B15002_015"), year = 2022, progress = FALSE)


df <- data.frame( x= c(1,2,3,4,5), 
                  y= c(1,5,8,15,26)) 

linear_model <- lm(y ~ x, data=df) # fit linear model 
summary(linear_model) # view summary of linear model 
