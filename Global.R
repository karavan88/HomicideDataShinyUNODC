#global environment codes
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(leaflet.minicharts)
library(readr) 
library(tidyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

#DATASET for a country map
country_map_data <<- read_csv("01_input_data/Country_Map.csv")
country_map_data$Variable <- factor(country_map_data$Variable)

#DATASET for a country map
country_map_data <<- 
  read_csv("01_input_data/Country_Map.csv") %>%  
  mutate(iso_a3 = countrycode::countrycode(code, destination = "iso3c", 
                                           origin = "iso2c"))

world <- ne_countries(scale = 110, type = 'countries', returnclass = "sf")

# Merge your data with the shapefile data
# Ensure that 'join_field' is the name of the common identifier in your dataset
# For example, it might be 'iso_a3' for the ISO alpha-3 country code
merged_data <- inner_join(world, country_map_data)


vars_for_hom_map = unique(country_map_data$Variable)
Gender = c("Male Homicide Rate", "Female Homicide Rate")
Male_by_Age = c("Male 0-14 Rate", "Male 15-29 Rate", "Male 30-44 Rate", "Male 45-59 Rate", "Male 60 or more Rate")
Female_by_Age = c("Female 0-14 Rate", "Female 15-29 Rate", "Female 30-44 Rate", "Female 45-59 Rate", "Female 60 or more Rate")                                           
Mechanism = c("Firearms Rate", "Sharp Objects Rate", "Other Mechanism Rate")        


#DATASET for the country data table
shiny_dt <<- read_csv("01_input_data/Shiny_DT.csv") 

#we need to produce values for inputs
countries <- unique(shiny_dt$Country)

variables <- unique(shiny_dt$Indicator)
homicide = variables[c(1,2)]
gender_hom = variables[c(3:6)]
gender_age_hom = variables[c(7:26)]
mech_hom = variables[c(27:34)]
mech_gender_hom = variables[c(35:50)]

###########new

#DATASET for regional and subregional data table
shiny_reg_dt <<- read_csv("01_input_data/shiny_reg_dt.csv") 
reg_vars <- unique(shiny_reg_dt$Indicator)
reg_regions <- unique(shiny_reg_dt$`Region/Subregion`) #this object will not  be used in the UI
subregions1 = reg_regions[7:28]
subregions = subregions1[-c(5,8,9,10,11,14,17,20)]

#DATASET for regional map
regmap <<- read_csv("01_input_data/regmap.csv")
regvarsmap <- names(regmap)[3:19]
regmap <- regmap %>%
  mutate_if(is.numeric, round, (0))

####------works
#DATASET for time-series plot
ts_data <<- read_csv("01_input_data/TS_plot_countries.csv")
ts_data <- ts_data %>% mutate_if(is.numeric, round, 3)
ts_vars <- names(ts_data)
ts_vars <- ts_vars[c(5:21)]
ts_countries <- sort(countries, decreasing = F)


#Data for Homicide in prisons
hom_prisons_dat <<- Homicide_in_Prisons_Shiny <- read_csv("01_input_data/Homicide in Prisons Shiny.csv", 
                                                          col_types = cols(`2010` = col_character(), 
                                                                           `2011` = col_character(), `2012` = col_character(), 
                                                                           `2013` = col_character(), `2014` = col_character(), 
                                                                           `2015` = col_character(), `2016` = col_character()))

#read_csv("Homicide in Prisons Shiny.csv", na = "empty", trim_ws = FALSE)
hom_prisons_dat$Indicator = factor(hom_prisons_dat$Indicator)
hom_prisons_dat$Country = factor(hom_prisons_dat$Country)  

#Data for citizenship
# citizenship <<- read_csv("citizenship.csv")
# citizenship$Indicator = factor(citizenship$Indicator) 
# citizenship$Country = factor(citizenship$Country)


#DATASET for criminal justice response
cjsr <<- read.csv('01_input_data/cjsr.csv', check.names = T, header = T)

#rename var names to get rid of X in front of each year
names(cjsr)[4] <- "2003"
names(cjsr)[5] <- "2004"
names(cjsr)[6] <- "2005"
names(cjsr)[7] <- "2006"
names(cjsr)[8] <- "2007"
names(cjsr)[9] <- "2008"
names(cjsr)[10] <- "2009"
names(cjsr)[11] <- "2010"
names(cjsr)[12] <- "2011"
names(cjsr)[13] <- "2012"
names(cjsr)[14] <- "2013"
names(cjsr)[15] <- "2014"
names(cjsr)[16] <- "2015"
names(cjsr)[17] <- "2016"
names(cjsr)[18] <- "2017"

#Dataset for ipfm
ipfm <<- read.csv("01_input_data/ipfm_shiny.csv")

names(ipfm)[4] <- "2005"
names(ipfm)[5] <- "2006"
names(ipfm)[6] <- "2007"
names(ipfm)[7] <- "2008"
names(ipfm)[8] <- "2009"
names(ipfm)[9] <- "2010"
names(ipfm)[10] <- "2011"
names(ipfm)[11] <- "2012"
names(ipfm)[12] <- "2013"
names(ipfm)[13] <- "2014"
names(ipfm)[14] <- "2015"
names(ipfm)[15] <- "2016"
names(ipfm)[16] <- "2017"

#Crime-Related Homicide

crime_hom <<- read.csv("01_input_data/crime related hom.csv")

names(crime_hom)[4] <- "2005"
names(crime_hom)[5] <- "2006"
names(crime_hom)[6] <- "2007"
names(crime_hom)[7] <- "2008"
names(crime_hom)[8] <- "2009"
names(crime_hom)[9] <- "2010"
names(crime_hom)[10] <- "2011"
names(crime_hom)[11] <- "2012"
names(crime_hom)[12] <- "2013"
names(crime_hom)[13] <- "2014"
names(crime_hom)[14] <- "2015"
names(crime_hom)[15] <- "2016"
names(crime_hom)[16] <- "2017"