
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

#DATASET for a country map
country_map_data <<- 
  read_csv("Country_Map.csv") %>%  
  mutate(iso_a3 = countrycode::countrycode(code, destination = "iso3c", 
                                            origin = "iso2c"))

world <- ne_countries(scale = 110, type = 'countries', returnclass = "sf")

# Merge your data with the shapefile data
# Ensure that 'join_field' is the name of the common identifier in your dataset
# For example, it might be 'iso_a3' for the ISO alpha-3 country code
merged_data <- inner_join(world, c)

  
