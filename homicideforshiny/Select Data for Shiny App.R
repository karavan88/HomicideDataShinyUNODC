#data file for app

library(readxl)
mhd <- read_excel("Master Homicide Data June17_2019.xlsx")


#View(mhd)

library(dplyr)
library(knitr)
library(kableExtra)
library(tidyr)
library(data.table)



#We need to set up to NAs all the estimated data

varstofactor <- c("Code_Total", "Source_Sex", "Source_Age", "Source_Mechanism")

mhd[, varstofactor] <- lapply(mhd[, varstofactor], factor)

summary(mhd$Code_Total)

#for total counts and rates
mhd$Hom_Total[mhd$Code_Total == "UNODC Estimate"] <- NA
mhd$Hom_Total[mhd$Code_Total == "ESTIMATE"] <- NA
mhd$Rate_Total[mhd$Code_Total == "UNODC Estimate"] <- NA
mhd$Rate_Total[mhd$Code_Total == "ESTIMATE"] <- NA

#for gender counts
summary(mhd$Source_Sex)
mhd$HomM[mhd$Source_Sex=="GHD Estimate"] <- NA
mhd$HomF[mhd$Source_Sex=="GHD Estimate"] <- NA

#for age - no estimated data were found
summary(mhd$Source_Age)
summary(mhd$Source_Mechanism)

View(mhd)

####Given the decision of Oman to withdraw the data for some years, we need to exclude it as well

mhd$Hom_Total[mhd$Code_Total=="NSO" & mhd$`UNODC Name` == "Oman"] <- NA

#We produce new file
Homicide_Selected <- 
  mhd %>%
  as_tibble() %>%
  #filter(!`UNODC Name` == c("World", "Africa", "Americas", "Asia", "Europe", "Oceania")) %>% #we dont need this line here because mhd doesnt have these values in the UNODC Name
  select(`UNODC Name`, Year, Region, Subregion, #these will remain as columns. all below will be grouped within variable Indicator
         Hom_Total, 
         #Rate_Total, 
         Pop, 
         PopM, 
         PopF, 
         HomM, 
         HomF, 
         PopM_0_14, 
         PopM_15_29,
         PopM_30_44,
         PopM_45_59,
         PopM_60_,
         PopF_0_14,
         PopF_15_29,
         PopF_30_44,
         PopF_45_59,
         PopF_60_, 
         HomM_0_14,
         HomM_15_29,
         HomM_30_44,
         HomM_45_59,
         HomM_60_,
         HomF_0_14,
         HomF_15_29,
         HomF_30_44,
         HomF_45_59,
         HomF_60_,
         Hom_Fire,
         Hom_Sharp,
         Hom_OthM,
         Hom_UnkM,
         Hom_Fire_M,
         Hom_Fire_F,
         Hom_Sharp_M,
         Hom_Sharp_F,
         Hom_OthM_M,
         Hom_OthM_F,
         Hom_UnkM_M,
         Hom_UnkM_F) %>%  
  #need also to produce rates by mechanism and gender
  mutate(`Homicide Rate` = Hom_Total/Pop *100000, #we need to calculate a new rate since the values in excel file are incorrect
         `Male Homicide Rate` = HomM/PopM * 100000, 
         `Female Homicide Rate` = HomF/PopF * 100000,
         `Male 0-14 Rate` =  HomM_0_14/PopM_0_14 * 100000, 
         `Male 15-29 Rate` =  HomM_15_29/PopM_15_29 * 100000,
         `Male 30-44 Rate` =  HomM_30_44/PopM_30_44 * 100000,
         `Male 45-59 Rate` =  HomM_45_59/PopM_45_59 * 100000,
         `Male 60 or more Rate` =  HomM_60_/PopM_60_ * 100000,
         `Female 0-14 Rate` =  HomF_0_14/PopF_0_14 * 100000, 
         `Female 15-29 Rate` =  HomF_15_29/PopF_15_29 * 100000,
         `Female 30-44 Rate` =  HomF_30_44/PopF_30_44 * 100000,
         `Female 45-59 Rate` =  HomF_45_59/PopF_45_59 * 100000,
         `Female 60 or more Rate` =  HomF_60_/PopF_60_ * 100000,
         `Firearms Rate` = Hom_Fire/Pop * 100000,
         `Sharp Objects Rate` = Hom_Sharp/Pop * 100000,
         `Other Mechanism Rate` = Hom_OthM/Pop * 100000,
         `Unknown Mechanism Rate` = Hom_UnkM/Pop * 100000,
         `Male Homicide Rate by Firearms` = Hom_Fire_M/PopM * 100000,
         `Female Homicide Rate by Firearms` = Hom_Fire_F/PopF * 100000,
         `Male Homicide Rate by Shapr Objects` = Hom_Sharp_M/PopM * 100000,
         `Female Homicide Rate by Shapr Objects` = Hom_Sharp_F/PopF * 100000,
         `Male Homicide Rate by Other Mechanisms` = Hom_OthM_M/PopM * 100000,
         `Female Homicide Rate by Other Mechanisms` = Hom_OthM_F/PopF * 100000,
         `Male Homicide Rate by Unknown Mechanisms` = Hom_UnkM_M/PopM * 100000,
         `Female Homicide Rate by Unknown Mechanisms` = Hom_UnkM_F/PopF * 100000) %>%
  rename(Country = `UNODC Name`, 
         `Homicide Total` = Hom_Total, 
         #`Homicide Rate` = Rate_Total, we cannot use the var Rate_Total since it is not correct in many cases
         `Male Homicide Count` = HomM, 
         `Female Homicide Count` = HomF,
         `Male 0-14 Count` = HomM_0_14,
         `Male 15-29 Count` = HomM_15_29,
         `Male 30-44 Count` = HomM_30_44,
         `Male 45-59 Count` = HomM_45_59,
         `Male 60 or more Count` = HomM_60_,
         `Female 0-14 Count` = HomF_0_14,
         `Female 15-29 Count` = HomF_15_29,
         `Female 30-44 Count` = HomF_30_44,
         `Female 45-59 Count` = HomF_45_59,
         `Female 60 or more Count` = HomF_60_,
         `Homicide by Firearms` = Hom_Fire,
         `Homicide by Sharp Objects` = Hom_Sharp,  
         `Homicide by Other Mechanisms` = Hom_OthM,
         `Homicide by Unknown Mechanisms` = Hom_UnkM,
         `Male Homicide by Firearms` = Hom_Fire_M,
         `Female Homicide by Firearms` = Hom_Fire_F,
         `Male Homicide by Shapr Objects` = Hom_Sharp_M,
         `Female Homicide by Shapr Objects` = Hom_Sharp_F,
         `Male Homicide by Other Mechanisms` = Hom_OthM_M,
         `Female Homicide by Other Mechanisms` = Hom_OthM_F,
         `Male Homicide by Unknown Mechanisms` = Hom_UnkM_M,
         `Female Homicide by Unknown Mechanisms` = Hom_UnkM_F) %>%
  select(-c(Pop, PopM, PopF,
            PopM_0_14, 
            PopM_15_29,
            PopM_30_44,
            PopM_45_59,
            PopM_60_,
            PopF_0_14,
            PopF_15_29,
            PopF_30_44,
            PopF_45_59,
            PopF_60_))

View(Homicide_Selected)

write.csv(Homicide_Selected, "Homicide_Selected.csv", na = "", row.names = F)



#####we need to add regional and subregional estimates to do a file for TS plot

#data with countries only
ts_plot_data_countries <-
  Homicide_Selected %>%
  select(Country, Year, Region, Subregion,
         `Homicide Rate`,   
         `Male Homicide Rate`, `Female Homicide Rate`,                      
         `Male 0-14 Rate`, `Male 15-29 Rate`, 
         `Male 30-44 Rate`, `Male 45-59 Rate`, `Male 60 or more Rate`,                       
         `Female 0-14 Rate`,  `Female 15-29 Rate`, `Female 30-44 Rate`,                         
         `Female 45-59 Rate`,`Female 60 or more Rate`,                    
         `Firearms Rate`, `Sharp Objects Rate`,                        
         `Other Mechanism Rate`,`Unknown Mechanism Rate`) %>%
  mutate_if(is.numeric, round, (4)) %>%
  rename(UNODC_Name = Country, 
         `Homicide_Rate`=`Homicide Rate`  ,   
         `Male_Homicide_Rate` = `Male Homicide Rate`, 
         `Female_Homicide_Rate` = `Female Homicide Rate`,                      
         `Male_0_14_Rate` = `Male 0-14 Rate`, 
         `Male_15_29_Rate` = `Male 15-29 Rate`, 
         `Male_30_44_Rate` = `Male 30-44 Rate`, 
         `Male_45_59_Rate` = `Male 45-59 Rate`, 
         `Male_60_or_more_Rate` = `Male 60 or more Rate`,                       
         `Female_0_14_Rate` = `Female 0-14 Rate`,  
         `Female_15_29_Rate` = `Female 15-29 Rate`, 
         `Female_30_44_Rate` = `Female 30-44 Rate`,                         
         `Female_45_59_Rate` = `Female 45-59 Rate`,
         `Female_60_or_more_Rate` = `Female 60 or more Rate`,                    
         `Firearms_Rate` = `Firearms Rate`, 
         `Sharp_Objects_Rate` = `Sharp Objects Rate`,                        
         `Other_Mechanism_Rate` = `Other Mechanism Rate`,
         `Unknown_Mechanism_Rate` = `Unknown Mechanism Rate`)

View(ts_plot_data_countries)


#regions and subregions

med <- read_csv("R/Master Estimates Data.csv")

med$Level <- as.factor(med$Level)

level <- c("Region", "Subregion", "World")

ts_plot_reg_subreg <- 
  med %>%
  filter(Level %in% level) %>%
  select(UNODC_Name,	Year,	Region,	Subregion, Estimate, 
         #need to add population statistics for producing rates
         Pop,	PopM,	PopF,	PopM_0_14,	PopM_15_29,	PopM_30_44,	PopM_45_59,	
         PopM_60_,	PopF_0_14,	PopF_15_29,	PopF_30_44,	PopF_45_59,	PopF_60_,
         #homicide vars
         HomM_Est, HomF_Est, 
         HomM_0_14_Est,	HomM_15_29_Est,	HomM_30_44_Est,	HomM_45_59_Est,	HomM_60_Est,	
         HomF_0_14_Est,	HomF_15_29_Est,	HomF_30_44_Est,	HomF_45_59_Est,	HomF_60_Est,
         Hom_Fire_Est,	Hom_Sharp_Est,	Hom_OthM_Est,	Hom_UnkM_Est) %>%
  mutate(Homicide_Rate = Estimate/Pop * 100000,
         Male_Homicide_Rate = HomM_Est/PopM * 100000,
         Female_Homicide_Rate = HomF_Est/PopF * 100000,
         `Male_0_14_Rate` =  HomM_0_14_Est/PopM_0_14 * 100000, 
         `Male_15_29_Rate` =  HomM_15_29_Est/PopM_15_29 * 100000,
         `Male_30_44_Rate` =  HomM_30_44_Est/PopM_30_44 * 100000,
         `Male_45_59_Rate` =  HomM_45_59_Est/PopM_45_59 * 100000,
         `Male_60_or_more_Rate` =  HomM_60_Est/PopM_60_ * 100000,
         `Female_0_14_Rate` =  HomF_0_14_Est/PopF_0_14 * 100000, 
         `Female_15_29_Rate` =  HomF_15_29_Est/PopF_15_29 * 100000,
         `Female_30_44_Rate` =  HomF_30_44_Est/PopF_30_44 * 100000,
         `Female_45_59_Rate` =  HomF_45_59_Est/PopF_45_59 * 100000,
         `Female_60_or_more_Rate` =  HomF_60_Est/PopF_60_ * 100000,
         Firearms_Rate = Hom_Fire_Est/Pop * 100000,
         Sharp_Objects_Rate = Hom_Sharp_Est/Pop * 100000,
         Other_Mechanism_Rate = Hom_OthM_Est/Pop * 100000,
         Unknown_Mechanism_Rate = Hom_UnkM_Est/Pop * 100000) %>%
  mutate_if(is.numeric, round, (4)) %>%
  select(-c(Pop,	PopM,	PopF,	PopM_0_14,	PopM_15_29,	PopM_30_44,	PopM_45_59,	
            PopM_60_,	PopF_0_14,	PopF_15_29,	PopF_30_44,	PopF_45_59,	PopF_60_, Estimate, HomM_Est, HomF_Est, 
            HomM_0_14_Est,	HomM_15_29_Est,	HomM_30_44_Est,	HomM_45_59_Est,	HomM_60_Est,	
            HomF_0_14_Est,	HomF_15_29_Est,	HomF_30_44_Est,	HomF_45_59_Est,	HomF_60_Est,
            Hom_Fire_Est,	Hom_Sharp_Est,	Hom_OthM_Est,	Hom_UnkM_Est)) 

View(ts_plot_reg_subreg)

#check that the names in two datasets match
x = names(ts_plot_data_countries)
y = names(ts_plot_reg_subreg)

identical(x,y)

ts_plot_data <-
  ts_plot_data_countries %>%
  bind_rows(ts_plot_reg_subreg)

#ts_plot_data$Region = factor(ts_plot_data$Region, levels = c())
#summary(ts_plot_data$Region)

View(ts_plot_data)

ts_plot_data <-
  ts_plot_data %>%
  mutate(Level = "Country")


subregs = unique(ts_plot_data$Subregion) #need to check names of subregs

ts_plot_data$Level[ts_plot_data$UNODC_Name == "World"] <- "World"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Africa"] <- "Region"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Americas"] <- "Region"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Asia"] <- "Region"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Europe"] <- "Region"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Oceania"] <- "Region"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Southern Asia"] <- "Subregion"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Southern Europe"] <- "Subregion"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Northern Africa"] <- "Subregion"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Polynesia"] <- "Subregion"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Middle Africa"] <- "Subregion"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Caribbean"] <- "Subregion"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "South America"] <- "Subregion"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Western Asia"] <- "Subregion"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Australia and New Zealand"] <- "Subregion"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Western Europe"] <- "Subregion"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Eastern Europe"] <- "Subregion"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Central America"] <- "Subregion"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Western Africa"] <- "Subregion"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Northern America"] <- "Subregion"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Southern Africa"] <- "Subregion"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "South-Eastern Asia"] <- "Subregion"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Eastern Africa"] <- "Subregion"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Northern Europe"] <- "Subregion"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Eastern Asia"] <- "Subregion"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Melanesia"] <- "Subregion"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Micronesia"] <- "Subregion"
ts_plot_data$Level[ts_plot_data$UNODC_Name == "Central Asia"] <- "Subregion"

ts_plot_data$Level = factor(ts_plot_data$Level, levels = c("World", "Region", "Subregion", "Country"))

summary(ts_plot_data$Level)

write.csv(ts_plot_data, "TS_plot_countries.csv", na = "", row.names = F)



####________________________________something new starts here
#create a new dataset
wide_selected_homicide <- 
  Homicide_Selected %>%
  gather(Indicator, Value, -c(Country, Year, Region, Subregion)) %>%
  spread(key = Year, value = Value) %>%     
  dplyr::arrange(Region, Subregion, Country) 

View(wide_selected_homicide)
#names <- unique(wide_selected_homicide$Indicator)                 
#names  

wide_selected_homicide$Indicator <-  factor(wide_selected_homicide$Indicator, ordered = T, 
                                            levels  = c("Homicide Total",
                                                         "Homicide Rate",
                                                        "Male Homicide Count",
                                                         "Female Homicide Count",
                                                         "Male Homicide Rate",
                                                         "Female Homicide Rate",
                                                         "Male 0-14 Count",
                                                         "Male 15-29 Count",
                                                         "Male 30-44 Count",
                                                         "Male 45-59 Count",
                                                         "Male 60 or more Count",
                                                         "Female 0-14 Count",
                                                         "Female 15-29 Count",
                                                         "Female 30-44 Count",
                                                         "Female 45-59 Count",
                                                         "Female 60 or more Count",
                                                         "Male 0-14 Rate",
                                                         "Male 15-29 Rate",
                                                         "Male 30-44 Rate",
                                                         "Male 45-59 Rate",
                                                         "Male 60 or more Rate",
                                                         "Female 0-14 Rate",
                                                         "Female 15-29 Rate",
                                                         "Female 30-44 Rate",
                                                         "Female 45-59 Rate",
                                                         "Female 60 or more Rate",
                                                        "Homicide by Firearms" ,
                                                        "Homicide by Sharp Objects" ,  
                                                        "Homicide by Other Mechanisms" ,
                                                        "Homicide by Unknown Mechanisms",
                                                        "Firearms Rate",
                                                        "Sharp Objects Rate",
                                                        "Other Mechanism Rate",   
                                                        "Unknown Mechanism Rate",
                                                        "Male Homicide by Firearms",
                                                        "Female Homicide by Firearms" ,
                                                        "Male Homicide by Shapr Objects" ,
                                                        "Female Homicide by Shapr Objects" ,
                                                        "Male Homicide by Other Mechanisms" ,
                                                        "Female Homicide by Other Mechanisms" ,
                                                        "Male Homicide by Unknown Mechanisms" ,
                                                        "Female Homicide by Unknown Mechanisms",
                                                        "Male Homicide Rate by Firearms" ,
                                                        "Female Homicide Rate by Firearms" ,
                                                        "Male Homicide Rate by Shapr Objects" ,
                                                        "Female Homicide Rate by Shapr Objects" ,
                                                        "Male Homicide Rate by Other Mechanisms" ,
                                                        "Female Homicide Rate by Other Mechanisms" ,
                                                        "Male Homicide Rate by Unknown Mechanisms" ,
                                                        "Female Homicide Rate by Unknown Mechanisms"))

####################### WE PRODUCE DATA FOR THE DATA TABLE BY COUNTRIES
Shiny_Data <- 
  wide_selected_homicide %>%
    plyr::arrange(Region, Subregion, Country, Indicator)


countries <- unique(Shiny_Data$Country)
countries

Shiny_Data$Country[Shiny_Data$Country == "Côte d'Ivoire"] <- "Cote d'Ivoire"
Shiny_Data$Country[Shiny_Data$Country == "Curaçao"] <- "Curacao"
Shiny_Data$Country[Shiny_Data$Country == "Réunion"] <- "Reunion"

View(Shiny_Data)

str(Shiny_Data)

#we need to round counts to 0 and rates to 1 decimals

#the current code does not reach this gole as it rounds all numeric values to 1 decimal
Shiny_Data <- 
  Shiny_Data %>%
    mutate_if(is.numeric, round, (1)) 

###this code reaches
Shiny_Data$rate = stringr::str_detect(Shiny_Data$Indicator, "(Rate){1}")
Shiny_Data$rate = as.factor(Shiny_Data$rate)
levels(Shiny_Data$rate) <- c("0", "1")

df <- split(Shiny_Data, Shiny_Data$rate)
View(df)

dffff <-
  df$`1` %>%
    mutate_if(is.numeric, round, 1)

df2222 <- 
  df$`0` %>%
  mutate_if(is.numeric, round, 0)
View(df2222)

shiny_dt <- 
  bind_rows(dffff, df2222) %>%
  plyr::arrange(Region, Subregion, Country, Indicator) %>%
  select(-rate)
  
  
View(shiny_dt)

str(shiny_dt$Indicator)
str(Shiny_Data$Indicator)

write.csv(shiny_dt, "Shiny_DT.csv", na = "", row.names = F)



############################################### DATA FOR COUNTRY MAP
cll <- read_excel("R/country long and lat.xlsx")
View(cll)

country_map <-
  Homicide_Selected %>%
  select(Country, Region, Subregion, Year,
         `Homicide Rate`, 
         `Male Homicide Rate`, 
         `Female Homicide Rate`,
         `Male 0-14 Rate`, 
         `Male 15-29 Rate`,
         `Male 30-44 Rate`,
         `Male 45-59 Rate`,
         `Male 60 or more Rate`,
         `Female 0-14 Rate`, 
         `Female 15-29 Rate`,
         `Female 30-44 Rate`,
         `Female 45-59 Rate`,
         `Female 60 or more Rate`,
         `Firearms Rate`,
         `Sharp Objects Rate`,
         `Other Mechanism Rate`#,
         #`Male Homicide Rate by Firearms`,
         #`Female Homicide Rate by Firearms`,
         #`Male Homicide Rate by Shapr Objects`,
         #`Female Homicide Rate by Shapr Objects`,
         #`Male Homicide Rate by Other Mechanisms`,
         #`Female Homicide Rate by Other Mechanisms`
         ) %>%
  left_join(cll, by = c("Country" =  "cn")) %>%
  tidyr::gather(key = Variable, value = Value, -c(Country, Region, Subregion, Year, code, lat, long)) 


country_map$Value <- as.numeric(country_map$Value)
country_map$Value <- round(country_map$Value, 1)

View(country_map)

write.csv(country_map, "Country_Map.csv", na = "", row.names = F)

###############DATA FOR TS PLOT - WE NEED TO MERGE FILE WITH REGIONS AND SUBREGIONS AND ACTUAL COUNTRY DATA (NEED TO BE UPDATED BECAUSE OF AFRICA)

med <- read_csv("R/Master Estimates Data.csv")

############DATA FOR DATA TABLE ON THE THIRD TAB (REGIONAL AND SUBREGIONAL) (NEED TO BE UPDATED BECAUSE OF AFRICA)
  

med <- read_csv("R/Master Estimates Data.csv")

med$Level <- as.factor(med$Level)

level <- c("Region", "Subregion", "World")

reg_subreg <- 
  med %>%
  filter(Level %in% level) %>%
  select(Level, UNODC_Name,	Year,	Region,	Subregion, Estimate, 
         #need to add population statistics for producing rates
         Pop,	PopM,	PopF,	PopM_0_14,	PopM_15_29,	PopM_30_44,	PopM_45_59,	
         PopM_60_,	PopF_0_14,	PopF_15_29,	PopF_30_44,	PopF_45_59,	PopF_60_,
         #homicide vars
         HomM_Est, HomF_Est, 
         HomM_0_14_Est,	HomM_15_29_Est,	HomM_30_44_Est,	HomM_45_59_Est,	HomM_60_Est,	
         HomF_0_14_Est,	HomF_15_29_Est,	HomF_30_44_Est,	HomF_45_59_Est,	HomF_60_Est,
         Hom_Fire_Est,	Hom_Sharp_Est,	Hom_OthM_Est,	Hom_UnkM_Est) %>%
  mutate(`Homicide Rate` = Estimate/Pop * 100000,
         `Male Homicide Rate` = HomM_Est/PopM * 100000,
         `Female Homicide Rate` = HomF_Est/PopF * 100000,
         `Male 0-14 Rate` =  HomM_0_14_Est/PopM_0_14 * 100000, 
         `Male 15-29 Rate` =  HomM_15_29_Est/PopM_15_29 * 100000,
         `Male 30-44 Rate` =  HomM_30_44_Est/PopM_30_44 * 100000,
         `Male 45-59 Rate` =  HomM_45_59_Est/PopM_45_59 * 100000,
         `Male 60 or more Rate` =  HomM_60_Est/PopM_60_ * 100000,
         `Female 0-14 Rate` =  HomF_0_14_Est/PopF_0_14 * 100000, 
         `Female 15-29 Rate` =  HomF_15_29_Est/PopF_15_29 * 100000,
         `Female 30-44 Rate` =  HomF_30_44_Est/PopF_30_44 * 100000,
         `Female 45-59 Rate` =  HomF_45_59_Est/PopF_45_59 * 100000,
         `Female 60 or more Rate` =  HomF_60_Est/PopF_60_ * 100000,
         `Firearms Rate` = Hom_Fire_Est/Pop * 100000,
         `Sharp Objects Rate` = Hom_Sharp_Est/Pop * 100000,
         `Other Mechanism Rate` = Hom_OthM_Est/Pop * 100000,
         `Unknown Mechanism Rate` = Hom_UnkM_Est/Pop * 100000) %>%
  rename(`Region/Subregion` = UNODC_Name,
         `Estimated Homicide Count` = Estimate,
         `Estimated Homicide Count: Male` = HomM_Est,
         `Estimated Homicide Count: Female` = HomF_Est,
         `Estimate: Male 0-14 Count` = HomM_0_14_Est,
         `Estimate: Male 15-29 Count` = HomM_15_29_Est,
         `Estimate: Male 30-44 Count` = HomM_30_44_Est,
         `Estimate: Male 45-59 Count` = HomM_45_59_Est,
         `Estimate: Male 60 or more Count` = HomM_60_Est,
         `Estimate: Female 0-14 Count` = HomF_0_14_Est,
         `Estimate: Female 15-29 Count` = HomF_15_29_Est,
         `Estimate: Female 30-44 Count` = HomF_30_44_Est,
         `Estimate: Female 45-59 Count` = HomF_45_59_Est,
         `Estimate: Female 60 or more Count` = HomF_60_Est,
         `Estimated Homicide Count: Firearms` = Hom_Fire_Est,
         `Estimated Homicide Count: Sharp Objects` = Hom_Sharp_Est,  
         `Estimated Homicide Count: Other Mechanisms` = Hom_OthM_Est,
         `Estimated Homicide Count: Unknown Mechanisms` = Hom_UnkM_Est) %>%
  select(-c(Pop,	PopM,	PopF,	PopM_0_14,	PopM_15_29,	PopM_30_44,	PopM_45_59,	
            PopM_60_,	PopF_0_14,	PopF_15_29,	PopF_30_44,	PopF_45_59,	PopF_60_)) 
  
reg_subreg$Level <- factor(reg_subreg$Level, ordered = T, levels = c("World", "Region", "Subregion")) 

View(reg_subreg)

reg_subreg_wide <- 
  reg_subreg %>%
  select(-c(Region, Subregion)) %>%
  gather(Indicator, Value, -c(Level, `Region/Subregion`, Year)) %>%
  spread(key = Year, value = Value) 
  #%>% dplyr::arrange(Region, Subregion, Country)

View(reg_subreg_wide)

factor_levels <- unique(reg_subreg_wide$Indicator)
factor_levels

#we need to reorder factor levels of indicator
reg_subreg_wide$Indicator <-  factor(reg_subreg_wide$Indicator, ordered = T, 
                                            levels  = c("Estimated Homicide Count",
                                                        "Homicide Rate" ,
                                                        "Estimated Homicide Count: Male",
                                                        "Estimated Homicide Count: Female",
                                                        "Male Homicide Rate",
                                                        "Female Homicide Rate",
                                                        "Estimate: Male 0-14 Count",                   
                                                        "Estimate: Male 15-29 Count",      
                                                        "Estimate: Male 30-44 Count",                  
                                                        "Estimate: Male 45-59 Count", 
                                                        "Estimate: Male 60 or more Count",
                                                        "Male 0-14 Rate",                              
                                                        "Male 15-29 Rate",          
                                                        "Male 30-44 Rate",                             
                                                        "Male 45-59 Rate",
                                                        "Male 60 or more Rate", 
                                                        "Estimate: Female 0-14 Count",                  
                                                        "Estimate: Female 15-29 Count",   
                                                        "Estimate: Female 30-44 Count",   
                                                        "Estimate: Female 45-59 Count",                
                                                        "Estimate: Female 60 or more Count",  
                                                        "Female 0-14 Rate",                            
                                                        "Female 15-29 Rate",      
                                                        "Female 30-44 Rate",                           
                                                        "Female 45-59 Rate",        
                                                        "Female 60 or more Rate",
                                                        "Estimated Homicide Count: Firearms",            
                                                        "Estimated Homicide Count: Other Mechanisms", 
                                                        "Estimated Homicide Count: Sharp Objects",     
                                                        "Estimated Homicide Count: Unknown Mechanisms",
                                                        "Firearms Rate",
                                                        "Other Mechanism Rate",                        
                                                        "Sharp Objects Rate",                           
                                                        "Unknown Mechanism Rate")) 

reg_subreg_wide <-
  reg_subreg_wide %>%
  plyr::arrange(Level, `Region/Subregion`, Indicator)

View(reg_subreg_wide)

#Now we need to round

###this code reaches
reg_subreg_wide$rate = stringr::str_detect(reg_subreg_wide$Indicator, "(Rate){1}")
reg_subreg_wide$rate = as.factor(reg_subreg_wide$rate)
levels(reg_subreg_wide$rate) <- c("0", "1")

df_reg <- split(reg_subreg_wide, reg_subreg_wide$rate)
View(df_reg)

dffff <-
  df_reg$`1` %>%
  mutate_if(is.numeric, round, 1)
View(dffff)

df2222 <- 
  df_reg$`0` %>%
  mutate_if(is.numeric, round, 0)
View(df2222)

shiny_dt_reg_subreg <- 
  bind_rows(dffff, df2222) %>%
  plyr::arrange(Level, `Region/Subregion`, Indicator) %>%
  select(-rate)

View(shiny_dt_reg_subreg)

write.csv(shiny_dt_reg_subreg, "shiny_reg_dt.csv", row.names = F, na = "")


###########regional data for maps

#create small dataset wiht the regional longs and lats


##update of the file is not finished because I still have to insert long and lat values for subregions

regions_subregions <- c("Africa", "Americas", "Asia", "Europe", "Oceania", "Caribbean", "Central America",           
             "Northern America",  "South America", "Central Asia", "Eastern Asia", "South-Eastern Asia", 
             "Southern Asia",  "Western Asia",  "Eastern Europe", "Northern Europe",          
             "Southern Europe", "Western Europe", "Australia and New Zealand", "Melanesia",                
             "Micronesia", "Polynesia")

lat <- c(4, 17,52, 48, -24)
long <- c(20, -95, 91, 8, 134)

contll <- data.frame(regions, lat, long)

varnamesreg <- names(reg_subreg)
varnamesreg

reg_subreg$Region <- as.character(reg_subreg$Region)
contll$regions <- as.character(contll$regions)

reg_subreg$Level <- as.character(reg_subreg$Level)

library(readr)
SubRegionCoord <- read_csv("SubRegionCoord.csv")

regmap_data <-
  reg_subreg %>%
  filter(Level %in% c("Region", "Subregion"), Year>2004) %>%
  select(`Region/Subregion`, Year, `Estimated Homicide Count`,
         `Estimated Homicide Count: Male`, `Estimated Homicide Count: Female`,
         `Estimate: Male 0-14 Count`, `Estimate: Male 15-29 Count`, 
         `Estimate: Male 30-44 Count`, `Estimate: Male 45-59 Count`, `Estimate: Male 60 or more Count`, 
         `Estimate: Female 0-14 Count`, `Estimate: Female 15-29 Count`, 
         `Estimate: Female 30-44 Count`, `Estimate: Female 45-59 Count`, `Estimate: Female 60 or more Count`, 
         `Estimated Homicide Count: Firearms`, `Estimated Homicide Count: Sharp Objects`,
         `Estimated Homicide Count: Other Mechanisms`, `Estimated Homicide Count: Unknown Mechanisms`) %>%
  drop_na(`Estimated Homicide Count`) %>%
  left_join(SubRegionCoord, by = c("Region/Subregion" = "SubRegion"))


regsubreg_list = unique(regmap_data$`Region/Subregion`)

View(regmap_data)

write.csv(regmap_data, "regmap.csv", row.names = F, na = "")







###################CRIMINAL JUSTICE SYSTEM RESPONSE

cjsr <- 
  mhd %>%
  filter(Year >= 2003) %>%
  select(`UNODC Name`, Year, Region, Subregion, #Hom_Total,
         Source_Formal,
         Formal_Hom,
         #Source_Prosecute,
         #Prosecute_Hom,
         Source_Convict,
         Convict_Hom,
         Source_Formal_Sex,
         Formal_HomM,
         Formal_HomF,
         #Source_Prosecute_Sex,
         #Prosecute_HomM,
         #Prosecute_HomF,
         Source_Convict_Sex,
         Convict_HomM,
         Convict_HomF,
         Source_Formal_Age,
         Formal_HomM_0_14,
         Formal_HomM_0_17,
         Formal_HomM_18_24,
         Formal_HomM_25_29,
         Formal_HomM_30_44,
         Formal_HomM_45_59,
         Formal_HomM_60_,
         Formal_HomF_0_14,
         Formal_HomF_0_17,
         Formal_HomF_18_24,
         Formal_HomF_25_29,
         Formal_HomF_30_44,
         Formal_HomF_45_59,
         Formal_HomF_60_,
         Source_Formal_Citizen,
         Formal_Hom_Citizen,
         Formal_Hom_Foreign,
         Source_Formal_Citizen_Sex,
         Formal_Hom_Citizen_M,
         Formal_Hom_Citizen_F,
         Formal_Hom_Foreign_M,
         Formal_Hom_Foreign_F,
         Source_Intoxication,
         Intox_Not_Hom,
         Alcohol_Hom,
         Illicit_Hom,
         Controlled_Hom,
         Combination_Hom,
         Exposure_Unk_Hom #,
         #Source_Cleared,
         #Hom_Cleared
         ) 

cjsr$Year <- factor(cjsr$Year)

nums <- unlist(lapply(cjsr, is.numeric)) 
numvars <- cjsr[, nums]

numvars$new =  apply(numvars, 1,  sum, na.rm = T)

View(numvars)

cjsr <- cbind(cjsr, numvars$new)

View(cjsr)

cjsr$`numvars$new`[cjsr$`numvars$new` == 0] <- NA

cjsr1 <-
  cjsr %>%
  group_by(`UNODC Name`) %>%
  drop_na(`numvars$new`) %>%
  rename(Country = `UNODC Name`) %>%
  select(-c(`numvars$new`, Source_Formal, Source_Prosecute, Source_Convict, Source_Formal_Sex, Source_Prosecute_Sex, 
            Source_Convict_Sex, Source_Formal_Age, 
            Source_Formal_Citizen, Source_Formal_Citizen_Sex, Source_Intoxication, Source_Cleared))


View(cjsr1)

states <- unique(cjsr1$`UNODC Name`)

cjsr_wide <-
  cjsr1 %>%
  gather(Indicator, Value, -c(Country, Region, Subregion, Year)) %>%
  spread(key = Year, value = Value) %>%
  mutate_if(is.numeric, round, 0)



# need to order factor Indicator
factorlevels <- unique(cjsr_wide$Indicator)
factorlevels

cjsr_wide$Indicator <- factor(cjsr_wide$Indicator, ordered = TRUE, 
                              levels = c("Formal_Hom","Formal_HomM","Formal_HomF",
                                        "Formal_HomM_0_14","Formal_HomM_0_17","Formal_HomM_18_24", 
                                         "Formal_HomM_25_29", "Formal_HomM_30_44",  "Formal_HomM_45_59", "Formal_HomM_60_",
                                         "Formal_HomF_0_14",  "Formal_HomF_0_17", "Formal_HomF_18_24", "Formal_HomF_25_29",  "Formal_HomF_30_44",   
                                         "Formal_HomF_45_59", "Formal_HomF_60_",
                                         "Formal_Hom_Citizen", "Formal_Hom_Foreign", "Formal_Hom_Citizen_M", "Formal_Hom_Citizen_F", 
                                        "Formal_Hom_Foreign_M","Formal_Hom_Foreign_F",  
                                        "Convict_Hom", "Convict_HomM", "Convict_HomF", 
                                        "Prosecute_Hom", "Prosecute_HomM",  "Prosecute_HomF",       
                                        "Intox_Not_Hom", "Alcohol_Hom", "Illicit_Hom", "Controlled_Hom", "Combination_Hom", "Exposure_Unk_Hom", 
                                        "Hom_Cleared" ))
  
cjsr_wide <-
  cjsr_wide %>%
  plyr::arrange(Region, Subregion, Country, Indicator)

View(cjsr_wide)

write.csv(cjsr_wide, "cjsr.csv", row.names = F, na = "")

##########################################################################
##################Data for interactive regional graphs with percentages
####################################################################

#graph of percentages globally and regionally
reg_graph =
  med %>%
  select(Level, UNODC_Name, Year, Estimate, HomM_Est, HomF_Est, 
         HomM_0_14_Est,	HomM_15_29_Est,	HomM_30_44_Est,	HomM_45_59_Est,	HomM_60_Est,	
         HomF_0_14_Est,	HomF_15_29_Est,	HomF_30_44_Est,	HomF_45_59_Est,	HomF_60_Est,
         Hom_Fire_Est,	Hom_Sharp_Est,	Hom_OthM_Est,	Hom_UnkM_Est) %>%
  filter(Level %in% c("Region", "World"), Year > 2004) %>%
  mutate(Male = HomM_Est/Estimate*100, Female = HomF_Est/Estimate*100,
         `Male 0-14` = HomM_0_14_Est/Estimate*100, `Male 15-29` = HomM_15_29_Est/Estimate*100, 
         `Male 30-44` = HomM_30_44_Est/Estimate*100, `Male 45-59` = HomM_45_59_Est/Estimate*100, 
         `Male 60 and More` =  HomM_60_Est/Estimate,
         `Female 0-14` = HomF_0_14_Est/Estimate*100, `Female 15-29` = HomF_15_29_Est/Estimate*100, 
         `Female 30-44` = HomF_30_44_Est/Estimate*100, `Female 45-59` = HomF_45_59_Est/Estimate*100, 
         `Female 60 and More` =  HomF_60_Est/Estimate,
         Firearms = Hom_Fire_Est/(Hom_Fire_Est +	Hom_Sharp_Est +	Hom_OthM_Est),
         `Sharp Objects/Knives` = Hom_Sharp_Est/(Hom_Fire_Est +	Hom_Sharp_Est +	Hom_OthM_Est),
         `Other Mechanism` = Hom_OthM_Est/(Hom_Fire_Est +	Hom_Sharp_Est +	Hom_OthM_Est)) %>%
  select(-c(Level, Estimate, HomM_Est, HomF_Est, 
            HomM_0_14_Est,	HomM_15_29_Est,	HomM_30_44_Est,	HomM_45_59_Est,	HomM_60_Est,	
            HomF_0_14_Est,	HomF_15_29_Est,	HomF_30_44_Est,	HomF_45_59_Est,	HomF_60_Est,
            Hom_Fire_Est,	Hom_Sharp_Est,	Hom_OthM_Est,	Hom_UnkM_Est)) %>%
  gather(Indicator, value, -c(UNODC_Name, Year)) %>%
  arrange(UNODC_Name)

View(reg_graph)

write.csv(reg_graph, "reg graph.csv", row.names = F)

#graph of world by regions
world =
  med %>%
  filter(Level == "World", Year>2004) %>%
  select(UNODC_Name, Year, 
         Estimate, 
         HomM_Est, HomF_Est, 
         HomM_0_14_Est,	HomM_15_29_Est,	HomM_30_44_Est,	HomM_45_59_Est,	HomM_60_Est,	
         HomF_0_14_Est,	HomF_15_29_Est,	HomF_30_44_Est,	HomF_45_59_Est,	HomF_60_Est,
         Hom_Fire_Est,	Hom_Sharp_Est,	Hom_OthM_Est,	Hom_UnkM_Est) %>%
  rename(World = UNODC_Name)

View(world)

world_reg_graph =
  med %>%
  filter(Level == "Region", Year >2004) %>%
  select(UNODC_Name, Year, 
         Estimate, 
         HomM_Est, HomF_Est, 
         HomM_0_14_Est,	HomM_15_29_Est,	HomM_30_44_Est,	HomM_45_59_Est,	HomM_60_Est,	
         HomF_0_14_Est,	HomF_15_29_Est,	HomF_30_44_Est,	HomF_45_59_Est,	HomF_60_Est,
         Hom_Fire_Est,	Hom_Sharp_Est,	Hom_OthM_Est,	Hom_UnkM_Est) %>%
  left_join(world, by = "Year") %>%
  mutate(`Homicide Shares` = Estimate.x/Estimate.y*100,
         `Male Share` = HomM_Est.x/HomM_Est.y*100,
         `Female Share` = HomF_Est.x/HomF_Est.y*100,
         `Male 0-14`  = HomM_0_14_Est.x/HomM_0_14_Est.y*100,
         `Male 15-29`  = HomM_15_29_Est.x/HomM_15_29_Est.y*100,
         `Male 30-44` = HomM_30_44_Est.x/HomM_30_44_Est.y*100,
         `Male 45-59` = HomM_45_59_Est.x/HomM_45_59_Est.y*100,
         `Male 60 and more` = HomM_60_Est.x/HomM_60_Est.y*100,
         `Female 0-14`  = HomF_0_14_Est.x/HomF_0_14_Est.y*100,
         `Female 15-29`  = HomF_15_29_Est.x/HomF_15_29_Est.y*100,
         `Female 30-44` = HomF_30_44_Est.x/HomF_30_44_Est.y*100,
         `Female 45-59` = HomF_45_59_Est.x/HomF_45_59_Est.y*100,
         `Female 60 and more` = HomF_60_Est.x/HomF_60_Est.y*100,
         `Firearms Share` = Hom_Fire_Est.x/Hom_Fire_Est.y*100,
         `Sharp Objects Share` = Hom_Sharp_Est.x/Hom_Sharp_Est.y*100,
         `Other Mechanism Share` = Hom_OthM_Est.x/Hom_OthM_Est.y*100) %>%
  select(c(1,2,38:53)) %>%
  mutate_if(is.numeric, round, (1)) %>%
  gather(Indicator, Value, -c(UNODC_Name, Year))

View(world_reg_graph)

write.csv(world_reg_graph, "regional shares of global counts.csv", row.names = F)

gfd <- colSums(world_reg_graph[, -c(1,2)])




##################IP and IPFM statistics



################################################
####code for METH ANNEX######################
############################################

ma <-
  Homicide_Selected %>%
  drop_na(`Homicide Total`) %>%
  group_by(Country) %>%
  summarise(share = n_distinct(Year)) 

%>%
  mutate(Share_ord = recode_factor(share,   
              1:10= "low", 11:20 = "medium",  
              21:28 = "high", .ordered = TRUE)))  
#/28*100) %>%
  #mutate_if(is.numeric, round, (1))

View(ma)

ma$share_cat = cut(ma$share, breaks = c(0,10,20, Inf))
summary(ma$share_cat)

sum(ma$share > 20)
sum(ma$share <= 10 )

#____________________TRASH

datnas <- subset(cjsr, !is.na(numnames))

cjsr$new = cjsr$Formal_Hom + Prosecute_Hom + cjsr$Convict_Hom + cjsr$Hom_Cleared + Formal_HomM + Formal_HomF +  
  Prosecute_HomM + Prosecute_HomF + Formal_HomM_0_14 +
  Formal_HomM_0_17 +
  Formal_HomM_18_24 +
  Formal_HomM_25_29 +
  Formal_HomM_30_44 +
  Formal_HomM_45_59 +
  Formal_HomM_60_ +
  Formal_HomF_0_14 +
  Formal_HomF_0_17 +
  Formal_HomF_18_24 +
  Formal_HomF_25_29 +
  Formal_HomF_30_44 +
  Formal_HomF_45_59 +
  Formal_HomF_60_ +
  Formal_Hom_Citizen +
  Formal_Hom_Foreign +
  Formal_Hom_Citizen_M +
  Formal_Hom_Citizen_F +
  Formal_Hom_Foreign_M +
  Formal_Hom_Foreign_F +
  Intox_Not_Hom +
  Alcohol_Hom +
  Illicit_Hom +
  Controlled_Hom+
  Combination_Hom +
  Exposure_Unk_Hom +
  Hom_Cleared




numvars <- cjsr[, nums]
View(numvars)

numvars$new =  apply(numvars, 1,  sum, na.rm = T)

numvars$new  = rowSums(numvars)

View(numvars)

#we need to remove 

  tidyr::drop_na(Source_Formal, Source_Prosecute, Source_Convict, Source_Formal_Sex, Source_Prosecute_Sex, Source_Convict_Sex, Source_Formal_Age, 
                 Source_Formal_Citizen, Source_Formal_Citizen_Sex, Source_Intoxication, Source_Cleared)

View(cjsr)





