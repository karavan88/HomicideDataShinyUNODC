ui <- navbarPage(title = "Homicide Dataset 2019",
                 #windowTitle = "UNODC Global Study on Homicide 2019",
                 
                 
                 theme = shinytheme("cerulean"),
                 
                 ###############FIRST BAR
                 tabPanel("Homicide Map", 
                          #set up style for absolute panel
                          tags$head(tags$style(
                            HTML('
                                 #input_date_control {background-color: white; opacity: 0.75;}
                                 #sel_date {background-color: black;;}')
                            )),
                          
                          
                          h4("Map of homicide in countries worldwide"), 
                          #br(),
                          htmlOutput("textformap"),
                          
                          leafletOutput("homicide_map", width = "100%", height = 650),
                          downloadButton(outputId = "savemap", label = "Download Map"),
                          
                          
                          absolutePanel(class = "panel panel-default", id = "input_date_control",
                                        #id = "controls",        
                                        fixed = TRUE, cursor = "move",
                                        draggable = TRUE, top = "210", left = "auto", right = "20", bottom = "auto",
                                        width = 330, height = "auto",
                                        
                                        h2("User Inputs"),
                                        
                                        selectInput(inputId = "IndicatorMap",
                                                    label = "Select Indicator:",
                                                    choices =  list("Homicide Rate", "Sex" = Gender, "Male by Age" = Male_by_Age, 
                                                                    "Female by Age" = Female_by_Age, "Mechanism" = Mechanism),
                                                    selected = "Homicide Rate"),
                                        
                                        sliderInput(inputId = "sliderMapYear", 
                                                    label = "Select Year:",
                                                    min = 1990, 
                                                    max = 2017, 
                                                    sep = "",
                                                    step = 1,
                                                    value = c(2015))
                                        
                          ) #closes absolute panel
                          
                          
                          
                          ), #closes tab 1
                 
                 #####################SECOND BAR                 
                 tabPanel("National Data",
                          #we need to customize styles
                          tags$head(tags$style(".checkbox-inline {margin: 0 !important;}")), #this code aligns years
                          sidebarLayout(
                            sidebarPanel(
                              h3('User Inputs'),
                              selectInput(inputId = "Country",
                                          label = "Select Country/Countries:",
                                          choices = c("All", countries), 
                                          multiple = TRUE,         
                                          selected = "All"), 
                              
                              selectInput(inputId = "Indicator",
                                          label = "Select Indicator(s):",
                                          choices = list(Homicide = homicide, "Sex" = gender_hom, "Sex and Age" = gender_age_hom, "Mechanism" = mech_hom, "Mechanism and Sex" = mech_gender_hom), 
                                          multiple = TRUE,
                                          selected = c("Homicide Total Count", "Homicide Rate")),
                              
                              tags$div(align = 'left', 
                                       class = 'multicol', 
                                       checkboxGroupInput(inputId = "YearVar",
                                                          label = "Select Year(s):",
                                                          choices = c(1990:2017),
                                                          selected = c(1990, 2000, 2010, 2017),
                                                          inline = T)),
                              
                              
                              downloadButton(outputId = "download_country_data", 
                                             label = "Download Selected Data")
                            ),
                            
                            mainPanel(
                              h4("Homicide Data by Countries"),
                              #br(),
                              htmlOutput("textforcountrydataexp"),
                              br(),
                              DT::dataTableOutput("datatab") 
                            ) #closes the main panel
                            
                          ) #closes sidebarLayout
                 ), #closes the tabpanel 2
                 
                 
                 
                 ###########THIRD BAR
                 tabPanel("Regional Estimates",
                          
                          mainPanel(
                            tabsetPanel(
                              tabPanel('Homicide Map',
                                       tags$head(tags$style(
                                         HTML('#input_controls {background-color: lightblue; opacity: 0.95;}
                                              #sel_date {background-color: black;;}')
                                         )), #closes tags$Head
                                       
                                       h4("Map of Regional Homicide Estimates"),
                                       leafletOutput("regional_map", width = "100%", height = 440),
                                       
                                       absolutePanel(class = "panel panel-default", id = "input_controls",
                                                     fixed = TRUE, cursor = "move",
                                                     draggable = TRUE, top = "200", left = "auto", right = "20", bottom = "auto",
                                                     width = 330, height = "auto",
                                                     
                                                     h3("User Inputs"),
                                                     tags$div(class="header", checked=NA,
                                                              tags$p("The map allows for visualization of one variable or several variables 
                                                                     that provide disaggregation of total homicide by categories. If more than one variable 
                                                                     is selected, for a meaningful visualization they should belong to the same 
                                                                     group (e.g., homicide by sex, age, or mechanism).")),
                                                     
                                                     
                                                     selectInput(inputId = "EstIndicatorMap",
                                                                 label = "Select Indicator(s):",
                                                                 choices = list("Estimated Homicide Count",
                                                                                "Homicide by Sex"  = c("Estimated Homicide Count: Male", 
                                                                                                       "Estimated Homicide Count: Female"),
                                                                                "Homicide by Mechanism"  = c("Estimated Homicide Count: Firearms", 
                                                                                                             "Estimated Homicide Count: Sharp Objects", 
                                                                                                             "Estimated Homicide Count: Other Mechanisms", 
                                                                                                             "Estimated Homicide Count: Unknown Mechanisms")),
                                                                 multiple = T) 
                                                              ) #close absolute panel
                                       
                                         ), #closes the tabsetpanel
                              
                              tabPanel("Estimated Data",
                                       h4('Data on Regional and Subregional Estimates'),
                                       
                                       DT::dataTableOutput("regdatatab"),
                                       
                                       absolutePanel(class = "panel panel-default", id = "input_controls",
                                                     fixed = TRUE, cursor = "move",
                                                     draggable = TRUE, top = "200", left = "auto", right = "20", bottom = "auto",
                                                     width = 310, height = "auto",
                                                     
                                                     h3("User Inputs"),               
                                                     selectInput(inputId = "Region",
                                                                 label = "Select Region/Subregion:",
                                                                 choices = list("All", "World", "Regions" = c("Africa", "Americas", "Asia", "Europe", "Oceania"), 
                                                                                "Subregions" = subregions), #closes list
                                                                 multiple = TRUE,
                                                                 selected = "All"), 
                                                     
                                                     selectInput(inputId = "RegIndicator",
                                                                 label = "Select Indicator(s):",
                                                                 choices = reg_vars, 
                                                                 multiple = TRUE,
                                                                 selected = c("Estimated Homicide Count", "Homicide Rate")),
                                                     
                                                     tags$div(align = 'left', 
                                                              class = 'multicol', checkboxGroupInput(inputId = "RegYearVar",
                                                                                                     label = "Select Year(s):",
                                                                                                     choices = c(1990:2017),
                                                                                                     selected = c(1990, 2000, 2010, 2017),
                                                                                                     inline = T)),
                                                     
                                                     downloadButton(outputId = "download_reg_data", 
                                                                    label = "Download Selected Data") 
                                       ) #closes absolute panel
                              ) #closes tabPanel on estimated data
                              
                            ) #closes the tab set panel
                            ) # closes the main panel
                              
                            ), #closes the third tab 
                 
                 ##########FOURTH TAB
                 tabPanel("Time Series Trends",
                          sidebarLayout(
                            sidebarPanel(
                              
                              h3('User Inputs'),
                              selectInput(inputId = "y", 
                                          label = "Select Indicator:",
                                          choices = list("Homicide Rate" =  "Homicide_Rate", 
                                                         "Sex" = c("Male Homicide Rate" = "Male_Homicide_Rate", "Female Homicide Rate" = "Female_Homicide_Rate"),
                                                         "Male Age Groups" = c("Male 0-14 Rate" = "Male_0_14_Rate", "Male 15-29 Rate" = "Male_15_29_Rate",  
                                                                               "Male 30-44 Rate" = "Male_30_44_Rate", "Male 45-59 Rate" = "Male_45_59_Rate",     
                                                                               "Male 60 or more Rate" = "Male_60_or_more_Rate"),
                                                         "Female Age Groups" = c("Female 0-14 Rate" = "Female_0_14_Rate",  "Female 15-29 Rate" = "Female_15_29_Rate",
                                                                                 "Female 30-44 Rate" = "Female_30_44_Rate", "Female 45-59 Rate" = "Female_45_59_Rate", 
                                                                                 "Female 60 or more Rate" = "Female_60_or_more_Rate"),
                                                         "Mechanism" = c("Firearms Rate" = "Firearms_Rate", "Sharp Objects Rate" = "Sharp_Objects_Rate", "Other Mechanism Rate" = "Other_Mechanism_Rate")),
                                          selected = "Homicide Rate"),
                              
                              # Select regions to plot
                              selectInput(inputId = "Country_Region", 
                                          label = "Select Country/Region:",
                                          choices = list("World", 
                                                         "Regions" = c("Africa", "Americas", "Asia", "Europe", "Oceania"), #####to add africa later
                                                         "Subregions" = subregions, 
                                                         "Countries" = ts_countries),  
                                          multiple = TRUE,
                                          selected = "World"),
                              
                              h3("Year range"),    # Third level header: Years
                              
                              sliderInput(inputId = "slider", 
                                          label = "Select Years Range:", #check english
                                          min = 1990, 
                                          max = 2017, 
                                          sep = "",
                                          step = 1,
                                          value = c(1990, 2017)) 
                              
                            ), #closes sidebar panel
                            mainPanel(
                              h4("Time Series Chart"),
                              plotlyOutput("timeSeries")
                            ) #closes main panel
                          ) #closes sidebar Layout
                          
                 ), #closes tabPanel
                 #use menu 
                 navbarMenu("Other Data",
                            tabPanel("Intimate Partner/Family Related Homicide",
                                     h4("Data on Homicide by Intimate Partner/Family Member"),
                                     dataTableOutput("ipfm")),
                            tabPanel("Crime-Related Homicide",
                                     h4("Data on Homicide by Crime-Related Situational Context linked to Gangs, Organized Crime, or Robbery"),
                                     dataTableOutput("crime_hom")),
                            #tabPanel("Homicide in Cities"),  
                            tabPanel("Criminal Justice System",
                                     h4("Data on Criminal Justice System Responce"),
                                     dataTableOutput("cjsr")),
                            
                            tabPanel("Homicide in Prisons",
                                     h4("Data on Homicide in Prisons"),
                                     dataTableOutput("prisons")
                            ),
                            
                            tabPanel("Homicide by Citizenship",
                                     h4("Data on Homicide by Citizenship"),
                                     dataTableOutput("citizenship")
                            ) #closes tabpanel on citizens
                 )#, #closes navbarMenu

                 
                 
) #the very last, closes navbar page