# Define server logic 
shinyServer(function(input, output) {
  
  ############FIRST PAGE   
  
  
  
  output$textformap <- renderText({
    paste(
      "The interactive map visualizes a single indicator by countries in a selected year. 
      Based on user inputs on the sidebar, it can be downloaded as an interacticve html 
      document by pressing a 'Download' button below the map."
    )
  })
  
  # Create a download handler
  output$download_country_data <- downloadHandler(
    # The downloaded file is named "homicide_data.csv"
    filename = "homicide_country_data.csv",
    content = function(file) {
      d <- filtered_country_data()
      write.csv(d, file, row.names = FALSE, na = "")
    }
  )
  
  #create reactive dataset for mapping
  maphom <- reactive({
    country_map_data %>%
      filter(Year == input$sliderMapYear & Variable == input$IndicatorMap) %>% 
      tidyr::drop_na(Value)  
  }) 
  
  # we need to create an empty map
  #then we put there additional values
  
  #Create map
  
  homicide_map <- reactive({ 
    
    
    leaflet(data = maphom(), options = leafletOptions(minZoom = 1.3)) %>% 
      addProviderTiles(provider = "Esri.WorldShadedRelief") %>%
      addCircleMarkers(lng =  ~long, lat = ~lat, 
                       radius = ~(Value/2), 
                       color = "red", 
                       label = ~paste0(Country, ", ", Variable, ", ", Value, ""),
                       opacity = 0.4,
                       group = "circles") %>%
      addMarkers(
        data = maphom(), 
        lng =  ~long, lat = ~lat,  
        label = ~paste0(Country), 
        popup = ~paste0(Country, ", ", Year, ", ", Variable, ", ",  Value, ""),
        group = "maphom()",
        icon = makeIcon( 
          iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
          iconWidth = 1, iconHeight = 1
        )) %>%  
      
      setView(lat = 25.664, lng = 20.303, zoom = 1.8) %>%
      setMaxBounds(lng1 = -175,
                   lat1 = 80,
                   lng2 = 175,
                   lat2 = -66) %>%
      addSearchFeatures(targetGroups = 'maphom()',  
                        options = searchFeaturesOptions(
                          zoom=6, openPopup = TRUE, firstTipSubmit = TRUE,
                          autoCollapse = TRUE, hideMarkerOnCollapse = TRUE )) %>%
      addResetMapButton() 
    
  })
  
  output$homicide_map <- renderLeaflet({
    
    homicide_map() 
    
  })
  
  output$savemap <- downloadHandler(
    
    filename = "homicide_map.html",
    content = function(file){
      saveWidget(
        widget = homicide_map(),
        file = file 
      )
      
    }
  )
  
  #################SECOND PAGE###############################
  
  output$textforcountrydataexp <- renderText({
    paste(
      "The table provides homicide data by countries. It allows to select based on 
      the sidebar panel inputs the countries, reported inidicators, and years. 
      By default, the table shows homicide count and rate in all countries every 10 years and the latest available year. 
      Based on the user inputs, the dataset can be exported in the csv format.", 
      "<br />"
    )
  })
  
  #create a reactive filtered data for table and download
  filtered_country_data <- reactive({
    validate(
      need(input$Indicator != "", "Please select at least one indicator"),
      need(input$Country != "", "Please select at least one country")
    ) 
    
    d <-
      shiny_dt %>% 
      filter(Indicator %in% input$Indicator) %>% 
      select(Country, Region, Subregion, Indicator, input$YearVar) 
    
    if (input$Country != "All"){     
      d <-   
        shiny_dt %>%
        filter(Country %in% input$Country & Indicator %in% input$Indicator) %>% 
        select(Country, Region, Subregion, Indicator, input$YearVar) 
    }
    d
  })
  
  
  output$datatab  <- DT::renderDataTable({
    d <- filtered_country_data()
    d
  })
  
  # Create a download handler
  output$download_country_data <- downloadHandler(
    # The downloaded file is named "homicide_data.csv"
    filename = "homicide_country_data.csv",
    content = function(file) {
      d <- filtered_country_data()
      write.csv(d, file, row.names = FALSE, na = "")
    }
  )
  
  #--------from here new - this might mnot
  
  ##############################------THIRD NAVBAR PANEL------------################
  #########First Tabset
  
  #################TABSET 1
  #create reactive dataset for mapping
  
  mapesthom <- reactive({
    
    regmap %>%
      drop_na(input$EstIndicatorMap)
  }) 
  
  #Create map
  
  regional_map <- reactive({ 
    
    leaflet(mapesthom()) %>%
      addProviderTiles(provider = "Esri.WorldShadedRelief") %>%
      setView(lat = 25.664, lng = 20.303, zoom = 1.6) %>%
      setMaxBounds(lng1 = -175,
                   lat1 = 80,
                   lng2 = 175,
                   lat2 = -66) %>%
      addResetMapButton() #%>%
    #addFullscreenControl()
    
  })
  
  output$regional_map <- renderLeaflet({
    regional_map() %>%
      addMinicharts(
        lng =  regmap$long, lat = regmap$lat, layerId = regmap$Region, 
        type = "pie") 
  })
  
  
  # Update map
  observe({
    
    if (length(input$EstIndicatorMap) == 0) {
      newdata <- 0
    } else {
      newdata <-  regmap[, input$EstIndicatorMap]
    }
    
    maxValue <- max(as.matrix(newdata))
    
    map_with_chart <- 
      leafletProxy("regional_map") %>% 
      updateMinicharts(regmap$Region,
                       chartdata = newdata, 
                       maxValues = maxValue,
                       time = regmap$Year,
                       width = 50, transitionTime = 0, showLabels = T) 
    
    map_with_chart
  })
  
  
  #######Second Tabset
  
  #create a reactive filtered dataset
  filtered_regional_data <- reactive({
    
    validate(
      need(input$Region != "", "Please select at least one indicator")
    ) 
    
    f <-
      shiny_reg_dt %>%
      filter(Indicator %in% input$RegIndicator) %>% 
      select(`Region/Subregion`, Indicator, input$RegYearVar) 
    
    if (input$Region != "All"){     
      f <-   
        shiny_reg_dt %>%
        filter(`Region/Subregion` %in% input$Region & Indicator %in% input$RegIndicator) %>% 
        select(`Region/Subregion`, Indicator, input$RegYearVar) 
    }
    f
  })
  
  output$regdatatab  <- DT::renderDataTable({
    f <- filtered_regional_data()
    f
  })
  
  # Create a download handler
  output$download_reg_data <- downloadHandler(
    # The downloaded file is named homicide_reg_data.csv"
    filename = "homicide_reg_data.csv",
    content = function(file) {
      # The code for filtering the data is copied from the
      # renderTable() function
      f <- filtered_regional_data()
      
      # Write the filtered data into a CSV file
      write.csv(f, file, row.names = FALSE, na = "")
    }
  )
  
  
  #####------------------------works
  ###############FOURTH TAB
  #Create a reactivce object for slider_range years innput  
  years <- reactive({
    seq(input$slider[1], input$slider[2], by = 1)
  })
  
  regions <- reactive({
    validate(
      need(input$Country_Region != "", "Please select at least one territorial unit")
    ) 
    
    ts_data %>%
      filter(Territory %in% input$Country_Region & Year %in% years()) 
  }) 
  
  # Create scatterplot object the plotOutput function is expecting
  output$timeSeries <- renderPlotly({
    
    p <- ggplot(data = regions(), aes_string( x = 'Year', y = input$y))+
      geom_line(aes(color = Territory))+ 
      geom_point()+
      ylab(input$y)
    
    ggplotly(p) %>%
      config(displaylogo = F)
    
  })
  
  ######-----this works
  
  #################################################
  #########Other: IPFM##############
  ################################################
  output$ipfm <- renderDT(
    ipfm,
    extensions = "Buttons",
    options = list(dom = "Blfrtip", orientation = "landscape",
                   buttons = list(c("copy", "csv", "excel", "print"), list(extend = "pdf",orientation = "landscape", pageSize = "A4")),
                   lengthMenu = list(c(10, 50, 100, 150, 200, -1), 
                                     list("10", "50", "100", "150", "200", "All")), paging = T),
    class = "display nowrap compact",
    filter = "top"
    
  )
  
  #################################################
  #########Other: Crime-Related##############
  ################################################
  output$crime_hom <- renderDT(
    crime_hom,
    extensions = "Buttons",
    options = list(dom = "Blfrtip", orientation = "landscape",
                   buttons = list(c("copy", "csv", "excel", "print"), list(extend = "pdf",orientation = "landscape", pageSize = "A4")),
                   lengthMenu = list(c(10, 50, 100, 150, 200, -1), 
                                     list("10", "50", "100", "150", "200", "All")), paging = T),
    class = "display nowrap compact",
    filter = "top"
    
  )
  
  #################################################
  #########Other: CJSR##############
  ################################################
  output$cjsr <- renderDT(
    cjsr,
    extensions = "Buttons",
    options = list(dom = "Blfrtip", orientation = "landscape",
                   buttons = list(c("copy", "csv", "excel", "print"), list(extend = "pdf",orientation = "landscape", pageSize = "A4")),
                   lengthMenu = list(c(10, 50, 100, 150, 200, -1), 
                                     list("10", "50", "100", "150", "200", "All")), paging = T),
    class = "display nowrap compact",
    filter = "top"
    
  )
  
  #################################################
  #########Other: Homicide in Prisons##############
  ################################################
  output$prisons <- renderDT(
    hom_prisons_dat,
    extensions = "Buttons", 
    options = list(dom = "Blfrtip", 
                   buttons = c("copy", "csv", "excel", "pdf", "print"),
                   lengthMenu = list(c(10, 50, 100, 150, 200, -1), 
                                     list("10", "50", "100", "150", "200", "All")), paging = T),
    
    class = "display nowrap compact",
    filter = "top"
  )
  
  #################################################
  #########Other: Citizenship##############
  ################################################
  # output$citizenship <- renderDT(
  #   citizenship ,
  #   extensions = "Buttons", 
  #   options = list(dom = "Blfrtip", 
  #                  buttons = c("copy", "csv", "excel", "pdf", "print"), 
  #                  pageLength = 10,
  #                  lengthMenu = list(c(10, 50, 100, 150, 200, -1), 
  #                                    list("10", "50", "100", "150", "200", "All")), paging = T),
  #   class = "display nowrap compact",
  #   filter = "top"
  # )
  
  
  
}) # the very last to close server function