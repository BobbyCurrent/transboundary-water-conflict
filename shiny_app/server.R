joined <- readRDS("joined.rds")

regression_prep_conflict_events <- joined %>%
    mutate(peaceful_1 = str_detect(event_summary, c("cooperation", "agreement", "meeting"))) %>%
    mutate(peaceful_2 = str_detect(event_summary, c("plan", "signed", "treaty"))) %>%
    mutate(peaceful_3 = str_detect(event_summary, c("coordination", "summit"))) %>%
    # filter(peaceful_1 == TRUE | peaceful_2 == TRUE | peaceful_3 == TRUE) %>%
    filter(peaceful_1 == FALSE | peaceful_2 == FALSE | peaceful_3 == FALSE) %>%
    group_by(ccode) %>%
    summarize(avg_gdp = mean(gdp, na.rm = TRUE), 
              avg_pop = mean(pop, na.rm = TRUE),
              avg_trade = mean(trade_percent_gdp, na.rm = TRUE),
              avg_water = mean(water, na.rm = TRUE),
              event_count = n())

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$water_regression <- renderPlot({
        p = regression_prep_conflict_events %>%
            filter(!is.na(avg_water)) %>%
            ggplot(aes_string(x = input$varOI_x, y = input$varOI_y)) + 
            geom_point() + 
            theme_classic() +
            geom_jitter() +
            labs(caption = "Sources: World Bank, Oregon State University)")
        if(input$varOI_x == "avg_water") p <- p + xlab("% of population with regular access to drinking water")
        if(input$varOI_x == "avg_pop") p <- p + xlab("Population")
        if(input$varOI_x == "avg_gdp") p <- p + xlab("Gross Domestic Product (GDP)")
        if(input$varOI_x == "avg_trade") p <- p + xlab("Average trade as a percent of GDP")
        if(input$varOI_x == "event_count") p <- p + xlab("Number of water conflict events since 1948")
        if(input$varOI_y == "avg_water") p <- p + ylab("% of population with regular access to drinking water")
        if(input$varOI_y == "avg_pop") p <- p + ylab("Population")
        if(input$varOI_y == "avg_gdp") p <- p + ylab("Gross Domestic Product (GDP)")
        if(input$varOI_y == "avg_trade") p <- p + ylab("Average trade as a percent of GDP")
        if(input$varOI_y == "event_count") p <- p + ylab("Number of water conflict events since 1948")
        if(input$toggleLinear) p <- p + geom_smooth(method = "lm", se = TRUE, formula = y ~ x)
        if(input$toggleLoess) p <- p + geom_smooth(method = "loess", se = TRUE, formula = y ~ x)
        if(input$toggleGlm) p <- p + geom_smooth(method = "glm", se = TRUE, formula = y ~ x)
        if(input$toggleGam) p <- p + geom_smooth(method = "gam", se = TRUE, formula = y ~ x)
        p
    })
    
    # Leaflet Map
    source("gather_leaflet.R")
    output$map <- renderLeaflet({
        # Set my color palettes for each variable. 
        
        binpal_num_events <- colorBin("Blues", basins_geometry$num_events, 5, pretty = FALSE, na.color = "#DFDFDF")
        binpal_num_treaties <- colorBin("Greens", basins_geometry$num_treaties, 5, pretty = FALSE, na.color = "#DFDFDF")
        binpal_num_orgs <- colorBin("Yellows", basins_geometry$num_orgs, 5, pretty = FALSE, na.color = "#DFDFDF")
        binpal_gdp <- colorBin("Reds", countries_geometry$gdp, 5, pretty = FALSE, na.color = "#DFDFDF")
        binpal_pop <- colorBin("Purples", countries_geometry$pop, 5, pretty = FALSE, na.color = "#DFDFDF")
        binpal_trade <- colorBin("Oranges", countries_geometry$trade_percent_gdp, 5, pretty = FALSE, na.color = "#DFDFDF")
        
        leaflet(width = "100%") %>% 
            addProviderTiles(providers$CartoDB.Positron) %>% 
            setView(lng = 0, lat = 30,zoom = 1.5) %>% 
            
            ## Conflict Events ## 
            
            addPolygons(data = basins_geometry, 
                        stroke = FALSE, 
                        smoothFactor = 0.2, 
                        fillOpacity = .6, 
                        popup= paste("Name:", 
                                     basins_geometry$NAME, 
                                     "Basin <br>", 
                                     "Number of Violent Conflicts:",
                                     basins_geometry$num_events), 
                        color = ~binpal_num_events(num_events), 
                        group = "Conflict Events"
            ) %>%
            addLegend("bottomright", 
                      pal = binpal_num_events, 
                      values = basins_geometry$num_events, 
                      title = "# of water conflict <br> events since 1948", 
                      opacity = 1, 
                      labFormat = labelFormat(digits = 0), 
                      group = "Conflict Events"
            ) %>%
            
            ## Treaties ## 
            
            addPolygons(data = basins_geometry, 
                        stroke = FALSE, 
                        smoothFactor = 0.2, 
                        fillOpacity = .6, 
                        popup= paste("Name:", 
                                     basins_geometry$NAME, 
                                     "Basin <br>", 
                                     "Number of Treaties:",
                                     basins_geometry$num_treaties), 
                        color = ~binpal_num_treaties(num_treaties), 
                        group = "Treaties"
            ) %>%
            addLegend("bottomright", 
                      pal = binpal_num_treaties, 
                      values = basins_geometry$num_treaties, 
                      title = "# of treaties <br> since 1948", 
                      opacity = 1, 
                      labFormat = labelFormat(digits = 0), 
                      group = "Treaties"
            ) %>%
            
            ## Organizations ## 
            
            # addPolygons(data = basins_geometry, 
            # stroke = FALSE, 
            # smoothFactor = 0.2, 
            # fillOpacity = .6, 
            # popup= paste("Name:", 
            # basins_geometry$NAME, 
            # "Basin <br>", 
            # "Number of Organizations:",
        # basins_geometry$num_orgs), 
        # color = ~binpal_num_orgs(num_orgs), 
        # group = "Organizations"
        # ) %>%
        # addLegend("bottomright", 
        # pal = binpal_num_orgs, 
        # values = basins_geometry$num_orgs, 
        # title = "# of Organizations", 
        # opacity = 1, 
        # labFormat = labelFormat(digits = 0), 
        # group = "Organizations"
        # ) %>%
        
        ## GDP ## 
        
        addPolygons(data = countries_geometry, 
                    stroke = FALSE, 
                    smoothFactor = 0.2, 
                    fillOpacity = .6, 
                    color = ~binpal_gdp(gdp), 
                    group = "GDP (2015)"
        ) %>%
            addLegend("bottomleft", 
                      pal = binpal_gdp, 
                      values = countries_geometry$gdp, 
                      title = "2015 GDP", 
                      opacity = 1, 
                      labFormat = labelFormat(digits = 0), 
                      group = "GDP (2015)"
            ) %>%
            
            ## Pop ## 
            
            addPolygons(data = countries_geometry, 
                        stroke = FALSE, 
                        smoothFactor = 0.2, 
                        fillOpacity = .6,
                        color = ~binpal_pop(pop), 
                        group = "Population (2015)"
            ) %>%
            addLegend("bottomleft", 
                      pal = binpal_pop, 
                      values = countries_geometry$pop, 
                      title = "2015 Population", 
                      opacity = 1, 
                      labFormat = labelFormat(digits = 0), 
                      group = "Population (2015)"
            ) %>%
            
            ## Trade ## 
            
            addPolygons(data = countries_geometry, 
                        stroke = FALSE, 
                        smoothFactor = 0.2, 
                        fillOpacity = .6, 
                        color = ~binpal_trade(trade_percent_gdp), 
                        group = "Trade % GDP (2015)"
            ) %>%
            addLegend("bottomleft", 
                      pal = binpal_trade, 
                      values = countries_geometry$gdp_percent_trade, 
                      title = "GDP Percent Trade", 
                      opacity = 1, 
                      labFormat = labelFormat(digits = 0), 
                      group = "Trade % GDP (2015)"
            ) %>%
            
            ## Countries ## 
            
            addPolygons(data = countries_geometry, 
                        stroke = TRUE, 
                        color = "black", 
                        weight = .4,
                        label = countries_geometry$CNTRY_NAME,
                        fill = FALSE,
                        smoothFactor = 0.2, 
                        group = "Countries"
            ) %>%
            
            ## Layer Control ## 
            
            addLayersControl(overlayGroups = c("Countries", "Conflict Events", "Treaties", "Population (2015)", "GDP (2015)", "Trade % GDP (2015)"),
                             options = layersControlOptions(collapsed = TRUE)
            ) %>%
            hideGroup(c("Treaties", "Population (2015)", "GDP (2015)", "Trade % GDP (2015)"))
    })
    
    lm1 <- reactive({lm(reformulate(input$varOI_x, input$varOI_y), data = regression_prep_conflict_events)})
    lm2 <- reactive({lm(reformulate(input$varOI_x, input$varOI_y), data = regression_prep_conflict_events, method = "loess")})
    lm3 <- reactive({lm(reformulate(input$varOI_x, input$varOI_y), data = regression_prep_conflict_events, method = "glm")})
    lm4 <- reactive({lm(reformulate(input$varOI_x, input$varOI_y), data = regression_prep_conflict_events, method = "gam")})
    lm5 <- reactive({lm(as.formula(paste(input$varOI_y," ~ ",paste(input$varOI_x, input$varOI_x2, collapse="+"))), data=regression_prep_conflict_events)})
    lm6 <- reactive({lm(as.formula(paste(input$varOI_y," ~ ",paste(input$varOI_x, input$varOI_x2, collapse="*"))), data=regression_prep_conflict_events)})
    
    output$RegSum <- renderTable({
        if(input$toggleLinear) lmsum <- lm1
        if(input$toggleLoess) lmsum <- lm2
        if(input$toggleGlm) lmsum <- lm3
        if(input$toggleGam) lmsum <- lm4
        if(input$toggleMulti) lmsum <- lm5
        if(input$toggleMultiinteraction) lmsum <- lm6
        tidy(lmsum())
        })
})
