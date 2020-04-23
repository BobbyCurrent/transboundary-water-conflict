############## PREP ##############

joined <- readRDS("joined.rds")

# Summarizing the joined data, as needed for my regressions and statistics
# throughout the project.

regression_prep_conflict_events <- joined %>%
    group_by(ccode, event_year) %>%
    summarize(
        avg_gdp = mean(gdp, na.rm = TRUE),
        avg_pop = mean(pop, na.rm = TRUE),
        avg_trade = mean(trade_percent_gdp, na.rm = TRUE),
        avg_water = mean(water, na.rm = TRUE),
        avg_water_withdraw = mean(water_withdraw, na.rm = TRUE),
        avg_ag_land = mean(ag_land, na.rm = TRUE),
        event_count = n()
    )

shinyServer(function(input, output) {
    ############################
    
    
    ############## FIRST PAGE ##############
    
    # Render histogram of water conflict events over time
    
    output$time_histogram <- renderPlot({
        ggplot(joined %>% filter(!is.na(continent_name)),
               aes(x = event_year, fill = continent_name)) +
            geom_histogram() +
            theme_classic() +
            labs(title = "Water Conflict Events Over Time",
                 subtitle = "Upward Trend Over Time",
                 fill = "Continent") +
            xlab("Year") +
            ylab("Number of Water Conflict Events") +
            scale_fill_brewer(palette = "Spectral")
    })
    
    # Render Leaflet map
    
    # Run a script to gather necessary Leaflet data and shapefiles.
    
    source("gather_leaflet.R")
    output$map <- renderLeaflet({
        # Set color palettes for each variable.
        
        binpal_num_events <-
            colorBin(
                "Blues",
                basins_geometry$num_events,
                5,
                pretty = FALSE,
                na.color = "#DFDFDF"
            )
        
        # Build map, including provider tiles. Center view in the Middle of the Earth.
        
        leaflet(width = "100%") %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = 0,
                    lat = 30,
                    zoom = 1.5) %>%
            
            # Build polygons that display the number of conflict events.
            
            addPolygons(
                data = basins_geometry,
                stroke = FALSE,
                smoothFactor = 0.2,
                fillOpacity = .6,
                popup = paste(
                    "Name:",
                    basins_geometry$NAME,
                    "Basin <br>",
                    "Number of Violent Conflicts:",
                    basins_geometry$num_events
                ),
                color = ~ binpal_num_events(num_events),
                group = "Conflict Events"
            ) %>%
            
            # Add a color legend.
            
            addLegend(
                "bottomright",
                pal = binpal_num_events,
                values = basins_geometry$num_events,
                title = "# of water conflict <br> events since 1948",
                opacity = 1,
                labFormat = labelFormat(digits = 0),
                group = "Conflict Events"
            )
    })
    
    ############################
    
    
    ############## SECOND PAGE ##############
    output$water_regression <- renderPlot({
        p = regression_prep_conflict_events %>%
            ggplot(aes_string(x = input$varOI_x, y = input$varOI_y)) +
            geom_point() +
            theme_classic() +
            geom_jitter() +
            labs(caption = "Sources: World Bank, Oregon State University")
        if (input$varOI_x == "avg_water")
            p <-
                p + xlab("% of population with regular access to drinking water")
        if (input$varOI_x == "avg_pop")
            p <- p + xlab("Population")
        if (input$varOI_x == "avg_gdp")
            p <- p + xlab("Gross Domestic Product (GDP)")
        if (input$varOI_x == "avg_trade")
            p <- p + xlab("Average trade as a percent of GDP")
        if (input$varOI_x == "event_count")
            p <- p + xlab("Number of water conflict events since 1948")
        if (input$varOI_y == "avg_water")
            p <-
                p + ylab("% of population with regular access to drinking water")
        if (input$varOI_y == "avg_pop")
            p <- p + ylab("Population")
        if (input$varOI_y == "avg_gdp")
            p <- p + ylab("Gross Domestic Product (GDP)")
        if (input$varOI_y == "avg_trade")
            p <- p + ylab("Average trade as a percent of GDP")
        if (input$varOI_y == "event_count")
            p <- p + ylab("Number of water conflict events since 1948")
        if (input$toggleLinear)
            p <- p + geom_smooth(method = "lm",
                                 se = TRUE,
                                 formula = y ~ x)
        if (input$toggleLoess)
            p <- p + geom_smooth(method = "loess",
                                 se = TRUE,
                                 formula = y ~ x)
        if (input$toggleGlm)
            p <- p + geom_smooth(method = "glm",
                                 se = TRUE,
                                 formula = y ~ x)
        if (input$toggleGam)
            p <- p + geom_smooth(method = "gam",
                                 se = TRUE,
                                 formula = y ~ x)
        p
    })
    
    output$RegSum <- renderPrint({
        if (input$toggleLinear)
            lmsum <-
                reactive({
                    lm(reformulate(input$varOI_x, input$varOI_y), data = regression_prep_conflict_events)
                })
        if (input$toggleLoess)
            lmsum <-
                reactive({
                    lm(
                        reformulate(input$varOI_x, input$varOI_y),
                        data = regression_prep_conflict_events,
                        method = "loess"
                    )
                })
        if (input$toggleGlm)
            lmsum <-
                reactive({
                    lm(
                        reformulate(input$varOI_x, input$varOI_y),
                        data = regression_prep_conflict_events,
                        method = "glm"
                    )
                })
        if (input$toggleGam)
            lmsum <-
                reactive({
                    lm(
                        reformulate(input$varOI_x, input$varOI_y),
                        data = regression_prep_conflict_events,
                        method = "gam"
                    )
                })
        if (input$toggleMulti)
            lmsum <-
                reactive({
                    lm(as.formula(paste(
                        input$varOI_y,
                        " ~ ",
                        paste(input$varOI_x, collapse = "+")
                    )), data = regression_prep_conflict_events)
                })
        if (input$toggleMultiinteraction)
            lmsum <-
                reactive({
                    lm(as.formula(paste(
                        input$varOI_y,
                        " ~ ",
                        paste(input$varOI_x, collapse = "*")
                    )), data = regression_prep_conflict_events)
                })
        summary(lmsum())
    })
    
    lm7 <-
        lm(event_count ~ avg_pop * avg_water_withdraw * avg_water, data = regression_prep_conflict_events)
    
    sliderValues <- reactive({
        data.frame(
            avg_pop = input$pop_pred,
            avg_water_withdraw = input$water_withdraw_pred,
            avg_water = input$water_pred
        )
    })
    
    prediction <- reactive({
        predict(lm7, sliderValues(), interval = "confidence")
    })
    
    output$predict <- renderPrint(prediction())
    
    ############################
    
    
    ############## THIRD PAGE ##############
    
    # No content yet.
    
    ############################
    
    
    ############## FOURTH PAGE ##############
    output$case_study_table <- renderDataTable(datatable(
        joined %>%
            filter(bcode == input$river_basin) %>%
            select("Event Date" = event_date, "Event Summary" = event_summary),
        options = list(pageLength = 5)
    ))
    
    output$case_conflict_over_time <- renderPlot(
        joined %>%
            filter(bcode == input$river_basin) %>%
            ggplot(aes(x = event_year)) +
            geom_histogram() +
            theme_classic() +
            labs(title = "Conflict Events Over Time")
    )
    
    output$treaties_over_time <- renderPlot(
        joined %>%
            filter(bcode == input$river_basin) %>%
            filter(!is.na(treaty_notes)) %>%
            ggplot(aes(x = event_year)) + geom_histogram() + theme_classic() + labs(title = "Treaties Signed Over Time")
    )
    
    ############################
    
})
