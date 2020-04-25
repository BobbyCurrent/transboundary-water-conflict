############## PREP ##############

joined <- readRDS("joined.rds")
joined_summarized <- readRDS("joined_summarized.rds")
joined_logistic <- readRDS("joined_logistic.rds")

# Summarizing the joined data, as needed for my regressions and statistics
# throughout the project.

shinyServer(function(input, output) {
    ############################
    
    
    ############## FIRST PAGE ##############
    
    # Render histogram of water conflict events over time
    
    output$time_histogram <- renderPlot({
        ggplot(joined %>% filter(!is.na(continent_code)),
               aes(x = event_year, fill = continent_code)) +
            geom_histogram() +
            theme_classic() +
            labs(title = "Water Conflict Events Over Time",
                 subtitle = "Upward trend over time.",
                 fill = "Continent") +
            xlab("Year") +
            ylab("Number of Water Conflict Events") +
            scale_fill_brewer(palette = "Spectral")
    })
    
    output$frequency <- renderDataTable({datatable(
        joined_summarized %>%
            arrange(desc(event_count)) %>%
            select("Basin Name" = basin_name, "Event Count" = event_count),
        options = list(pageLength = 10)
    )
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
                9,
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
                title = "# of water conflict <br> events, 1960 - 2009",
                opacity = 1,
                labFormat = labelFormat(digits = 0),
                group = "Conflict Events"
            )
    })
    
    ############################
    
    
    ############## SECOND PAGE ##############
    output$water_regression <- renderPlot({
        p = joined_summarized %>%
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
        p
    })
    
    output$RegSum <- renderPrint({
        
        if (input$toggleLinear)
            lmsum <-
                reactive({
                    lm(
                        reformulate(input$varOI_x, input$varOI_y),
                        data = joined_summarized
                    )
                })
        
        if (input$toggleLoess)
            lmsum <-
                reactive({
                    lm(
                        reformulate(input$varOI_x, input$varOI_y),
                        data = joined_summarized,
                        method = "loess"
                    )
                })
        
        if (input$toggleMulti)
            lmsum <-
                reactive({
                    lm(as.formula(paste(
                        input$varOI_y,
                        " ~ ",
                        paste(input$varOI_x, collapse = "+")
                    )), data = joined_summarized)
                })
        
        if (input$toggleMultiinteraction)
            lmsum <-
                reactive({
                    lm(as.formula(paste(
                        input$varOI_y,
                        " ~ ",
                        paste(input$varOI_x, collapse = "*")
                    )), data = joined_summarized)
                })
        print(summary(lmsum()))
    })
    
    output$predictSum <- renderPrint({
        
        if (input$toggleLinear)
            lmsum <-
                reactive({
                    lm(
                        reformulate(input$varOI_x, input$varOI_y),
                        data = joined_summarized
                    )
                })
        
        if (input$toggleLoess)
            lmsum <-
                reactive({
                    lm(
                        reformulate(input$varOI_x, input$varOI_y),
                        data = joined_summarized,
                        method = "loess"
                    )
                })
        
        if (input$toggleMulti)
            lmsum <-
                reactive({
                    lm(as.formula(paste(
                        input$varOI_y,
                        " ~ ",
                        paste(input$varOI_x, collapse = "+")
                    )), data = joined_summarized)
                })
        
        if (input$toggleMultiinteraction)
            lmsum <-
                reactive({
                    lm(as.formula(paste(
                        input$varOI_y,
                        " ~ ",
                        paste(input$varOI_x, collapse = "*")
                    )), data = joined_summarized)
                })
        
        sliderValues <- reactive({
            data.frame(gdp_total = input$gdp_total_pred,
                       gdp_avg = input$gdp_avg_pred,
                       pop_total = input$pop_total_pred,
                       pop_avg = input$pop_avg_pred,
                       trade_percent_gdp_avg = input$trade_percent_gdp_avg_pred,
                       water_withdraw_avg = input$water_withdraw_avg_pred,
                       ag_land_total = input$ag_land_total_pred,
                       ag_land_avg = input$ag_land_avg_pred,
                       droughts_avg = input$droughts_avg_pred,
                       eiu_avg = input$eiu_avg_pred,
                       gdp_total_log = log(input$gdp_total_pred),
                       gdp_avg_log = log(input$gdp_avg_pred),
                       pop_total_log = log(input$pop_total_pred),
                       pop_avg_log = log(input$pop_avg_pred),
                       trade_percent_gdp_avg_log = log(input$trade_percent_gdp_avg_pred),
                       water_withdraw_avg_log = log(input$water_withdraw_avg_pred),
                       ag_land_total_log = log(input$ag_land_total_pred),
                       ag_land_avg_log = log(input$ag_land_avg_pred),
                       droughts_avg_log = log(input$droughts_avg_pred),
                       eiu_avg_log = log(input$eiu_avg_pred))
        })
        
        prediction <- reactive({
            predict(lmsum(), newdata = sliderValues(), interval = "confidence")})
        
        print(prediction())
    })
    
    # lm_predict <-
    # lm(event_count ~ pop_avg * water_withdraw_avg, data = joined_summarized)
    
    
    # output$predict <- renderPrint(prediction())
    
    ############################
    
    
    ############## THIRD PAGE ##############
    joined_wc_prep <- joined %>%
        distinct(event_summary)
    
    myCorpus = Corpus(VectorSource(joined_wc_prep$event_summary))
    myCorpus = tm_map(myCorpus, content_transformer(tolower))
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))
    myDTM = TermDocumentMatrix(myCorpus,
                               control = list(minWordLength = 1))
    m = as.matrix(myDTM)
    terms <- sort(rowSums(m), decreasing = TRUE)
    
    # Make the wordcloud drawing predictable during a session
    wordcloud_rep <- repeatable(wordcloud)
    
    output$plot <- renderPlot({
        wordcloud_rep(names(terms), terms, c(4,0.5),
                      min.freq = input$freq, max.words=input$max,
                      colors=brewer.pal(8, "Paired"))
    })
    
    ############################
    
    
    ############## FOURTH PAGE ##############
    output$case_study_table <- renderDataTable(datatable(
        joined %>%
            filter(basin_name == input$river_basin) %>%
            filter(!is.na(event_summary)) %>%
            distinct(event_summary, .keep_all = TRUE) %>%
            select("Event Date" = date.x, "Event Summary" = event_summary),
        options = list(pageLength = 10)
    ))
    
    output$case_conflict_over_time <- renderPlot(
        joined %>%
            filter(basin_name == input$river_basin) %>%
            distinct(event_summary, .keep_all = TRUE) %>%
            ggplot(aes(x = event_year)) +
            geom_histogram(fill = "palegreen3") +
            xlab("Year") +
            ylab("Number of Conflict Events") +
            theme_classic() +
            labs(title = "Water Conflict Events Over Time")
    )
    
    output$treaties_over_time <- renderPlot(
        joined %>%
            filter(basin_name == input$river_basin) %>%
            distinct(document_name, .keep_all = TRUE) %>%
            ggplot(aes(x = year_signed)) + 
            geom_histogram(fill = "dodgerblue3") +
            xlab("Year") +
            ylab("Number of Treaties Signed") +
            theme_classic() + 
            labs(title = "Treaties Signed Over Time")
    )
    
    ############################
    
})
