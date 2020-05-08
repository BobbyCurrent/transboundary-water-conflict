############## PREP ##############

# Read in datasets created using the 'gather_raw_data.R' script. 

joined <- readRDS("joined.rds")
joined_summarized <- readRDS("joined_summarized.rds")
joined_logistic <- readRDS("joined_logistic.rds")
terms <- readRDS("terms.RDS")

shinyServer(function(input, output) {
    
    ############################
    
    
    ############## FIRST PAGE ##############
    
    # Render histogram of water conflict events over time, grouped by continent.
    
    output$time_histogram <- renderPlot({
        ggplot(joined %>% distinct(event_summary, .keep_all = TRUE) %>% filter(!is.na(continent_code)),
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
    
    # Render table with river basins, sorted by frequency.
    
    output$frequency <- renderDataTable({
        datatable(
            joined %>%
                distinct(event_summary, .keep_all = TRUE) %>%
                filter(!is.na(basin_name)) %>%
                group_by(basin_name) %>%
                summarize(event_count = n()) %>%
                arrange(desc(event_count)) %>%
                select("Basin Name" = basin_name, "Event Count" = event_count),
            options = list(pageLength = 10)
        )
    })
    
    # Render Leaflet map. First, run a script to gather the necessary Leaflet
    # data and shapefiles.
    
    source("gather_leaflet.R")
    output$map <- renderLeaflet({
        
        # Set color palette, using 9 bins to show the full range of variation.
        
        binpal_num_events <-
            colorBin(
                "Blues",
                basins_geometry$num_events,
                9,
                pretty = FALSE,
                na.color = "#DFDFDF"
            )
        
        # Build map using CARTO DB Positron provider tiles. Center view in the
        # middle of the Earth, zoomed out enough to see all countries.
        
        leaflet(width = "100%") %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = 0,
                    lat = 30,
                    zoom = 1.5) %>%
            
            # Build river basin polygons that display the number of conflict
            # events.
            
            addPolygons(
                data = basins_geometry,
                stroke = FALSE,
                smoothFactor = 0.2,
                fillOpacity = 1,
                popup = paste(
                    basins_geometry$NAME,
                    "Basin <br>",
                    "Water Conflict Events:",
                    basins_geometry$num_events
                ),
                color = ~ binpal_num_events(num_events)
                # group = "Conflict Events" (deprecated after removing other
                # layers due to Shiny memory limits).
            ) %>%
            
            # Add a legend to the bottom right corner.
            
            addLegend(
                "bottomright",
                pal = binpal_num_events,
                values = basins_geometry$num_events,
                title = "# of water conflict <br> events, 1960 - 2005",
                opacity = 1,
                labFormat = labelFormat(digits = 0)
                # group = "Conflict Events" (deprecated after removing other
                # layers due to Shiny memory limits).
            )
    })
    
    ############################
    
    
    ############## SECOND PAGE ##############
    
    # Render plot that displays a scatterplot and line of best fit when 1 X and 1
    # Y variable are selected.
    
    output$water_regression <- renderPlot({
        p = joined_summarized %>%
            ggplot(aes_string(x = input$varOI_x, y = input$varOI_y)) +
            geom_point() +
            theme_classic() +
            geom_jitter() +
            labs(title = "Basin-level Water Conflict Metrics",
                 subtitle = "Summarized at the basin-country level over the period 1960-2005",
                 caption = "Sources: World Bank, Oregon State University")
        
        # Add appropriate axis labels based on variables selected.
        
        if (input$varOI_x == "event_count")
            p <- p + xlab("Number of water conflict events")
        if (input$varOI_x == "gdp_total")
            p <- p + xlab("Total GDP ($)")
        if (input$varOI_x == "gdp_avg")
            p <- p + xlab("Average GDP ($)")
        if (input$varOI_x == "pop_total")
            p <- p + xlab("Total population")
        if (input$varOI_x == "pop_avg")
            p <- p + xlab("Average population")
        if (input$varOI_x == "trade_percent_gdp_avg")
            p <- p + xlab("Average % GDP made up by international trade")
        if (input$varOI_x == "water_withdraw_avg")
            p <- p + xlab("Average annual rate of water consumption (as % of total water resources)")
        if (input$varOI_x == "ag_land_total")
            p <- p + xlab("Total basin agricultural land (km^2)")
        if (input$varOI_x == "ag_land_avg")
            p <- p + xlab("Average agricultural land (km^2)")
        if (input$varOI_x == "droughts_avg")
            p <- p + xlab("Average % of population affected by drought between 1960 and 2005")
        if (input$varOI_x == "eiu_avg")
            p <- p + xlab("Average democratization index score (10 most democratic)")
        
        if (input$varOI_x == "event_count_log")
            p <- p + xlab("Number of water conflict events (logged)")
        if (input$varOI_x == "gdp_total_log")
            p <- p + xlab("Total GDP (logged $)")
        if (input$varOI_x == "gdp_avg_log")
            p <- p + xlab("Average GDP (logged $)")
        if (input$varOI_x == "pop_total_log")
            p <- p + xlab("Total population (logged)")
        if (input$varOI_x == "pop_avg_log")
            p <- p + xlab("Average population (logged)")
        if (input$varOI_x == "trade_percent_gdp_avg_log")
            p <- p + xlab("Average % GDP made up by international trade (logged)")
        if (input$varOI_x == "water_withdraw_avg_log")
            p <- p + xlab("Average annual rate of water consumption (as logged % of total water resources)")
        if (input$varOI_x == "ag_land_total_log")
            p <- p + xlab("Total basin agricultural land (logged km^2)")
        if (input$varOI_x == "ag_land_avg_log")
            p <- p + xlab("Average agricultural land (logged km^2)")
        if (input$varOI_x == "droughts_avg_log")
            p <- p + xlab("Average % of population affected by drought between 1960 and 2005 (logged)")
        if (input$varOI_x == "eiu_avg_log")
            p <- p + xlab("Average democratization index score (logged)")
        
        if (input$varOI_y == "event_count")
            p <- p + ylab("Number of water conflict events")
        if (input$varOI_y == "gdp_total")
            p <- p + ylab("Total GDP ($)")
        if (input$varOI_y == "gdp_avg")
            p <- p + ylab("Average GDP ($)")
        if (input$varOI_y == "pop_total")
            p <- p + ylab("Total population")
        if (input$varOI_y == "pop_avg")
            p <- p + ylab("Average population")
        if (input$varOI_y == "trade_percent_gdp_avg")
            p <- p + ylab("Average % GDP made up by international trade")
        if (input$varOI_y == "water_withdraw_avg")
            p <- p + ylab("Average annual rate of water consumption (as % of total water resources)")
        if (input$varOI_y == "ag_land_total")
            p <- p + ylab("Total basin agricultural land (km^2)")
        if (input$varOI_y == "ag_land_avg")
            p <- p + ylab("Average agricultural land (km^2)")
        if (input$varOI_y == "droughts_avg")
            p <- p + ylab("Average % of population affected by drought between 1960 and 2005")
        if (input$varOI_y == "eiu_avg")
            p <- p + ylab("Average democratization index score")
        
        if (input$varOI_y == "event_count_log")
            p <- p + ylab("Number of water conflict events (logged)")
        if (input$varOI_y == "gdp_total_log")
            p <- p + ylab("Total GDP (logged $)")
        if (input$varOI_y == "gdp_avg_log")
            p <- p + ylab("Average GDP (logged $)")
        if (input$varOI_y == "pop_total_log")
            p <- p + ylab("Total population (logged)")
        if (input$varOI_y == "pop_avg_log")
            p <- p + ylab("Average population (logged)")
        if (input$varOI_y == "trade_percent_gdp_avg_log")
            p <- p + ylab("Average % GDP made up by international trade (logged)")
        if (input$varOI_y == "water_withdraw_avg_log")
            p <- p + ylab("Average annual rate of water consumption (as logged % of total water resources)")
        if (input$varOI_y == "ag_land_total_log")
            p <- p + ylab("Total basin agricultural land (logged km^2)")
        if (input$varOI_y == "ag_land_avg_log")
            p <- p + ylab("Average agricultural land (logged km^2)")
        if (input$varOI_y == "droughts_avg_log")
            p <- p + ylab("Average % of population affected by drought between 1960 and 2005 (logged)")
        if (input$varOI_y == "eiu_avg_log")
            p <- p + ylab("Average democratization index score (logged)")
        
        # Add appropriate trendline, based on user selection. 
        
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
    
    # Render regression output. First, choose the appropriate model, based on
    # user input, and calculate using the selected X and Y variables.
    
    output$RegSum <- renderPrint({
        if (input$toggleLinear)
            lmsum <-
                reactive({
                    lm(reformulate(input$varOI_x, input$varOI_y),
                       data = joined_summarized)
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
        
        # Print a summary of the model. 
        
        print(summary(lmsum()))
    })
    
    # Generate a predicted number of conflicted events, using the same models
    # constructed above and user inputs of independent variables.
    
    output$predictSum <- renderPrint({
        if (input$toggleLinear)
            lmsum <-
                reactive({
                    lm(reformulate(input$varOI_x, input$varOI_y),
                       data = joined_summarized)
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
        
        # Set independent variable slider values equal to the input names
        # required by the above models. Note that the slider names could not be
        # the same as the input names, or else Shiny threw an error.
        
        sliderValues <- reactive({
            data.frame(
                gdp_total = input$gdp_total_pred,
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
                eiu_avg_log = log(input$eiu_avg_pred)
            )
        })
        
        # Generate prediction, with a 95% confidence interval. 
        
        prediction <- reactive({
            predict(lmsum(),
                    newdata = sliderValues(),
                    interval = "confidence")
        })
        
        # Print prediction.
        
        print(prediction())
    })
    
    ############################
    
    
    ############## THIRD PAGE ##############
    
    # Create a plot that shows the most frequently-used terms in the conflict
    # event descriptions.
    
    output$plot <- renderPlot({
        
        # Exclude '0' sentiment values, if the appropriate box is checked. 
        
        if (input$toggleZero) terms <- terms %>% filter(value != 0)
        
        # Set filter terms based on user input through the slider sidebar.
        
        terms %>%
            filter(count >= input$freq[1]) %>%
            filter(count <= input$freq[2]) %>%
            filter(value >= input$sentiment[1]) %>%
            filter(value <= input$sentiment[2]) %>%
            # top_n(100, wt = count) %>%
            
            # Create plot, using geom_text_repel() to minimize word overlap.
            
            ggplot(aes(x = value, y = count, label = term)) + 
            geom_text_repel(segment.alpha = 0, aes(color = value, size = count)) + 
            scale_color_gradient(high="green3", low="violetred", 
                                 guide = FALSE) + 
            
            # Set word size restrictions, to make them as readable as possible
            # (not too large or too small).
            
            scale_size_continuous(range = c(4, 8),
                                  guide = FALSE) + 
            labs(title = "Most Frequent Keywords in Water Conflict Event Descriptions",
                 subtitle = "Sorted by sentiment, where - 2 is most negative and + 2 is most positive",
                 caption = "Event descriptions written by the OSU Program on Water Conflict Transformation",
                 x = "Sentiment", y = "Frequency") +
            theme_classic()
    })
    
    # Create a plot that displays the frequency of each issue type, from 1960 to
    # 2005.
    
    output$issuePlot <- renderPlot({
        joined %>%
            
            # Tidy the 'joined' data, so that each issue_type is its own observation. 
            
            pivot_longer(cols = c(issue_type1, issue_type2), names_to = "number", values_to = "issue_type") %>%
            filter(!is.na(issue_type) & !is.na(continent_code)) %>%
            
            # Find the frequency of each issue type. 
            
            group_by(issue_type, continent_code) %>%
            summarize(count = n()) %>%

            # Create the plot, reordering the issue types in descending order by
            # frequency, and flipping the coordinates to yield a horizontal bar
            # chart (which fit better on the page aesthetically.)
            
            ggplot(aes(x = reorder(issue_type, count), y = count, fill = continent_code)) + 
            geom_col() + 
            coord_flip() + 
            labs(title = "Most Frequent Issues Sparking Water Conflict",
                 caption = "Coded by the OSU Program on Water Conflict Transformation",
                 x = "Issue Type", y = "Frequency",
                 fill = "Continent") + 
            scale_y_continuous(expand = c(0, 0), limits = c(0, 2000)) +
            theme_classic() + 
            
            # Use same colors as on page 1, for continuity. 
            
            scale_fill_brewer(palette = "Spectral")
    })
    
    ############################
    
    
    ############## FOURTH PAGE ##############
    
    # Render a data table that displays the water conflict events in each river
    # basin.
    
    output$case_study_table <- renderDataTable(datatable(
        joined %>%
            filter(basin_name == input$river_basin) %>%
            filter(!is.na(event_summary)) %>%
            distinct(event_summary, .keep_all = TRUE) %>%
            select("Event Date" = date.x, "Event Summary" = event_summary),
        options = list(pageLength = 10)
    ))
    
    # Render a histogram that displays the number of water conflict events in a
    # given year.
    
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
    
    # Render a histogram that displays the number of treaties signed in a given year.
    
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
    
    # I am interested in potentially adding more to this page, but ran out of
    # Shiny memory.
    
    ############################
    
})
