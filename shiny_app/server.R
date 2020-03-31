joined <- readRDS("joined.RDS")

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
            ggplot(aes(x = avg_gdp, y = event_count)) + 
            geom_point() + 
            theme_classic() +
            labs(title = "Access to water resources vs. number of water conflict events since 1948", 
                 caption = "Sources: World Bank, Oregon State University)") + 
            xlab("% of population with regular access to drinking water") + 
            ylab("Water conflict events since 1948")
        if(input$toggleLinear) p = p + geom_smooth(method = "lm", se = FALSE, formula = y ~ x)
        if(input$toggleExponential) p = p + geom_smooth(method = "nls", start = list(a = max(avg_water), b = 1), formula = y ~ a*b**x)
        p
    })
})
