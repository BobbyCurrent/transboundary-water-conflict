############## PREP ##############

# Necessary libraries

library(shiny)
library(shinythemes)
library(stringr)
library(DT)
library(leaflet)
library(dplyr)

# Read in joined water conflict dataset.

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

shinyUI(
  navbarPage(
    theme = shinytheme("yeti"),
    "International Water Conflict",
    
    ############################
    
    
    ############## FIRST PAGE ##############
    
    tabPanel(
      "The Puzzle",
      h2("Water conflict: A puzzling issue."),
      p(
        "In recent years, journalists, policymakers, and academics
                              have become increasingly worried about the potential for
                              climate-induced water scarcity to cause international conflict.
                              Approximately",
        a("1.2 billion people",
          href = "https://www.un.org/waterforlifedecade/scarcity.shtml"),
        "live
                                in water-scarce areas worldwide, and this number is expected to
                              increase significantly under the stress of ",
        a("climate change",
          href = "https://blogs.ei.columbia.edu/2019/09/23/climate-change-impacts-water/"),
        "."
      ),
      fluidPage(
        fluidRow(style = 'padding:30px;',
                 column(8,
                        plotOutput("time_histogram")),
                 column(
                   4,
                   h3("Water conflict is increasing with time."),
                   p("Summary describing the puzzle")
                 ),),
        fluidRow(column(
          4,
          h3("Water conflict occurs around the world."),
          p("Summary describing the puzzle")
        ),
        column(8,
               leafletOutput("map", height = 500)),)
      )
    ),
    
    ############################
    
    
    ############## SECOND PAGE ##############
    
    tabPanel(
      "Predicting Water Conflict",
      p(
        "[Logistic model examining propensity for conflict in the first place]"
      ),
      p("[Add predictor function]"),
      p(
        "High degree of uncertainty [lessons]. Let's try to predict avg number of conflicts over 20 year period"
      ),
      p(
        "Regression based on number of avg water conflict events in past 20 yrs)"
      ),
      p("predicting severity of conflict"),
      sidebarLayout(
        sidebarPanel(
          h5("Test"),
          selectInput(
            "varOI_x",
            "X variable:",
            choices = c(
              "Water Access" = "avg_water",
              "GDP" = "avg_gdp",
              "Population" = "avg_pop",
              "Trade as % of GDP" = "avg_trade",
              "Population" = "avg_pop",
              "Water Withdrawal" = "avg_water_withdraw",
              "Agricultural Land" = "avg_ag_land"
            ),
            multiple = TRUE,
            selected = "avg_pop"
          ),
          selectInput(
            "varOI_y",
            "Y variable:",
            choices = c(
              "Event Count" = "event_count",
              "Water Access" = "avg_water",
              "GDP" = "avg_gdp",
              "Population" = "avg_pop",
              "Trade as % of GDP" = "avg_trade",
              "Population" = "avg_pop",
              "Water Withdrawal" = "avg_water_withdraw",
              "Agricultural Land" = "avg_ag_land"
            ),
            selected = "event_count"
          ),
          checkboxInput("toggleLinear", label = "Linear Model", value = TRUE),
          checkboxInput("toggleLoess", label = "Loess Model", value = FALSE),
          checkboxInput("toggleGlm", label = "GLM Model", value = FALSE),
          checkboxInput("toggleGam", label = "GAM Model", value = FALSE),
          checkboxInput("toggleMulti", label = "Multivariate Regression", value = FALSE),
          checkboxInput("toggleMultiinteraction", label = "Multivariate Regression w/ Interaction", value = FALSE),
        ),
        
        mainPanel(
          plotOutput("water_regression", height = 500),
          verbatimTextOutput(outputId = "RegSum"),
          tags$style(
            type = "text/css",
            ".shiny-output-error {display: none;}",
            ".shiny-output-error:before {display: none;}"
          )
        )
      ),
      sidebarLayout(
        sidebarPanel(
          sliderInput(
            "pop_pred",
            "Population:",
            min = 0,
            max = 1500000000,
            value = 1,
            step = 5
          ),
          sliderInput(
            "water_withdraw_pred",
            "Water Withdraw:",
            min = 0,
            max = 100,
            value = 1,
            step = 1
          ),
          sliderInput(
            "water_pred",
            "Water Access:",
            min = 0,
            max = 4500,
            value = 1,
            step = 5
          )
        ),
        
        mainPanel(verbatimTextOutput("predict"))
      ),
      p("Select country, see frequency over time and a list of conflicts"),
    ),
    
    ############################
    
    
    ############## THIRD PAGE ##############
    
    tabPanel(
      "Key Issues",
      p("The frequency of water conflict can only tell us so much"),
      p("[Event descriptions]"),
      p("[Word cloud of agreements]"),
      p("[Twitter]")
    ),
    
    ############################
    
    
    ############## FOURTH PAGE ##############
    
    tabPanel("Case Studies",
             fluidRow(
               column(
                 width = 7,
                 selectInput(
                   "river_basin",
                   "River Basin:",
                   choices = c("Jordan River" = "JORD",
                               "Mekong River" = "MEKO")
                 ),
                 dataTableOutput("case_study_table")
               ),
               column(
                 width = 5,
                 plotOutput("case_conflict_over_time"),
                 plotOutput("treaties_over_time")
               )
             )),
    
    ############################
    
    
    ############## FIFTH PAGE ##############
    
    tabPanel(
      "About",
      h3("Background"),
      p(
        "In recent years, journalists, policymakers, and academics
                              have become increasingly worried about the potential for
                              climate-induced water scarcity to cause international conflict.
                              Approximately",
        a("1.2 billion people",
          href = "https://www.un.org/waterforlifedecade/scarcity.shtml"),
        "live
                                in water-scarce areas worldwide, and this number is expected to
                              increase significantly under the stress of ",
        a("climate change",
          href = "https://blogs.ei.columbia.edu/2019/09/23/climate-change-impacts-water/"),
        "."
      ),
      p(
        "Some academics have suggested that countries will turn to violent conflict as they
                              attempt to preserve precious transboundary water supplies. Others, led by",
        a("Elinor Ostrom", href = "https://www.econlib.org/library/Enc/bios/Ostrom.html"),
        "winner
                              of the Nobel Prize in Economics, have suggested that increased scarcity will motivate states
                              to cooperate with one another as they attempt to collectively govern shared waters."
      ),
      p(
        "While many academics have conducted in-depth qualitative case studies of water governance
                            in transboundary river basins—including the Jordan River, Nile River, and Indus River—few
                            quantitative studies have examined the effect of resource scarcity on water conflict.
                            This project aims to fill that research gap using newly-released datasets covering water
                            governance outcomes over the past 200 years. In so doing, it seeks to answer the following
                            question: Is water scarcity correlated with cooperation or conflict between states?"
      ),
      p(
        "This project's GitHub repository lives",
        a("here", href = "https://github.com/wyatthurt/gov1005-final-project"),
        "."
      ),
      
      h3("The Data"),
      p(
        "This project draws heavily on the following datasets from the Oregon State University",
        a("Program in Water Conflict Management and Transformation", href = "https://transboundarywaters.science.oregonstate.edu/content/data-and-datasets"),
        tags$ol(
          tags$li(
            "International Freshwater Treaties Database: A dataset of over 600 international,
                                 freshwater-related agreements, covering the years 1820 to 2007. This dataset
                                 provides detailed information on the issues covered by treaties and is coded
                                 by river basin, enabling it to be joined with other datasets from OSU."
          ),
          tags$li(
            "International Water Events Database: A dataset of water conflicts between 1948
                                 and 2008, which provides information on the source of conflict, duration of conflict,
                                 and how resolution was achieved."
          ),
          tags$li(
            "International River Basin Organization Database: A dataset of currently-extant
                                 international river basin organizations, coded by basin and countries involved, including
                                 level and type of collaboration, principal issue(s), date of creation, and a short
                                 description of the organization's activities."
          )
        ),
        "This research also uses data from the",
        a("World Bank", href = "https://data.worldbank.org"),
        "accessed using the",
        a("wbstats() package", href = "https://cran.r-project.org/web/packages/wbstats/index.html"),
        "in R."
      ),
      
      h3("Acknowledgements"),
      p(
        "This project conceptually builds on several of my previous and current research projects, completed
                              under the supervision of Professor William Clark, Professor Rosie Bsheer, Dr. Michaela Thompson, and
                              Dr. Alicia Harley. I am also grateful for the guidance provided by David Kane and Alyssa Huberts as I
                              learned the art of data science and developed this project."
      ),
      
      h3("About Me"),
      p(
        "I am a third-year undergraduate at Harvard University, pursuing a joint degree in Environmental
                              Science & Public Policy and Near Eastern Languages & Civilizations. My research (and upcoming
                              senior honors thesis) focuses on mechanisms for improving the governance of transboundary natural
                              resources in conflict zones. You can reach me at",
        a("wyatthurt@college.harvard.edu", href = "mailto:wyatthurt@college.harvard.edu"),
        "."
      )
      
    )
  )
)
