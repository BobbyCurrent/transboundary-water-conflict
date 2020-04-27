############## PREP ##############

# Load necessary libraries.

library(shiny)
library(shinythemes)
library(DT)
library(leaflet)
library(lubridate)
library(stats)
library(dplyr)
library(tm)
# library(sjPlot)
library(wordcloud)

# Read in datasets created using the 'gather_raw_data.R' script.

joined <- readRDS("joined.rds")
joined_summarized <- readRDS("joined_summarized.rds")
joined_logistic <- readRDS("joined_logistic.rds")

shinyUI(
  navbarPage(
    theme = shinytheme("yeti"),
    "Transboundary Water Conflict",
    
    ############################
    
    
    ############## FIRST PAGE ##############
    
    tabPanel(
      "The Puzzle",
      h2("Water conflict: A puzzling issue."),
      p(
        "Since the 1990s, there has been a precipitous increase in the frequency and severity of water conflicts around the world.
      Scholars have primarily studied this phenomenon qualitatively and at a regional level, hypothesizing that increased water conflict might be due to scarcity resulting from population increases, economic development, climatic conditions, or anti-democratic governance.
      This project compiles and analyzes a wide variety of metrics for the 254 major river basins of the world, taking a first step toward evaluating the claims of these qualitative scholars.
      By showing which factors are correlated with water conflict, this project serves as the groundwork for my senior honors thesis on the governance of transboundary natural resources in the Middle East."
      ),
      
      # Generate a 2x2 column/row layout.
      
      fluidPage(
        fluidRow(style = 'padding:30px;',
                 column(7,
                        
                        # Plot histogram of water conflict events over time.
                        
                        plotOutput("time_histogram")),
                 column(
                   5,
                   h3("Water conflict is increasing with time."),
                   p(
                     "As the graph at left shows, there has been a general upward trend in water conflict over time, with an especially significant increase beginning in the 1990s.
                   This increase has been especially significant in Asia and Europe, while South America has seen a large decrease in conflict.
                     Scholars have attributed this increase to a number of different factors.
                     Interestingly, note that Africa, which frequently dominates global narratives about water conflict, has experienced relatively few conflict events.
                     Of course, it is important to remember that the severity of conflict is independent from the frequency of conflict events."
                   )
                 )),
        fluidRow(column(
          4,
          h3("Water conflict occurs around the world."),
          p(
            "As can be seen in the map at right, water conflict is a ubiquitous phenomenon.
            However, there are hotspots.
            Click on the river basins at right for counts of their water conflict events."
          ),
          
          # Plot table displaying water conflict events per basin. 
          
          dataTableOutput("frequency")
        ),
        column(8,
               
               # Plot Leaflet map.
               
               leafletOutput("map", height = 700)))
      )
    ),
    
    ############################
    
    
    ############## SECOND PAGE ##############
    
    tabPanel(
      "Predicting Water Conflict",
      h2("Predicting Water Conflict"),
      p(
        "This project's dataset is the first publicly-available data source to provide comprehensive statistics at a basin level for all of the world's major River Basins.
        Combining data from the Oregon State University Program on Water Conflict Transformation, the World Bank, the United Nations, and the EIU Democracy Index, this dataset covers all 254 major river basins of the world from 1960 to 2009:"
      ),
      tags$ul(
        tags$li(
          "Basin GDP is country-level GDP for each year, either summed or averaged across each river basin (source: World Bank)"
        ),
        tags$li(
          "Basin population is country-level population for each year, either summed or averaged across each river basin (source: United Nations"
        ),
        tags$li(
          "Trade as a percentage of GDP is given at a country level for each year, averaged across each river basin (source: World Bank)"
        ),
        tags$li(
          "Rate of water consumption is the amount of water each basin country consumes, divided by the country's total available water resources. This metric is averaged across each river basin (source: World Bank)"
        ),
        tags$li(
          "Agricultural land is the total area of agricultural land per country for each year (in km^2), either summed or averaged (source: World Bank)"
        ),
        tags$li(
          "Percent of population affected by drought is the percentage of each country's population that experienced a drought event over the 60 year period, averaged across each river basin (source: World Bank)"
        ),
        tags$li(
          "The Democratization index score is the democracy score (from 1 to 10, where 10 is most democratic) assigned to each country in 2015, averaged across each river basin (source: EIU Democratization Index)"
        ),
        tags$li(
          "All of the above variables can be logged() as well, which can improve the quality of regression."
        )
      ),
      p(
        "Having so many statistics at the basin (rather than country) level provides a rich opportunity to test various hypotheses about water conflict. Use the dashboard below to construct a model.
        Then, use the sliders at left to predict the expected level of water conflict over a 50 year period, given your inputs.
        Note that several other modeling options were explored as part of this project, but I could not establish strong relationships between any of the given explanatory variables and the propensity for water conflict within a given river bain in a given year (using either logistic or linear regression).
        However, as can be seen below, strong correlations can be observed when data is aggregated over a 50 year period. This suggests that it is very difficult to predict whether water conflict will occur within a given year, but that long-range estimates can be quite accurate."
      ),
      sidebarLayout(
        sidebarPanel(
          h4("Construct the Model:"),
          
          # Select X variable(s) for model.
          
          selectInput(
            "varOI_x",
            "X variable:",
            choices = c(
              "Number of conflict events" = "event_count",
              "Total basin GDP" = "gdp_total",
              "Average GDP" = "gdp_avg",
              "Total basin population" = "pop_total",
              "Average population" = "pop_avg",
              "Average % GDP made up by trade" = "trade_percent_gdp_avg",
              "Average rate of water consumption" = "water_withdraw_avg",
              "Total basin agricultural land" = "ag_land_total",
              "Average agricultural land" = "ag_land_avg",
              "Average % of population affected by drought" = "droughts_avg",
              "Average democratization index score" = "eiu_avg",
              "Log(number of conflict events)" = "event_count_log",
              "Log(total basin GDP)" = "gdp_total_log",
              "Log(average GDP)" = "gdp_avg_log",
              "Log(total basin population)" = "pop_total_log",
              "Log(average population)" = "pop_avg_log",
              "Log(average % gdp made up by trade)" = "trade_percent_gdp_avg_log",
              "Log(average rate of water consumption)" = "water_withdraw_avg_log",
              "Log(total basin agricultural land)" = "ag_land_total_log",
              "Log(average agricultural land)" = "ag_land_avg_log",
              "Log(average % of population affected by drought)" = "droughts_avg_log",
              "Log(average democratization index score)" = "eiu_avg_log"
            ),
            multiple = TRUE,
            selected = "gdp_avg"
          ),
          
          # Select Y variable(s) for model.
          
          selectInput(
            "varOI_y",
            "Y variable:",
            choices = c(
              "Number of conflict events" = "event_count",
              "Total basin GDP" = "gdp_total",
              "Average GDP" = "gdp_avg",
              "Total basin population" = "pop_total",
              "Average population" = "pop_avg",
              "Average % GDP made up by trade" = "trade_percent_gdp_avg",
              "Average rate of water consumption" = "water_withdraw_avg",
              "Total basin agricultural land" = "ag_land_total",
              "Average agricultural land" = "ag_land_avg",
              "Average % of population affected by drought" = "droughts_avg",
              "Average democratization index score" = "eiu_avg",
              "Log(number of conflict events)" = "event_count_log",
              "Log(total basin GDP)" = "gdp_total_log",
              "Log(average GDP)" = "gdp_avg_log",
              "Log(total basin population)" = "pop_total_log",
              "Log(average population)" = "pop_avg_log",
              "Log(average % gdp made up by trade)" = "trade_percent_gdp_avg_log",
              "Log(average rate of water consumption)" = "water_withdraw_avg_log",
              "Log(total basin agricultural land)" = "ag_land_total_log",
              "Log(average agricultural land)" = "ag_land_avg_log",
              "Log(average % of population affected by drought)" = "droughts_avg_log",
              "Log(average democratization index score)" = "eiu_avg_log"
            ),
            selected = "event_count"
          ),
          
          # Select method for constructing model.
          
          checkboxInput("toggleLinear", label = "Linear Model", value = TRUE),
          checkboxInput("toggleLoess", label = "Loess Model", value = FALSE),
          checkboxInput("toggleMulti", label = "Multivariate Regression", value = FALSE),
          checkboxInput("toggleMultiinteraction", label = "Multivariate Regression w/ Interaction", value = FALSE),
          
          # Dependent variable slider inputs, used to predict the number of
          # water conflict events in a hypothetical basin with the given inputs
          # over a 50 year period.
          
          h4("Predict Future Water Conflict:"),
          sliderInput(
            "event_count_pred",
            "Number of conflict events:",
            min = 0,
            max = 250,
            value = 50
          ),
          sliderInput(
            "gdp_total_pred",
            "Total basin GDP ($):",
            min = 0,
            max = 100000000000000,
            value = 100000
          ),
          sliderInput(
            "gdp_avg_pred",
            "Average GDP ($):",
            min = 0,
            max = 100000000000000,
            value = 100000
          ),
          sliderInput(
            "pop_total_pred",
            "Total basin population:",
            min = 0,
            max = 6000000000,
            value = 100000000
          ),
          sliderInput(
            "pop_avg_pred",
            "Average population:",
            min = 0,
            max = 6000000000,
            value = 100000000
          ),
          sliderInput(
            "trade_percent_gdp_avg_pred",
            "Average % GDP made up by trade:",
            min = 0,
            max = 100,
            value = 50
          ),
          sliderInput(
            "water_withdraw_avg_pred",
            "Average rate of water consumption (as % of total resources):",
            min = 0,
            max = 1000,
            value = 50
          ),
          sliderInput(
            "ag_land_total_pred",
            "Total basin agricultural land (km^2):",
            min = 0,
            max = 1000000,
            value = 10000
          ),
          sliderInput(
            "ag_land_avg_pred",
            "Average basin agricultural land (km^2):",
            min = 0,
            max = 1000000,
            value = 10000
          ),
          sliderInput(
            "droughts_avg_pred",
            "Average % of population affected by drought:",
            min = 0,
            max = 100,
            value = 50
          ),
          sliderInput(
            "eiu_avg_pred",
            "Average democratization index score:",
            min = 0,
            max = 10,
            value = 5
          )
        ),
        
        mainPanel(
          
          # Output scatterplot with line of best fit based on model.
          
          plotOutput("water_regression", height = 500),
          
          # Output summary of regression output. I would like to present this in a nicer format over the next 10 days.
          
          verbatimTextOutput(outputId = "RegSum"),
          p(
            "Now, let's take the model above and predict the number of water conflict events over the next 50 years for a a hypothetical river basin with the inputs given at left.
          Three numbers are provided. 'Fit' is the model's best guess for the number of conflict events. 'Low' and 'high' provide the range of values within which we are 95% confident that the true value lies."
          ),
          
          # Output predict() results. 
          
          verbatimTextOutput(outputId = "predictSum"),
          
          # Use CSS styling to hide all error messages. This is necessary
          # because the scatterplot displays an error if more than 1 X variable
          # is selected. I ran many tests and could not find a reason why this
          # would be a problem (i.e. where displaying error results would be
          # necessary).
          
          tags$style(
            type = "text/css",
            ".shiny-output-error {display: none;}",
            ".shiny-output-error:before {display: none;}"
          )
        )
      )
    ),
    
    ############################
    
    
    ############## THIRD PAGE ##############
    
    tabPanel("Key Issues",
             sidebarLayout(
               
               # Slider inputs to set the minimum number of occurences and
               # maximum number of issues that can appear in the interactive
               # word cloud.
               
               sidebarPanel(
                 sliderInput(
                   "freq",
                   "Minimum Number of Occurences:",
                   min = 1,
                   max = 500,
                   value = 200
                 ),
                 sliderInput(
                   "max",
                   "Maximum Number of Issues:",
                   min = 1,
                   max = 300,
                   value = 100
                 )
               ),
               
               mainPanel(
                 p(
                   "Event frequency can only tell us so much.
               Now, we examine the issues that most frequently arise in water conflicts.
                 The word cloud below displays terms from water conflict event descriptions written by the Oregon State University Program on Water Conflict Transformation.
                 The more frequently a word appears in the dataset, the larger it is in the plot below.
                 Use the sliders at left to adjust the thresholds required for a word to appear in the word cloud."
                 ),
                 
                 # Render word cloud.
                 
                 plotOutput("plot")
               )
             )),
    
    ############################
    
    
    ############## FOURTH PAGE ##############
    
    tabPanel(
      "Case Studies",
      h2("Case Studies"),
      p(
        "Aggregate data are useful, but it is also important to examine variation and nuances between individual river basins.
             The dashboard below provides a number of measures at a basin-specific level.
             Use the dropdown to select one of the 254 basins included in this dataset.
             If there are no relevant elements for a given measure, its corresponding graph or table will not appear."
      ),
      fluidRow(
        column(
          width = 7,
          
          # Dropdown menu to select the relevant river basin. If I could find a
          # way to automaically generate a dropdown, that would be great. For
          # now, I just wrote a quick R script that generates the list based on
          # the 'joined' dataset.
          
          selectInput(
            "river_basin",
            "River Basin:",
            choices = c(
              "Aral Sea",
              "Cestos",
              "Helmand",
              "Indus",
              "Akpa",
              "Alsek",
              "Amacuro",
              "Amur",
              "An Nahr Al Kabir",
              "Asi/Orontes",
              "Artibonite",
              "Astara Chay",
              "Atrak",
              "Aviles",
              "Awash",
              "Aysen",
              "Baker",
              "Bann",
              "Bidasoa",
              "Benito/Ntem",
              "Bia",
              "Beilun",
              "Belize",
              "Bangau",
              "Baraka",
              "Barima",
              "Barta",
              "Buzi",
              "Candelaria",
              "Changuinola",
              "Carmen Silva/Chico",
              "Chira",
              "Choluteca",
              "Chuy",
              "Chilkat",
              "Columbia",
              "Chiloango",
              "Cancoso/Lauca",
              "Congo/Zaire",
              "Coco/Segovia",
              "Comau",
              "Cross",
              "Coruh",
              "Castletown",
              "Coatan Achute",
              "Catatumbo",
              "Cullen",
              "Cavally",
              "Daoura",
              "Dnieper",
              "Dniester",
              "Don",
              "Dragonja",
              "Dra",
              "Dasht",
              "Daugava",
              "Douro/Duero",
              "Ebro",
              "Elancik",
              "Erne",
              "Cuvelai/Etosha",
              "Fane",
              "Flurry",
              "Fenney",
              "Foyle",
              "Fraser",
              "Firth",
              "Gash",
              "Grijalva",
              "Glama",
              "Golok",
              "Goascoran",
              "Garonne",
              "Great Scarcies",
              "Guadiana",
              "Guir",
              "Gauja",
              "Han",
              "Hari/Harirud",
              "Hamun-i-Mashkel/Rakshan",
              "Hondo",
              "Bei Jiang/Hsi",
              "Incomati",
              "Ili/Kunes He",
              "Isonzo",
              "Jayapura",
              "Jacobs",
              "Jordan",
              "Juba-Shibeli",
              "Jurado",
              "Kaladan",
              "Kemi",
              "Kogilnik",
              "Komoe",
              "Karnaphuli",
              "Kowl E Namaksar",
              "Krka",
              "Klaralven",
              "Kunene",
              "Kura-Araks",
              "Lava/Pregel",
              "Lotagipi Swamp",
              "Lima",
              "Lake Chad",
              "Lake Fagnano",
              "Lake Natron",
              "Lake Titicaca-Poopo System",
              "Lake Turkana",
              "Lake Ubsa-Nur",
              "Lielupe",
              "Lough Melvin",
              "Lempa",
              "Limpopo",
              "Lagoon Dos Patos-Lagoon Mirim",
              "Loes",
              "Loffa",
              "Little Scarcies",
              "Mana-Morro",
              "Maro",
              "Massacre",
              "Mbe",
              "Medjerda",
              "Mekong",
              "Muhuri (aka Little Feni)",
              "Mino",
              "Mira",
              "Mius",
              "Moa",
              "Moho",
              "Mono",
              "Motaqua",
              "Murgab",
              "Maritsa",
              "Naatamo",
              "Negro",
              "Nelson-Saskatchewan",
              "Nahr El Kebir",
              "Neman",
              "Neretva",
              "Narva",
              "Nestos",
              "Nyanga",
              "Oued Bon Naima",
              "Oder/Odra",
              "Ogooue",
              "Okavango",
              "Olanga",
              "Oral/Ural",
              "Orange",
              "Oueme",
              "Oulu",
              "Oiapoque/Oyupock",
              "Pangani",
              "Paz",
              "Pedernales",
              "Pakchan",
              "Palena",
              "Pandaruan",
              "Po",
              "Prohladnaja",
              "Parnu",
              "Pascua",
              "Psou",
              "Pasvik",
              "Patia",
              "Puelo",
              "Pu Lun T'o",
              "Red/Song Hong",
              "Rezvaya",
              "Rio Grande (North America)",
              "Rio Grande (South America)",
              "Rhone",
              "Roia",
              "Sabi",
              "Salaca",
              "Samur",
              "Sanaga",
              "Sassandra",
              "Sebuku",
              "Seine",
              "Senegal",
              "Seno Union/Serrano",
              "Sepik",
              "Shu/Chu",
              "Sixaola",
              "St. John (North America)",
              "San Juan",
              "Skagit",
              "St. Lawrence",
              "San Martin",
              "Sembakung",
              "St. Paul",
              "Sarata",
              "Sarstun",
              "Stikine",
              "Struma",
              "Suchiate",
              "Sujfun",
              "Tafna",
              "Tagus/Tejo",
              "Taku",
              "Talas",
              "Tami",
              "Tana",
              "Tano",
              "Temash",
              "Terek",
              "Tijuana",
              "Tjeroaka-Wanggoe",
              "Torne/Tornealven",
              "Tarim",
              "Tuloma",
              "Tumbes",
              "Umba",
              "Utamboni",
              "Vanimo-Green",
              "Valdivia",
              "Venta",
              "Vijose",
              "Velaka",
              "Volga",
              "Vardar",
              "Vistula/Wista",
              "Vuoksa",
              "Whiting",
              "Wiedau",
              "Yalu",
              "Yaqui",
              "Yelcho",
              "Jenisej/Yenisey",
              "Yser",
              "Yukon",
              "Zapaleri",
              "Essequibo",
              "Amazon",
              "Lake Prespa",
              "Elbe",
              "Zambezi",
              "Zarumilla",
              "Niger",
              "Gambia",
              "Pungwe",
              "Mississippi",
              "Gallegos/Chico",
              "La Plata",
              "Orinoco",
              "Digul",
              "Maputo",
              "Nile",
              "Drin",
              "Adige",
              "Annole",
              "Bahr at Tubat",
              "Oued Bou Namoussa",
              "Caetani",
              "Cetina",
              "Connecticut",
              "Copper",
              "Galana",
              "Gruzskiy Yelanchik",
              "Indalsalven",
              "Laguna Filaret",
              "Lak Dera",
              "Lake Azuei",
              "Lake Cayo",
              "Lake Chilwa",
              "Lake Enriquillo",
              "Lake Rukwa",
              "Lake Sarygamesh",
              "Lucia",
              "Narynka",
              "Nidelva",
              "Peschanaya",
              "Poldnevaya",
              "Rach Giang Thanh",
              "Santa Clara",
              "Song Tien Yen",
              "Berbyelva",
              "Umbeluzi",
              "Unuk",
              "Vecht",
              "Vefsna",
              "Angerman",
              "Colorado",
              "Connecticut",
              "Danube",
              "Fly",
              "Ganges-Brahmaputra-Meghna",
              "Geba-Corubal",
              "Mataje",
              "Rann of Kutch",
              "Rhine-Meuse",
              "Ruvuma",
              "Nha Be-Saigon-Song Vam Co Dong",
              "St. Croix",
              "Schelde",
              "St. John (Africa)",
              "Tumen",
              "Volta",
              "Corantijn/Courantyne",
              "Har Us Nur",
              "Maroni",
              "Ob",
              "Sulak",
              "Alakol",
              "Ca/Song Lam",
              "Irrawaddy",
              "Ma",
              "Salween",
              "Tigris-Euphrates/Shatt al Arab",
              "Naaf River"
            )
          ),
          
          # Display event data table.
          
          dataTableOutput("case_study_table")
        ),
        column(
          width = 5,
          
          # Display histogram with conflict over time.
          
          plotOutput("case_conflict_over_time"),
          
          # Display histogram with treaties signed over time. 
          
          plotOutput("treaties_over_time")
        )
      )
    ),
    
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
                            governance outcomes between 1960 and 2009. In so doing, it seeks to take a first step toward evaluating claims about the causes of water conflict."
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
    
    ############################
    
  )
)
