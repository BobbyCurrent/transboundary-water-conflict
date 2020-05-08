# Transboundary Water Conflict
#### A Final Project for GOV 1005 (Data) at Harvard University

### Project Summary
Since the 1990s, there has been a precipitous increase in the frequency and severity of water conflicts around the world. Scholars have primarily studied this phenomenon qualitatively and at a regional level, hypothesizing that increased water conflict might be due to scarcity resulting from population increases, economic development, climatic conditions, or anti-democratic governance. This project compiles and analyzes a wide variety of metrics for the 254 major river basins of the world, taking a first step toward evaluating the claims of these qualitative scholars. By showing which factors are correlated with water conflict, this project serves as the groundwork for my senior honors thesis on the governance of transboundary natural resources in the Middle East.

[View the live Shiny App here](https://wyatthurt.shinyapps.io/water_conflict/).

### Repository Guide
* /archive: Archived course milestone assignments.
* /shiny-app: Files to render Shiny App.
    * /raw_data: Raw data files from the Oregon State University Program on Water Conflict Transformation, and country/river basin polygons from the World Bank.
    * ag_land.rds: Agricultural land per country-year. Rendered using the 'gather_raw_data.R' script.
    * basins.rds: List of world river basins, compiled by joining the World Bank river basin index and the Oregon State University river basin index.
    * eiu_mod.rds: Democracy data per country in 2015. Rendered using the 'gather_raw_data.R' script.
    * gather_leaflet.R: Modifies river basin and country shapefiles to include relevant metrics.
    * gather_raw_data.R: Gathers, cleans, tidies and joins 8 datasets (water conflict metrics and country-level statistics).
    * gdp.rds: GDP per country-year. Rendered using the 'gather_raw_data.R' script.
    * joined.rds: Master dataset with all relevant metrics, organized by country-year, joined and cleaned from all the raw datasets. Renered using the 'gather_raw_data.R' script.
    * joined_logistic.rds: Transforms the 'joined.rds' dataset to be used for logistic regression (each basin-year is coded 0/1, where 1 means that an event occured).
    * joined_summarized.rds: Transforms the 'joined.rds' dataset to be used for glm regression (by performing summary statistics at a basin-year level, averaged over the period extending from 1960 to 2009).
    * pop.rds: Population per country-year. Rendered using the 'gather_raw_data.R' script.
    * server.R: Shiny App server script.
    * terms.rds: Terms for chatterplot. Rendered using the 'gather_raw_data.R' script.
    * trade_percent_gdp.rds: Trade as a percent of GDP per country-year. Rendered using the 'gather_raw_data.R' script.
    * ui.R: Shiny App UI script.
    * water_withdraw.rds: Water withdrawal as a percentage of total water resources per country-year. Rendered using the 'gather_raw_data.R' script.
* .gitignore
* README.md



