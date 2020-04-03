library(readr)
library(janitor)
library(lubridate)
library(wbstats)
library(naniar)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)
library(rgdal)
library(gt)
library(tidytext)
library(twitteR)

joined <- read_rds("joined.rds")
pop <- read_rds("pop.rds")
gdp <- read_rds("gdp.rds")
trade_percent_gdp <- read_rds("trade_percent_gdp.rds")
water_avail <- read_rds("water_avail.rds")

# Downloaded river basin shapefiles from the World Bank.Source link:
# https://datacatalog.worldbank.org/dataset/major-river-basins-world

basins_geometry <- readOGR("raw_data/geometry/wb_major_basins", layer = "Major_Basins_of_the_World")

# Downloaded country shapefiles from the ArcGIS Hub. Source link:
# https://hub.arcgis.com/datasets/a21fdb46d23e4ef896f31475217cbb08_1

countries_geometry <- readOGR("raw_data/geometry/countries_wgs84", layer = "Countries_WGS84")

events_n <- joined %>%
  # filter(str_detect(event_summary, c("conflict", "war", "violence", "military"))) %>%
  distinct(event_date, .keep_all = TRUE) %>%
  group_by(basin_name) %>%
  rename(NAME = basin_name) %>%
  count() %>%
  rename(num_events = n)

# Same approach, for treaties.

treaties_n <- joined %>%
  distinct(document_name, .keep_all = TRUE) %>%
  group_by(basin_name) %>%
  rename(NAME = basin_name) %>%
  count() %>%
  rename(num_treaties = n)

# Same approach, for organizations. R is throwing a weird error when I try to
# plot this data, so I commented it out for now.

# orgs_n <- joined %>%
# distinct(rbo_name, .keep_all = TRUE) %>%
# group_by(rbo_name) %>%
# rename(NAME = rbo_name) %>%
# count() %>%
# rename(num_orgs = n)

# It would be best to derive gdp, population, and trade from my "joined"
# dataset, and allow the user to select a year to see these variables, in
# addition to the organizations, treaties, and conflicts present in that year.
# Once I have Shiny set up, I think I will be able to do that. For now, I just
# displayed the most recently-available GDP from the World Bank dataset (2015).

gdp_mapping <- gdp %>%
  filter(date == 2015) %>%
  rename(CNTRY_NAME = country_name)

pop_mapping <- pop %>%
  filter(date == 2015) %>%
  rename(CNTRY_NAME = country_name)

trade_mapping <- trade_percent_gdp %>%
  filter(date == 2015) %>%
  rename(CNTRY_NAME = country_name)


################# Modifying Polygon Shapefiles #################

# I then merged these counts with my polygon data. Leaflet throws an error if
# the variable column includes NA values, so I replaced all NA values.

basins_geometry@data <- left_join(basins_geometry@data, events_n, by = "NAME") %>%
  left_join(treaties_n, by = "NAME")

# %>% left_join(orgs_n, by = "NAME")

# Same approach for countries.

countries_geometry@data <- left_join(countries_geometry@data, gdp_mapping, by = "CNTRY_NAME") %>%
  left_join(pop_mapping, by = "CNTRY_NAME") %>%
  left_join(trade_mapping, by = "CNTRY_NAME")
