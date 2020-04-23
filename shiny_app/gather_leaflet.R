############## PREP ##############

# Necessary libraries

library(readr)
library(janitor)
library(rgdal)
library(tidyverse)

# Read in joined water conflict dataset.
joined <- read_rds("joined.rds")

# Read in World Bank country code key.

wb_codes <- read_csv("raw_data/wb_country_codes_2.csv") %>%
  clean_names() %>%
  mutate(country_code = three_letter_country_code)

# Read in World Bank population dataset. Left-join with WB country code key.
pop <- read_rds("pop.rds") %>%
  left_join(wb_codes, by = c("code" = "country_code"))

# Read in World Bank GDP dataset. Left-join with WB country code key.
gdp <- read_rds("gdp.rds") %>%
  left_join(wb_codes, by = c("code" = "country_code"))

# Read in World Bank Trade as a Percentage of GDP dataset. Left-join with WB country code key.

trade_percent_gdp <- read_rds("trade_percent_gdp.rds") %>%
  left_join(wb_codes, by = c("code" = "country_code"))

# Read in World Bank % of the Population With Regular Access to Fresh Water dataset. Left-join with WB country code key.

water_avail <- read_rds("water_avail.rds") %>%
  left_join(wb_codes, by = c("code" = "country_code"))

# Downloaded river basin shapefiles from the World Bank. Source link:
# https://datacatalog.worldbank.org/dataset/major-river-basins-world

basins_geometry <-
  readOGR("raw_data/geometry/wb_major_basins", layer = "Major_Basins_of_the_World")

# Downloaded country shapefiles from the ArcGIS Hub. Source link:
# https://hub.arcgis.com/datasets/a21fdb46d23e4ef896f31475217cbb08_1

countries_geometry <-
  readOGR("raw_data/geometry/countries_wgs84", layer = "Countries_WGS84")

############################


############## PROCESS DATA ##############

# Find the number of events per basin per year.

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

# Filter for the most recent measure of each relevant variable per country.

gdp_mapping <- gdp %>%
  filter(date == 2015) %>%
  rename(CNTRY_NAME = country_name)

pop_mapping <- pop %>%
  filter(date == 2015) %>%
  rename(CNTRY_NAME = country_name)

trade_mapping <- trade_percent_gdp %>%
  filter(date == 2015) %>%
  rename(CNTRY_NAME = country_name)


############################


################# Modifying Polygon Shapefiles #################

# I then merged these counts with my polygon data. Leaflet throws an error if
# the variable column includes NA values, so I replaced all NA values.

basins_geometry@data <-
  left_join(basins_geometry@data, events_n, by = "NAME") %>%
  left_join(treaties_n, by = "NAME")

# %>% left_join(orgs_n, by = "NAME")

# Same approach for countries.

# countries_geometry@data <-
# left_join(countries_geometry@data, gdp_mapping, by = "CNTRY_NAME") %>%
# left_join(pop_mapping, by = "CNTRY_NAME") %>%
# left_join(trade_mapping, by = "CNTRY_NAME")
