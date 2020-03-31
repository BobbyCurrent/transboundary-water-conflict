#################### Setup ##############################

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

# I began by downloading river basin shapefiles from the World Bank. It took me
# several different tries (and packages) before I figured out that readOGR(),
# from the rgdal package, was the best option, despite the fact that it produces
# a strange output: a PolygonsDataFrame. Wrestling with this strange format
# became more important when creating my demo map. Source link:
# https://datacatalog.worldbank.org/dataset/major-river-basins-world

basins_geometry <- readOGR("raw_data/geometry/wb_major_basins", layer = "Major_Basins_of_the_World")

# I also downloaded country shapefiles from the ArcGIS Hub. Source link:
# https://hub.arcgis.com/datasets/a21fdb46d23e4ef896f31475217cbb08_1

countries_geometry <- readOGR("raw_data/geometry/countries_wgs84", layer = "Countries_WGS84")

# I tried the following approaches when Deprecated approaches
# CRS("+proj=longlat +datum=WGS84") already has CRS arguments
# reproject <- CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
# basins_geometry <- st_transform(basins_geometry, "+proj=longlat +ellps=WGS84 +datum=WGS84")

consumer_key <- "cWehMlSV3XSkYPOmx8uTbVHkB"
consumer_secret <- "xTbcXiXSNH7fEm7P6kh81Kkf5XTUdlHp3sysCukt5JeewMmqtN"
access_token <- "2722523293-qNjrF5uWr7VDnSuSSzzJr50bEWChsigqD2g6wNl"
access_secret <- "EihgBXI9Ew8vSfNPjsjzaqP5VtWv0gxvYCAjv6fvRMs5F"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#################### Events ##############################

# The "events" data is already in tidy format, but it is very messy. For now, I
# selected the columns relevant to my analysis, set each column to the correct
# type, and recoded appropriate values as NA. Once it becomes clearer which data
# is relevant to my final product, I will be able to further narrow down columns
# and perform even more rigorous data cleaning.

na_strings <- c("NA", "N.A.", "n/a", "?location", "?relvant", ".", " ")

events <- read_csv("raw_data/events_raw.csv") %>%
  clean_names() %>%
  replace_with_na_all(condition = ~.x %in% na_strings) %>%
  select(!c(id, id1, event_master, event_type)) %>%
  mutate(date = ymd(date), doc_date = ymd(doc_date)) %>%
  mutate(year = year(date))

# I spent several hours attempting to recode messy observations in the
# "event_type" column, before realizing that two other columns ("issue_type1"
# and "issue_type2") provided what I think is the same data in a format that was
# much easier to parse. However, the code is included below in case it is of use
# later.

# See raw_data/events_codebook.doc for a detailed description of each column.

#################### Organizations ##############################

# The "organizations" data is also already in tidy format. Since it has fewer
# columns, it was relatively less messy and required less cleaning.

organizations <- read_csv("raw_data/organizations_raw.csv") %>%
  clean_names() %>%
  replace_with_na_all(condition = ~.x %in% na_strings)

# See raw_data/organizations_codebook.pdf for a detailed description of each column.

#################### Treaties ##############################

# The "treaties" data is also already in tidy format. It contains many columns
# (114), and it is unclear precisely which will be most relevant to my analysis.
# Thus, I refrained from recoding any of their values until I have a better idea
# where I want to take the final project. For now, I replaced the appropriate
# values with NA and selected the relevant columns.

treaties <- read_csv("raw_data/treaties_raw.csv") %>%
  clean_names() %>%
  replace_with_na_all(condition = ~.x %in% na_strings) %>%
  select(!c(entry_id))

# See raw_data/treaties_codebook.pdf for a detailed description of each column.

#################### World Bank Stats ##############################

# Import World Bank country codes:
# https://wits-worldbank-org.ezp-prod1.hul.harvard.edu/wits/wits/witshelp/content/codes/country_codes.htm):
wb_codes <- read_csv("raw_data/wb_country_codes.csv")

# I will use the wbstats() package to pull World Bank data for the countries
# involved in water treaties and conflicts. I expect that the most relevant
# measures will be population, GDP and trade as a percentage of GDP (which could
# serve as a rough proxy for international integration).

pop <- wb(indicator = "SP.POP.TOTL", startdate = 1900, enddate = 2015) %>%
  select(code = iso3c, date, pop = value) %>%
  mutate(date = as.double(date)) %>%
  left_join(wb_codes, by = c("code" = "country_code"))

gdp <- wb(indicator = "NY.GDP.PCAP.CD", startdate = 1900, enddate = 2015) %>%
  select(code = iso3c, date, gdp = value) %>%
  mutate(date = as.double(date)) %>%
  left_join(wb_codes, by = c("code" = "country_code"))

trade_percent_gdp <- wb(indicator = "TG.VAL.TOTL.GD.ZS", startdate = 1900, enddate = 2015) %>%
  select(code = iso3c, date, trade_percent_gdp = value) %>%
  mutate(date = as.double(date)) %>%
  left_join(wb_codes, by = c("code" = "country_code"))

water_avail <- wb(indicator = "SH.H2O.SMDW.ZS", startdate = 1900, enddate = 2015) %>%
  select(code = iso3c, date, water = value) %>%
  mutate(date = as.double(date)) %>%
  left_join(wb_codes, by = c("code" = "country_code"))

# SH.H2O.BASW.ZS

# See http://127.0.0.1:15723/library/wbstats/doc/Using_the_wbstats_package.html
# for a user guide on using the wbstats() package.

#################### Joining ##############################
events_tidy <- events %>%
  pivot_longer(cols = c(ccode1, ccode2),
               names_to = "ccode_id",
               values_to = "ccode") %>%
  mutate(event_year = year(date))

joined <- events_tidy %>%
  full_join(organizations, by = "ccode", suffix = c("_events", "_orgs")) %>%
  full_join(treaties, by = "ccode", suffix = c("_events_orgs", "_treaties")) %>%
  left_join(pop, by = c("ccode" = "code", "event_year" = "date"), suffix = c("", "_pop")) %>%
  left_join(gdp, by = c("ccode" = "code", "event_year" = "date"), suffix = c("", "_gdp")) %>%
left_join(water_avail, by = c("ccode" = "code", "event_year" = "date"), suffix = c("", "_water"))%>%
  left_join(trade_percent_gdp, by = c("ccode" = "code", "event_year" = "date"), suffix = c("", "_trade_percent_gdp")) %>%
  distinct(ccode, event_year, event_summary, .keep_all = TRUE) %>%
  select(ccode,
         event_year,
         bcode,
         bccode1,
         bccode2,
         basin_name, 
         document_name, 
         date_signed, 
         signatories, 
         treaty_notes = notes, 
         treaty_issue_area = issue_area,
         basin_name_1 = basin_name, 
         basin_name_2, 
         rbo_name, 
         agreement_name, 
         agreement_date, 
         issues_names, 
         issue_types, 
         org_type, 
         event_date = date,
         event_summary, 
         event_comments = comments, 
         event_issue, 
         pop, 
         gdp, 
         water,
         trade_percent_gdp)

# bccode2 transfers as its own column, so it should be okay that I am only
# merging by bccode 1.

# match by year as well
# if not annual data, create a new "decade" column that could match wb data
# or, in wb data, could create artificial years that populate the data
