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

################### Cleaning Events ###################
na_strings <- c("NA", "N.A.", "n/a", "?location", "?relvant", ".", " ")

events <- read_csv("raw_data/events_raw.csv") %>%
  clean_names() %>%
  replace_with_na_all(condition = ~.x %in% na_strings) %>%
  select(!c(id, id1, event_master, event_type)) %>%
  mutate(date = ymd(date), doc_date = ymd(doc_date)) %>%
  mutate(year = year(date))

################### Organizations ###################

organizations <- read_csv("raw_data/organizations_raw.csv") %>%
  clean_names() %>%
  replace_with_na_all(condition = ~.x %in% na_strings)

################### Treaties ###################
treaties <- read_csv("raw_data/treaties_raw.csv") %>%
  clean_names() %>%
  replace_with_na_all(condition = ~.x %in% na_strings) %>%
  select(!c(entry_id))

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
saveRDS(pop, file = "pop.rds")

gdp <- wb(indicator = "NY.GDP.PCAP.CD", startdate = 1900, enddate = 2015) %>%
  select(code = iso3c, date, gdp = value) %>%
  mutate(date = as.double(date)) %>%
  left_join(wb_codes, by = c("code" = "country_code"))
saveRDS(gdp, file = "gdp.rds")

trade_percent_gdp <- wb(indicator = "TG.VAL.TOTL.GD.ZS", startdate = 1900, enddate = 2015) %>%
  select(code = iso3c, date, trade_percent_gdp = value) %>%
  mutate(date = as.double(date)) %>%
  left_join(wb_codes, by = c("code" = "country_code"))
saveRDS(trade_percent_gdp, file = "trade_percent_gdp.rds")

water_avail <- wb(indicator = "SH.H2O.SMDW.ZS", startdate = 1900, enddate = 2015) %>%
  select(code = iso3c, date, water = value) %>%
  mutate(date = as.double(date)) %>%
  left_join(wb_codes, by = c("code" = "country_code"))
saveRDS(water_avail, file = "water_avail.rds")

water_withdraw <- wb(indicator = "ER.H2O.FWTL.ZS", startdate = 1900, enddate = 2016) %>%
  select(code = iso3c, date, water_withdraw = value) %>%
  mutate(date = as.double(date)) %>%
  left_join(wb_codes, by = c("code" = "country_code"))

ag_land <- wb(indicator = "AG.LND.AGRI.ZS", startdate = 1900, enddate = 2016) %>%
  select(code = iso3c, date, ag_land = value) %>%
  mutate(date = as.double(date)) %>%
  left_join(wb_codes, by = c("code" = "country_code"))

droughts_floods <- wb(indicator = "EN.CLC.MDAT.ZS", startdate = 1900, enddate = 2016) %>%
  select(code = iso3c, date, droughts_floods = value) %>%
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
  left_join(ag_land, by = c("ccode" = "code", "event_year" = "date"), suffix = c("", "_ag_land")) %>%
  left_join(water_withdraw, by = c("ccode" = "code", "event_year" = "date"), suffix = c("", "_water_withdraw")) %>%
  left_join(droughts_floods, by = c("ccode" = "code", "event_year" = "date"), suffix = c("", "_droughts_floods")) %>%
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
         trade_percent_gdp,
         water_withdraw,
         ag_land,
         droughts_floods)

saveRDS(joined, "shiny_app/joined.rds")
