############## PREP ##############

# Necessary libraries

library(readr)
library(janitor)
library(gt)
library(tidytext)
library(twitteR)
library(lubridate)
library(wbstats)
library(naniar)
library(democracyData)
library(tidymodels)
library(tidyverse)

# Import World Bank country codes:
# https://wits-worldbank-org/wits/wits/witshelp/content/codes/country_codes.htm):
wb_codes <- read_csv("shiny_app/raw_data/wb_country_codes_2.csv") %>%
  clean_names() %>%
  mutate(country_code = three_letter_country_code)

############################


############## Cleaning Events ##############

# Definining improperly formatted NA values.

na_strings <- c("NA", "N.A.", "n/a", "?location", "?relvant", ".", " ")

# Clean events data.

events <- read_csv("shiny_app/raw_data/events_raw.csv") %>%
  clean_names() %>%
  replace_with_na_all(condition = ~.x %in% na_strings) %>%
  select(!c(id, id1, event_master, event_type)) %>%
  mutate(date = ymd(date), doc_date = ymd(doc_date)) %>%
  mutate(year = year(date)) %>%
  
# Filter for conflict events only (rather than including conflict mitigation events. 
  
  mutate(peaceful_1 = str_detect(event_summary, c("cooperation", "agreement", "meeting"))) %>%
  mutate(peaceful_2 = str_detect(event_summary, c("plan", "signed", "treaty"))) %>%
  mutate(peaceful_3 = str_detect(event_summary, c("coordination", "summit"))) %>%
  # filter(peaceful_1 == TRUE | peaceful_2 == TRUE | peaceful_3 == TRUE) %>%
  filter(peaceful_1 == FALSE | peaceful_2 == FALSE | peaceful_3 == FALSE)

############################


############## Organizations ##############

# Clean organizations data.

organizations <- read_csv("shiny_app/raw_data/organizations_raw.csv") %>%
  clean_names() %>%
  replace_with_na_all(condition = ~.x %in% na_strings)

############################


############## Treaties ##############

# Clean treaties data.

treaties <- read_csv("shiny_app/raw_data/treaties_raw.csv") %>%
  clean_names() %>%
  replace_with_na_all(condition = ~.x %in% na_strings) %>%
  select(!c(entry_id))

############################


############## World Bank Stats ##############

# Use the wbstats() package to pull World Bank data for the countries involved
# in water treaties and conflicts.

pop <- wb(indicator = "SP.POP.TOTL", startdate = 1900, enddate = 2015) %>%
  select(code = iso3c, date, pop = value) %>%
  mutate(date = as.double(date))

write_rds(pop, "shiny_app/pop.rds")

gdp <- wb(indicator = "NY.GDP.PCAP.CD", startdate = 1900, enddate = 2015) %>%
  select(code = iso3c, date, gdp = value) %>%
  mutate(date = as.double(date))

write_rds(gdp, "shiny_app/gdp.rds")

trade_percent_gdp <- wb(indicator = "TG.VAL.TOTL.GD.ZS", startdate = 1900, enddate = 2015) %>%
  select(code = iso3c, date, trade_percent_gdp = value) %>%
  mutate(date = as.double(date))

write_rds(trade_percent_gdp, "shiny_app/trade_percent_gdp.rds")

water_avail <- wb(indicator = "SH.H2O.SMDW.ZS", startdate = 1900, enddate = 2015) %>%
  select(code = iso3c, date, water = value) %>%
  mutate(date = as.double(date))

write_rds(water_avail, "shiny_app/water_avail.rds")

water_withdraw <- wb(indicator = "ER.H2O.FWTL.ZS", startdate = 1900, enddate = 2016) %>%
  select(code = iso3c, date, water_withdraw = value) %>%
  mutate(date = as.double(date))

write_rds(water_avail, "shiny_app/water_withdraw.rds")

ag_land <- wb(indicator = "AG.LND.AGRI.ZS", startdate = 1900, enddate = 2016) %>%
  select(code = iso3c, date, ag_land = value) %>%
  mutate(date = as.double(date))

write_rds(ag_land, "shiny_app/ag_land.rds")

eiu_mod <- eiu %>%
  select(country = eiu_country, x_eiu = eiu) %>%
  right_join(wb_codes, by = c("country" = "country_name")) %>%
  distinct(country, .keep_all = TRUE) %>%
  select(country_code, x_eiu)

write_rds(eiu_mod, "shiny_app/eiu_mod.rds")

############################


############## Joining ##############

# Tidy events data, such that each country's event is an individual observation.

events_tidy <- events %>%
  pivot_longer(cols = c(ccode1, ccode2),
               names_to = "ccode_id",
               values_to = "ccode") %>%
  mutate(event_year = year(date))

# Join OSU datasets, select relevant columns. 

joined <- events_tidy %>%
  full_join(organizations, by = "ccode", suffix = c("_events", "_orgs")) %>%
  full_join(treaties, by = "ccode", suffix = c("_events_orgs", "_treaties")) %>%
  left_join(pop, by = c("ccode" = "code", "event_year" = "date"), suffix = c("", "_pop")) %>%
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
         event_issue)

# Join with World Bank data.

joined <- joined %>%
  left_join(pop, by = c("ccode" = "code", "event_year" = "date"), suffix = c("", "_pop")) %>%
  left_join(gdp, by = c("ccode" = "code", "event_year" = "date"), suffix = c("", "_gdp")) %>%
  left_join(water_avail, by = c("ccode" = "code", "event_year" = "date"), suffix = c("", "_water"))%>%
  left_join(trade_percent_gdp, by = c("ccode" = "code", "event_year" = "date"), suffix = c("", "_trade_percent_gdp")) %>%
  left_join(ag_land, by = c("ccode" = "code", "event_year" = "date"), suffix = c("", "_ag_land")) %>%
  left_join(water_withdraw, by = c("ccode" = "code", "event_year" = "date"), suffix = c("", "_water_withdraw")) %>%
  left_join(eiu_mod, by = c("ccode" = "country_code"), suffix = c("", "_eiu")) 

# Join with World Bank codes, to allow for grouping by continent (and the
# display of country names.)

joined <- joined %>%
  left_join(wb_codes, by = c("ccode" = "country_code"))

write_rds(joined, "shiny_app/joined.rds")
