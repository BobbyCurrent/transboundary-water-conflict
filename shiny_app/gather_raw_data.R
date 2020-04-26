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
wb_codes <- read_csv("raw_data/wb_country_codes_2.csv") %>%
  clean_names() %>%
  mutate(country_code = three_letter_country_code)

############################


############## Cleaning Events ##############
basin_register <- read_csv("raw_data/intl_river_basin_register.csv") %>%
  select(bcode, ccode, basin_name, country_name, continent_code, area)

country_years <- tibble(year = rep(1960:2005, 812)) %>%
  arrange(desc(year))

country_years <- tibble(event_year = country_years %>% pull(year),
                        ccode = rep(basin_register %>% pull(ccode), 46),
                        bcode = rep(basin_register %>% pull(bcode), 46),
                        basin_name = rep(basin_register %>% pull(basin_name), 46),
                        country_name = rep(basin_register %>% pull(country_name), 46),
                        continent_code = rep(basin_register %>% pull(continent_code), 46),
                        area = rep(basin_register %>% pull(area), 46))

pop <- wb(indicator = "SP.POP.TOTL", startdate = 1900, enddate = 2015) %>%
  select(code = iso3c, date, pop = value) %>%
  mutate(date = as.double(date))

gdp <- wb(indicator = "NY.GDP.PCAP.CD", startdate = 1900, enddate = 2015) %>%
  select(code = iso3c, date, gdp = value) %>%
  mutate(date = as.double(date))

trade_percent_gdp <- wb(indicator = "TG.VAL.TOTL.GD.ZS", startdate = 1900, enddate = 2015) %>%
  select(code = iso3c, date, trade_percent_gdp = value) %>%
  mutate(date = as.double(date))

water_avail <- wb(indicator = "SH.H2O.SMDW.ZS", startdate = 1900, enddate = 2015) %>%
  select(code = iso3c, date, water = value) %>%
  mutate(date = as.double(date))

water_withdraw <- wb(indicator = "ER.H2O.FWTL.ZS", startdate = 1900, enddate = 2016) %>%
  select(code = iso3c, date, water_withdraw = value) %>%
  mutate(date = as.double(date))

ag_land <- wb(indicator = "AG.LND.AGRI.ZS", startdate = 1900, enddate = 2016) %>%
  select(code = iso3c, date, ag_land = value) %>%
  mutate(date = as.double(date))

droughts <- wb(indicator = "EN.CLC.MDAT.ZS", startdate = 1900, enddate = 2016) %>%
  select(code = iso3c, date, droughts = value) %>%
  mutate(date = as.double(date))

eiu_mod <- eiu %>%
  select(country = eiu_country, x_eiu = eiu) %>%
  right_join(wb_codes, by = c("country" = "country_name")) %>%
  distinct(country, .keep_all = TRUE) %>%
  select(country_code, x_eiu)

joined_initial <- country_years %>%
  left_join(pop, by = c("ccode" = "code", "event_year" = "date"), suffix = c("", "_pop")) %>%
  left_join(gdp, by = c("ccode" = "code", "event_year" = "date"), suffix = c("", "_gdp")) %>%
  left_join(water_avail, by = c("ccode" = "code", "event_year" = "date"), suffix = c("", "_water"))%>%
  left_join(trade_percent_gdp, by = c("ccode" = "code", "event_year" = "date"), suffix = c("", "_trade_percent_gdp")) %>%
  left_join(ag_land, by = c("ccode" = "code", "event_year" = "date"), suffix = c("", "_ag_land")) %>%
  left_join(water_withdraw, by = c("ccode" = "code", "event_year" = "date"), suffix = c("", "_water_withdraw")) %>%
  left_join(droughts, by = c("ccode" = "code"), suffix = c("", "_droughts")) %>%
  left_join(eiu_mod, by = c("ccode" = "country_code"), suffix = c("", "_eiu"))

############################


############## Cleaning Events ##############

# Definining improperly formatted NA values.

na_strings <- c("NA", "N.A.", "n/a", "?location", "?relvant", ".", " ")

# Clean events data.

events <- read_csv("raw_data/events_raw.csv") %>%
  clean_names() %>%
  select(!c(id, id1, event_master, event_type)) %>%
  mutate(date = ymd(date), doc_date = ymd(doc_date)) %>%
  mutate(year = year(date)) %>%
  
  # Filter for conflict events only (rather than including conflict mitigation events. 
  
  mutate(peaceful_1 = str_detect(event_summary, c("cooperation", "agreement", "meeting"))) %>%
  mutate(peaceful_2 = str_detect(event_summary, c("plan", "signed", "treaty"))) %>%
  mutate(peaceful_3 = str_detect(event_summary, c("coordination", "summit"))) %>%
  # filter(peaceful_1 == TRUE | peaceful_2 == TRUE | peaceful_3 == TRUE) %>%
  filter(peaceful_1 == FALSE | peaceful_2 == FALSE | peaceful_3 == FALSE)

events_tidy <- events %>%
  pivot_longer(cols = c(ccode1, ccode2),
               names_to = "ccode_id",
               values_to = "ccode") %>%
  mutate(event_year = year(date)) %>%
  select(date,
         dyad_code,
         b_code,
         number_of_countries,
         event_summary,
         issue_type1,
         issue_type2,
         event_issue,
         event_year = year,
         peaceful_1,
         peaceful_2,
         peaceful_3,
         ccode,
         bccode = bccode1) %>%
  replace_with_na_all(condition = ~.x %in% na_strings)

############################


############## Organizations ##############

# Clean organizations data.

organizations <- read_csv("raw_data/organizations_raw.csv") %>%
  clean_names() %>%
  select(ccode,
         rbo_name,
         agreement_name,
         agreement_date,
         bccode) %>%
  replace_with_na_all(condition = ~.x %in% na_strings)

############################


############## Treaties ##############

# Clean treaties data.

treaties <- read_csv("raw_data/treaties_raw.csv") %>%
  clean_names() %>%
  mutate(date_signed_parse = parse_date(date_signed, "%m/%d/%y")) %>%
  mutate(year_signed = year(date_signed_parse)) %>%
  mutate(year_signed = ifelse(year_signed > 2015, year_signed - 100, year_signed)) %>%
  select(ccode,
         document_name,
         date_signed,
         year_signed,
         signatories,
         bccode) %>%
  replace_with_na_all(condition = ~.x %in% na_strings)

############################


############## Perform Join ##############
joined <- joined_initial %>%
  full_join(events_tidy, by = c("ccode", "bcode" = "b_code", "event_year" = "event_year")) %>%
  # left_join(organizations, by = c("ccode", "bccode")) %>%
  left_join(treaties, by = c("ccode", "bccode"))

write_rds(joined, "joined.rds")
############################


############## Summarize Join ##############
joined_summarized <- joined %>%
  filter(event_year <= 2005) %>%
  filter(event_year >= 1960) %>%
  filter(!is.na(basin_name) & bcode != "UNKN") %>%
  distinct(event_year, event_summary, .keep_all = TRUE) %>%
  group_by(basin_name) %>%
  summarize(event_count = n(),
            gdp_total = sum(gdp, na.rm = TRUE),
            gdp_avg = mean(gdp, na.rm = TRUE),
            pop_total = sum(pop, na.rm = TRUE),
            pop_avg = mean(pop, na.rm = TRUE),
            trade_percent_gdp_avg = mean(trade_percent_gdp, na.rm = TRUE),
            water_withdraw_avg = mean(water_withdraw, na.rm = TRUE),
            ag_land_total = sum(ag_land, na.rm = TRUE),
            ag_land_avg = mean(ag_land, na.rm = TRUE),
            droughts_avg = mean(ag_land, na.rm = TRUE),
            eiu_avg = mean(x_eiu, na.rm = TRUE)) %>%
  mutate(event_count_log = log(event_count),
         gdp_total_log = log(gdp_total),
         gdp_avg_log = log(gdp_avg),
         pop_total_log = log(pop_total),
         pop_avg_log = log(pop_avg),
         trade_percent_gdp_avg_log = log(trade_percent_gdp_avg),
         water_withdraw_avg_log = log(water_withdraw_avg),
         ag_land_total_log = log(ag_land_total),
         ag_land_avg_log = log(ag_land_avg),
         droughts_avg_log = log(droughts_avg),
         eiu_avg_log = log(eiu_avg))

write_rds(joined_summarized, "joined_summarized.rds")
############################


############## Logistic Join ##############
joined_logistic <- joined %>%
  filter(event_year <= 2005) %>%
  filter(event_year >= 1960) %>%
  filter(!is.na(basin_name) & bcode != "UNKN") %>%
  arrange(event_summary) %>%
  distinct(bcode, event_year, .keep_all = TRUE) %>%
  group_by(bcode) %>%
  mutate(conflict = ifelse(!is.na(event_summary), 1, 0)) %>%
  mutate(conflict = as.factor(conflict))

write_rds(joined_logistic, "joined_logistic.rds")


############### Word Cloud ##################
# library(wordcloud)
# library(RColorBrewer)
# library(wordcloud2)
# library(tm)



