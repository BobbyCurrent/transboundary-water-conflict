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
library(textdata)
library(tm)
library(tidyverse)

# Import World Bank country codes:
# https://wits-worldbank-org/wits/wits/witshelp/content/codes/country_codes.htm):
wb_codes <- read_csv("raw_data/wb_country_codes_2.csv") %>%
  clean_names() %>%
  mutate(country_code = three_letter_country_code)

############################


############## Cleaning Events ##############

# Read in Basin Register data

basin_register <- read_csv("raw_data/intl_river_basin_register.csv") %>%
  select(bcode, ccode, basin_name, country_name, continent_code, area)

# Create a tibble for each country-year from 1960 to 2005. 

country_years <- tibble(year = rep(1960:2005, 812)) %>%
  arrange(desc(year))

country_years <- tibble(event_year = country_years %>% pull(year),
                        ccode = rep(basin_register %>% pull(ccode), 46),
                        bcode = rep(basin_register %>% pull(bcode), 46),
                        basin_name = rep(basin_register %>% pull(basin_name), 46),
                        country_name = rep(basin_register %>% pull(country_name), 46),
                        continent_code = rep(basin_register %>% pull(continent_code), 46),
                        area = rep(basin_register %>% pull(area), 46))

# Import World Bank population data. 

pop <- wb(indicator = "SP.POP.TOTL", startdate = 1900, enddate = 2015) %>%
  select(code = iso3c, date, pop = value) %>%
  mutate(date = as.double(date))

# Import World Bank GDP data. 

gdp <- wb(indicator = "NY.GDP.MKTP.CD", startdate = 1900, enddate = 2015) %>%
  select(code = iso3c, date, gdp = value) %>%
  mutate(date = as.double(date))

# Import World Bank trade as a percentage of GDP data. 

trade_percent_gdp <- wb(indicator = "TG.VAL.TOTL.GD.ZS", startdate = 1900, enddate = 2015) %>%
  select(code = iso3c, date, trade_percent_gdp = value) %>%
  mutate(date = as.double(date))

# Import World Bank water availability data. 

water_avail <- wb(indicator = "SH.H2O.SMDW.ZS", startdate = 1900, enddate = 2015) %>%
  select(code = iso3c, date, water = value) %>%
  mutate(date = as.double(date))

# Import World Bank water withdrawal data data. 

water_withdraw <- wb(indicator = "ER.H2O.FWTL.ZS", startdate = 1900, enddate = 2016) %>%
  select(code = iso3c, date, water_withdraw = value) %>%
  mutate(date = as.double(date))

# Import World Bank agricultural land data. 

ag_land <- wb(indicator = "AG.LND.AGRI.ZS", startdate = 1900, enddate = 2016) %>%
  select(code = iso3c, date, ag_land = value) %>%
  mutate(date = as.double(date))

# Import World Bank drought data. 

droughts <- wb(indicator = "EN.CLC.MDAT.ZS", startdate = 1900, enddate = 2016) %>%
  select(code = iso3c, date, droughts = value) %>%
  mutate(date = as.double(date))

# Import Democratization index data. 

eiu_mod <- eiu %>%
  select(country = eiu_country, x_eiu = eiu) %>%
  right_join(wb_codes, by = c("country" = "country_name")) %>%
  distinct(country, .keep_all = TRUE) %>%
  select(country_code, x_eiu)

# Join World Bank and democratization data with previously-created country-year
# tibble.

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

# Define improperly-formatted NA values.

na_strings <- c("NA", "N.A.", "n/a", "?location", "?relvant", ".", " ")

# Define terms that characterize peaceful events.

peace_terms <- c("cooperation", "cooperate", "cooperating", "agreement", "meeting", "plan", "signed", "treaty", "coordination", "coordinate", "talks", "summit", "agreed")

# Clean events data by filtering for events whose bar_scale value is less than 0
# (the bar_scale is a political science index used to denote water conflict
# severity, where values less than 0 indicate violent events). Also, filter out
# evens matching the peace_terms defined above.

events <- read_csv("raw_data/events_raw.csv") %>%
  clean_names() %>%
  select(!c(id, id1, event_master, event_type)) %>%
  mutate(date = ymd(date), doc_date = ymd(doc_date)) %>%
  mutate(year = year(date)) %>%
  filter(bar_scale <= 0) %>%
  mutate(conflict = str_detect(event_summary, regex(paste(peace_terms, collapse = "|"), ignore_case = TRUE), negate = TRUE)) %>%
  
  # Filter for conflict events only (rather than including conflict mitigation events).
  
  filter(conflict == TRUE)

# Tidy the conflict events data by making each observation a country-event,
# rather than a country-country-event (in diads). This is necessary so that I
# can join the World Bank and democratization datsets, which are at the country
# level.

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
         bar_scale,
         ccode,
         bccode = bccode1) %>%
  
  # Replace improperly-formatted NA strings with the standard NA value.
  
  replace_with_na_all(condition = ~.x %in% na_strings) %>%
  
  # Rename issue type factors based on the codebook (included in the 'raw-data'
  # folder).
  
  mutate(issue_type1 = as.character(issue_type1),
         issue_type2 = as.character(issue_type2)) %>%
  mutate(issue_type1 = ifelse(issue_type1 == "0", NA, issue_type1)) %>%
  mutate(issue_type1 = ifelse(issue_type1 == "1", "Water Quality", issue_type1)) %>%
  mutate(issue_type1 = ifelse(issue_type1 == "2", "Water Quantity", issue_type1)) %>%
  mutate(issue_type1 = ifelse(issue_type1 == "3", "Hydro-Power", issue_type1)) %>%
  mutate(issue_type1 = ifelse(issue_type1 == "4", "Navigation", issue_type1)) %>%
  mutate(issue_type1 = ifelse(issue_type1 == "5", "Fishing", issue_type1)) %>%
  mutate(issue_type1 = ifelse(issue_type1 == "6", "Flood Control/Relief", issue_type1)) %>%
  mutate(issue_type1 = ifelse(issue_type1 == "7", "Economic Development", issue_type1)) %>%
  mutate(issue_type1 = ifelse(issue_type1 == "8", "Joint Management", issue_type1)) %>%
  mutate(issue_type1 = ifelse(issue_type1 == "9", "Irrigation", issue_type1)) %>%
  mutate(issue_type1 = ifelse(issue_type1 == "10", "Infrastructure/Development", issue_type1)) %>%
  mutate(issue_type1 = ifelse(issue_type1 == "11", "Technical Assistance", issue_type1)) %>%
  mutate(issue_type1 = ifelse(issue_type1 == "12", "Border Issues", issue_type1)) %>%
  mutate(issue_type1 = ifelse(issue_type1 == "13", "Territorial Issues", issue_type1)) %>%
  mutate(issue_type2 = ifelse(issue_type2 == "0", NA, issue_type2)) %>%
  mutate(issue_type2 = ifelse(issue_type2 == "1", "Water Quality", issue_type2)) %>%
  mutate(issue_type2 = ifelse(issue_type2 == "2", "Water Quantity", issue_type2)) %>%
  mutate(issue_type2 = ifelse(issue_type2 == "3", "Hydro-Power", issue_type2)) %>%
  mutate(issue_type2 = ifelse(issue_type2 == "4", "Navigation", issue_type2)) %>%
  mutate(issue_type2 = ifelse(issue_type2 == "5", "Fishing", issue_type2)) %>%
  mutate(issue_type2 = ifelse(issue_type2 == "6", "Flood Control/Relief", issue_type2)) %>%
  mutate(issue_type2 = ifelse(issue_type2 == "7", "Economic Development", issue_type2)) %>%
  mutate(issue_type2 = ifelse(issue_type2 == "8", "Joint Management", issue_type2)) %>%
  mutate(issue_type2 = ifelse(issue_type2 == "9", "Irrigation", issue_type2)) %>%
  mutate(issue_type2 = ifelse(issue_type2 == "10", "Infrastructure/Development", issue_type2)) %>%
  mutate(issue_type2 = ifelse(issue_type2 == "11", "Technical Assistance", issue_type2)) %>%
  mutate(issue_type2 = ifelse(issue_type2 == "12", "Border Issues", issue_type2)) %>%
  mutate(issue_type2 = ifelse(issue_type2 == "13", "Territorial Issues", issue_type2)) 

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

# Join the 'events' and 'treaties' datasets. Unfortunately, including the
# 'organizations' dataset led my Shiny App to run out of memory, so I had to
# exclude it for now.

joined <- joined_initial %>%
  full_join(events_tidy, by = c("ccode", "bcode" = "b_code", "event_year" = "event_year")) %>%
  # left_join(organizations, by = c("ccode", "bccode")) %>%
  left_join(treaties, by = c("ccode", "bccode"))

write_rds(joined, "joined.rds")

############################


############## Summarize Join ##############

# Summarize the joined data, for use in regressions. 

joined_summarized <- joined %>%
  filter(event_year <= 2005) %>%
  filter(event_year >= 1960) %>%
  filter(!is.na(basin_name) & bcode != "UNKN") %>%
  distinct(event_year, event_summary, ccode, .keep_all = TRUE) %>%
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

# Summarize the joined data, for use in logistic regressions. Each river
# basin-year is coded as '1' or '0', depending on whether or not a conflict
# event occured in that basin-year.

joined_logistic <- joined %>%
  filter(event_year <= 2005) %>%
  filter(event_year >= 1960) %>%
  filter(!is.na(basin_name) & bcode != "UNKN") %>%
  arrange(event_summary) %>%
  distinct(bcode, event_year, .keep_all = TRUE) %>%
  group_by(bcode) %>%
  mutate(conflict = ifelse(!is.na(event_summary), 1, 0)) %>%
  mutate(conflict = as.factor(conflict))

# Save the resulting data for use in Shiny App.

write_rds(joined_logistic, "joined_logistic.rds")

############################


############## Text Corpus for Chatterplot ##############

# Filter for distinct events only. 

joined_wc_prep <- joined %>%
  distinct(event_summary)

# Create a text corpus using the terms in the event summaries.

myCorpus = Corpus(VectorSource(joined_wc_prep$event_summary))

# Clean up the corpus by making all words lowercase, removing punctuation
# and numbers, and removing insignificant English words.

myCorpus = tm_map(myCorpus, content_transformer(tolower))
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))

# Convert corpus into a matrix input, which is needed to generate a frequency
# distribution.

myDTM = TermDocumentMatrix(myCorpus,
                           control = list(minWordLength = 1))

# Generate the frequency of each term. 

terms <- tidy(myDTM) %>%
  group_by(term) %>%
  summarize(count = sum(count)) %>%
  
  # Download the AFINN Sentiment Lexicon and join it to the term-frequency
  # tibble.
  
  left_join(get_sentiments("afinn"), by = c(term = "word")) %>%
  mutate(value = ifelse(is.na(value), 0, value))

# Save the resulting data for use in Shiny App.

saveRDS(terms, "terms.RDS")
