# For experimenting with new ideas. 

library(rmarkdown)
render("gather.R")
library(tidytext)
library(lubridate)n

############# FOR MILESTONE ###############

# To do: Use facet to combine into one graphic
# Put into Shiny app
# Create option to substitute in different values

regression_prep_conflict_events <- joined %>%
  mutate(peaceful_1 = str_detect(event_summary, c("cooperation", "agreement", "meeting"))) %>%
  mutate(peaceful_2 = str_detect(event_summary, c("plan", "signed", "treaty"))) %>%
  mutate(peaceful_3 = str_detect(event_summary, c("coordination", "summit"))) %>%
  # filter(peaceful_1 == TRUE | peaceful_2 == TRUE | peaceful_3 == TRUE) %>%
  filter(peaceful_1 == FALSE | peaceful_2 == FALSE | peaceful_3 == FALSE) %>%
  group_by(ccode) %>%
  summarize(avg_gdp = mean(gdp, na.rm = TRUE), 
            avg_pop = mean(pop, na.rm = TRUE),
            avg_trade = mean(trade_percent_gdp, na.rm = TRUE),
            avg_water = mean(water, na.rm = TRUE),
            event_count = n())

saveRDS(joined, "shiny_app/joined.rds")

regression_prep_conflict_events %>%
  ggplot(aes(x = avg_gdp, y = event_count)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x)

regression_prep_conflict_events %>%
  ggplot(aes(x = avg_pop, y = event_count)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x)
# Two potential outliers are China and India, though it seems unlikely to me that they are outliers.

regression_prep_conflict_events %>%
  ggplot(aes(x = avg_trade, y = event_count)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x)

regression_prep_conflict_events %>%
  filter(ccode != "SGP") %>%
  ggplot(aes(x = avg_water, y = event_count)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x)


###########################################

gdp_mean <- gdp %>%
  group_by(date) %>%
  summarize(mean_gdp = mean(gdp)/10)

pop_mean <- pop %>%
  group_by(date) %>%
  summarize(mean_pop = mean(pop)/1000000)

ggplot(joined, aes(x = event_year)) + 
  geom_histogram() + 
  geom_line(data = gdp_mean, aes(x = date, y = mean_gdp)) + 
  geom_line(data = pop_mean, aes(x = date, y = mean_pop))
  # scale_y_log10() 

ggplot(joined, aes(x = event_year, y = trade_percent_gdp)) + 
  geom_point() + geom_jitter()

ggplot(gdp, aes(x = gdp)) + geom_histogram()





# Inverse relationship between avg_gdp and event_count for peaceful events, but flat line for conflict events.
# Part of my analysis could be to suggest that these variables are OVERRATED among the scholarly community. 

# Pop variable very interesting, as is trade. 

##########################################################
gdp_2015 <- gdp %>%
  filter(date == 2015)

words <- events %>%
  unnest_tokens(output = "word", input = event_summary) %>%
  group_by(word) %>%
  count()

events_conflict <- events %>%
  mutate(year = year(date)) %>%
  left_join(gdp, by = c("ccode1" = "code", "year" = "date"), suffix = c("", "_c1")) %>%
  left_join(gdp, by = c("ccode2" = "code", "year" = "date"), suffix = c("", "_c2")) %>%
  mutate(dyad_gdp = (gdp + gdp_c2) / 2000) %>%
 filter(!str_detect(event_summary, c("cooperation", "agreement", "meeting", "plan", "signed", "treaty", "coordination", "summit"))) %>%
  # filter(str_detect(event_summary, c("conflict", "war", "violence", "military", "threat*", "condemn", "critic*", "argue", "fight"))) %>%
  group_by(dyad_code, dyad_gdp) %>%
  summarize(event_count = n()) %>%
  filter(!is.na(dyad_gdp)) 

# maybe would be better to filter for NOT

ggplot(events_conflict, aes(x = dyad_gdp, y = event_count)) + 
  geom_point(position = "jitter") + 
  geom_jitter() + 
  geom_smooth(method = "lm", se = FALSE)

# GDP not correlated with peace events

events_peace <- events %>%
  mutate(year = year(date)) %>%
  left_join(gdp, by = c("ccode1" = "code", "year" = "date"), suffix = c("", "_c1")) %>%
  left_join(gdp, by = c("ccode2" = "code", "year" = "date"), suffix = c("", "_c2")) %>%
  mutate(dyad_gdp = (gdp + gdp_c2) / 2000) %>%
  filter(str_detect(event_summary, c("cooperation", "agreement", "meeting", "plan", "signed", "treaty", "coordination", "summit"))) %>%
  # filter(str_detect(event_summary, c("conflict", "war", "violence", "military", "threat*", "condemn", "critic*", "argue", "fight"))) %>%
  group_by(dyad_code, dyad_gdp) %>%
  summarize(event_count = n()) %>%
  filter(!is.na(dyad_gdp)) 

ggplot(events_peace, aes(x = dyad_gdp, y = event_count)) + 
  geom_point(position = "jitter") + 
  geom_jitter() + 
  geom_smooth(method = "lm", se = FALSE)

exp.model <-lm(event_count ~ exp(dyad_gdp), events_2)

events_model <- lm(event_count ~ exp(dyad_gdp), data = events_2)
events_model %>% tidy(conf.int = TRUE)

# Questions for Alyssa: 
# Nonlinear regression
# Best way to run logistic regression in R. 

##########################################################
