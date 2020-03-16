library(data.table)
library(openxlsx)
library(janitor)
library(lubridate)
library(tidyverse)
library(broom)
library(platus)

fread("https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")


x <- util_scrape_table("https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

y <- x %>% row_to_names(1) %>% clean_names %>% remove_empty() %>% 
  gather(date, infections, -1:-4) %>% 
  mutate(date = str_remove_all(date, "x")) %>% 
  mutate(date = mdy(date)) %>% 
  mutate(infections = parse_number(infections))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Similar countries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


italy <- y %>% filter(str_detect(country_region, regex("italy", ignore_case = TRUE))) %>% filter(date >= dmy("22/02/2020"))
us    <- y %>% filter(country_region == "US") %>% filter(date >= dmy("03/03/2020"))
oz    <- y %>% filter(country_region == "Australia") %>% filter(date >= dmy("12/03/2020"))
uk    <- y %>% filter(country_region == "United Kingdom") %>% filter(date >= dmy("05/03/2020"))

nz    <- y %>% filter(country_region == "New Zealand") # %>% filter(date >= dmy("05/03/2020"))




# uk %>% group_by(date) %>% summarise(infections = sum(infections)) %>% xlopen
# italy %>% group_by(date) %>% summarise(infections = sum(infections)) %>% xlopen
# us %>% group_by(date) %>% summarise(infections = sum(infections)) %>% xlopen
# oz %>% group_by(date) %>% summarise(infections = sum(infections)) %>% xlopen



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# OZ only
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

oz_only <- oz %>% group_by(date, province_state) %>% summarise(infections_oz = sum(infections))  %>% 
  # mutate(count = row_number()) %>% select(count, everything()) %>%
  rename(date_oz = date) %>% 
  spread(province_state, infections_oz, fill = 0) %>% 
  ungroup %>% 
  mutate(count = row_number()) %>% 
  select(count, everything())


  # xlopen


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
from_date <- dmy("15/03/2020")

model_us <- us %>% 
  filter(date <= from_date) %>% 
  group_by(date) %>% 
  summarise(infections = sum(infections)) %>% 
  mutate(count = row_number()) %>% 
  lm(log(infections) ~ count, data = .)


model_uk <- uk %>% 
  filter(date <= from_date) %>% 
  group_by(date) %>% 
  summarise(infections = sum(infections)) %>% 
  mutate(count = row_number()) %>% 
  lm(log(infections) ~ count, data = .)


model_italy <- italy %>% 
  filter(date <= from_date) %>% 
  group_by(date) %>% 
  summarise(infections = sum(infections)) %>% 
  mutate(count = row_number()) %>% 
  lm(log(infections) ~ count, data = .)


model_nsw <- oz_only %>% 
  filter(date_oz <= from_date) %>% 
  mutate(count = row_number()) %>% 
  lm(log(`New South Wales`) ~ count, data = .)




list(model_italy, model_us, model_uk, model_nsw) %>% map_df(broom::glance)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# OZ Forecast
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

start_count <- 4
increments  <- 11


prediction_count <- start_count:(increments + start_count - 1)


date_fcast <- seq(from = from_date, 
                        to = from_date + max(prediction_count) - min(prediction_count),
                        by = "day")


oz_predictions <- data.frame(count = prediction_count,
           date_oz_fcast  = date_fcast,
           oz_fcast = data.frame(count = prediction_count, infections = NA) %>% 
             predict(model_us, newdata = .) %>% 
             exp() %>% round()
  
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# US Forecast
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


start_count <- 13

prediction_count <- start_count:(increments + start_count - 1)
from_date <- dmy("15/03/2020")

date_fcast <- seq(from = from_date, 
                  to = from_date + max(prediction_count) - min(prediction_count),
                  by = "day")


date_fcast <- seq(from = from_date, 
                  to = from_date + max(prediction_count) - min(prediction_count),
                  by = "day")


us_predictions <- data.frame(count = prediction_count,
                             date_us_fcast  = date_fcast,
                             us_fcast = data.frame(count = prediction_count, infections = NA) %>% 
                               predict(model_us, newdata = .) %>% 
                               exp() %>% round()
                             
)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# UK Forecast
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


start_count <- 11

prediction_count <- start_count:(increments + start_count - 1)
from_date <- dmy("15/03/2020")

date_fcast <- seq(from = from_date, 
                  to = from_date + max(prediction_count) - min(prediction_count),
                  by = "day")


date_fcast <- seq(from = from_date, 
                  to = from_date + max(prediction_count) - min(prediction_count),
                  by = "day")


uk_predictions <- data.frame(count = prediction_count,
                             date_uk_fcast  = date_fcast,
                             uk_fcast = data.frame(count = prediction_count, infections = NA) %>% 
                               predict(model_uk, newdata = .) %>% 
                               exp() %>% round()
                             
)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Output with prediction
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



list(
  italy %>% group_by(date) %>% summarise(infections_italy = sum(infections)) %>% mutate(count = row_number()) %>% select(count, everything()) %>% rename(date_italy = date), 
  
  us %>% group_by(date) %>% summarise(infections_us = sum(infections)) %>% mutate(count = row_number()) %>% select(count, everything()) %>% rename(date_us = date),
  us_predictions,
  
  uk %>% group_by(date) %>% summarise(infections_uk = sum(infections)) %>% mutate(count = row_number()) %>% select(count, everything()) %>% rename(date_uk = date),
  uk_predictions,
  
  oz %>% group_by(date) %>% summarise(infections_oz = sum(infections))  %>% mutate(count = row_number()) %>% select(count, everything()) %>% rename(date_oz = date),
  oz_predictions
  
  
) %>% 
  reduce(full_join, by = "count") %>% 
  
  xlopen(df = ., decimal = FALSE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Model OZ only
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



start_count <- 4
increments  <- 11


prediction_count <- start_count:(increments + start_count - 1)


date_fcast <- seq(from = from_date, 
                  to = from_date + max(prediction_count) - min(prediction_count),
                  by = "day")


data.frame(count = prediction_count,
                             date_nsw_fcast  = date_fcast,
                             nsw_fcast = data.frame(count = prediction_count, infections = NA) %>% 
                               predict(model_nsw, newdata = .) %>% 
                               exp() %>% round()
                             
) %>% 
  
  full_join(oz_only, ., by = "count") %>% 
  rename(date = date_oz) %>% 
  select(count, date, `New South Wales`, date_nsw_fcast, nsw_fcast, everything()) %>% 
  xlopen






