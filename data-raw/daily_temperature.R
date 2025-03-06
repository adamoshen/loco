library(tibble)
library(dplyr)
library(tidyr)
library(lubridate)
library(anyflights)

hourly_temperature <- get_weather(c("LGA", "DTW"), year=2013)

daily_temperature <- hourly_temperature %>%
  group_by(origin, year, month, day) %>%
  summarise(temp = mean(temp, na.rm=TRUE)) %>%
  ungroup() %>%
  unite(date, c(year, month, day), sep="-") %>%
  mutate(date = ymd(date))

usethis::use_data(daily_temperature, overwrite=TRUE)
