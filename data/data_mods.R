library(tidyverse)
library(lubridate)
rainfall <- read_csv('https://tinyurl.com/Oz-fire-rain')
temperature <- read_csv('https://tinyurl.com/Oz-fire-temp')

rainfall <- rainfall %>% mutate(date = ymd(paste0(year,month,day)))
ggplot(rainfall, aes(date,rainfall)) +
  geom_point()

yearly_rain <- rainfall %>% 
  filter(!is.na(rainfall)) %>%
  filter(year < 2020) %>%
  group_by(year,station_code,city_name) %>%  #sum all rain for that year at each station
  summarize(rainfall = sum(rainfall)) %>%
  group_by(year,city_name) %>%
  summarize(rainfall = mean(rainfall)) #mean across stations
ggplot(filter(yearly_rain,year >2000), aes(year,rainfall,color=city_name)) +
  geom_line()

temperature <- temperature %>% mutate(year = lubridate::year(date))
yearly_temp <- temperature %>%
  filter(!is.na(temperature)) %>%
  filter(year < 2020) %>%
  group_by(year,site_name,city_name) %>% #mean temp for each site
  summarize(temperature = mean(temperature)) %>%
  group_by(year,city_name) %>%
  summarize(temperature = mean(temperature)) #mean across stations
write_csv(yearly_temp,path="data/Oz_mean_temp.csv")
ggplot(yearly_temp, aes(year,temperature,color=city_name)) +
  geom_line()
