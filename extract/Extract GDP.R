library(ncdf4)
library(raster)
library(dplyr)
library(readxl)
library(countrycode)

test <- 
  mynewcode('var')

#Ok well lets get super tidyverse-ey
data <- read.csv('G://My Drive/DHS Processed/Mortality_geodata.csv') %>%
  select(-X) %>%
  mutate(earliest_date=ifelse(earliest_date < latest_date - 15*12, latest_date - 15*12, earliest_date),
         start_year = floor((earliest_date/12) + 1900),
         end_year = floor((latest_date/12) + 1900),
         cc = substr(code, 1, 2)) %>%
  group_by(cc) %>%
  summarize(start=min(start_year),
            end=max(end_year)) %>%
  transmute(cc, year = map2(start, end, seq)) %>%
  unnest %>%
  merge(read.csv('C://Users/matt/mortality/cc_map.csv')) %>%
  mutate(countrycode=countrycode(country, origin = 'country.name', destination = 'iso3c')) %>%
  #Use Maddison Project Data For Now, Although you will need SSP-harmonized data in the future
  merge(read_excel('G://My Drive/SSPs/GDP/mpd2018.xlsx', sheet='Full data') %>%
          select(countrycode, year, gdp=cgdppc), all.x=T, all.y=F) %>%
  arrange(countrycode, year) %>%
  #Data missing for 2017 and 2018, so we will just fill down
  fill(gdp) %>%
  select(year, cc, gdp) %>%
  write.csv('G://My Drive/SSPs/Processed Data/Mortality_GDP.csv', row.names=F)
  
