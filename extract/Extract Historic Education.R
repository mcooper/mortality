setwd('G://My Drive/SSPs/Education')

library(tidyverse)
library(zoo)
library(countrycode)
library(rdhs)

options(stringsAsFactors = F)

#Filter to DHS countries
ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode"))

ids$CountryName[ids$CountryName=='Eswatini'] <- 'Swaziland'

mydhs <- c("ZA", "LS", "NM", "SZ", "MZ", "MD", "BO", "ZW", "PE", "AO", 
           "ZM", "MW", "CD", "KM", "TZ", "ID", "TL", "KE", "BU", "CO", "GA", 
           "RW", "UG", "BD", "BF", "BJ", "CI", "CM", "DR", "EG", "ET", "GH", 
           "GN", "GU", "GY", "HN", "HT", "IA", "JO", "KH", "KY", "LB", "MB", 
           "ML", "NG", "PH", "PK", "SL", "SN", "TG", "TJ", "CF", "TD", "MM", 
           "NI", "NP", "MA", "AM", "AL")

ids$iso3 <- countrycode(ids$CountryName, origin='country.name', destination='iso3c')


dat_f <- read.csv('API_SE.PRM.CUAT.FE.ZS_DS2_EN_csv_v2_222700.csv', skip=4) %>%
  select(-Country.Name, -Indicator.Name, -Indicator.Code, -X) %>%
  gather(Year, Female, -Country.Code) %>%
  mutate(Country.Code == countrycode(Country.Code, origin='wb', destination='iso3c')) %>%
  filter(Country.Code %in% ids$iso3)
  arrange(Country.Code, Year) %>%
  group_by(Country.Code) %>%
  mutate(Female = na.approx(Female, rule=2))

#Really not enough data
table(dat_f$Country.Code, is.na(dat_f$Female))
