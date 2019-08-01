library(tidyverse)
library(zoo)
library(rdhs)
library(countrycode)

setwd('G://My Drive/SSPs')

#Get Pre2009 data from SSP harmonized Dataset
pre2009 <- read.csv('GDP/GDP-per-capita-national_PPP2005_SSP-harmonized_1850-2009_v2.csv', skip = 12) %>%
  gather(Year, Value, -ISO) %>%
  mutate(YEAR = as.numeric(gsub('X', '', Year))) %>%
  select(REGION=ISO, YEAR, GDP=Value) %>%
  filter(YEAR >= 1950)

#Get Post2009 data from SSPs
dat <- read.csv('SspDb_country_data_2013-06-12.csv', stringsAsFactors = F)

dat2 <- dat %>%
  filter(VARIABLE %in% c('GDP|PPP', 'Population') & MODEL == "OECD Env-Growth") %>%
  dplyr::select(-UNIT, -MODEL) %>%
  gather(YEAR, Value, -SCENARIO, -REGION, -VARIABLE) %>%
  spread(VARIABLE, Value) %>%
  mutate(`GDP|PPP`=`GDP|PPP`*1000000000,
         Population=Population*1000000,
         `GDP Per Capita`=`GDP|PPP`/Population,
         YEAR = as.numeric(gsub('X', '', YEAR))) %>%
  na.omit

meta <- dat2 %>%
  dplyr::select(SCENARIO, REGION) %>%
  unique %>%
  merge(data.frame(YEAR=seq(2000, 2100)))

dat3 <- merge(meta, dat2, all.x=T) %>%
  arrange(SCENARIO, REGION, YEAR) %>%
  mutate(`GDP Per Capita` = na.approx(`GDP Per Capita`)) %>%
  group_by(REGION, YEAR) %>%
  summarize(GDP=mean(`GDP Per Capita`)) %>%
  filter(YEAR > 2009 & YEAR < 2020)

#Combine
comb <- bind_rows(pre2009, dat3)

comb$date_cmc <- (comb$YEAR - 1900)*12 + 1

#Filter to DHS countries
ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode"))

mydhs <- c("ZA", "LS", "NM", "SZ", "MZ", "MD", "BO", "ZW", "PE", "AO", 
           "ZM", "MW", "CD", "KM", "TZ", "ID", "TL", "KE", "BU", "CO", "GA", 
           "RW", "UG", "BD", "BF", "BJ", "CI", "CM", "DR", "EG", "ET", "GH", 
           "GN", "GU", "GY", "HN", "HT", "IA", "JO", "KH", "KY", "LB", "MB", 
           "ML", "NG", "PH", "PK", "SL", "SN", "TG", "TJ", "CF", "TD", "MM", 
           "NI", "NP", "MA", "AM", "AL")

ids$CountryName[ids$CountryName=='Eswatini'] <- 'Swaziland'

ids <- ids %>%
  filter(DHS_CountryCode %in% mydhs) %>%
  mutate(REGION = countrycode(CountryName, origin = 'country.name', destination = 'iso3c')) %>%
  select(-CountryName)

comb_new <- merge(comb, ids) %>%
  na.omit %>%
  select(GDP, date_cmc, cc=DHS_CountryCode)

#Interpolate all date_cmc
new_meta <- expand.grid(list(cc=unique(comb_new$cc), date_cmc=seq(601, 1429)))

final <- merge(comb_new, new_meta, all=T)

final <- final %>%
  arrange(cc, date_cmc) %>%
  mutate(GDP = na.approx(GDP))

write.csv(final, 'G://My Drive/DHS Processed/Mortality_GDP_SSP_Harmonized.csv', row.names=F)
