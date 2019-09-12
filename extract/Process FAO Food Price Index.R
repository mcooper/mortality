setwd('G://My Drive/DHS Spatial Covars/FAO Food Price Indices/')

library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)

options(stringsAsFactors = F)

monthly <- read.csv('Food_price_indices_data_sep.csv', skip=2) %>%
  select(Date, Food.Price.Index)
long_term <- read_xls('food_price_index_nominal_real_aug19.xls', skip=4, col_names = c('Year', 'Nominal', 'Real'))

#Data From: http://www.fao.org/worldfoodsituation/foodpricesindex/en/

#Looks like real is better, but we dont have real for monthly data
#So, combine, and guess real from monthly data

monthly$Date <- strsplit(monthly$Date, '/') %>%
  sapply(function(x){paste0(x[2], '-', x[1], '-01')}) %>%
  ymd

long_term$Date <- ymd(paste0(long_term$Year, '-07-01'))

fullts <- data.frame(Date=seq(ymd('1961-01-01'), ymd('2019-09-01'), by='month'))

comb <- Reduce(f=function(x, y){merge(x, y, all.x=T, all.y=T)}, list(fullts, monthly, long_term)) %>%
  mutate(real_approx = na.approx(Real, na.rm=F))

library(ggplot2)

ggplot(comb %>% filter(Date > ymd('1990-01-01'))) + 
  geom_line(aes(x=Date, y=real_approx), color='red') + 
  geom_line(aes(x=Date, y=Food.Price.Index), color='blue')

#TODO: project the monthly variation of the nominal FPI onto the annual trends of the real FPI

#Get CMC
comb$date <- 12*(year(comb$Date) - 1900) + month(comb$Date)

sel <- comb %>% select(date, FPI=real_approx) %>%
  na.omit

ggplot(sel) + 
  geom_line(aes(x=date, y=FPI))

write.csv(sel, 'G://My Drive/DHS Processed/FoodPriceIndex.csv', row.names=F)
