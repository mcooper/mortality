setwd('/home/mattcoop/mortalityblob')

library(tidyverse)
library(readr)
library(data.table)
# 
# child.months <- read.csv(file='allchild-months.csv')
# 
# names(child.months) <- c("ind_code", "date", "age", "mother_years_ed", "mothers_age",
#                          "months_in_loc", "months_before_survey", "alive")
# 
# #Filter to the child.months that we want
# #  Children under 5
# #  At least three months in location
# #  And within 15 years of the survey
# child.months <- child.months %>%
#   filter((age <= 60) & (months_in_loc >= 3) & (months_before_survey < 180))
# 
# write.csv(child.months, 'child.months-reduced.csv', row.names=F)
child.months <- read_csv('child.months-reduced.csv') %>%
  mutate(cc = substr(ind_code, 1, 2),
         year = floor(date/12) + 1900)

ind <- read_csv(file='Mortality_individualdata.csv') %>%
  select(ind_code, resp_code, code, birth_order, male)

spei <- read_csv(file='Mortality_SPI_Temps_Ewembi.csv') %>%
  select(date=date_cmc, code, spei3, spei36)

gdp <- read_csv('Mortality_GDP_SSP_Harmonized.csv') %>%
  select(date=date_cmc, cc, GDP)

# #Skip wealth Data For Now, since it is not temporal
# 
# res <- read.csv('Mortality_respondentdata.csv') %>%
#   select(wealth_factor_harmonized, hhsize, resp_code)
# comb <- Reduce(function(x,y){merge(x,y,all.x=T,all.y=F)}, list(child.months, ind, res, spei))

#Convert to data.table for easy merging
child.months <- data.table(child.months, key='ind_code')
ind <- data.table(ind, key='ind_code')
spei <- data.table(spei, key=c('date', 'code'))
gdp <- data.table(gdp, key=c('cc', 'date'))

child.months <- merge(child.months, ind, all.x=T, all.y=F)
child.months <- merge(child.months, spei, all.x=T, all.y=F, by=c('code', 'date'))
child.months <- merge(child.months, gdp, all.x=T, all.y=F, by=c('cc', 'date'))

write.csv(child.months, 'Mortality-combined.csv', row.names=F)

system('/home/mattcoop/telegram.sh "Combine Done"')


