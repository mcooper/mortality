setwd('/home/mattcoop/child-months')

library(tidyverse)
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
child.months <- read.csv('child.months-reduced.csv') %>%
  mutate(cc = substr(ind_code, 1, 2),
         year = floor(date/12) + 1900)

ind <- read.csv(file='Mortality_individualdata.csv') %>%
  select(ind_code, resp_code, code, birth_order, male)

spei <- read.csv(file='Mortality_SPI_Temps.csv') %>%
  select(date=date_cmc, code, spei3, spei36, temp3, last_year_precip, last_3month_precip)

gdp <- read.csv('Mortality_GDP.csv')

# #Skip wealth Data For Now, since it is not temporal
# 
# res <- read.csv('Mortality_respondentdata.csv') %>%
#   select(wealth_factor_harmonized, hhsize, resp_code)
# comb <- Reduce(function(x,y){merge(x,y,all.x=T,all.y=F)}, list(child.months, ind, res, spei))

dim(child.months)
child.months <- merge(child.months, ind, all.x=T, all.y=F)
dim(child.months)
child.months <- merge(child.months, spei, all.x=T, all.y=F)
dim(child.months)
child.months <- merge(child.months, gdp, all.x=T, all.y=F)
dim(child.months)

write.csv(child.months, 'Mortality-combined.csv', row.names=F)

system('/home/mattcoop/telegram.sh "Combine Done"')


