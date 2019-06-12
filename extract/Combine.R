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
child.months <- read.csv('child.months-reduced.csv')

ind <- read.csv(file='Mortality_individualdata.csv') %>%
  select(ind_code, resp_code, code, birth_order, male)

spei <- read.csv(file='Mortality_SPI_Temps.csv') %>%
  select(date=date_cmc, code, spei3, spei6, spei12, spei24, spei36, spei48)

# #Skip wealth Data For Now, since it is not temporal
# 
# res <- read.csv('Mortality_respondentdata.csv') %>%
#   select(wealth_factor_harmonized, hhsize, resp_code)
# comb <- Reduce(function(x,y){merge(x,y,all.x=T,all.y=F)}, list(child.months, ind, res, spei))

comb <- Reduce(function(x,y){merge(x,y,all.x=T,all.y=F)}, list(child.months, ind, spei))

write.csv(comb, 'Mortality-combined.csv', row.names=F)

system('/home/mattcoop/telegram.sh "Combine Done"')


