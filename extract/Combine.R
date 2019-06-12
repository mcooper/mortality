setwd('/home/mattcoop/child-months')

library(tidyverse)

child.months <- read.csv(file='allchild-months.csv')

names(child.months) <- c("ind_code", "date", "age", "mother_years_ed", "mothers_age",
                           "months_in_loc", "months_before_survey", "alive")

#Filter to the child.months that we want
#  Children under 5
#  At least three months in location
#  And within 15 years of the survey
child.months <- child.months %>%
  filter((age <= 60) & (months_in_loc >= 3) & (months_before_survey < 180))

write.csv(child.months, 'child.months-reduced.csv', row.names=F)

ind <- read.csv(file='Mortality_individualdata.csv') %>%
  select(ind_code, resp_code, code, birth_order, male)

spei <- read.csv(file='Mortality_SPI_Temps.csv') %>%
  select(date=date_cmc, code, spei24, spei12, spei36) %>%
  mutate(spei24 = round(spei24, 2),
         spei36 = round(spei36, 2),
         spei12 = round(spei12, 2))

res <- read.csv('Mortality_respondentdata.csv') %>%
  select(wealth_factor_harmonized, hhsize, resp_code)

comb <- Reduce(function(x,y){merge(x,y,all.x=T,all.y=F)}, list(child.months, ind, res, spei))

comb$surveycode <- substr(comb$code, 1, 6)

write.csv(comb, 'Mortality-combined.csv', row.names=F)

##Do a quick initial analysis

comb$spei <- cut(comb$spei24, c(-10, -1, -0.5, 0.5, 1, 10))

comb$spei <- relevel(comb$spei,  "(-0.5,0.5]")

comb$mortality <- !comb$alive

mod <- glm(mortality ~ age + mother_years_ed + birth_order + spei + hhsize + wealth_factor_harmonized, family='binomial', data=comb)
summary(mod)


