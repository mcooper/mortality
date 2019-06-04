setwd('/home/mattcoop/child-months')

library(tidyverse)
library(ff)
library(ffbase)

child.months <- read.csv(file='allchild-months.csv')
spei <- read.csv(file='Mortality_SPI_Temps.csv')
ind <- read.csv(file='Mortality_individualdata.csv')
#res <- read.csv('Mortality_respondentdata.csv')

#Filter to the child.months that we want
child.months <- child.months %>%
  filter((age <= 60) & (months_in_loc >= 3) & (months_before_survey < 180))

write.csv(child.months, 'child.months-reduced.csv', row.names=F)

#Get the individual vars that we want and join
ind <- ind %>%
  select(-age, -mother_years_ed, -mothers_age, -alive, -years_in_loc, -interview_cmc,
         -birthdate_cmc)


#Get the respondent vars that we want and join
#Skip for now until we have the harmonized wealth data

#Lets just get SPEI data
spei <- spei %>%
  select(date=date_cmc, code, spei24, spei24ymn, spei24ymx)

child.months2 <- merge(child.months, ind, all.x=T, all.y=F)

child.months3 <- merge(child.months2, spei, all.x=T, all.y=F)

child.months3 <- child.months3 %>% 
   filter(!is.na(spei24) & !is.na(spei24ymn) & !is.na(spei24ymx))


write.csv(child.months3, 'Mortality-combined.csv', row.names=F)

##Do a quick initial analysis

child.months3$spei <- cut(child.months3$spei24, c(-2, -1, -0.5, 0.5, 1, 2))

child.months3$mortality <- !child.months3$alive

mod <- glm(mortality ~ age + mother_years_ed + birth_order + spei, family='binomial', data=child.months3)
summary(mod)


