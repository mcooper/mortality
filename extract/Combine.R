setwd('/home/mattcoop/mortalityblob/mortality-dhs')

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
child.months <- fread('child.months-reduced.csv', data.table=T, key=c('ind_code'))

ind <- fread(file='Mortality_individualdata.csv', data.table=T, key=c('ind_code'),
             select=c('ind_code', 'code', 'birth_order', 'male'))
temp <- fread('Temps_month.csv', data.table=T, key=c('code', 'date'),
              select=c('ct.9019.90', 'ct.9019.95', 'ct.tmax.30', 'ct.tmax.35', 'code', 'date'))
tempz <- fread('Mortality_SPI_Temps_Ewembi.csv', data.table=T, key=c('code', 'date_cmc'),
               select=c('code', 'date_cmc', 'temp1monthZ', 'temp2monthZ'))
tempz <- rename(tempz, date=date_cmc)
#spei <- fread(file='Mortality_SPI_Temps_Ewembi.csv', data.table=T, key=c('date', 'code')) %>%
#  select(date=date_cmc, code, spei3, spei6, spei12, spei24, spei36)

#gdp <- fread('Mortality_GDP_SSP_Harmonized.csv', data.table=T, key=c('cc', 'date')) %>%
#  select(date=date_cmc, cc, GDP)

# #Skip wealth Data For Now, since it is not temporal
# 
# res <- read.csv('Mortality_respondentdata.csv') %>%
#   select(wealth_factor_harmonized, hhsize, resp_code)
# comb <- Reduce(function(x,y){merge(x,y,all.x=T,all.y=F)}, list(child.months, ind, res, spei))


child.months <- merge(child.months, ind, all.x=T, all.y=F)

setkeyv(child.months, cols=c('code', 'date'))

child.months <- merge(child.months, temp, all.x=T, all.y=F)
child.months <- merge(child.months, tempz, all.x=T, all.y=F, by=c('code', 'date'))
#child.months <- merge(child.months, spei, all.x=T, all.y=F, by=c('code', 'date'))
#child.months <- merge(child.months, gdp, all.x=T, all.y=F, by=c('cc', 'date'))

#write.csv(child.months, 'Mortality-combined.csv', row.names=F)

#Do Subsample a la Wood 
n <- nrow(child.months)

S <- 0.05 #Get 5% of all zeros

sel <- child.months[!child.months$alive | runif(n) < S, ] #Sampling

sel$offset <- rep(log(nrow(sel)/n), nrow(sel))

fwrite(sel, 'Mortality-combined-subsample.csv', row.names=F)

system('/home/mattcoop/telegram.sh "Combine Done"')


