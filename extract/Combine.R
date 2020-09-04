setwd('/home/mattcoop/mortalityblob/mortality-dhs')

library(tidyverse)
library(data.table)
 
#  child.months <- fread(file='allchild-months.csv')
#  
#  names(child.months) <- c("ind_code", "date", "age", "mother_years_ed", "mothers_age",
#                           "months_in_loc", "months_before_survey", "alive")
#  
#  #Filter to the child.months that we want
#  #  Children under 5
#  #  At least three months in location
#  #  And within 15 years of the survey
#  child.months <- child.months %>%
#    filter((age <= 60) & (months_in_loc >= 3) & (months_before_survey < 180))
# 
#  write.csv(child.months, 'child.months-reduced.csv', row.names=F)

child.months <- fread('child.months-reduced.csv', data.table=T, key=c('ind_code'))
ind <- fread(file='Mortality_individualdata.csv', data.table=T, key=c('ind_code'),
             select=c('ind_code', 'code', 'birth_order', 'male', 'resp_code'))
house <- fread('Mortality_household.csv', data.table=T, key=c('resp_code'),
               select=c('adequate_sanitation', 'wealth_factor_harmonized','resp_code'))
clim <- fread('Mortality_SPI_Temps_TerraClimate.csv', data.table=T, key=c('code', 'date_cmc'))
clim <- rename(clim, date=date_cmc)

child.months <- merge(child.months, ind, all.x=T, all.y=F)
setkeyv(child.months, cols=c('code', 'date'))

child.months <- merge(child.months, house, all.x=T, all.y=F, by="resp_code")

child.months <- merge(child.months, clim, all.x=T, all.y=F, by=c('date', 'code'))

child.months <- child.months %>%
  select(-ind_code, -resp_code)

fwrite(child.months, 'Mortality-combined.csv', row.names=F)


#Subset to five years before survey

#Do Subsample a la Wood 
n <- nrow(child.months)
S <- 0.05 #Get 5% of all zeros
sel <- child.months[!child.months$alive | runif(n) < S, ] #Sampling
sel$offset <- rep(log(nrow(sel)/n), nrow(sel))
fwrite(sel, 'Mortality-combined-subsample.csv', row.names=F)


#Do Subsample a la Wood 
n <- nrow(child.months)
S <- 0.05 #Get 5% of all zeros
sel <- child.months[!child.months$alive | runif(n) < S, ] #Sampling
sel$offset <- rep(log(nrow(sel)/n), nrow(sel))
fwrite(sel, 'Mortality-combined-subsample.csv', row.names=F)

system('/home/mattcoop/telegram.sh "Combine Done"')


