setwd('/home/mattcoop/mortalityblob/mortality-dhs')

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
# house <- fread('Mortality_household.csv', data.table=T, key=c('resp_code'),
#                select=c('adequate_sanitation', 'wealth_factor_harmonized','resp_code'))
# temps <- fread('Mortality_SPI_Temps_TerraClimate.csv', data.table=T, key=c('code', 'date_cmc'),
#                select=c('code', 'date_cmc', 'wbgtZ1', 'wbgtZ2', 'wbgtZ3')) %>% 
#   rename(date=date_cmc)
spei <- fread('Mortality_SPI_Temps_ERA5.csv', data.table=T, key=c('date_cmc', 'code'))
names(spei)[names(spei) == 'date_cmc'] <- 'date'

dim(child.months)
child.months <- merge(child.months, ind, all.x=T, all.y=F)
setkeyv(child.months, cols=c('code', 'date'))
dim(child.months)
# child.months <- merge(child.months, house, all.x=T, all.y=F, by="resp_code")
# dim(child.months)
# child.months <- merge(child.months, temps, all.x=T, all.y=F, by=c('date', 'code'))
# dim(child.months)
child.months <- merge(child.months, spei, all.x=T, all.y=F, by=c('date', 'code'))
dim(child.months)

child.months <- na.omit(child.months)
child.months$mortality <- !child.months$alive
child.months[ , c("ind_code", "resp_code", "alive"):=NULL]

fwrite(child.months, 'Mortality-combined.csv', row.names=F)

system('~/telegram.sh "Done Combining"')
