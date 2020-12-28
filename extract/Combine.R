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
spei0 <- fread('Mortality_SPI_Temps.csv', data.table=T, key=c('date_cmc', 'code'))
names(spei0)[names(spei0) == 'date_cmc'] <- 'date'
spei0 <- spei0[ , c('date', 'code', 'spei3', 
                    'spei6', 'spei12', 'spei24', 'spei36', 'spei48')]
names(spei0)[grepl('spei', names(spei0))] <- gsub('spei', 'spei.ch.', names(spei0)[grepl('spei', names(spei0))])
spei0 <- unique(spei0)

spei1 <- fread('Mortality_SPI_Temps_FLDAS.csv', data.table=T, key=c('date_cmc', 'code'))
names(spei1)[names(spei1) == 'date_cmc'] <- 'date'
spei1 <- spei1[ , c('date', 'code', 'spei1', 'spei2', 'spei3', 
                    'spei6', 'spei12', 'spei24', 'spei36', 'spei48')]
names(spei1)[grepl('spei', names(spei1))] <- gsub('spei', 'spei.fl.', names(spei1)[grepl('spei', names(spei1))])
spei1 <- unique(spei1)

spei2 <- fread('Mortality_SPI_Temps_ERA5.csv', data.table=T, key=c('date_cmc', 'code'))
names(spei2)[names(spei2) == 'date_cmc'] <- 'date'
spei2 <- spei2[ , c('date', 'code', 'spei1', 'spei2', 'spei3', 
                    'spei6', 'spei12', 'spei24', 'spei36', 'spei48')]
names(spei2)[grepl('spei', names(spei2))] <- gsub('spei', 'spei.er.', names(spei2)[grepl('spei', names(spei2))])
spei2 <- unique(spei2)

spei3 <- fread('Mortality_SPI_Temps_TerraClimate.csv', data.table=T, key=c('date_cmc', 'code'))
names(spei3)[names(spei3) == 'date_cmc'] <- 'date'
spei3 <- spei3[ , c('date', 'code', 'spei1', 'spei2', 'spei3', 
                    'spei6', 'spei12', 'spei24', 'spei36', 'spei48')]
names(spei3)[grepl('spei', names(spei3))] <- gsub('spei', 'spei.tc.', names(spei3)[grepl('spei', names(spei3))])
spei3 <- unique(spei3)

setkeyv(child.months, cols=c('code', 'date'))
dim(child.months)
child.months <- merge(child.months, ind, all.x=T, all.y=F, by=c('ind_code'))
dim(child.months)
child.months <- merge(child.months, spei0, all.x=T, all.y=F, by=c('date', 'code'))
dim(child.months)
child.months <- merge(child.months, spei1, all.x=T, all.y=F, by=c('date', 'code'))
dim(child.months)
child.months <- merge(child.months, spei2, all.x=T, all.y=F, by=c('date', 'code'))
dim(child.months)
child.months <- merge(child.months, spei3, all.x=T, all.y=F, by=c('date', 'code'))
dim(child.months)

child.months <- na.omit(child.months)
child.months$mortality <- !child.months$alive
child.months[ , c("ind_code", "resp_code", "alive"):=NULL]

fwrite(child.months, 'Mortality-combined.csv', row.names=F)

system('~/telegram.sh "Done Combining"')
