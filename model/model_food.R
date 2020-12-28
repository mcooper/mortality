library(data.table)
library(mgcv)
library(ggplot2)
library(dplyr)
library(parallel)

setwd('~/mortalityblob/mortality-dhs/')

cl <- makeCluster(8, outfile = '')

###########################################################
# Read in and combine indiviudal and month level data
#############################################################

child.months <- fread('child.months-reduced.csv', data.table=T, key=c('ind_code'))
ind <- fread(file='Mortality_individualdata.csv', data.table=T, key=c('ind_code'),
           select=c('ind_code', 'code', 'birth_order', 'male', 'resp_code'))
child.months <- merge(child.months, ind, all.x=T, all.y=F, by=c('ind_code'))

child.months[ , ind_code:=NULL]
child.months[ , resp_code:=NULL]

child.months$cc <- substr(child.months$code, 1, 2)
child.months$Year <- 1900 + floor((child.months$date - 1)/12)
child.months$mortality <- !child.months$alive

#Add Fao food production indices
fao <- fread('food_production.csv')

#Combine all
child.months <- merge(child.months, fao, all.x=T, all.y=F, by=c('cc', 'Year'))
child.months <- child.months[!is.na(child.months$cereals), ]
child.months <- child.months[!is.na(child.months$mother_years_ed), ]

###########################################################
# Run Model
######################################################
child.months$cc <- factor(child.months$cc)

start <- Sys.time()
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + 
           s(cereals, bs='cr') + s(date, bs='cr') + cc,
           family='binomial', data=child.months, cluster=cl)
end <- Sys.time()

system('~/telegram.sh "Done with Mortality - food model"')

