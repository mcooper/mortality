library(tidyverse)
library(mgcv)
library(parallel)
library(data.table)

cl <- makeCluster(32, outfile = '')

setwd('/home/mattcoop/mortalityblob/')

#############################
# Analyze full Subset
#############################

data <- fread('mortality-dhs/Mortality-subset-full.csv') %>%
  filter(!is.infinite(spei3) & !is.infinite(spei36))

###spei3
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei3) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei3_full.Rdata')
system('/home/mattcoop/telegram.sh "spei3 done!"')

###spei36
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei36) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei36_full.Rdata')
system('/home/mattcoop/telegram.sh "spei36 done!"')

#############################
# Analyze 2yo Subset
#############################

data <- fread('mortality-dhs/Mortality-subset-2yo.csv') %>%
  filter(!is.infinite(spei3) & !is.infinite(spei36))

###spei3
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei3) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei3_2yo.Rdata')
system('/home/mattcoop/telegram.sh "spei3 done!"')

###spei36
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei36) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei36_2yo.Rdata')
system('/home/mattcoop/telegram.sh "spei36 done!"')

#############################
# Analyze 5yb Subset
#############################

data <- fread('mortality-dhs/Mortality-subset-5yb.csv') %>%
  filter(!is.infinite(spei3) & !is.infinite(spei36))

###spei3
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei3) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei3_5yb.Rdata')
system('/home/mattcoop/telegram.sh "spei3 done!"')

###spei36
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei36) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei36_5yb.Rdata')
system('/home/mattcoop/telegram.sh "spei36 done!"')

#############################
# Analyze 5yb2yo Subset
#############################

data <- fread('mortality-dhs/Mortality-subset-5yb2yo.csv') %>%
  filter(!is.infinite(spei3) & !is.infinite(spei36))

###spei3
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei3) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei3_5yb2yo.Rdata')
system('/home/mattcoop/telegram.sh "spei3 done!"')

###spei36
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei36) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei36_5yb2yo.Rdata')
system('/home/mattcoop/telegram.sh "spei36 done!"')


