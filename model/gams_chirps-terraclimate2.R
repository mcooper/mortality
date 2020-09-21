library(tidyverse)
library(mgcv)
library(parallel)
library(data.table)

cl <- makeCluster(48, outfile = '')

setwd('/home/mattcoop/mortalityblob/')

#############################
# Analyze full Subset
#############################

data <- fread('mortality-dhs/Mortality-subset-full.csv') %>%
  filter(!is.infinite(spei.mm.3) & !is.infinite(spei.mm.36))

###spei.mm.3
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei.mm.3) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.3_full.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.3 done!"')

###spei.mm.36
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei.mm.36) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.36_full.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.36 done!"')

#############################
# Analyze 2yo Subset
#############################

data <- fread('mortality-dhs/Mortality-subset-2yo.csv') %>%
  filter(!is.infinite(spei.mm.3) & !is.infinite(spei.mm.36))

###spei.mm.3
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei.mm.3) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.3_2yo.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.3 done!"')

###spei.mm.36
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei.mm.36) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.36_2yo.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.36 done!"')

#############################
# Analyze 5yb Subset
#############################

data <- fread('mortality-dhs/Mortality-subset-5yb.csv') %>%
  filter(!is.infinite(spei.mm.3) & !is.infinite(spei.mm.36))

###spei.mm.3
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei.mm.3) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.3_5yb.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.3 done!"')

###spei.mm.36
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei.mm.36) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.36_5yb.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.36 done!"')

#############################
# Analyze 5yb2yo Subset
#############################

data <- fread('mortality-dhs/Mortality-subset-5yb2yo.csv') %>%
  filter(!is.infinite(spei.mm.3) & !is.infinite(spei.mm.36))

###spei.mm.3
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei.mm.3) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.3_5yb2yo.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.3 done!"')

###spei.mm.36
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei.mm.36) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei.mm.36_5yb2yo.Rdata')
system('/home/mattcoop/telegram.sh "spei.mm.36 done!"')



#############################
# Analyze full Subset
#############################

data <- fread('mortality-dhs/Mortality-subset-full.csv') %>%
  filter(!is.infinite(spei.tc.3) & !is.infinite(spei.tc.36))

###spei.tc.3
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei.tc.3) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.3_full.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.3 done!"')

###spei.tc.36
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei.tc.36) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.36_full.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.36 done!"')

#############################
# Analyze 2yo Subset
#############################

data <- fread('mortality-dhs/Mortality-subset-2yo.csv') %>%
  filter(!is.infinite(spei.tc.3) & !is.infinite(spei.tc.36))

###spei.tc.3
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei.tc.3) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.3_2yo.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.3 done!"')

###spei.tc.36
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei.tc.36) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.36_2yo.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.36 done!"')

#############################
# Analyze 5yb Subset
#############################

data <- fread('mortality-dhs/Mortality-subset-5yb.csv') %>%
  filter(!is.infinite(spei.tc.3) & !is.infinite(spei.tc.36))

###spei.tc.3
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei.tc.3) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.3_5yb.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.3 done!"')

###spei.tc.36
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei.tc.36) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.36_5yb.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.36 done!"')

#############################
# Analyze 5yb2yo Subset
#############################

data <- fread('mortality-dhs/Mortality-subset-5yb2yo.csv') %>%
  filter(!is.infinite(spei.tc.3) & !is.infinite(spei.tc.36))

###spei.tc.3
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei.tc.3) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.3_5yb2yo.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.3 done!"')

###spei.tc.36
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei.tc.36) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/chirps-terraclimate/spei.tc.36_5yb2yo.Rdata')
system('/home/mattcoop/telegram.sh "spei.tc.36 done!"')


