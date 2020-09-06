library(tidyverse)
library(mgcv)
library(parallel)
library(data.table)

cl <- makeCluster(8, outfile = '')

setwd('/home/mattcoopcoop/mortalityblob/')

#############################
# Analyze full Subset
#############################

data <- fread('mortality-dhs/Mortality-subset-full.csv') %>%
  filter(!is.infinite(spei1) & !is.infinite(spei3) & !is.infinite(spei36))

###wbgtZ
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(wbgtZ1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/terraclimate/wbgtZ_full.Rdata')
system('/home/mattcoop/telegram.sh "wbgtZ done!"')

###tmax
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(tmaxZ1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/terraclimate/tmax_full.Rdata')
system('/home/mattcoop/telegram.sh "tmax done!"')

###spei1
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/terraclimate/spei1_full.Rdata')
system('/home/mattcoop/telegram.sh "spei1 done!"')

###spei3
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei3) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/terraclimate/spei3_full.Rdata')
system('/home/mattcoop/telegram.sh "spei3 done!"')

###spei36
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei36) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/terraclimate/spei36_full.Rdata')
system('/home/mattcoop/telegram.sh "spei36 done!"')

#############################
# Analyze 2yo Subset
#############################

data <- fread('mortality-dhs/Mortality-subset-2yo.csv') %>%
  filter(!is.infinite(spei1) & !is.infinite(spei3) & !is.infinite(spei36))

###wbgtZ
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(wbgtZ1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/terraclimate/wbgtZ_2yo.Rdata')
system('/home/mattcoop/telegram.sh "wbgtZ done!"')

###tmax
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(tmaxZ1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/terraclimate/tmax_2yo.Rdata')
system('/home/mattcoop/telegram.sh "tmax done!"')

###spei1
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/terraclimate/spei1_2yo.Rdata')
system('/home/mattcoop/telegram.sh "spei1 done!"')

###spei3
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei3) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/terraclimate/spei3_2yo.Rdata')
system('/home/mattcoop/telegram.sh "spei3 done!"')

###spei36
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei36) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/terraclimate/spei36_2yo.Rdata')
system('/home/mattcoop/telegram.sh "spei36 done!"')

#############################
# Analyze 5yb Subset
#############################

data <- fread('mortality-dhs/Mortality-subset-5yb.csv') %>%
  filter(!is.infinite(spei1) & !is.infinite(spei3) & !is.infinite(spei36))

###wbgtZ
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(wbgtZ1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/terraclimate/wbgtZ_5yb.Rdata')
system('/home/mattcoop/telegram.sh "wbgtZ done!"')

###tmax
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(tmaxZ1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/terraclimate/tmax_5yb.Rdata')
system('/home/mattcoop/telegram.sh "tmax done!"')

###spei1
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/terraclimate/spei1_5yb.Rdata')
system('/home/mattcoop/telegram.sh "spei1 done!"')

###spei3
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei3) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/terraclimate/spei3_5yb.Rdata')
system('/home/mattcoop/telegram.sh "spei3 done!"')

###spei36
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei36) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/terraclimate/spei36_5yb.Rdata')
system('/home/mattcoop/telegram.sh "spei36 done!"')

#############################
# Analyze 5yb2yo Subset
#############################

data <- fread('mortality-dhs/Mortality-subset-5yb2yo.csv') %>%
  filter(!is.infinite(spei1) & !is.infinite(spei3) & !is.infinite(spei36))

###wbgtZ
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(wbgtZ1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/terraclimate/wbgtZ_5yb2yo.Rdata')
system('/home/mattcoop/telegram.sh "wbgtZ done!"')

###tmax
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(tmaxZ1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/terraclimate/tmax_5yb2yo.Rdata')
system('/home/mattcoop/telegram.sh "tmax done!"')

###spei1
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/terraclimate/spei1_5yb2yo.Rdata')
system('/home/mattcoop/telegram.sh "spei1 done!"')

###spei3
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei3) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/terraclimate/spei3_5yb2yo.Rdata')
system('/home/mattcoop/telegram.sh "spei3 done!"')

###spei36
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + date + 
           s(spei36) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/terraclimate/spei36_5yb2yo.Rdata')
system('/home/mattcoop/telegram.sh "spei36 done!"')


