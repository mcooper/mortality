library(tidyverse)
library(mgcv)
library(parallel)
library(data.table)

cl <- makeCluster(8, outfile = '')

setwd('/home/mattcoop/mortalityblob/')

#############################
# Analyze full Subset
#############################

data <- fread('mortality-dhs/Mortality-subset-full.csv')

###wbgtZ
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + 
           s(wbgtZ1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/wbgtZ_full.Rdata')
system('/home/matt/telegram.sh "wbgtZ done!"')

###tmax
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + 
           s(tmax1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/tmax_full.Rdata')
system('/home/matt/telegram.sh "tmax done!"')

###spei1
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + 
           s(spei1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/spei1_full.Rdata')
system('/home/matt/telegram.sh "spei1 done!"')

###spei3
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + 
           s(spei3) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/spei3_full.Rdata')
system('/home/matt/telegram.sh "spei3 done!"')

###spei36
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + 
           s(spei36) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/spei36_full.Rdata')
system('/home/matt/telegram.sh "spei36 done!"')

#############################
# Analyze 2yo Subset
#############################

data <- fread('mortality-dhs/Mortality-subset-2yo.csv')

###wbgtZ
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + 
           s(wbgtZ1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/wbgtZ_2yo.Rdata')
system('/home/matt/telegram.sh "wbgtZ done!"')

###tmax
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + 
           s(tmax1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/tmax_2yo.Rdata')
system('/home/matt/telegram.sh "tmax done!"')

###spei1
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + 
           s(spei1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/spei1_2yo.Rdata')
system('/home/matt/telegram.sh "spei1 done!"')

###spei3
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + 
           s(spei3) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/spei3_2yo.Rdata')
system('/home/matt/telegram.sh "spei3 done!"')

###spei36
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + 
           s(spei36) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/spei36_2yo.Rdata')
system('/home/matt/telegram.sh "spei36 done!"')

#############################
# Analyze 5yb Subset
#############################

data <- fread('mortality-dhs/Mortality-subset-5yb.csv')

###wbgtZ
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + 
           s(wbgtZ1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/wbgtZ_5yb.Rdata')
system('/home/matt/telegram.sh "wbgtZ done!"')

###tmax
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + 
           s(tmax1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/tmax_5yb.Rdata')
system('/home/matt/telegram.sh "tmax done!"')

###spei1
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + 
           s(spei1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/spei1_5yb.Rdata')
system('/home/matt/telegram.sh "spei1 done!"')

###spei3
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + 
           s(spei3) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/spei3_5yb.Rdata')
system('/home/matt/telegram.sh "spei3 done!"')

###spei36
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + 
           s(spei36) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/spei36_5yb.Rdata')
system('/home/matt/telegram.sh "spei36 done!"')

#############################
# Analyze 5yb2yo Subset
#############################

data <- fread('mortality-dhs/Mortality-subset-5yb2yo.csv')

###wbgtZ
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + 
           s(wbgtZ1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/wbgtZ_5yb2yo.Rdata')
system('/home/matt/telegram.sh "wbgtZ done!"')

###tmax
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + 
           s(tmax1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/tmax_5yb2yo.Rdata')
system('/home/matt/telegram.sh "tmax done!"')

###spei1
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + 
           s(spei1) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/spei1_5yb2yo.Rdata')
system('/home/matt/telegram.sh "spei1 done!"')

###spei3
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + 
           s(spei3) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/spei3_5yb2yo.Rdata')
system('/home/matt/telegram.sh "spei3 done!"')

###spei36
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + 
           s(spei36) + offset(offset),
           family='binomial', data=data, cluster=cl)
save(mod, file='mod-results/spei36_5yb2yo.Rdata')
system('/home/matt/telegram.sh "spei36 done!"')
