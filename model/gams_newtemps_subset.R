library(tidyverse)
library(mgcv)
library(parallel)
library(data.table)

data <- fread('/home/mattcoop/mortalityblob/mortality-dhs/Mortality-combined-subsample.csv') 
data$mortality <- !data$alive

cl <- makeCluster(4, outfile = '')

data <- data %>%
  select(mortality, age, mother_years_ed, mothers_age, birth_order, male, x, y, 
				 ct.9019.999, ct.9019.99, ct.9019.95, ct.9019.90, 
				 ct.tmax.30, ct.tmax.35, ct.tmax.40, offset) %>%
  na.omit

###ct.9019.999
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(ct.9019.999) + offset(offset),
           family='binomial', data=data, cluster=cl)

saveRDS(mod, file='~/mortalityblob/mod-results/mod_ct.9019.999.subset.RDS')

system('/home/matt/telegram.sh "ct.9019.999 done!"')

###ct.9019.99
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(ct.9019.99) + offset(offset),
           family='binomial', data=data, cluster=cl)

saveRDS(mod, file='~/mortalityblob/mod-results/mod_ct.9019.99.subset.RDS')

system('/home/matt/telegram.sh "ct.9019.99 done!"')

###ct.9019.95
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(ct.9019.95) + offset(offset),
           family='binomial', data=data, cluster=cl)

saveRDS(mod, file='~/mortalityblob/mod-results/mod_ct.9019.95.subset.RDS')

system('/home/matt/telegram.sh "ct.9019.95 done!"')

###ct.9019.90
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(ct.9019.90) + offset(offset),
           family='binomial', data=data, cluster=cl)

saveRDS(mod, file='~/mortalityblob/mod-results/mod_ct.9019.90.subset.RDS')

system('/home/matt/telegram.sh "ct.9019.90 done!"')

###ct.tmax.30
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(y, x, bs='sos') + s(ct.tmax.30) + offset(offset),
           family='binomial', data=data, cluster=cl)

saveRDS(mod, file='~/mortalityblob/mod-results/mod_ct.tmax.30.subset.RDS')

system('/home/matt/telegram.sh "ct.tmax.30 done!"')

###ct.tmax.35
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(y, x, bs='sos') + s(ct.tmax.35) + offset(offset),
           family='binomial', data=data, cluster=cl)

saveRDS(mod, file='~/mortalityblob/mod-results/mod_ct.tmax.35.subset.RDS')

system('/home/matt/telegram.sh "ct.tmax.35 done!"')

###ct.tmax.40
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(y, x, bs='sos') + s(ct.tmax.40) + offset(offset),
           family='binomial', data=data, cluster=cl)

saveRDS(mod, file='~/mortalityblob/mod-results/mod_ct.tmax.40.subset.RDS')

system('/home/matt/telegram.sh "ct.tmax.40 done!"')

system('poweroff')
