library(tidyverse)
library(mgcv)
library(parallel)
library(data.table)

data <- fread('/home/mattcoop/mortalityblob/dhs/Mortality-combined.csv')

data$mortality <- !data$alive
data$cc <- substr(data$code, 1, 2)

cl <- makeCluster(8, outfile = '')

data <- data %>%
  select(mortality, age, mother_years_ed, mothers_age, birth_order, male, cc, 
				 ct.9019.999, ct.9019.99, ct.9019.95, ct.9019.90, 
				 ct.tmax.30, ct.tmax.35, ct.tmax.40=ct.tmax40) %>%
  na.omit

# ###ct.9019.999
# mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(ct.9019.999),
#            family='binomial', data=data, cluster=cl)
# 
# save(mod, file='~/mortalityblob/mod-results/mod_ct.9019.999.Rdata')
# 
# system('/home/matt/telegram.sh "ct.9019.999 done!"')
# 
# ###ct.9019.99
# mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(ct.9019.99),
#            family='binomial', data=data, cluster=cl)
# 
# save(mod, file='~/mortalityblob/mod-results/mod_ct.9019.99.Rdata')
# 
# system('/home/matt/telegram.sh "ct.9019.99 done!"')

###ct.9019.95
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(ct.9019.95),
           family='binomial', data=data, cluster=cl)

save(mod, file='~/mortalityblob/mod-results/mod_ct.9019.95.Rdata')

system('/home/matt/telegram.sh "ct.9019.95 done!"')

###ct.9019.90
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(ct.9019.90),
           family='binomial', data=data, cluster=cl)

save(mod, file='~/mortalityblob/mod-results/mod_ct.9019.90.Rdata')

system('/home/matt/telegram.sh "ct.9019.90 done!"')

###ct.tmax.30
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + cc + s(ct.tmax.30),
           family='binomial', data=data, cluster=cl)

save(mod, file='~/mortalityblob/mod-results/mod_ct.tmax.30.Rdata')

system('/home/matt/telegram.sh "ct.tmax.30 done!"')

###ct.tmax.35
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + cc + s(ct.tmax.35),
           family='binomial', data=data, cluster=cl)

save(mod, file='~/mortalityblob/mod-results/mod_ct.tmax.35.Rdata')

system('/home/matt/telegram.sh "ct.tmax.35 done!"')

###ct.tmax.40
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + cc + s(ct.tmax.40),
           family='binomial', data=data, cluster=cl)

save(mod, file='~/mortalityblob/mod-results/mod_ct.tmax.40.Rdata')

system('/home/matt/telegram.sh "ct.tmax.40 done!"')

system('poweroff')
