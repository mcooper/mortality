library(tidyverse)
library(mgcv)
library(parallel)
library(data.table)

data <- fread('/home/mattcoop/mortalityblob/mortality-dhs/Mortality-combined-subsample.csv')

data$mortality <- !data$alive

cl <- makeCluster(8, outfile = '')

###temp1monthZ
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(temp1monthZ),
           family='binomial', data=data[data$months_before_survey <= 48, ], cluster=cl)

save(mod, file='~/mortalityblob/mod-results/mod_temp1monthZ_recent.Rdata')

system('/home/matt/telegram.sh "temp1monthZ done!"')

###temp2monthZ
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(temp2monthZ),
           family='binomial', data=data[data$months_before_survey <= 48, ], cluster=cl)

save(mod, file='~/mortalityblob/mod-results/mod_temp2monthZ_recent.Rdata')

system('/home/matt/telegram.sh "temp2monthZ done!"')

###ct.9019.95
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(ct.9019.95),
           family='binomial', data=data, cluster=cl)

save(mod, file='~/mortalityblob/mod-results/mod_ct.9019.95.Rdata')

system('/home/mattcoop/telegram.sh "ct.9019.95 done!"')

###ct.9019.90
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(ct.9019.90),
           family='binomial', data=data[data$months_before_survey <= 48, ], cluster=cl)

save(mod, file='~/mortalityblob/mod-results/mod_ct.9019.90_recent.Rdata')

system('/home/matt/telegram.sh "ct.9019.90 done!"')

###ct.tmax.30
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(code, bs='re'),
           family='binomial', data=data[data$months_before_survey <= 48, ], cluster=cl)

save(mod, file='~/mortalityblob/mod-results/mod_ct.tmax.30.fe.Rdata')

system('/home/mattcoop/telegram.sh "ct.tmax.30 done!"')

###ct.tmax.35
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(code, bs='re') + s(ct.tmax.35),
           family='binomial', data=data, cluster=cl)

save(mod, file='~/mortalityblob/mod-results/mod_ct.tmax.35.fe.Rdata')

system('/home/mattcoop/telegram.sh "ct.tmax.35 done!"')
