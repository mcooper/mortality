library(tidyverse)
library(mgcv)
library(parallel)

data <- read_csv('/home/mattcoop/mortalityblob/dhs/Mortality-combined.csv')

data$mortality <- !data$alive

cl <- makeCluster(8, outfile = '')

data <- data %>%
  select(mortality, age, mother_years_ed, mothers_age, birth_order, male, temp1monthZ, temp2monthZ, temp3monthZ, temp6monthZ, temp12monthZ) %>%
  na.omit

###temp1monthZ
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(temp1monthZ),
           family='binomial', data=data, cluster=cl)

save(mod, file='~/mortalityblob/mod-results/mod_temp1monthZ.Rdata')

system('/home/matt/telegram.sh "temp1monthZ done!"')

###temp2monthZ
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(temp2monthZ),
           family='binomial', data=data, cluster=cl)

save(mod, file='~/mortalityblob/mod-results/mod_temp2monthZ.Rdata')

system('/home/matt/telegram.sh "temp2monthZ done!"')

###temp3monthZ
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(temp3monthZ),
           family='binomial', data=data, cluster=cl)

save(mod, file='~/mortalityblob/mod-results/mod_temp3monthZ.Rdata')

system('/home/matt/telegram.sh "temp3monthZ done!"')

###temp6monthZ
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(temp6monthZ),
           family='binomial', data=data, cluster=cl)

save(mod, file='~/mortalityblob/mod-results/mod_temp6monthZ.Rdata')

system('/home/matt/telegram.sh "temp6monthZ done!"')

###temp12monthZ
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(temp12monthZ),
           family='binomial', data=data, cluster=cl)

save(mod, file='~/mortalityblob/mod-results/mod_temp12monthZ.Rdata')

system('/home/matt/telegram.sh "temp12monthZ done!"')
