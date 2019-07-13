library(tidyverse)
library(mgcv)
library(parallel)

data <- read_csv('/home/mattcoop/mortalityblob/Mortality-combined.csv')

data$mortality <- !data$alive

cl <- makeCluster(4, outfile = '')

data <- data %>%
  select(mortality, age, mother_years_ed, mothers_age, birth_order, male, spei3, spei36) %>%
  na.omit %>%
  filter(!is.infinite(spei3) & !is.infinite(spei36))

mod3 <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei3), 
             family='binomial', data=data, cluster=cl)

save(mod3, file='~/mortalityblob/mod-results/mod3_elewi.Rdata')

system('/home/climatedisk/telegram.sh "3 Smooths Done!"')

rm(mod3)

mod36 <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei36), 
             family='binomial', data=data, cluster=cl)

save(mod36, file='~/mortalityblob/mod-results/mod36_elewi.Rdata')

system('/home/matt/telegram.sh "36 Smooths Done!"')




