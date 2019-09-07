#Note: Didnt work

library(tidyverse)
library(mgcv)
library(scam)

data <- read_csv('/home/mattcoop/mortalityblob/Mortality-combined.csv')

data$mortality <- !data$alive

data <- data %>%
  select(mortality, age, mother_years_ed, mothers_age, birth_order, male, spei3, spei36, GDP) %>%
  na.omit %>%
  filter(!is.infinite(spei3) & !is.infinite(spei36))

mod <- scam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei3, bs='cv'),
           family='binomial', data=data, cluster=cl)

save(mod, file='~/mortalityblob/mod-results/mod_scam_spei3.Rdata')

system('~/telegram.sh "Done with first smooth!"')

rm(mod)

mod <- scam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei36, bs='cv'),
             family='binomial', data=data, cluster=cl)

save(mod, file='~/mortalityblob/mod-results/mod_scam_spei36.Rdata')

system('~/telegram.sh "Done with second smooth!"')


