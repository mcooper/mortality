library(tidyverse)
library(mgcv)
library(beepr)

data <- read.csv('G://My Drive/DHS Processed/Mortality-combined-subsample.csv')

data$mortality <- !data$alive

data <- data %>%
  select(mortality, age, mother_years_ed, mothers_age, birth_order, male, spei3, spei6, spei12, spei24, spei36,
         offset) %>%
  na.omit

###spei3
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + offset(offset) + s(spei3),
           family='binomial', data=data)

save(mod, file='G://My Drive/Mortality/Mods/mod_spei3-subsample.Rdata')
beep('mario')

###spei6
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + offset(offset) + s(spei6),
           family='binomial', data=data)

save(mod, file='G://My Drive/Mortality/Mods/mod_spei6-subsample.Rdata')
beep('mario')

###spei12
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + offset(offset) + s(spei12),
           family='binomial', data=data)

save(mod, file='G://My Drive/Mortality/Mods/mod_spei12-subsample.Rdata')
beep('mario')

###spei24
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + offset(offset) + s(spei24),
           family='binomial', data=data)

save(mod, file='G://My Drive/Mortality/Mods/mod_spei24-subsample.Rdata')
beep('mario')

###spei36
mod <- gam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + offset(offset) + s(spei36),
           family='binomial', data=data)

save(mod, file='G://My Drive/Mortality/Mods/mod_spei36-subsample.Rdata')
beep('mario')
