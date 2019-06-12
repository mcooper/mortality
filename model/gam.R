library(tidyverse)
library(mgcv)

data <- read.csv('/home/mattcoop/child-months/Mortality-combined.csv')

data$mortality <- !data$alive

data <- data %>%
  select(mortality, age, mother_years_ed, mothers_age, birth_order, male, spei3, spei6, spei12, spei24, spei36, spei48) %>%
  na.omit %>%
  filter(!is.infinite(spei3) & !is.infinite(spei6) & !is.infinite(spei12) & !is.infinite(spei24) & !is.infinite(spei12) & !is.infinite(spei48))

mod3 <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei3), 
             family='binomial', data=data)

save(mod3, file='~/mod-results/mod3.Rdata')

mod6 <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei6), 
            family='binomial', data=data)

save(mod6, file='~/mod-results/mod6.Rdata')

mod12 <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei12), 
             family='binomial', data=data)

save(mod12, file='~/mod-results/mod12.Rdata')

mod24 <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei24), 
             family='binomial', data=data)

save(mod24, file='~/mod-results/mod24.Rdata')

mod36 <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei36), 
             family='binomial', data=data)

save(mod36, file='~/mod-results/mod36.Rdata')

mod48 <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei48), 
             family='binomial', data=data)

save(mod48, file='~/mod-results/mod48.Rdata')

system('/home/matt/telegram.sh "New Smooths Done!"')



