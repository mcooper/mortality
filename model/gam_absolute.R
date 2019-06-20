library(tidyverse)
library(mgcv)

data <- read.csv('/home/mattcoop/child-months/Mortality-combined.csv')

data$mortality <- !data$alive

data <- data %>%
  select(mortality, age, mother_years_ed, mothers_age, birth_order, male, temp3, last_year_precip) %>%
  na.omit

modtemp3 <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(temp3), 
             family='binomial', data=data)

save(modtemp3, file='~/mod-results/modtemp3.Rdata')

modlast_year_precip <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(last_year_precip), 
            family='binomial', data=data)

save(modlast_year_precip, file='~/mod-results/modlast_year_precip.Rdata')

system('/home/matt/telegram.sh "New Smooths Done!"')



