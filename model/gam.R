library(tidyverse)
library(mgcv)
library(parallel)

data <- read_csv('/home/mattcoop/child-months/Mortality-combined.csv')

data$mortality <- !data$alive

cl <- makeCluster(4, outfile = '')

data <- data %>%
  select(mortality, age, mother_years_ed, mothers_age, birth_order, male, spei3, spei36) %>%
  na.omit %>%
  filter(!is.infinite(spei3) & !is.infinite(spei12) & !is.infinite(spei36))

mod3 <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei3), 
             family='binomial', data=data, cluster=cl)

save(mod3, file='~/mod-results/mod3_new.Rdata')

system('/home/matt/telegram.sh "3 Smooths Done!"')


mod36 <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei36), 
             family='binomial', data=data, cluster=cl)

save(mod36, file='~/mod-results/mod36_new.Rdata')

system('/home/matt/telegram.sh "36 Smooths Done!"')




