library(tidyverse)
library(mgcv)
library(Hmisc)

data <- read_csv('/home/mattcoop/child-months/Mortality-combined.csv')

data$mortality <- !data$alive

data <- data %>%
  select(mortality, age, mother_years_ed, mothers_age, birth_order, male, spei3, spei36, gdp) %>%
  na.omit %>%
  filter(!is.infinite(spei3) &  !is.infinite(spei36)) %>%
  mutate(gdp = cut2(gdp, g=3))

mod3factor <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei3, by=gdp), 
             family='binomial', data=data)

save(mod3factor, file='~/child-months/mod3factor.Rdata')

mod36factor <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei36, by=gdp), 
             family='binomial', data=data)

save(mod36factor, file='~/child-months/mod36factor.Rdata')

system('/home/matt/telegram.sh "New Smooths Done!"')



