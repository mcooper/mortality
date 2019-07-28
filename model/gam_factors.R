library(tidyverse)
library(mgcv)
library(Hmisc)
library(parallel)

data <- read_csv('/home/mattcoop/mortalityblob/Mortality-combined.csv')

data$mortality <- !data$alive

data <- data %>%
  select(mortality, age, mother_years_ed, mothers_age, birth_order, male, spei3, spei36, gdp) %>%
  na.omit %>%
  filter(!is.infinite(spei3) &  !is.infinite(spei36)) %>%
  mutate(gdp = cut2(gdp, g=3))

cl <- makeCluster(8, outfile = '')

mod3factor <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei3, by=gdp), 
             family='binomial', data=data, cluster=cl, discrete=TRUE)

save(mod3factor, file='~/mortalityblob/mod-results/mod3factor.Rdata')

system('~/telegram.sh "Mod 3 Smooths Done!"')

rm(mod3factor)

mod36factor <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei36, by=gdp), 
             family='binomial', data=data, cluster=cl, discrete=TRUE)

save(mod36factor, file='~/mortalityblob/mod-results/mod36factor.Rdata')

system('./telegram.sh "Mod 36 Smooths Done!"')



