library(tidyverse)
library(mgcv)
library(parallel)

data <- read_csv('/home/mattcoop/mortalityblob/dhs/Mortality-combined-subsample.csv')

data$mortality <- !data$alive

data <- data %>%
  select(mortality, age, mother_years_ed, mothers_age, birth_order, male, spei3, spei12, spei36, GDP, offset) %>%
  na.omit %>%
  filter(!is.infinite(spei3) & !is.infinite(spei12) & !is.infinite(spei36)) %>%
  mutate(GDP = log(30000) - log(GDP))

data$GDP[data$GDP < 0] <- 0

cl <- makeCluster(16, outfile = '')

mod3gdpREML <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei3, by=GDP) + offset(offset), 
                   family='binomial', data=data, cluster=cl, method="REML")
save(mod3gdpREML, file='/home/mattcoop/mortalityblob/mod-results/mod3gdpREML.Rdata')
mod3REML <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei3) + offset(offset), 
                family='binomial', data=data, cluster=cl, method="REML")
save(mod3REML, file='/home/mattcoop/mortalityblob/mod-results/mod3REML.Rdata')



mod12gdpREML <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei12, by=GDP) + offset(offset), 
                    family='binomial', data=data, cluster=cl, method="REML")
save(mod12gdpREML, file='/home/mattcoop/mortalityblob/mod-results/mod12gdpREML.Rdata')
mod12REML <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei12) + offset(offset), 
                 family='binomial', data=data, cluster=cl, method="REML")
save(mod12REML, file='/home/mattcoop/mortalityblob/mod-results/mod12REML.Rdata')


mod36gdpREML <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei36, by=GDP) + offset(offset), 
                    family='binomial', data=data, cluster=cl, method="REML")
save(mod36gdpREML, file='/home/mattcoop/mortalityblob/mod-results/mod36gdpREML.Rdata')
mod36REML <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei36) + offset(offset), 
                 family='binomial', data=data, cluster=cl, method="REML")
save(mod36REML, file='/home/mattcoop/mortalityblob/mod-results/mod36REML.Rdata')

system('./telegram.sh "Mod 36 Smooths Done!"')



