library(tidyverse)
library(mgcv)
library(parallel)

data <- read_csv('/home/mattcoop/mortalityblob/Mortality-combined.csv')

data$mortality <- !data$alive

cl <- makeCluster(4, outfile = '')

data <- data %>%
  select(mortality, age, mother_years_ed, mothers_age, birth_order, male, spei3, spei36, GDP) %>%
  na.omit %>%
  filter(!is.infinite(spei3) & !is.infinite(spei36))
  
data$GDP <- log(60000) - log(data$GDP)

data$GDP[data$GDP < 0] <- 0

# mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei3, by=GDP), 
#            family='binomial', data=data, cluster=cl)
# 
# save(mod, file='~/mortalityblob/mod-results/mod_gam_spei3only_gdp.Rdata')


# mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei36, by=GDP), 
#              family='binomial', data=data, cluster=cl)

# save(mod, file='~/mortalityblob/mod-results/mod_gam_spei36only_gdp.Rdata')


mod <- bam(mortality ~ s(spei36, by=GDP) + s(spei3, by=GDP), 
             family='binomial', data=data, cluster=cl)

save(mod, file='~/mortalityblob/mod-results/mod_gam_spei_gdp_nocovars.Rdata')

system('/home/mattcoop/telegram.sh "GAM Done!"')

rm(mod)

mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei3, by=GDP, bs='tp') + s(spei36, by=GDP, bs='tp'), 
             family='binomial', data=data, cluster=cl)

save(mod, file='~/mortalityblob/mod-results/mod_gam_spei_gdp_tp.Rdata')

rm(mod)

mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei3, by=GDP) + s(spei36, by=GDP), 
             family='binomial', data=data, cluster=cl, method = "REML")

save(mod, file='~/mortalityblob/mod-results/mod_gam_spei36only_gdp_REML.Rdata')



# Clip to well-sampled SPEI levels

data <- data %>%
  filter(spei36 > -1.75 & spei36 < 1.75 & spei3 > -1.75 & spei3 < 1.75)
 
mod <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei3, by=GDP, bs='cr') + s(spei36, by=GDP, bs='cr'), 
             family='binomial', data=data, cluster=cl)

save(mod, file='~/mortalityblob/mod-results/mod_gam_spei_gdp_speifilter.Rdata')


mod <- bam(mortality ~ s(spei3, by=GDP, bs='cr') + s(spei36, by=GDP, bs='cr'), 
             family='binomial', data=data, cluster=cl)

save(mod, file='~/mortalityblob/mod-results/mod_gam_spei_gdp_speifilter_nocovars.Rdata')
 