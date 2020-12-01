library(data.table)
library(glmnet)
library(Matrix)
library(dplyr)
library(countrycode)

setwd('~/mortalityblob/mortality-dhs')

data <- fread('Mortality-combined.csv') %>%
  filter(!is.infinite(spei.ch.3), 
         !is.infinite(spei.ch.6), !is.infinite(spei.ch.12), !is.infinite(spei.ch.24), 
         !is.infinite(spei.ch.36),
         countrycode(substr(code, 1, 2), 'dhs', 'region') == 'Sub-Saharan Africa') %>%
  na.omit %>%
  select(-spei.tc.3, -spei.tc.6, -spei.tc.12, -spei.tc.24, -spei.tc.36)

hdi <- read.csv('Mortality_HDI.csv') %>%
  select(-latitude, -longitude)

data$year <- 1900 + floor((data$date - 1)/12)

data <- merge(data, hdi, by=c('code', 'year'))

#TerraClim, SHDI, AEZ, 3 + 24
for (i in c(3, 6, 12, 24, 36)){
  print(i)

  form <- paste0("mortality ~ shdi*spei.ch.", i, " + shdi*I(pmax(spei.ch.", i, " + 1, 0)) + shdi*I(pmax(spei.ch.", i, " - 0, 0)) + shdi*I(pmax(spei.ch.", i, " - 1, 0)) + healthindex + incindex + edindex")
  mm <- sparse.model.matrix(as.formula(form), data=data)

  mod <- glmnet(mm, data$mortality, family='binomial', alpha=1, trace.it=1)
  save(mod, file=paste0('~/mortalityblob/glmnet/spei', i, '.afr.hdi.ch.Rdata'))
  rm(mm)
}

system('~/telegram.sh "Done with Individual SPEI glmnet smooths"')



