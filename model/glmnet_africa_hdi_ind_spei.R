library(data.table)
library(glmnet)
library(Matrix)
library(dplyr)
library(countrycode)

setwd('~/mortalityblob/mortality-dhs')

data <- fread('Mortality-combined.csv')

data <- data %>%
  filter(!is.infinite(spei1), !is.infinite(spei2), !is.infinite(spei3), 
         !is.infinite(spei6), !is.infinite(spei12), !is.infinite(spei24), 
         !is.infinite(spei36), !is.infinite(spei48),
         countrycode(substr(code, 1, 2), 'dhs', 'region') == 'Sub-Saharan Africa') %>%
  na.omit

hdi <- read.csv('Mortality_HDI.csv') %>%
  select(-latitude, -longitude)

data$year <- 1900 + floor((data$date - 1)/12)

data <- merge(data, hdi, by=c('code', 'year'))

#TerraClim, SHDI, AEZ, 3 + 24
for (i in c(1, 2, 3, 6, 12, 24, 36, 48)){
  print(i)

  form <- paste0("mortality ~ shdi*spei", i, " + shdi*I(pmax(spei", i, " + 1, 0)) + shdi*I(pmax(spei", i, " - 0, 0)) + shdi*I(pmax(spei", i, " - 1, 0)) + healthindex + incindex + edindex")
  mm <- sparse.model.matrix(as.formula(form), data=data)

  mod <- glmnet(mm, data$mortality, family='binomial', alpha=1, trace.it=1)
  save(mod, file=paste0('~/mortalityblob/glmnet/spei', i, '.afr.hdi.fl.Rdata'))
  rm(mm)
}

system('~/telegram.sh "Done with Individual SPEI glmnet smooths"')



