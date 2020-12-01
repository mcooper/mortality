library(data.table)
library(glmnet)
library(Matrix)
library(dplyr)
library(countrycode)

setwd('~/mortalityblob/mortality-dhs/')

piece.formula <- function(var.name, knots) {
  formula.sign <- rep(" - ", length(knots))
  formula.sign[knots < 0] <- " + "
  paste(var.name, "+",
        paste("I(pmax(", var.name, formula.sign, abs(knots), ", 0))",
              collapse = " + ", sep=""))
}

data <- fread('Mortality-combined.csv') %>%
  filter(!is.infinite(spei.tc.1), !is.infinite(spei.tc.2), !is.infinite(spei.tc.3), 
         !is.infinite(spei.tc.6), !is.infinite(spei.tc.12), !is.infinite(spei.tc.24), 
         !is.infinite(spei.tc.36), !is.infinite(spei.tc.48),
         countrycode(substr(code, 1, 2), 'dhs', 'region') == 'Sub-Saharan Africa') %>%
  na.omit %>%
  select(-spei.mm.1, -spei.mm.2, -spei.mm.3, -spei.mm.6, -spei.mm.12, -spei.mm.24, -spei.mm.36,
         -spei.mm.48)

aez <- read.csv('Mortality_AEZs.csv') %>%
  select(-X, -latitude, -longitude, -earliest_date, -latest_date)

hdi <- read.csv('Mortality_HDI.csv') %>%
  select(-latitude, -longitude)

data$year <- 1900 + floor((data$date - 1)/12)

data <- merge(data, aez)
data <- merge(data, hdi, by=c('code', 'year'))

data$shdi <- 1 - data$shdi

data$aez <- factor(data$aez)

#TerraClim, SHDI, AEZ, 3 + 24
mm <- sparse.model.matrix(mortality ~ 
  aez*shdi*spei.tc.3 + aez*shdi*I(pmax(spei.tc.3 + 1, 0)) + 
  aez*shdi*I(pmax(spei.tc.3 - 0, 0)) + aez*shdi*I(pmax(spei.tc.3 - 1, 0)) + 
  aez*shdi*spei.tc.24 + aez*shdi*I(pmax(spei.tc.24 + 1, 0)) + 
  aez*shdi*I(pmax(spei.tc.24 - 0, 0)) + aez*shdi*I(pmax(spei.tc.24 - 1, 0)) + 
  healthindex + incindex + edindex, data=data)

mod <- glmnet(mm, data$mortality, family='binomial', alpha=0, lambda=0)
save(mod, file='~/mortalityblob/glmnet/spei0324.afr.aez.hdi.Rdata')

mod <- glmnet(mm, data$mortality, family='binomial', alpha=0.5, trace.it=1)
save(mod, file='~/mortalityblob/glmnet/spei0324.afr.aez.hdi.elasticnet.Rdata')

mod <- glmnet(mm, data$mortality, family='binomial', alpha=1, trace.it=1)
save(mod, file='~/mortalityblob/glmnet/spei0324.afr.aez.hdi.lasso.Rdata')

system('~/telegram.sh "Donezeo!"')

#TerraClim, SHDI, 3 + 24
mm <- sparse.model.matrix(mortality ~ 
  shdi*spei.tc.3 + shdi*I(pmax(spei.tc.3 + 1, 0)) + 
  shdi*I(pmax(spei.tc.3 - 0, 0)) + shdi*I(pmax(spei.tc.3 - 1, 0)) + 
  shdi*spei.tc.24 + shdi*I(pmax(spei.tc.24 + 1, 0)) + 
  shdi*I(pmax(spei.tc.24 - 0, 0)) + shdi*I(pmax(spei.tc.24 - 1, 0)) + 
  healthindex + incindex + edindex, data=data)

mod <- glmnet(mm, data$mortality, family='binomial', alpha=0.5, trace.it=1)
save(mod, file='~/mortalityblob/glmnet/spei0324.afr.noaez.hdi.Rdata')
rm(mm)

data$shdi <- 1 - data$shdi
#TerraClim, SHDI, 3 + 24
mm <- sparse.model.matrix(mortality ~ 
  shdi*spei.tc.3 + shdi*I(pmax(spei.tc.3 + 1, 0)) + 
  shdi*I(pmax(spei.tc.3 - 0, 0)) + shdi*I(pmax(spei.tc.3 - 1, 0)) + 
  shdi*spei.tc.24 + shdi*I(pmax(spei.tc.24 + 1, 0)) + 
  shdi*I(pmax(spei.tc.24 - 0, 0)) + shdi*I(pmax(spei.tc.24 - 1, 0)) + 
  healthindex + incindex + edindex, data=data)

mod <- glmnet(mm, data$mortality, family='binomial', alpha=0.5, trace.it=1)
save(mod, file='~/mortalityblob/glmnet/spei0324.afr.noaez.noinvhdi.Rdata')

system('~/telegram.sh "Donezeo!"')

