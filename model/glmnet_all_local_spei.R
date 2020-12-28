library(data.table)
library(glmnet)
library(Matrix)
library(dplyr)
library(countrycode)

setwd('~/mortalityblob/mortality-dhs')

data <- fread('Mortality-combined.csv')

data <- data %>%
  filter(!is.infinite(spei.er.1), !is.infinite(spei.er.2), !is.infinite(spei.er.3), 
         !is.infinite(spei.er.6), !is.infinite(spei.er.12), !is.infinite(spei.er.24), 
         !is.infinite(spei.er.36), !is.infinite(spei.er.48),
         !is.infinite(spei.tc.1), !is.infinite(spei.tc.2), !is.infinite(spei.tc.3), 
         !is.infinite(spei.tc.6), !is.infinite(spei.tc.12), !is.infinite(spei.tc.24), 
         !is.infinite(spei.tc.36), !is.infinite(spei.tc.48),
         !is.infinite(spei.fl.1), !is.infinite(spei.fl.2), !is.infinite(spei.fl.3), 
         !is.infinite(spei.fl.6), !is.infinite(spei.fl.12), !is.infinite(spei.fl.24), 
         !is.infinite(spei.fl.36), !is.infinite(spei.fl.48))

for (i in c(1, 2, 3, 6, 12, 24, 36, 48)){
  print(i)

  #ERA5
  form <- paste0("mortality ~ age + mother_years_ed + birth_order + male + spei.er.", i, " + I(pmax(spei.er.", i, " + 1, 0)) + I(pmax(spei.er.", i, " - 0, 0)) + I(pmax(spei.er.", i, " - 1, 0))")
  mm <- sparse.model.matrix(as.formula(form), data=data)

  mod <- glmnet(mm, data$mortality, family='binomial', alpha=1, trace.it=1)
  save(mod, file=paste0('~/mortalityblob/glmnet/spei.er.', i, '.all.local.Rdata'))
  rm(mm)

  #FLDAS
  form <- paste0("mortality ~ age + mother_years_ed + birth_order + male + spei.fl.", i, " + I(pmax(spei.fl.", i, " + 1, 0)) + I(pmax(spei.fl.", i, " - 0, 0)) + I(pmax(spei.fl.", i, " - 1, 0))")
  mm <- sparse.model.matrix(as.formula(form), data=data)

  mod <- glmnet(mm, data$mortality, family='binomial', alpha=1, trace.it=1)
  save(mod, file=paste0('~/mortalityblob/glmnet/spei.fl.', i, '.all.local.Rdata'))
  rm(mm)

  #TerraClimate
  form <- paste0("mortality ~ age + mother_years_ed + birth_order + male + spei.tc.", i, " + I(pmax(spei.tc.", i, " + 1, 0)) + I(pmax(spei.tc.", i, " - 0, 0)) + I(pmax(spei.tc.", i, " - 1, 0))")
  mm <- sparse.model.matrix(as.formula(form), data=data)

  mod <- glmnet(mm, data$mortality, family='binomial', alpha=1, trace.it=1)
  save(mod, file=paste0('~/mortalityblob/glmnet/spei.tc.', i, '.all.local.Rdata'))
  rm(mm)
}

system('~/telegram.sh "Done with Individual SPEI glmnet smooths"')
system('sudo poweroff')


