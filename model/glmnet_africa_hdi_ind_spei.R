library(data.table)
library(glmnet)
library(Matrix)
library(dplyr)
library(countrycode)
library(doMC)

registerDoMC(cores = 20)

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

hdi <- read.csv('Mortality_HDI.csv') %>%
  select(-latitude, -longitude)

data$year <- 1900 + floor((data$date - 1)/12)

data <- merge(data, hdi, by=c('code', 'year'))

#TerraClim, SHDI, AEZ, 3 + 24
for (i in c(1, 2, 3, 6, 12, 24, 36, 48)){
  print(i)

  form <- paste0("mortality ~ shdi*spei.tc.", i, " + shdi*I(pmax(spei.tc.", i, " + 1, 0)) + shdi*I(pmax(spei.tc.", i, " - 0, 0)) + shdi*I(pmax(spei.tc.", i, " - 1, 0)) + healthindex + incindex + edindex")
  mm <- sparse.model.matrix(as.formula(form), data=data)

  mod <- cv.glmnet(mm, data$mortality, family='binomial', alpha=1, parallel=T, trace.it=1)
  save(mod, file=paste0('~/mortalityblob/glmnet/spei', i, '.afr.hdi.Rdata'))
  rm(mm)
}

system('~/telegram.sh "Done with Individual SPEI glmnet smooths"')



