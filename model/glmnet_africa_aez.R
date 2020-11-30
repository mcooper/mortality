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
        paste("aez*I(pmax(", var.name, formula.sign, abs(knots), ", 0))",
              collapse = " + ", sep=""))
}

data <- fread('Mortality-combined.csv') %>%
  filter(!is.infinite(spei.tc.3), !is.infinite(spei.tc.24), 
         !is.infinite(spei.ch.3), !is.infinite(spei.ch.24),
         countrycode(substr(code, 1, 2), 'dhs', 'region') == 'Sub-Saharan Africa')

geo <- fread('Mortality_geodata.csv') %>%
   filter(countrycode(substr(code, 1, 2), 'dhs', 'region') == 'Sub-Saharan Africa')

aez <- read.csv('Mortality_AEZs.csv') %>%
  select(-X, -latitude, -longitude, -earliest_date, -latest_date)

data <- merge(data, aez)

data$cc <- substr(data$code, 1, 2)
data$year <- 1900 + floor((data$date - 1)/12)
data$year <- as.character(floor(data$year/5))

#SPEI03 TerraClim
mm <- sparse.model.matrix(formula(paste0("mortality ~ age + mother_years_ed + mothers_age + aez*",
                          piece.formula("spei.tc.3", c(-1, 0, 1)), 
                         " + birth_order + male + cc*year")), data=data)
mod <- glmnet(mm, data$mortality, family='binomial', alpha=0, lambda=0)
save(mod, file='~/mortalityblob/glmnet/spei03.tc.afr.aez.Rdata')
rm(mm)

system('~/telegram.sh "Done with TerraClimate SPEI03"')

#SPEI24 TerraClim
mm <- sparse.model.matrix(formula(paste0("mortality ~ age + mother_years_ed + mothers_age + aez*",
                          piece.formula("spei.tc.24", c(-1, 0, 1)), 
                         " + birth_order + male + cc*year")), data=data)
mod <- glmnet(mm, data$mortality, family='binomial', alpha=0, lambda=0)
save(mod, file='~/mortalityblob/glmnet/spei24.tc.afr.aez.Rdata')
rm(mm)

system('~/telegram.sh "Done with TerraClimate SPEI24"')

#SPEI03 CHIRPS
mm <- sparse.model.matrix(formula(paste0("mortality ~ age + mother_years_ed + mothers_age + aez*",
                          piece.formula("spei.ch.3", c(-1, 0, 1)), 
                         " + birth_order + male + cc*year")), data=data)
mod <- glmnet(mm, data$mortality, family='binomial', alpha=0, lambda=0)
save(mod, file='~/mortalityblob/glmnet/spei03.ch.afr.aez.Rdata')
rm(mm)

system('~/telegram.sh "Done with CHRIPS SPEI03"')

#SPEI24 CHIRPS
mm <- sparse.model.matrix(formula(paste0("mortality ~ age + mother_years_ed + mothers_age + aez*",
                          piece.formula("spei.ch.24", c(-1, 0, 1)), 
                         " + birth_order + male + cc*year")), data=data)
mod <- glmnet(mm, data$mortality, family='binomial', alpha=0, lambda=0)
save(mod, file='~/mortalityblob/glmnet/spei24.ch.afr.aez.Rdata')
rm(mm)

system('~/telegram.sh "Done with CHRIPS SPEI24"')

#SPEI0324 TerraClim
mm <- sparse.model.matrix(formula(paste0("mortality ~ age + mother_years_ed + mothers_age + aez*",
                          piece.formula("spei.tc.3", c(-1, 0, 1)), 
                          " + aez*",
                          piece.formula("spei.tc.24", c(-1, 0, 1)), 
                         " + birth_order + male + cc*year")), data=data)
mod <- glmnet(mm, data$mortality, family='binomial', alpha=0, lambda=0)
save(mod, file='~/mortalityblob/glmnet/spei0324.tc.afr.aez.Rdata')
rm(mm)

system('~/telegram.sh "Done with TerraClimate SPEI0324"')

#SPEI0324 CHIRPS
mm <- sparse.model.matrix(formula(paste0("mortality ~ age + mother_years_ed + mothers_age + aez*",
                          piece.formula("spei.ch.3", c(-1, 0, 1)), 
                          " + aez*",
                          piece.formula("spei.ch.24", c(-1, 0, 1)), 
                         " + birth_order + male + cc*year")), data=data)
mod <- glmnet(mm, data$mortality, family='binomial', alpha=0, lambda=0)
save(mod, file='~/mortalityblob/glmnet/spei0324.ch.afr.aez.Rdata')
rm(mm)

system('~/telegram.sh "Done with CHRIPS SPEI0324"')
