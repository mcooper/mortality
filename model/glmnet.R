library(data.table)
library(glmnet)
library(Matrix)
library(dplyr)
library(doParallel)

registerDoParallel(8)

setwd('~/mortalityblob/mortality-dhs/')

piece.formula <- function(var.name, knots) {
  formula.sign <- rep(" - ", length(knots))
  formula.sign[knots < 0] <- " + "
  paste(var.name, "+",
        paste("I(pmax(", var.name, formula.sign, abs(knots), ", 0))",
              collapse = " + ", sep=""))
}

data <- fread('Mortality-combined.csv') %>%
  filter(!is.infinite(spei.tc.3), !is.infinite(spei.tc.36))

mm <- sparse.model.matrix(formula(paste0("mortality ~ age + mother_years_ed + mothers_age + ",
                          piece.formula("spei.tc.3", c(-1, 0, 1)), 
                         " + birth_order + male + date + code")), data=data)
mod <- glmnet(mm, data$mortality, family='binomial', alpha=0, lambda=0)
save(mod, file='~/mortalityblob/glmnet/spei3.Rdata')

mm <- sparse.model.matrix(formula(paste0("mortality ~ age + mother_years_ed + mothers_age + ",
                          piece.formula("spei.mm.36", c(-1, 0, 1)),
                         " + birth_order + male + date + code", data=data)
mod <- glmnet(mm, data$mortality, family='binomial', alpha=0, lambda=0)
save(mod, file='~/mortalityblob/glmnet/spei36.Rdata')
