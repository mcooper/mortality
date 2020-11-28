library(data.table)
library(glmnet)
library(Matrix)
library(dplyr)
library(doParallel)

registerDoParallel(32)

setwd('~/mortalityblob/mortality-dhs/')

data <- fread('Mortality-combined.csv') %>%
  select(-matches('tc|1$|2$|\\.6$|12$|24$|48$')) %>%
  filter(!is.infinite(spei.mm.3), !is.infinite(spei.mm.36)) %>%
  na.omit

data$spei.mm.3.q <- cut(data$spei.mm.3, c(-100, -1, 0, 1, 100))
data$spei.mm.36.q <- cut(data$spei.mm.36, c(-100, -1, 0, 1, 100))

mm <- sparse.model.matrix(mortality ~ age + mother_years_ed + mothers_age + 
                          spei.mm.3*spei.mm.3.q + 
                         birth_order + male + date + code, data=data)
mod <- glmnet(mm, data$mortality, family='binomial', alpha=0, lambda=0)
save(mod, file='~/mortalityblob/glmnet/spei3.Rdata')

mm <- sparse.model.matrix(mortality ~ age + mother_years_ed + mothers_age + 
                          spei.mm.36*spei.mm.36.q + 
                         birth_order + male + date + code, data=data)
mod <- glmnet(mm, data$mortality, family='binomial', alpha=0, lambda=0)
save(mod, file='~/mortalityblob/glmnet/spei36.Rdata')
