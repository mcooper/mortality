library(data.table)
library(tidyverse)
library(sgd)

setwd('~/mortalityblob')

dat <- fread('mortality-dhs/Mortality-combined.csv')

sel <- dat %>%
  filter(grepl('^ML', code))

sel <- sel %>%
  filter(!is.na(spei.mm.36) & !is.infinite(spei.mm.36),
         !is.na(spei.mm.3) & !is.infinite(spei.mm.3))

mm <- model.matrix(mortality ~ age + mother_years_ed + mothers_age + male + spei.tc.3 + code,
           data=sel)

mod <- sgd(y=sel$mortality, x=mm, model="glm", model.control=list(family='binomial'))

#Keep getting errors of 
# error: Mat::init(): requested size is too large
# Error in run(dataset, model.control, sgd.control) :
# Mat::init(): requested size is too large
