library(tidyverse)
library(glmnet)

setwd('~/mortalityblob/glmnet')

load('spei0324.afr.aez.hdi.lasso.Rdata')

preddat <- data.frame(spei.tc.3 = c(seq(-2.5, 2.5, 0.1), rep(0, 51)),
                      spei.tc.24 = c(rep(0, 51), seq(-2.5, 2.5, 0.1)))

preddat <- bind_rows(preddat %>% mutate(aez='Mediterranean'),
                     preddat %>% mutate(aez='Desert'),
                     preddat %>% mutate(aez='Forest'),
                     preddat %>% mutate(aez='Savanna'),
                     preddat %>% mutate(aez='Highlands'),
                     preddat %>% mutate(aez='SemiForest'))

preddat <- bind_rows(preddat %>% mutate(shdi=0.65),
                     preddat %>% mutate(shdi=0.6),
                     preddat %>% mutate(shdi=0.55),
                     preddat %>% mutate(shdi=0.5),
                     preddat %>% mutate(shdi=0.45),
                     preddat %>% mutate(shdi=0.4),
                     preddat %>% mutate(shdi=0.35))

preddat$mortality <- 0
preddat$incindex <- 0.5
preddat$edindex <- 0.5
preddat$healthindex <- 0.5

mm <- sparse.model.matrix(mortality ~ 
  aez*shdi*spei.tc.3 + aez*shdi*I(pmax(spei.tc.3 + 1, 0)) + 
  aez*shdi*I(pmax(spei.tc.3 - 0, 0)) + aez*shdi*I(pmax(spei.tc.3 - 1, 0)) + 
  aez*shdi*spei.tc.24 + aez*shdi*I(pmax(spei.tc.24 + 1, 0)) + 
  aez*shdi*I(pmax(spei.tc.24 - 0, 0)) + aez*shdi*I(pmax(spei.tc.24 - 1, 0)) + 
  healthindex + incindex + edindex, data=preddat)

p <- predict(mod, mm)[ , 1]

preddat$mortality <- predict(mod, mm)[ , which.min(mod$lambda)] 

preddat$shdi <- factor(preddat$shdi)

ggplot(preddat %>% filter(spei.tc.24 == 0)) +
  geom_line(aes(x=spei.tc.3, y=mortality, color=shdi)) + 
  facet_wrap(aez ~ .)
ggsave('~/mortality/res/SPEI03_Segment_HDI_AEZ.png')

ggplot(preddat %>% filter(spei.tc.3 == 0)) +
  geom_line(aes(x=spei.tc.24, y=mortality, color=shdi)) + 
  facet_wrap(aez ~ .)
ggsave('~/mortality/res/SPEI24_Segment_HDI_AEZ.png')

######################################
# No AEZ re-scaled and not re-scaled

load('spei0324.afr.noaez.hdi.Rdata')

preddat <- data.frame(spei.tc.3 = c(seq(-2.5, 2.5, 0.1), rep(0, 51)),
                      spei.tc.24 = c(rep(0, 51), seq(-2.5, 2.5, 0.1)))

preddat <- bind_rows(preddat %>% mutate(shdi=0.9),
                     preddat %>% mutate(shdi=0.8),
                     preddat %>% mutate(shdi=0.7),
                     preddat %>% mutate(shdi=0.6),
                     preddat %>% mutate(shdi=0.5),
                     preddat %>% mutate(shdi=0.4),
                     preddat %>% mutate(shdi=0.3),
                     preddat %>% mutate(shdi=0.2),
                     preddat %>% mutate(shdi=0.1))

preddat$mortality <- 0
preddat$incindex <- 0.5
preddat$edindex <- 0.5
preddat$healthindex <- 0.5

preddat$shdi <- 1 - preddat$shdi

mm <- sparse.model.matrix(mortality ~ 
  shdi*spei.tc.3 + shdi*I(pmax(spei.tc.3 + 1, 0)) + 
  shdi*I(pmax(spei.tc.3 - 0, 0)) + shdi*I(pmax(spei.tc.3 - 1, 0)) + 
  shdi*spei.tc.24 + shdi*I(pmax(spei.tc.24 + 1, 0)) + 
  shdi*I(pmax(spei.tc.24 - 0, 0)) + shdi*I(pmax(spei.tc.24 - 1, 0)) + 
  healthindex + incindex + edindex, data=preddat)


preddat$mortality_inv <- predict(mod, mm)[ , which.min(mod$lambda)] 

preddat$shdi <- factor(preddat$shdi)

ggplot(preddat %>% filter(spei.tc.24 == 0)) +
  geom_line(aes(x=spei.tc.3, y=mortality_inv, color=shdi))

ggplot(preddat %>% filter(spei.tc.3 == 0)) +
  geom_line(aes(x=spei.tc.24, y=mortality_inv, color=shdi))
