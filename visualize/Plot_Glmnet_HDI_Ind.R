library(tidyverse)
library(glmnet)

setwd('~/mortalityblob/glmnet')


preddat <- data.frame(spei = seq(-2.5, 2.5, 0.1))

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

for (i in c(1, 2, 3, 6, 12, 24, 36, 48)){
  load(paste0('spei', i, '.afr.hdi.Rdata'))
  print(i)

  preddat[ , paste0('spei.tc.', i)] <- preddat$spei

  form <- paste0("mortality ~ shdi*spei.tc.", i, " + shdi*I(pmax(spei.tc.", i, " + 1, 0)) + shdi*I(pmax(spei.tc.", i, " - 0, 0)) + shdi*I(pmax(spei.tc.", i, " - 1, 0)) + healthindex + incindex + edindex")
  mm <- sparse.model.matrix(as.formula(form), data=preddat)

  preddat[ , paste0('mort.tc.', i)] <- predict(mod, mm)[ , which.min(mod$lambda)] 

}

for (i in c(3, 6, 12, 24, 36)){
  load(paste0('spei', i, '.afr.hdi.ch.Rdata'))
  print(i)

  preddat[ , paste0('spei.ch.', i)] <- preddat$spei

  form <- paste0("mortality ~ shdi*spei.ch.", i, " + shdi*I(pmax(spei.ch.", i, " + 1, 0)) + shdi*I(pmax(spei.ch.", i, " - 0, 0)) + shdi*I(pmax(spei.ch.", i, " - 1, 0)) + healthindex + incindex + edindex")
  mm <- sparse.model.matrix(as.formula(form), data=preddat)

  preddat[ , paste0('mort.ch.', i)] <- predict(mod, mm)[ , which.min(mod$lambda)] 

}

plotdat <- preddat %>%
  dplyr::select(spei, shdi, matches('mort....\\d')) %>%
  gather(window, value, -spei, -shdi) %>%
  mutate(dataset = substr(window, 6, 7),
         window = as.numeric(substr(window, 9, nchar(window))))

ggplot(plotdat) + 
  geom_line(aes(x=spei, y=value, color=factor(shdi))) + 
  facet_grid(dataset ~ window)
ggsave('~/mortality/res/SPEI_Segment_HDI.png')





