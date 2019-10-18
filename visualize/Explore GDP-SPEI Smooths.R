library(tidyverse)
library(plotly)
library(mgcv)
library(ggplot2)

load('G://My Drive/Mortality/Mods/mod12gdpREML.Rdata')

preddf <- expand.grid(spei12=seq(-3, 3, 0.1),
                      GDP=seq(0, 5.5, 0.1))

preddf$age <- 12
preddf$mother_years_ed <- 18
preddf$birth_order <- 3
preddf$male <- FALSE
preddf$mothers_age <- 18
preddf$offset <- -3.828974

preddf$pred <- predict(mod12gdpREML, preddf, type='link') + mod12gdpREML$model$`offset(offset)`[1]

preddf$pred_prob <- exp(preddf$pred)/(1 + exp(preddf$pred))

GDP2 = log(30000) - log(GDP)
GDP2 - log(30000) = -log(GDP)
log(30000) - GDP2 = log(GDP)
exp(log(30000) - GDP2) = GDP


ggplot(preddf) + 
  geom_tile(aes(x=spei12, y=GDP, fill=pred_prob)) + 
  scale_fill_viridis_c() + 
  scale_y_continuous(labels = function(x){exp(log(30000) - x)}, 
                     breaks = seq(0, 5.5, 0.5))

predmat <- preddf %>%
  select(spei12, GDP, pred_prob) %>%
  spread(GDP, pred_prob) %>%
  select(-spei12) %>%
  as.matrix

plot_ly(z = ~ predmat) %>% add_surface()









