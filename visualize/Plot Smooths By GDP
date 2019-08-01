library(tidyverse)
library(ggthemes)
library(mgcv)

#Read in models
load('/home/mattcoop/mortalityblob/mod-results/mod_gam_spei_gdp.Rdata')

##Make Prediction Plots
preddf <- merge(data.frame(age=12, mother_years_ed=5, mothers_age=18, birth_order=3, male=FALSE, 
                     spei36=seq(-7, 7, 0.1), spei3=0),
                data.frame(GDP=c(350000, 60000, 40000, 20000, 10000, 5000, 3000, 1000, 500, 100)))

preddf$GDP <- log(60000) - log(preddf$GDP)

preddf <- bind_rows(preddf, 
                    preddf %>% rename(spei36=spei3, spei3=spei36))

preddf[ , c('pred', 'se')] <- predict(object=mod, preddf, se=TRUE)


preddf <- preddf %>%
  mutate(maxpred=exp(pred + se*2),
         maxpred=maxpred/(1+maxpred),
         minpred=exp(pred - se*2),
         minpred=minpred/(1+minpred),
         pred=exp(pred),
         pred=pred/(1+pred)) %>%
  mutate(GDP_PerCap = as.factor(round(exp(11.0021 - GDP))))

spei36df <- preddf %>% 
  filter(spei36 != 0)

spei3df <- preddf %>%  
  filter(spei3 != 0)

ggplot(spei36df) +
  geom_ribbon(aes(x=spei36, ymin=minpred, ymax=maxpred, fill=GDP_PerCap), alpha=0.1) +
  geom_line(aes(x=spei36, y=pred, color=GDP_PerCap), size=1)

ggplot(spei3df) +
  geom_ribbon(aes(x=spei3, ymin=minpred, ymax=maxpred, fill=GDP_PerCap), alpha=0.1) +
  geom_line(aes(x=spei3, y=pred, color=GDP_PerCap), size=1)
