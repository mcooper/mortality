library(tidyverse)
library(ggthemes)
library(mgcv)

#Read in models
load('~/mod-results/modtemp3.Rdata')

##Make Prediction Plots
preddf <- data.frame(age=12, mother_years_ed=5, mothers_age=18, birth_order=3, male=FALSE, 
                     temp3=seq(0, 43, 0.5))

preddf[ , c('pred', 'se')] <- predict(object=modtemp3, preddf, se=TRUE)

plotdf <- preddf %>%
  mutate(maxpred=exp(pred + se*2),
         maxpred=maxpred/(1+maxpred),
         minpred=exp(pred - se*2),
         minpred=minpred/(1+minpred),
         pred=exp(pred),
         pred=pred/(1+pred))

ggplot(plotdf) + 
  geom_ribbon(aes(x=temp3, ymin=minpred, ymax=maxpred), alpha=0.1) + 
  geom_line(aes(x=temp3, y=pred), size=1) + 
  geom_hline(aes(yintercept=0.002983045), linetype=2)

ggsave('~/Mortality_3Month_Temp_Smooths.png', width=9, height=7)


