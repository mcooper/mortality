library(tidyverse)
library(ggthemes)

#Read in models
load('~/modresults/mod3.Rdata')
load('~/modresults/mod6.Rdata')
load('~/modresults/mod12.Rdata')
load('~/modresults/mod24.Rdata')
load('~/modresults/mod36.Rdata')
load('~/modresults/mod48.Rdata')

##Make Prediction Plots
preddf <- data.frame(age=12, mother_years_ed=5, mothers_age=18, birth_order=3, male=FALSE, 
                     spei3=seq(-4, 4, 0.1), spei6=seq(-4, 4, 0.1), spei12=seq(-4, 4, 0.1), 
                     spei24=seq(-4, 4, 0.1), spei36=seq(-4, 4, 0.1), spei48=seq(-4, 4, 0.1))

preddf[ , c('spei3pred', 'spei3se')] <- predict(object=mod3, preddf, se=TRUE)
preddf[ , c('spei6pred', 'spei6se')] <- predict(object=mod6, preddf, se=TRUE)
preddf[ , c('spei12pred', 'spei12se')] <- predict(object=mod12, preddf, se=TRUE)
preddf[ , c('spei24pred', 'spei24se')] <- predict(object=mod24, preddf, se=TRUE)
preddf[ , c('spei36pred', 'spei36se')] <- predict(object=mod36, preddf, se=TRUE)
preddf[ , c('spei48pred', 'spei48se')] <- predict(object=mod48, preddf, se=TRUE)

plotdf <- preddf %>%
  mutate(spei=spei24) %>%
  select(-spei3, -spei6, -spei12, -spei24, -spei36, -spei48) %>%
  gather(window, value, -spei) %>%
  mutate(parameter=ifelse(grepl('se', window), 'se', 'pred'),
         window=substr(window, 1, 6)) %>%
  spread(parameter, value) %>%
  mutate(maxpred=exp(pred + se*2),
         maxpred=maxpred/(1+maxpred),
         minpred=exp(pred - se*2),
         minpred=minpred/(1+minpred),
         pred=exp(pred),
         pred=pred/(1+pred))

ggplot(plotdf) + 
  geom_ribbon(aes(x=spei, ymin=minpred, ymax=maxpred, fill=window), alpha=0.1) + 
  geom_line(aes(x=spei, y=pred, color=window), size=1) + 
  geom_hline(aes(yintercept=0.002983045), linetype=2) + 
  labs(title='Probability of Death in A Given Month', 
       subtitle = 'Based on 138 million child-months from 160 surveys across 59 countries',
       caption = "For a 1 year old female child, born 3rd to an 18-year-old mother with primary school eduction",
       color="Span", fill="Span") + 
  scale_color_discrete(labels=c('12 Months', '24 Months', '36 Months')) + 
  scale_fill_discrete(labels=c('12 Months', '24 Months', '36 Months')) + 
  scale_x_continuous(expand=c(0,0)) + 
  theme_few() + 
  ylab('Probability of Death') + 
  xlab('SPEI') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  theme(legend.position = c(0.9, 0.9))

ggsave('~/Mortality_Smooths.png', width=9, height=7)


