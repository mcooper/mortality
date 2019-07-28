library(tidyverse)
library(ggthemes)
library(mgcv)

#Read in models
load('/home/mattcoop/mortalityblob/mod-results/mod3factor.Rdata')
load('/home/mattcoop/mortalityblob/mod-results/mod36factor.Rdata')

##Make Prediction Plots
preddf <- data.frame(age=12, mother_years_ed=5, mothers_age=18, birth_order=3, male=FALSE, 
                     spei3=seq(-3, 3, 0.1), spei36=seq(-3, 3, 0.1))

preddf <- bind_rows(preddf %>% mutate(gdp="[ 134, 1848)"),
                    preddf %>% mutate(gdp="[1848, 4047)"),
                    preddf %>% mutate(gdp="[4047,12083]"))

preddf[ , c('spei3pred', 'spei3se')] <- predict(object=mod3factor, preddf, se=TRUE)
preddf[ , c('spei36pred', 'spei36se')] <- predict(object=mod36factor, preddf, se=TRUE)

plotdf <- preddf %>%
  mutate(spei=spei36) %>%
  select(-spei3, -spei36,
         -age, -mother_years_ed, -mothers_age, -birth_order, -male) %>% 
  gather(window, value, -spei, -gdp) %>%
  mutate(parameter=ifelse(grepl('se', window), 'se', 'pred'),
         window=gsub('se|pred', '', window)) %>% 
  spread(parameter, value) %>%
  mutate(maxpred=exp(pred + se*2),
         maxpred=maxpred/(1+maxpred),
         minpred=exp(pred - se*2),
         minpred=minpred/(1+minpred),
         pred=exp(pred),
         pred=pred/(1+pred))

#SPEI36
ggplot(plotdf %>% filter(window=='spei36')) + 
  geom_ribbon(aes(x=spei, ymin=minpred, ymax=maxpred, fill=gdp), alpha=0.1) + 
  geom_line(aes(x=spei, y=pred, color=gdp), size=1) + 
  geom_hline(aes(yintercept=0.002983045), linetype=2) + 
  labs(title='Probability of Death in A Given Month', 
       subtitle = 'Based on 138 million child-months from 160 surveys across 59 countries',
       caption = "For a 1 year old female child, born 3rd to an 18-year-old mother with primary school eduction",
       color="Span", fill="Span") + 
  #scale_color_discrete(labels=c('3 Months', '36 Months')) + 
  #scale_fill_discrete(labels=c('3 Months', '36 Months')) + 
  theme_few() + 
  ylab('Probability of Death') + 
  xlab('SPEI') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  theme(legend.position = c(0.9, 0.9))


#SPEI3
ggplot(plotdf %>% filter(window=='spei3')) + 
  geom_ribbon(aes(x=spei, ymin=minpred, ymax=maxpred, fill=gdp), alpha=0.1) + 
  geom_line(aes(x=spei, y=pred, color=gdp), size=1) + 
  geom_hline(aes(yintercept=0.002983045), linetype=2) + 
  labs(title='Probability of Death in A Given Month', 
       subtitle = 'Based on 138 million child-months from 160 surveys across 59 countries',
       caption = "For a 1 year old female child, born 3rd to an 18-year-old mother with primary school eduction",
       color="Span", fill="Span") + 
  #scale_color_discrete(labels=c('3 Months', '36 Months')) + 
  #scale_fill_discrete(labels=c('3 Months', '36 Months')) + 
  theme_few() + 
  ylab('Probability of Death') + 
  xlab('SPEI') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  theme(legend.position = c(0.9, 0.9))

