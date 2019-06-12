library(tidyverse)
library(mgcv)
library(parallel)
library(ggthemes)

data <- read.csv('/home/mattcoop/child-months/Mortality-combined.csv')

data$mortality <- !data$alive

data <- data %>%
  select(mortality, age, mother_years_ed, mothers_age, birth_order, male, spei12, spei24, spei36) %>%
  na.omit %>%
  filter(!is.infinite(spei12) & !is.infinite(spei24) & !is.infinite(spei36))

mod12 <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei12), 
             family='binomial', data=data)

save(mod12, file='~/mod-results/mod12.Rdata')

mod24 <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei24), 
             family='binomial', data=data)

save(mod24, file='~/mod-results/mod24.Rdata')

mod36 <- bam(mortality ~ age + mother_years_ed + mothers_age + birth_order + male + s(spei36), 
             family='binomial', data=data)

save(mod36, file='~/mod-results/mod36.Rdata')

##Make Prediction Plots

preddf <- data.frame(age=12, mother_years_ed=5, mothers_age=18, birth_order=3, male=FALSE, 
                     spei12=seq(-4, 4, 0.1), spei24=seq(-4, 4, 0.1), spei36=seq(-4, 4, 0.1))

preddf[ , c('spei12pred', 'spei12se')] <- predict(object=mod12, preddf, se=TRUE)
preddf[ , c('spei24pred', 'spei24se')] <- predict(object=mod24, preddf, se=TRUE)
preddf[ , c('spei36pred', 'spei36se')] <- predict(object=mod36, preddf, se=TRUE)

plotdf <- preddf %>%
  mutate(spei=spei24) %>%
  select(spei, spei12pred, spei12se, spei24pred, spei24se, spei36pred, spei36se) %>%
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


