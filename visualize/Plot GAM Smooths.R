library(tidyverse)
library(ggthemes)
library(mgcv)

#Read in models
load('/home/mattcoop/mortalityblob/mod-results/mod3.Rdata')
load('/home/mattcoop/mortalityblob/mod-results/mod6.Rdata')
load('/home/mattcoop/mortalityblob/mod-results/mod12.Rdata')
load('/home/mattcoop/mortalityblob/mod-results/mod24.Rdata')
load('/home/mattcoop/mortalityblob/mod-results/mod36.Rdata')
load('/home/mattcoop/mortalityblob/mod-results/mod48.Rdata')

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
  select(-spei3, -spei6, -spei12, -spei24, -spei36, -spei48,
         -age, -mother_years_ed, -mothers_age, -birth_order, -male) %>% 
  gather(window, value, -spei) %>%
  mutate(parameter=ifelse(grepl('se', window), 'se', 'pred'),
         window=gsub('se|pred', '', window)) %>% 
  spread(parameter, value) %>%
  mutate(maxpred=exp(pred + se*2),
         maxpred=maxpred/(1+maxpred),
         minpred=exp(pred - se*2),
         minpred=minpred/(1+minpred),
         pred=exp(pred),
         pred=pred/(1+pred))

write.csv(plotdf, 'Mortality-smooths-plotdf.csv', row.names=F)

plotdf <- read.csv('/home/matt/mortalityblob/dhs/Mortality-smooths-plotdf.csv')

plotdf$maxpred[plotdf$maxpred > 0.0045] <- 0.0045
plotdf$minpred[plotdf$minpred < 0.0028] <- 0.0028

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

pal <- gg_color_hue(5)

plotdf$window <- factor(plotdf$window, levels(plotdf$window)[c(3,6,1,2,4,5)])

make_gg <- function(data, labels, colvals, fillvals, ltyvals, alphvals){
  ggplot(data) + 
    geom_ribbon(aes(x=spei, ymin=minpred, ymax=maxpred, fill=window), alpha=0.1) + 
    geom_line(aes(x=spei, y=pred, color=window, linetype=window, alpha=window), size=1) + 
    geom_hline(aes(yintercept=0.002983045), linetype=2) + 
    labs(title='Probability of Death in A Given Month', 
         subtitle = 'Based on 138 million child-months from 160 surveys across 59 countries',
         caption = "For a 1 year old female child, born 3rd to an 18-year-old mother with primary school eduction",
         color="Span", fill="Span", linetype='Span', alpha='Span') + 
    scale_color_manual(labels=labels, values=colvals) + 
    scale_fill_manual(labels=labels, values=fillvals) + 
    scale_linetype_manual(labels=labels, values = ltyvals) + 
    scale_alpha_manual(labels=labels, values = alphvals) + 
    scale_x_continuous(expand=c(0,0)) + 
    scale_y_continuous(expand=c(0,0), limits = c(0.0028, 0.0045)) +
    theme_few() + 
    ylab('Probability of Death') + 
    xlab('SPEI') +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) + 
    theme(legend.position = c(0.9, 0.95),
          legend.justification = c(0.5, 1))
}

make_gg(data=plotdf %>% filter(spei < 3 & spei > -3 & window %in% c('spei24')),
        labels=c(spei24='24 Months'),
        colvals=c(spei24=pal[5]),
        fillvals=c(spei24=pal[5]),
        ltyvals=c(spei24=2),
        alphvals=c(spei24=1))
ggsave('~/Mortality_Smooths_a.png', width=9, height=7)

make_gg(data=plotdf %>% filter(spei < 3 & spei > -3 & window %in% c('spei24', 'spei12')),
        labels=c(spei24='24 Months', spei12='12 Months'),
        colvals=c(spei24='#111111', spei12=pal[4]),
        fillvals=c(spei24='#FFFFFF', spei12=pal[4]),
        ltyvals=c(spei24=2, spei12=3),
        alphvals=c(spei24=0.25, spei12=1))
ggsave('~/Mortality_Smooths_b.png', width=9, height=7)

make_gg(data=plotdf %>% filter(spei < 3 & spei > -3 & window %in% c('spei24', 'spei12', 'spei6')),
        labels=c(spei24='24 Months', spei12='12 Months', spei6='6 Months'),
        colvals=c(spei24='#111111', spei12='#111111', spei6=pal[3]),
        fillvals=c(spei24='#FFFFFF', spei12='#FFFFFF', spei6=pal[3]),
        ltyvals=c(spei24=2, spei12=3, spei6=4),
        alphvals=c(spei24=0.25, spei12=0.25, spei6=1))
ggsave('~/Mortality_Smooths_c.png', width=9, height=7)

make_gg(data=plotdf %>% filter(spei < 3 & spei > -3 & window %in% c('spei24', 'spei12', 'spei6', 'spei3')),
        labels=c(spei24='24 Months', spei12='12 Months', spei6='6 Months', spei3='3 Months'),
        colvals=c(spei24='#111111', spei12='#111111', spei6='#111111', spei3=pal[2]),
        fillvals=c(spei24='#FFFFFF', spei12='#FFFFFF', spei6='#FFFFFF', spei3=pal[2]),
        ltyvals=c(spei24=2, spei12=3, spei6=4, spei3=5),
        alphvals=c(spei24=0.25, spei12=0.25, spei6=0.25, spei3=1))
ggsave('~/Mortality_Smooths_d.png', width=9, height=7)

make_gg(data=plotdf %>% filter(spei < 3 & spei > -3 & window %in% c('spei24', 'spei12', 'spei6', 'spei3', 'spei36')),
        labels=c(spei24='24 Months', spei12='12 Months', spei6='6 Months', spei3='3 Months', spei36='36 Months'),
        colvals=c(spei24='#111111', spei12='#111111', spei6='#111111', spei3='#111111', spei36=pal[1]),
        fillvals=c(spei24='#FFFFFF', spei12='#FFFFFF', spei6='#FFFFFF', spei3='#FFFFFF', spei36=pal[1]),
        ltyvals=c(spei24=2, spei12=3, spei6=4, spei3=5, spei36=6),
        alphvals=c(spei24=0.25, spei12=0.25, spei6=0.25, spei3=0.25, spei36=1))
ggsave('~/Mortality_Smooths_e.png', width=9, height=7)

make_gg(data=plotdf %>% filter(spei < 3 & spei > -3 & window %in% c('spei24', 'spei12', 'spei6', 'spei3', 'spei36')),
        labels=c(spei24='24 Months', spei12='12 Months', spei6='6 Months', spei3='3 Months', spei36='36 Months'),
        colvals=c(spei24=pal[5], spei12=pal[4], spei6=pal[3], spei3=pal[2], spei36=pal[1]),
        fillvals=c(spei24=pal[5], spei12=pal[4], spei6=pal[3], spei3=pal[2], spei36=pal[1]),
        ltyvals=c(spei24=2, spei12=3, spei6=4, spei3=5, spei36=6),
        alphvals=c(spei24=1, spei12=1, spei6=1, spei3=1, spei36=1))
ggsave('~/Mortality_Smooths_f.png', width=9, height=7)
