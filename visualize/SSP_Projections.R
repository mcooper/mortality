library(tidyverse)
library(lubridate)
library(zoo)
library(ggplot2)
library(ggthemes)

setwd('/home/mattcoop/climatedisk2/climatedisk/')

#Based on Figure 2 Here: https://www.geosci-model-dev.net/9/3461/2016/gmd-9-3461-2016.pdf
ssp_rcp <- data.frame(ssp=c('ssp1', 'ssp2', 'ssp3', 'ssp4', 'ssp5'),
                      rcp=c('rcp26', 'rcp45', 'rcp60', 'rcp60', 'rcp85'),
                      stringsAsFactors = F)

alldf <- data.frame()
for (i in 1:nrow(ssp_rcp)){
  ssp <- ssp_rcp$ssp[i]
  rcp <- ssp_rcp$rcp[i]
  
  load(paste0('pred/', ssp, "_", rcp, "_pred.Rdata"))
  load(paste0('pop/', toupper(ssp), '_POP.Rdata'))
  load(paste0('pop/', toupper(ssp), '_U5FRAC.RData'))
  
  final <- exp(final)
  final <- final/(1 + final)
  
  u5pop <- pop*u5frac
  mort <- final*u5pop
  
  monthly_total <- apply(mort, MARGIN=c(3), FUN=sum, na.rm=T)
  u5pop_total <- apply(u5pop, MARGIN=c(3), FUN=sum, na.rm=T)
  
  df <- data.frame(monthly_total,
                   u5pop_total,
                   date=seq(ymd('2020-01-01'), ymd('2099-12-01'), by='month')) %>%
    mutate(year=year(date)) %>%
    group_by(year) %>%
    summarize(u5pop_total=unique(u5pop_total),
              annual_total=sum(monthly_total)) %>%
    mutate(FiveYrTotal=rollapply(annual_total, width=5, FUN=sum, align='right', fill=NA),
           U5MR=(FiveYrTotal/u5pop_total)*1000,
           SSP=ssp)
  
  alldf <- bind_rows(alldf, df)
  
  print(ssp)
}

plotdf <- alldf %>%
  select(year, SSP, `Global Under-5 Mortality Rate`=U5MR, `Global Total Under-5 Deaths`=annual_total) %>%
  gather(Statistic, Value, -year, -SSP) %>%
  mutate(Statistic=relevel(as.factor(Statistic), ref="Global Under-5 Mortality Rate"))

options(scipen=100)

ssp_rcp <- data.frame(ssp=c('ssp1', 'ssp2', 'ssp3', 'ssp4', 'ssp5'),
                      rcp=c('rcp26', 'rcp45', 'rcp60', 'rcp60', 'rcp85'),

ggplot(plotdf) + 
  geom_line(aes(x=year, y=Value, color=SSP)) + 
  facet_wrap(. ~ Statistic, scales='free_y') + 
  theme_few() + 
  labs(x='Year', y='') + 
  scale_color_discrete(labels = c('ssp1'='SSP1 - RCP2.6 "Sustainability"', 
                                  'ssp2'='SSP2 - RCP4.5 "Middle of the Road"', 
                                  'ssp3'='SSP3 - RCP6.0 "Regional Rivalry"', 
                                  'ssp4'='SSP4 - RCP6.0 "Inequality"', 
                                  'ssp5'='SSP5 - RCP8.5 "Fossil-Fueled Development"')) + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.direction = "vertical")

ggsave('~/SSP_Projections.png', width=10, height=6)


