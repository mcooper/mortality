library(mgcv)
library(tidyverse)

#Decision to make:
#1. Datasets to use
#2. Curve fitting methods (or just linear?) (REML vs GCV)
#3. How to controll for spatial and temporal autocorrelation.
#4. What to subset to (age, recentness, birth certificate? breadfeeding?)


setwd('~/mortalityblob/mod-results/')

mods <- expand.grid(list(path=c('terraclimate', 'chirps-terraclimate'),
                         out=c('spei3', 'spei36'),
                         scope=c('full', '2yo', '5yb', '5yb2yo')))
mods$mod <- paste0(mods$path, '/', mods$out, '_', mods$scope, '.Rdata')


all <- data.frame()
for (i in 1:nrow(mods)){
  load(mods$mod[i])

  m <- plot(mod)[[1]]

  s <- data.frame(m$x, m$fit, m$se)

  s$data <- substr(mods$path[i], 1, 6)
  s$out <- mods$out[i]
  s$scope <- mods$scope[i]

  all <- bind_rows(all, s)
}

all <- all %>%
  filter(m.x >= -3 & m.x <= 3)

#Different SPEI Values
ggplot(all %>% filter(scope=='full')) + 
  geom_line(aes(x=m.x, y=m.fit)) + 
  facet_grid(data ~ out)
ggsave('chirps_tc_effectonmortality.png')

ggplot(all %>% filter(scope=='2yo')) + 
  geom_line(aes(x=m.x, y=m.fit)) + 
  facet_grid(data ~ out)

ggplot(all %>% filter(scope=='5yb')) + 
  geom_line(aes(x=m.x, y=m.fit)) + 
  facet_grid(data ~ out)

ggplot(all %>% filter(scope=='5yb2yo')) + 
  geom_line(aes(x=m.x, y=m.fit)) + 
  facet_grid(data ~ out)


#Different temp values
mods <- c('terraclimate/tmax_full.Rdata', 'terraclimate/tmax_2yo.Rdata', 'terraclimate/tmax_5yb2yo.Rdata', 'terraclimate/tmax_5yb.Rdata', 'mod_temp1monthZ-subsample.Rdata', 'mod_temp1monthZ_recent.Rdata')

temps <- data.frame()
for (i in 1:length(mods)){
  load(mods[i])

  m <- plot(mod)[[1]]

  s <- data.frame(m$x, m$fit, m$se)

  s$mod <- mods[i]

  temps <- bind_rows(temps, s)
}

temps <- temps %>%
  filter(m.x >= -3 & m.x <= 3) %>%
  mutate(data = ifelse(grepl('terra', mod), 'terrclim', 'princeton'),
         scale = case_when(grepl('recent|5yb.R', mod) ~ 'recent',
                           grepl('subsam|full', mod) ~ 'full',
                           grepl('_2yo', mod) ~ '2yo',
                           TRUE ~ '2yo_recent'))


ggplot(temps %>% filter() + 
  geom_line(aes(x=m.x, y=m.fit)) + 
  facet_grid(scale ~ data)


#Old SPEI models
mods <- c('mod3REML.Rdata',
          'mod36REML.Rdata')

spei <- data.frame()
for (i in 1:length(mods)){
  load(mods[i])

  m <- plot(mod36REML)[[1]]

  s <- data.frame(m$x, m$fit, m$se)

  s$mod <- mods[i]

  spei <- bind_rows(spei, s)
}

spei <- spei %>%
  filter(m.x >= -3 & m.x <= 3)


ggplot(spei) + 
  geom_line(aes(x=m.x, y=m.fit)) + 
  facet_grid(mod ~ .)

