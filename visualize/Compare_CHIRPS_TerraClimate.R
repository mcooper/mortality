library(tidyverse)
library(data.table)

setwd('~/mortalityblob/mortality-dhs')

chirps <- read_csv('Mortality_SPI_Temps.csv')
tc <- read_csv('Mortality_SPI_Temps_TerraClimate.csv')

int <- intersect(names(chirps), names(tc))
int <- int[!int %in% c('date_cmc', 'code')]

names(chirps)[names(chirps) %in% int] <- paste0('chirps_', names(chirps)[names(chirps) %in% int])
names(tc)[names(tc) %in% int] <- paste0('tc_', names(tc)[names(tc) %in% int])

s <- chirps %>%
  sample_n(100000)

m <- merge(s %>%
             select(date_cmc, code, matches('chirps')),
           tc %>%
             select(date_cmc, code, matches('tc')),
           all.x=T, all.y=F)

ggplot(m) + 
  geom_bin2d(aes(x=chirps_spei3, y=tc_spei3), bins=200) + 
  theme_bw()
ggsave('~/spei3.png')

ggplot(m) + 
  geom_bin2d(aes(x=chirps_spei36, y=tc_spei36), bins=200) + 
  theme_bw()
ggsave('~/spei36.png')

m %>%
  select(tc_spei36, chirps_spei36) %>%
  na.omit %>%
  filter(!is.infinite(tc_spei36),
         !is.infinite(chirps_spei36)) %>%
  cor
