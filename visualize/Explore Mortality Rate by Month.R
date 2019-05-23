library(tidyverse)
library(ggplot2)

#This script calculates and plots the 5m0 mortality rate by month
#5m0 is distinct form 5q0
#5m0 is the number of deaths in children aged 0-5 divided by the number of births
#5q0 is the number of deaths in children aged 0-5 divided by the number of children that were alive aged 0-5

all <- read.csv('G://My Drive/DHS Processed/Mortality_raw.csv')

#Lets just look at mortality rate 5m0 over time by country
deathcount <- all %>%
  filter(b5_int == 0 & b7 < 60) %>%
  mutate(death_cmc = b3 + b7,
         country = substr(surveycode, 1, 2),
         death_cmc = ifelse(country=='NP', death_cmc - 681, death_cmc)) %>%
  group_by(country, v101_chr, death_cmc) %>%
  summarize(deaths=n()) %>%
  select(country, region=v101_chr, cmc=death_cmc, deaths)

birthcount <- all %>%
  filter(is.na(b7)) %>%
  mutate(country = substr(surveycode, 1, 2),
         b3 = ifelse(country=='NP', b3 - 681, b3)) %>%
  group_by(country, v101_chr, b3) %>%
  summarize(births=n()) %>%
  select(country, region=v101_chr, cmc=b3, births)

comb <- merge(deathcount, birthcount, all=T) %>%
  mutate(births=ifelse(is.na(births), 0, births),
         deaths=ifelse(is.na(deaths), 0, deaths))


#By Region
regional <- comb %>% 
  filter(births > 3) %>%
  mutate(U5MR=deaths/births)

ggplot(regional) + geom_line(aes(x=cmc, y=U5MR, color=region)) + 
  facet_grid(country ~ ., scales = "free") + 
  theme(legend.position = 'none')

ggsave('G://My Drive/Dissertation/Mortality/U5MR_Region_TS.png', height=45, width=10)

#By Country
national <- comb %>%
  group_by(country, cmc) %>%
  summarize(births=sum(births), deaths=sum(deaths)) %>%
  filter(births > 10) %>%
  mutate(U5MR=deaths/births)

ggplot(national) + geom_line(aes(x=cmc, y=U5MR, color=country)) + 
  facet_grid(country ~ ., scales = "free") + 
  theme(legend.position = 'none')

ggsave('G://My Drive/Dissertation/Mortality/U5MR_Country_TS.png', height=45, width=10)


