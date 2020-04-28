setwd('G://My Drive/SSPs')

library(tidyverse)
library(sp)
library(sf)
library(rnaturalearth)
library(zoo)
library(raster)
library(abind)
library(lubridate)

dat <- read.csv('SspDb_country_data_2013-06-12.csv', stringsAsFactors = F)

dat2 <- dat %>%
  filter((grepl('Female', VARIABLE) & grepl('Education', VARIABLE) & !grepl('Aged0|Aged10|Aged15|Aged20', VARIABLE)) & MODEL == "IIASA-WiC POP") %>%
  dplyr::select(-UNIT, -MODEL) %>%
  gather(YEAR, Value, -SCENARIO, -REGION, -VARIABLE) %>%
  mutate(Ed = ifelse(grepl('No Education', VARIABLE), 'No Primary', 'Primary Plus')) %>%
  group_by(SCENARIO, REGION, YEAR, Ed) %>%
  summarize(Value=sum(Value)) %>%
  na.omit %>%
  spread(Ed, Value) %>%
  ungroup %>%
  mutate(Perc_Primary = `Primary Plus`/(`Primary Plus` + `No Primary`),
         YEAR = gsub('X', '', YEAR))

meta <- dat2 %>%
  dplyr::select(SCENARIO, REGION) %>%
  unique %>%
  merge(data.frame(YEAR=seq(2000, 2100)))

dat3 <- merge(meta, dat2, all.x=T) %>%
  arrange(SCENARIO, REGION, YEAR) %>%
  group_by(SCENARIO, REGION) %>%
  mutate(Perc_Primary = na.approx(Perc_Primary, rule=2)) %>%
  dplyr::select(SCENARIO, YEAR, Perc_Primary, iso_a3=REGION)

cty <- ne_countries(scale = 110, type = "countries", continent = NULL,
                    country = NULL, geounit = NULL, sovereignty = NULL,
                    returnclass = c("sf"))

cty$SSP <- cty$iso_a3 %in% dat3$iso_a3

cty$iso_a3[cty$iso_a3 == "ESH"] <- "MAR"  #Western Sahara has Stats of Morocco
cty$iso_a3[cty$iso_a3 == "SSD"] <- "CAF"  #South Sudan has Stats of Cenral African Republic
cty$iso_a3[cty$iso_a3 == "FLK"] <- "GBR"  #Falkland Islands has Stats of Great Britain
cty$iso_a3[cty$iso_a3 == "ATF"] <- "FRA"  #French Southern Territories are France
cty$iso_a3[cty$iso_a3 == "TWN"] <- "CHN"  #Taiwan is same as China
cty$iso_a3[cty$sovereignt == 'Somaliland'] <- 'SOM'

cty <- as(cty, Class = "Spatial")

rast <- raster(xmx=180, xmn=-180, ymx=90, ymn=-90, nrow=360, ncol=720)
for (ssp in unique(dat3$SCENARIO)){
  print(ssp)
  
  years <- list()
  for (year in seq(2000, 2100)){
    print(year)
    
    sel <- dat3 %>% filter(YEAR==year & SCENARIO == ssp)
    
    cty_sel <- sp::merge(cty, sel)
    
    r <- rasterize(cty_sel, rast, field='Perc_Primary')
    a <- as.matrix(r, dim=c(720, 360))
    
    for (i in 1:12){
      years[[length(years) + 1]] <- a
    }
  }
  
  edu <- abind(years, along = 3)
  
  dimnames(edu)[[3]] <- as.character(seq(ymd('2000-01-01'), ymd('2100-12-31'), by='month'))
  
  edu <- edu[ , , as.character(seq(ymd('2020-01-01'), ymd('2099-12-31'), by='month'))]
  
  save(edu, file=paste0('Education/', substr(ssp, 1, 4), '_Education.RData'))
}
