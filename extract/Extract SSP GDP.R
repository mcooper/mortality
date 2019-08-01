setwd('G://My Drive/SSPs')

library(tidyverse)
library(sp)
library(rnaturalearth)
library(zoo)
library(raster)
library(abind)
library(lubridate)

dat <- read.csv('SspDb_country_data_2013-06-12.csv', stringsAsFactors = F)

dat2 <- dat %>%
  filter(VARIABLE %in% c('GDP|PPP', 'Population') & MODEL == "OECD Env-Growth") %>%
  dplyr::select(-UNIT, -MODEL) %>%
  gather(YEAR, Value, -SCENARIO, -REGION, -VARIABLE) %>%
  spread(VARIABLE, Value) %>%
  mutate(`GDP|PPP`=`GDP|PPP`*1000000000,
         Population=Population*1000000,
         `GDP Per Capita`=`GDP|PPP`/Population,
         YEAR = as.numeric(gsub('X', '', YEAR))) %>%
  na.omit

meta <- dat2 %>%
  dplyr::select(SCENARIO, REGION) %>%
  unique %>%
  merge(data.frame(YEAR=seq(2000, 2100)))

dat3 <- merge(meta, dat2, all.x=T) %>%
  arrange(SCENARIO, REGION, YEAR) %>%
  mutate(`GDP Per Capita` = na.approx(`GDP Per Capita`)) %>%
  dplyr::select(SCENARIO, YEAR, GDP=`GDP Per Capita`, iso_a3=REGION)

cty <- ne_countries(scale = 110, type = "countries", continent = NULL,
             country = NULL, geounit = NULL, sovereignty = NULL,
             returnclass = c("sp", "sf"))

####################################################
#Quick side analysis to see which country is closest to North Korea
# un_gdp <- read.csv('G://My Drive/DHS Spatial Covars/GDP/UNdata_Export_20180310_200921296.csv')
# prk <- un_gdp %>% filter(Country.or.Area == "Democratic People's Republic of Korea") %>%
#   dplyr::select(Year, PRK=Value)
# 
# df <- data.frame()
# for (country in unique(un_gdp$Country.or.Area)){
#   sel <- un_gdp %>% filter(Country.or.Area == country) %>%
#     merge(prk)
#   
#   sse <- sum((sel$Value - sel$PRK)^2)
#   
#   df <- bind_rows(df, data.frame(country, sse))
# }
# 
# > head(df %>% arrange(sse))
# country     sse
# 1 Democratic People's Republic of Korea       0
# 2 United Republic of Tanzania: Zanzibar 1100111
# 3                         Guinea-Bissau 1237847
# 4                                Gambia 1383762
# 5                                Guinea 1527610
# 6                            Tajikistan 1549328
# #Closest Asian country is Tajikistan

cty@data$iso_a3[cty@data$iso_a3 == "ESH"] <- "MAR"  #Western Sahara has GDP of Morocco
cty@data$iso_a3[cty@data$iso_a3 == "SSD"] <- "CAF"  #South Sudan has GDP of Cenral African Republic
cty@data$iso_a3[cty@data$iso_a3 == "FLK"] <- "GBR"  #Falkland Islands has GDP of Great Britain
cty@data$iso_a3[cty@data$iso_a3 == "PRK"] <- "TJK"  #North Korea has GDP of Tajikistan
cty@data$iso_a3[cty@data$sovereignt == 'Northern Cyprus'] <- "CYP"
cty@data$iso_a3[cty@data$sovereignt == 'Kosovo'] <- "SRB"
cty@data$iso_a3[cty@data$sovereignt == 'Somaliland'] <- 'SOM'

rast <- raster(xmx=180, xmn=-180, ymx=90, ymn=-90, nrow=360, ncol=720)
for (ssp in unique(dat3$SCENARIO)){
  print(ssp)
  
  years <- list()
  for (year in seq(2000, 2100)){
    print(year)
    
    sel <- dat3 %>% filter(YEAR==year & SCENARIO == ssp)
    
    cty_sel <- sp::merge(cty, sel)
    
    r <- rasterize(cty_sel, rast, field='GDP')
    a <- as.matrix(r, dim=c(720, 360))
    
    for (i in 1:12){
      years[[length(years) + 1]] <- a
    }
  }
  
  gdp <- abind(years, along = 3)
  
  dimnames(gdp)[[3]] <- as.character(seq(ymd('2000-01-01'), ymd('2100-12-31'), by='month'))
  
  save(gdp, file=paste0('GDP/', substr(ssp, 1, 4), '_GDP.RData'))
}
