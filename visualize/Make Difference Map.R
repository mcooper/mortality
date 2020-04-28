library(ggplot2)
library(raster)
library(rgdal)
library(sp)
library(RColorBrewer)
library(dplyr)
library(viridis)
library(ggthemes)
library(stars)
library(rnaturalearth)

load('G://My Drive/Mortality/Trends/SSP3_POP.Rdata')
load('G://My Drive/Mortality/Trends/SSP3_U5FRAC.RData')

load('G://My Drive/Mortality/Trends/ssp3_rcp26_pred.Rdata')
final <- exp(final)
final <- final/(1 + final)
u5pop <- pop*u5frac
mort <- final*u5pop
rcp26 <- apply(mort[ , , 1:360], MARGIN = c(1, 2), FUN=sum)

load('G://My Drive/Mortality/Trends/ssp3_rcp60_pred.Rdata')
final <- exp(final)
final <- final/(1 + final)
u5pop <- pop*u5frac
mort <- final*u5pop
rcp60 <- apply(mort[ , , 1:360], MARGIN = c(1, 2), FUN=sum)

#########################
#Read Data
########################

sp <- ne_countries(returnclass = 'sp')

final <- raster(rcp60 - rcp26, 
                crs='+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',
                xmn=-180, xmx=180, ymn=-90, ymx=90)

latrast <- raster(matrix(rep(seq(89.75, -89.75, -0.5), 720), ncol=720), xmn=-180, xmx=180, ymn=-90, ymx=90, 
                  crs='+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
lonrast <- raster(matrix(rep(seq(179.75, -179.75, -0.5), each=360), nrow=360), xmn=-180, xmx=180, ymn=-90, ymx=90, 
                  crs='+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

final[final < 0] <- 0
final[latrast > 26.3979 & latrast < 53.4588 & lonrast > -101.18 & lonrast < -73.67538 & is.na(final)] <- 0

sp$Mortality <- extract(final, sp, fun=sum, na.rm=T)

final <- st_as_stars(final)
sp <- st_as_sf(sp) %>%
  filter(!admin %in% c("Greenland", "Antarctica"))

ggplot() + 
  geom_stars(data=log(final + 1)) + 
  scale_fill_viridis(label=function(x)(ifelse(x > 0, round(exp(x) + 1), 0)), breaks=c(0, 1.94591, 3.89182, 5.765191, 7.695758, 9.61575)) + 
  geom_sf(data=sp, fill='transparent', color='#FFFFFF', size=0.1) + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0), limits = c(-60, 85)) + 
  theme_few() + 
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = "#AAAAAA"),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) + 
  labs(title = "Excess Under-5 Mortality by 2050 For RCP60 vs RCP26 Under SSP3")

ggsave('G://My Drive/Mortality/ExcessSSP3_2050.png', height=5, width=9)  

ggplot() + 
  geom_sf(data=sp, aes(fill=log(Mortality + 1)), color='#FFFFFF', size=0.1) + 
  scale_fill_viridis(label=function(x)(ifelse(x > 0, round(exp(x) + 1), 0)), breaks=c(0, log(19), log(399), log(8099), log(162999))) + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0), limits = c(-60, 85)) + 
  theme_few() + 
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = "#AAAAAA"),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) + 
  labs(title = "Excess Under-5 Mortality by 2050 For RCP60 vs RCP26 Under SSP3")
  
ggsave('G://My Drive/Mortality/ExcessSSP3_2050_cty.png', height=5, width=9)  

