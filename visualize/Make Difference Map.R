library(ggplot2)
library(raster)
library(rgdal)
library(sp)
library(RColorBrewer)
library(dplyr)
library(viridis)
library(ggthemes)

#########################
#Read Data
########################

sp <- readOGR('G://My Drive/DHS Spatial Covars/Global Codes and Shapefile',
              'ne_50m_admin_0_countries')

final <- raster('G://My Drive/DHS Processed/SSP4-SSP1 Diff.tif', 
                crs='+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',
                xmn=-180, xmx=180, ymn=-90, ymx=90)

extent(final) <- c(-180, 180, -90, 90)

df <- as.data.frame(as(final, 'SpatialPixelsDataFrame'))

names(df) <- c('Value', 'x', 'y')

df$Value[df$Value < 0] <- 0

df$Value <- log(df$Value + 1)

sp <- sp[!sp$ADMIN %in% c('Greenland', 'Antarctica'), ]

spdf <- fortify(sp)

ggplot() + 
  geom_tile(data=df, aes(x=x, y=y, fill=Value)) + 
  scale_fill_viridis(breaks=c(0, 4.787492, 9.615805, 14.4033),
                     labels=function(x){ifelse(x > 14, "1,800,000",
                                               ifelse(x > 9, "15,000",
                                                      ifelse(x > 4, "120", "0")))}) + 
  geom_polygon(data=spdf, aes(x=long, y=lat, group=group), 
               fill='transparent', color='#FFFFFF', size=0.1) + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) + 
  theme_few() + 
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = "#AAAAAA"),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) + 
  labs(title = "Excess Under-5 Mortality from 2020-2040 Under SSP4 vs SSP1")

ggsave('G://My Drive/Mortality/Excess2040.png', height=5, width=9)  
