library(rasterVis)
library(raster)
library(rgdal)
library(sp)
library(RColorBrewer)
library(dplyr)

#########################
#Read Data
########################

sp <- readOGR('G://My Drive/DHS Spatial Covars/Global Codes and Shapefile',
              'ne_50m_admin_0_countries')

spt <- spTransform(sp, CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

data <- read.csv('G://My Drive/DHS Processed/Mortality_geodata.csv') %>%
  select(latitude, longitude, earliest_date)


data$earliest_date <- ifelse(data$earliest_date > 1000, "Wet",
                    ifelse(data$earliest_date < 900, "Dry", "Normal")) %>%
  as.factor %>%
  relevel(ref = "Normal")

spdat <- SpatialPointsDataFrame(coords=data[ , c('longitude', 'latitude')],
                                data=data, proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

spdat_t <- spTransform(spdat, CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

spdat_t@data[ , c('longitude', 'latitude')] <- spdat_t@coords

####################
#Make Images
####################

setwd('G://My Drive/Mortality/')

##################
#DHS Points
###################

plt2 <- spplot(spdat_t, "earliest_date", 
               col.regions = c("#000078", "#000078", "#000078"), 
               cex = 0.1, 
               sp.layout=list('sp.polygons', spt, fill="#DDDDDD"))

plt2$legend$bottom$args$key$text[[1]] <- c("", "", "")
plt2$legend$bottom$args$key$points$cex <- c(0,0,0)

pdf("DHSPoints.pdf", width=8, height=6)
plot(plt2)
dev.off()

system("pdfcrop DHSPoints.pdf DHSPoints.pdf")
