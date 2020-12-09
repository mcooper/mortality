library(gdalUtils)
library(raster)
library(dplyr)
library(SPEI)
library(lubridate)
library(zoo)
library(foreach)
library(doParallel)

setwd('~/mortalityblob/fldas/')

dat <- read.csv('~/mortalityblob/mortality-dhs/Mortality_geodata.csv') %>%
  filter(!(latitude==0 & longitude==0))

sp <- SpatialPointsDataFrame(coords=dat[ c('longitude', 'latitude')], data = dat)

r <- raster('prec_198201.tif')
codes <- raster(matrix(seq(1, ncell(r)), nrow=nrow(r), ncol=ncol(r)), xmx=xmax(r), xmn=xmin(r), ymx=ymax(r), ymn=ymin(r))
codes[is.na(r)] <- NA

sp@data$tmpcode <- extract(codes, sp)

codes <- focal(codes, w=matrix(rep(1, 9), ncol=3), fun=function(x){ifelse(any(is.na(x)), x[5], NA)})

codespts <- rasterToPoints(codes)

#Deal with points near a coast, coming up NA
spna <- sp[is.na(sp@data$tmpcode) , ]
spna$tmpcode <- NULL
badcoords <- spna@coords
dd <- pointDistance(badcoords, codespts[ , c('x', 'y')], lonlat=T)
tmpcode <- codespts[apply(dd, MARGIN=1, FUN=which.min) , 'layer']
badcoords <- cbind.data.frame(badcoords, tmpcode)
spna <- merge(spna@data, badcoords)
sp <- bind_rows(spna, sp@data[!is.na(sp@data$tmpcode), ])

rll <- sp %>% group_by(tmpcode) %>%
  summarize(x=mean(longitude),
            y=mean(latitude)) 	

cl <- makeCluster(16, outfile = '')
registerDoParallel(cl)

#Read in ppt data
ppt_vrt_file <- extension(rasterTmpFile(), 'ivrt')
ppt_files <- list.files('.', pattern='^prec.*tif$')[1:456]
gdalbuildvrt(paste0('./', ppt_files), ppt_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE, a_srs='EPSG:4326')

#Read in tmp data
tmp_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tmp_files <- list.files('.', pattern='^tavg.*tif$')[1:456]
gdalbuildvrt(paste0('./', tmp_files), tmp_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE, a_srs='EPSG:4326')

extract <- function(vrt, x, y){
  
  dat <- gdallocationinfo(vrt, x, y, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  return(dat)
  
}

getTempZ <- function(temp){
	month_means <- apply(matrix(temp, ncol=12, byrow=T), MARGIN=2, FUN=mean, na.rm=T)
	month_sd <- apply(matrix(temp, ncol=12, byrow=T), MARGIN=2, FUN=sd, na.rm=T)

	(temp - month_means)/month_sd
}

foreach(n=1:nrow(rll), .packages=c('raster', 'gdalUtils', 'SPEI', 'dplyr', 'zoo')) %dopar% {
  
  precip <- extract(ppt_vrt_file, rll$x[n], rll$y[n])
  tmp <- extract(tmp_vrt_file, rll$x[n], rll$y[n])
  pet <- thornthwaite(tmp, rll$y[n])
  s <- precip - pet
  
  temp1 <- tmp - 273.15
  temp2 <- rollmean(tmp, k=2, fill=NA, na.rm=T, align='right')
  temp3 <- rollmean(tmp, k=3, fill=NA, na.rm=T, align='right')
  temp1l1 <- lead(tmp)
  temp2l1 <- lead(rollmean(tmp, k=2, fill=NA, na.rm=T, align='right'))
  temp3l1 <- lead(rollmean(tmp, k=3, fill=NA, na.rm=T, align='right'))
  
  interview <- data.frame(date_cmc=seq(985,1440),
                          
              #spei
              spei1=as.numeric(spei(s, 1, na.rm=TRUE)$fitted),
              spei2=as.numeric(spei(s, 2, na.rm=TRUE)$fitted),
              spei3=as.numeric(spei(s, 3, na.rm=TRUE)$fitted),
              spei6=as.numeric(spei(s, 6, na.rm=TRUE)$fitted),
              spei12=as.numeric(spei(s, 12, na.rm=TRUE)$fitted),
              spei24=as.numeric(spei(s, 24, na.rm=TRUE)$fitted),
              spei36=as.numeric(spei(s, 36, na.rm=TRUE)$fitted),
              spei48=as.numeric(spei(s, 48, na.rm=TRUE)$fitted),
              spei1l1=as.numeric(spei(lead(s), 1, na.rm=TRUE)$fitted),
              spei2l1=as.numeric(spei(lead(s), 2, na.rm=TRUE)$fitted),
              spei3l1=as.numeric(spei(lead(s), 3, na.rm=TRUE)$fitted),
              
              #TempZ
						  #Get Z score for that month!
						  temp1monthZ = getTempZ(temp1),
						  temp2monthZ = getTempZ(temp2),
						  temp3monthZ = getTempZ(temp3),
						  temp1l1monthZ = getTempZ(temp1l1),
						  temp2l1monthZ = getTempZ(temp2l1),
						  temp3l3monthZ = getTempZ(temp3l1),
						  
						  temp3,
						  temp1,
						  temp2,
						  temp3l1,
						  temp1l1,
						  temp2l1
						  )
  
  spsel <- sp[sp$tmpcode==rll$tmpcode[n], ]

  all <- data.frame()
  for (i in 1:nrow(spsel)){
    new <- data.frame(code=spsel$code[i], date_cmc=seq(spsel$earliest_date[i], spsel$latest_date[i]))
    new <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=F)}, list(new, interview))
    all <- bind_rows(all, new)
  }
  
  #Reduce data size by eliminating uncessary precision:
  all <- all %>%
    mutate_if(is.numeric, function(x){round(x, 3)})
  
  cat(n, round(n/nrow(rll)*100, 4), 'percent done\n') 
  write.csv(all, paste0('/mnt/SPIres/', n), row.names=F)
}

setwd('/mnt/SPIres/')

precip <- list.files()%>%
  lapply(read.csv) %>%
  bind_rows

#Write
write.csv(precip, '~/mortalityblob/mortality-dhs/Mortality_SPI_Temps_FLDAS.csv', row.names=F)

system('/home/mattcoop/telegram.sh "SPI for Mortality Done!"')






