library(data.table)
library(raster)
library(tidyverse)
library(foreach)
library(doParallel)

geo <- fread('~/mortalityblob/dhs/Mortality_geodata.csv') %>%
	rename(x=longitude, y=latitude)

#Map individual coords to nearest grid coords
geo$x_l <- floor((geo$x - 2.5)/2.5)*2.5 + 2.5
geo$y_l <- floor((geo$y - 2.5)/2.5)*2.5 + 2.5
geo$x_u <- ceiling((geo$x - 2.5)/2.5)*2.5 + 2.5
geo$y_u <- ceiling((geo$y - 2.5)/2.5)*2.5 + 2.5

coords <- bind_rows(geo %>% select(x=x_l, y=y_l),
								 geo %>% select(x=x_u, y=y_l),
								 geo %>% select(x=x_l, y=y_u),
								 geo %>% select(x=x_u, y=y_u)) %>%
	unique

cl <- makeCluster(16, outfile = '')
registerDoParallel(cl)

alldat <- foreach(y=seq(1960, 2019), .combine=bind_rows, .packages=c('tidyverse', 'raster', 'ncdf4')) %dopar% {
	cat(y, "\n")
	s <- stack(paste0('~/mortalityblob/temps_ncep_ncar/air.sig995.', y, '.nc')) %>%
		rasterToPoints %>%
		data.frame %>%
		filter(x %in% coords$x,
					 y %in% coords$y) %>%
		gather(date, temp, -x, -y) %>%
		mutate(temp = temp - 273.15,
					 date = substr(date, 2, 11)) %>%
		group_by(x, y, date) %>%
		summarize(tmax = max(temp[temp < 100], na.rm=T))

	s
}

write.csv(alldat, '~/mortalityblob/dhs/Temps_res.csv', row.names=F)
