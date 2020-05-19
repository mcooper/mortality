library(tidyverse)
library(data.table)
library(lubridate)

monthToCMC <- function(x){
	y <- as.numeric(substr(x, 1, 4))
	m <- as.numeric(substr(x, 6, 7))

	return(12*(y - 1900) + m)
}

CMCtoMonth <- function(x){
	y <- 1900 + floor((x - 1)/12)
	m <- x - 12*(y - 1900)
	m <- substr(100 + m, 2, 3)
	
	return(paste0(y, '-', m, '-01'))
}

day <- fread('/home/mattcoop/mortalityblob/dhs/Temps_res_GEE.csv') %>%
				#Subset for testing
				#filter(x <= -6.25, x >= -8.75,
				#			 y >= 11.25, y <= 13.78) %>%
				group_by(x, y, date) %>%
				summarize(tmax=max(temp[temp < 400]) - 273.15) %>%
				group_by(x, y) %>%
				mutate(tmaxP=percent_rank(tmax)) %>%
				filter(!is.infinite(tmax))

geo <- fread('~/mortalityblob/dhs/Mortality_geodata.csv') %>%
				#Subset for testing
				#filter(longitude <= -6.25, longitude >= -8.75,
				#			 latitude >= 11.25, latitude <= 13.78) %>%
				mutate(earliest_date = ymd(CMCtoMonth(earliest_date - 2)),
							 latest_date = ymd(CMCtoMonth(latest_date + 1)),
							 date = map2(earliest_date, latest_date, seq, by=1)) %>%
				unnest() %>%
				filter(date >= ymd("1973-01-01")) %>%
				mutate(date = as.character(date)) %>%
				select(x=longitude, y=latitude, code, date)

#Map individual coords to nearest grid coords
geo$x_l <- floor((geo$x - 1.25)/2.5)*2.5 + 1.25
geo$y_l <- floor((geo$y - 1.25)/2.5)*2.5 + 1.25
geo$x_u <- ceiling((geo$x - 1.25)/2.5)*2.5 + 1.25
geo$y_u <- ceiling((geo$y - 1.25)/2.5)*2.5 + 1.25

geo <- data.table(geo, key=c('x_l', 'y_l', 'date'))

#Join tmax values at each coorner of grid
geo <- merge(geo, day %>% 
									 select(x_l=x, y_l=y, q_ll=tmax, date) %>%
									 data.table(key=c('x_l', 'y_l', 'date')), 
						 all.x=T, all.y=F)
geo <- merge(setkeyv(geo, c('x_l', 'y_u', 'date')), 
						 day %>% 
									 select(x_l=x, y_u=y, q_lu=tmax, date) %>%
									 data.table(key=c('x_l', 'y_u', 'date')), 
						 all.x=T, all.y=F)
geo <- merge(setkeyv(geo, c('x_u', 'y_l', 'date')), 
						 day %>% 
									 select(x_u=x, y_l=y, q_ul=tmax, date) %>%
									 data.table(key=c('x_u', 'y_l', 'date')), 
						 all.x=T, all.y=F)
geo <- merge(setkeyv(geo, c('x_u', 'y_u', 'date')), 
						 day %>% 
									 select(x_u=x, y_u=y, q_uu=tmax, date) %>%
									 data.table(key=c('x_u', 'y_u', 'date')), 
						 all.x=T, all.y=F)

#Conduct bilinear interpolation
geo$dx2 <- geo$x_u - geo$x
geo$dx1 <- geo$x - geo$x_l
geo$dy2 <- geo$y_u - geo$y
geo$dy1 <- geo$y - geo$y_l
geo$r1 <- (geo$dx2/2.5)*geo$q_ll + (geo$dx1/2.5)*geo$q_ul
geo$r2 <- (geo$dx2/2.5)*geo$q_lu + (geo$dx1/2.5)*geo$q_uu
geo$tmax <- (geo$dy2/2.5)*geo$r1 + (geo$dy1/2.5)*geo$r2

#Calcuate percentiles of interpolated tmax values, comare
geo <- geo %>%
				group_by(code) %>%
				mutate(tmaxP = percent_rank(tmax))

geo$date <- monthToCMC(geo$date)

#Summarize
month <- data.table(geo)[ , list(count30 = sum(tmax > 30, na.rm=T),
										 count35 = sum(tmax > 35, na.rm=T),
										 count40 = sum(tmax > 40, na.rm=T),
										 count90 = sum(tmaxP > 0.9, na.rm=T),
										 count95 = sum(tmaxP > 0.95, na.rm=T),
										 count99 = sum(tmaxP > 0.99, na.rm=T),
										 count999 = sum(tmaxP > 0.999, na.rm=T)),
							  list(code, date)]

write.csv(month, '/home/mattcoop/mortalityblob/dhs/Temps_month.csv', row.names=F)

system('~/telegram.sh "Done with temp processing"')

