library(tidyverse)
library(data.table)
library(lubridate)
library(foreach)
library(doParallel)

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

getPercentile <- function(vect, mini=0, maxi=length(vect)){
	#Get percentiles for a vector, with the ecdf based on a subset of the vector
	#For example, get a baseline with historic climate data, then get percentiles based on that historic distribution
	res <- ecdf(vect[mini:maxi])
		
	percs <- res(vect)

	return(percs)
}

day_all <- fread('/home/mattcoop/mortalityblob/dhs/Temps_res.csv') %>%
				mutate(date = str_replace_all(date, '\\.', '-'))

geo_all <- fread('~/mortalityblob/dhs/Mortality_geodata.csv') %>%
				select(x=longitude, y=latitude, code) %>%
				#Add a little bit of jitter so it doesnt fall on grid cell boundary
				mutate(x = x + 0.00001,
							 y = y + 0.00001)

#Map individual coords to nearest grid coords
geo_all$x_l <- floor((geo_all$x - 2.5)/2.5)*2.5 + 2.5
geo_all$y_l <- floor((geo_all$y - 2.5)/2.5)*2.5 + 2.5
geo_all$x_u <- ceiling((geo_all$x - 2.5)/2.5)*2.5 + 2.5
geo_all$y_u <- ceiling((geo_all$y - 2.5)/2.5)*2.5 + 2.5

uu <- geo_all %>% 
	select(x_l, y_l, x_u, y_u) %>% 
	filter(x_l != x_u, 
				 y_l != y_u) %>%
	unique

cl <- makeCluster(4, outfile = '')
registerDoParallel(cl)

allmonths <- foreach(r=1:nrow(uu), .packages=c('tidyverse', 'data.table', 'lubridate'), .combine=bind_rows) %dopar% {
	cat(r, r/nrow(uu), '\n')

	day <- day_all %>%
		filter(x == uu$x_l[r] | x == uu$x_u[r],
					 y == uu$y_l[r] | y == uu$y_u[r])

	geo <- geo_all %>%
		filter(x >= uu$x_l[r] & x < uu$x_u[r],
					 y >= uu$y_l[r] & y < uu$y_u[r]) %>%
		mutate(date = map2(ymd('1960-01-01'), ymd('2019-12-31'), seq, by=1)) %>%
		unnest() %>%
		mutate(date = as.character(date))

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
		select(-x_u, -y_u, -x_l, -y_l, -q_ll, -q_lu, -q_ul, -q_uu, -dx2, -dx1, -dy2, -dy1, -r1, -r2) %>%
		arrange(code, date) %>%
		group_by(code) %>%
		mutate(tmax6089p = getPercentile(tmax, mini=10959),
					 tmax9019p = getPercentile(tmax, maxi=10958))

	geo$date <- monthToCMC(geo$date)

	#Summarize
	month <- data.table(geo)[ , list(ct.tmax.30 = sum(tmax > 30, na.rm=T),
												 ct.tmax.35 = sum(tmax > 35, na.rm=T),
												 ct.tmax.40 = sum(tmax > 40, na.rm=T),
												 ct.6089.90 = sum(tmax6089p > 0.9, na.rm=T),
												 ct.6089.95 = sum(tmax6089p > 0.95, na.rm=T),
												 ct.6089.99 = sum(tmax6089p > 0.99, na.rm=T),
												 ct.6089.999 = sum(tmax6089p > 0.999, na.rm=T),
												 ct.9019.90 = sum(tmax9019p > 0.9, na.rm=T),
												 ct.9019.95 = sum(tmax9019p > 0.95, na.rm=T),
												 ct.9019.99 = sum(tmax9019p > 0.99, na.rm=T),
												 ct.9019.999 = sum(tmax9019p > 0.999, na.rm=T)),
									list(code, date)]
	
	month <- month %>%
		filter(date >= monthToCMC('1973-01'))

	month
}	

geo_all <- geo_all %>%
				#Remove jitter
				mutate(x = x - 0.00001,
							 y = y - 0.00001)

allmonths <- merge(allmonths, geo_all)

write.csv(allmonths, '/home/mattcoop/mortalityblob/dhs/Temps_month.csv', row.names=F)

system('~/telegram.sh "Done with temp processing"')

