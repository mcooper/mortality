setwd('Y://watxene/ISIMIP/ISIMIP2b/input')

library(tidyverse)
library(lubridate)

options(stringsAsFactors = F)

rcp26 <- paste0('rcp26/', list.files('rcp26', recursive=T))
rcp45 <- paste0('rcp45/', list.files('rcp45', recursive=T))
rcp60 <- paste0('rcp60/', list.files('rcp60', recursive=T))
rcp85 <- paste0('rcp85/', list.files('rcp85', recursive=T))

all <- c(rcp26, rcp45, rcp60, rcp85)

sel <- all[grepl('pr_day|tas_day', all)]

l <- str_split(sel, '/')
df <- data.frame(matrix(unlist(l), nrow=length(l), byrow=T))

names(df) <- c('rcp', 'model', 'filename')

df$EWEMBI <- grepl('EWEMBI|ewembi', df$filename)
df$landonly <- grepl('landonly|landonly', df$filename)

df$var <- ifelse(grepl('pr_day', df$filename), 'pr', 'tas')

df$start <- ymd(substr(df$filename, nchar(df$filename) - 20, nchar(df$filename) - 13))
df$end <- ymd(substr(df$filename, nchar(df$filename) - 11, nchar(df$filename) - 4))

df <- df %>% 
  filter(start < ymd('2100-01-01')) %>%
  filter(landonly | model=='MIROC5') %>%
  filter(!(model=='MIROC5' & rcp == 'rcp45' & landonly))

df$filename <- paste0(df$rcp, '/', df$model, '/', df$filename)

output.file <- file("G://My Drive/SSPs/Climate/files", "wb")

cat(paste0(df$filename[5:length(df$filename)], collapse = '\n'), file = output.file)

close(output.file)
