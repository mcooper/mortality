library(tidyverse)


have <- list.files(path='G://My Drive/DHS New', pattern='.zip|ZIP$') %>%
  substr(1, 8)

avail <- read.csv('C://Users/matt/mortality/scope/available_files_20190522.txt') %>%
  mutate(file=substr(filename, 79, 86),
         type=substr(file, 3, 4),
         have=file %in% have)

to.download <- avail %>%
  filter(type %in% c('BR', 'WI', 'GE') & !have) %>%
  select(filename)

write.csv(to.download, 'C://Users/matt/Desktop/todownload.txt', row.names=F)
