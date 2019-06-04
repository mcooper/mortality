library(tidyverse)

have <- list.files(path='G://My Drive/DHS New', pattern='.zip|ZIP$') %>%
  substr(1, 8) %>%
  toupper

avail <- read.csv('C://Users/matt/mortality/scope/available_files_20190522.txt',
                  col.names='filename') %>%
  mutate(file=substr(filename, 75, 82),
         type=substr(file, 3, 4),
         have=toupper(file) %in% have)

to.download <- avail %>%
  filter(!have) %>%
  select(filename)

write.csv(to.download, 'C://Users/matt/Desktop/todownload.txt', row.names=F)
