setwd('/home/mattcoop/mortalityblob/mortality-dhs')

library(tidyverse)
library(data.table)

house <- fread('Mortality_respondentdata.csv', data.table=T, key=c('resp_code'),
               select=c('drinking_source_int', 'toilet_type_int', 'wealth_factor_harmonized',
                        'drinking_source_chr', 'toilet_type_chr',
                        'resp_code'))

fwrite(unique(house[ , c('drinking_source_chr', 'drinking_source_int')]), 'drinking_source.csv',
       row.names=F)
fwrite(unique(house[ , c('toilet_type_chr', 'toilet_type_int')]), 'toilet_type.csv',
       row.names=F)

drinking_source <- fread('drinking_source.csv')
toilet_type <- fread('toilet_type.csv')

house <- merge(house, drinking_source, by=c('drinking_source_chr', 'drinking_source_int'))
house <- merge(house, toilet_type, by=c('toilet_type_chr', 'toilet_type_int'))

house$adequate_sanitation <- house$drinking_source_adequate & house$toilet_adequate

fwrite(house[ , c('resp_code', 'adequate_sanitation', 'wealth_factor_harmonized')],
       'Mortality_household.csv', row.names=F)
