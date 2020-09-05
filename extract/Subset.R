library(tidyverse)
library(data.table)

setwd('/home/mattcoop/mortalityblob/mortality-dhs/')

data <- fread('Mortality-combined.csv')
data$mortality <- !data$alive

#Use only data where months_before_survey < months_in_loc
data <- data %>%
  filter(months_before_survey < months_in_loc)

#Do Full Subset
n <- nrow(data)
S <- 0.05 #Get 5% of all zeros
sel <- data[!data$alive | runif(n) < S, ] #Sampling
sel$offset <- rep(log(nrow(sel)/n), nrow(sel))
fwrite(sel, 'Mortality-subset-full.csv')

#Subset To Under 2 Years Old
sub <- data %>%
  filter(age < 24)
n <- nrow(sub)
S <- 0.05 #Get 5% of all zeros
sel <- sub[!sub$alive | runif(n) < S, ] #Sampling
sel$offset <- rep(log(nrow(sel)/n), nrow(sel))
fwrite(sel, 'Mortality-subset-2yo.csv')

#Subset To Survey Less Than 5 Years Before 
sub <- data %>%
  filter(months_before_survey < 60)
n <- nrow(sub)
S <- 0.05 #Get 5% of all zeros
sel <- sub[!sub$alive | runif(n) < S, ] #Sampling
sel$offset <- rep(log(nrow(sel)/n), nrow(sel))
fwrite(sel, 'Mortality-subset-5yb.csv')


#Subset To Survey Less Than 5 Years Ago and Under 2 Years Old
sub <- data %>%
  filter(age < 24 & months_before_survey < 60)
n <- nrow(sub)
S <- 0.05 #Get 5% of all zeros
sel <- sub[!sub$alive | runif(n) < S, ] #Sampling
sel$offset <- rep(log(nrow(sel)/n), nrow(sel))
fwrite(sel, 'Mortality-subset-5yb2yo.csv')


