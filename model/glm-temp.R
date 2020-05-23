library(tidyverse)
library(data.table)

child.months <- fread('~/mortalityblob/dhs/Mortality-combined.csv')

child.months$death <- !child.months$alive

m.6089.90 <- glm(death ~ age + mother_years_ed + mothers_age + birth_order + male + ct.6089.90, 
					 data=child.months, family='binomial')

m.6089.95 <- glm(death ~ age + mother_years_ed + mothers_age + birth_order + male + ct.6089.95, 
					 data=child.months, family='binomial')

m.6089.99 <- glm(death ~ age + mother_years_ed + mothers_age + birth_order + male + ct.6089.99, 
					 data=child.months, family='binomial')

m.9019.90 <- glm(death ~ age + mother_years_ed + mothers_age + birth_order + male + ct.9019.90, 
					 data=child.months, family='binomial')

m.9019.95 <- glm(death ~ age + mother_years_ed + mothers_age + birth_order + male + ct.9019.95, 
					 data=child.months, family='binomial')

m.9019.99 <- glm(death ~ age + mother_years_ed + mothers_age + birth_order + male + ct.9019.99, 
					 data=child.months, family='binomial')

child.months$cc <- substr(child.months$code, 1, 2)

m.tmx30 <- glm(death ~ age + mother_years_ed + mothers_age + birth_order + male + ct.tmax.30 + cc, 
					 data=child.months, family='binomial')

m.tmx35 <- glm(death ~ age + mother_years_ed + mothers_age + birth_order + male + ct.tmax.35 + cc, 
					 data=child.months, family='binomial')

m.tmx40 <- glm(death ~ age + mother_years_ed + mothers_age + birth_order + male + ct.tmax.40 + cc, 
					 data=child.months, family='binomial')

