library(tidyverse)

dat <- read_csv('/home/mattcoop/mortalityblob/dhs/Mortality-combined.csv')

fpi <- read.csv('/home/mattcoop/mortalityblob/dhs/FoodPriceIndex.csv')

dat2 <- merge(dat, fpi)

dat2$mortality <- !dat2$alive

mod <- glm(mortality ~ date + age + mother_years_ed + mothers_age + birth_order + male + GDP + FPI,
           data=dat2, family = 'binomial')
summary(mod)


#Food Price Index is totally irrelevant!!

# Call:
#   glm(formula = mortality ~ date + age + mother_years_ed + mothers_age + 
#         birth_order + male + GDP + FPI, family = "binomial", data = dat2)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -0.3789  -0.0656  -0.0362  -0.0198   4.8949  
# 
# Coefficients:
#   Estimate Std. Error  z value Pr(>|z|)    
# (Intercept)     -1.086e+00  2.466e-02  -44.021   <2e-16 ***
#   date            -2.120e-03  2.304e-05  -91.984   <2e-16 ***
#   age             -7.340e-02  1.812e-04 -405.100   <2e-16 ***
#   mother_years_ed -7.214e-02  9.091e-04  -79.349   <2e-16 ***
#   mothers_age     -4.423e-02  5.052e-04  -87.552   <2e-16 ***
#   birth_order      1.336e-01  1.299e-03  102.870   <2e-16 ***
#   maleTRUE         9.440e-02  3.999e-03   23.607   <2e-16 ***
#   GDP             -2.414e-04  1.807e-06 -133.615   <2e-16 ***
#   FPI             -1.444e-04  1.251e-04   -1.154    0.248    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 3707321  on 142984007  degrees of freedom
# Residual deviance: 3312077  on 142983999  degrees of freedom
# (49312 observations deleted due to missingness)
# AIC: 3312095
# 
# Number of Fisher Scoring iterations: 10
# 
