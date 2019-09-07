library(tidyverse)
library(biglm)

data <- read_csv('/home/mattcoop/mortalityblob/Mortality-combined.csv')

data$mortality <- !data$alive

data <- data %>%
  select(mortality, age, mother_years_ed, mothers_age, birth_order, male, spei3, spei36, GDP) %>%
  na.omit %>%
  filter(!is.infinite(spei3) & !is.infinite(spei36))

data$GDP <- log(60000) - log(data$GDP)

data$GDP[data$GDP < 0] <- 0

data$spei3.2 <- data$spei3^2
data$spei3.3 <- data$spei3^3
data$spei3.4 <- data$spei3^4

data$spei36.2 <- data$spei36^2
data$spei36.3 <- data$spei36^3
data$spei36.4 <- data$spei36^4

mod <- bigglm(mortality ~ GDP*spei3 + GDP*spei3.2 + GDP*spei3.3 + GDP*spei3.4 + GDP*spei36 + GDP*spei36.2 + GDP*spei36.3 + GDP*spei36.4, 
           data=data, family=binomial(link = "logit"))

summary(mod)


##Make Prediction Plots
preddf <- merge(data.frame(age=12, mother_years_ed=5, mothers_age=18, birth_order=3, male=FALSE, 
                           spei36=seq(-7, 7, 0.1), spei3=0),
                data.frame(GDP=c(350000, 60000, 40000, 20000, 10000, 5000, 3000, 1000, 500, 100)))

preddf$GDP <- log(60000) - log(preddf$GDP)

preddf <- bind_rows(preddf, 
                    preddf %>% rename(spei36=spei3, spei3=spei36))

preddf$spei3.2 <- preddf$spei3^2
preddf$spei3.3 <- preddf$spei3^3
preddf$spei3.4 <- preddf$spei3^4

preddf$spei36.2 <- preddf$spei36^2
preddf$spei36.3 <- preddf$spei36^3
preddf$spei36.4 <- preddf$spei36^4

preddf[ , c('pred')] <- predict(object=mod, preddf)


preddf <- preddf %>%
  mutate(pred=exp(pred),
         pred=pred/(1+pred)) %>%
  mutate(GDP_PerCap = as.factor(round(exp(11.0021 - GDP))))

spei36df <- preddf %>% 
  filter(spei36 != 0)

spei3df <- preddf %>%  
  filter(spei3 != 0)

ggplot(spei36df) +
  #geom_ribbon(aes(x=spei36, ymin=minpred, ymax=maxpred, fill=GDP_PerCap), alpha=0.1) +
  geom_line(aes(x=spei36, y=pred, color=GDP_PerCap), size=1)

ggplot(spei3df) +
  #geom_ribbon(aes(x=spei3, ymin=minpred, ymax=maxpred, fill=GDP_PerCap), alpha=0.1) +
  geom_line(aes(x=spei3, y=pred, color=GDP_PerCap), size=1)
