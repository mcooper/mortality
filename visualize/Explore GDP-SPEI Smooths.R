library(tidyverse)
library(plotly)
library(mgcv)
library(ggplot2)

load('G://My Drive/Mortality/Mods/mod12gdpREML.Rdata')

preddf <- expand.grid(spei12=seq(-3, 3, 0.1),
                      GDP=seq(0, 5.5, 0.1))

preddf$age <- 12
preddf$mother_years_ed <- 18
preddf$birth_order <- 3
preddf$male <- FALSE
preddf$mothers_age <- 18
preddf$offset <- -3.828974

preddf$pred <- predict(mod12gdpREML, preddf, type='link') + mod12gdpREML$model$`offset(offset)`[1]

preddf$pred_prob <- exp(preddf$pred)/(1 + exp(preddf$pred))

preddf$GDP_real <- round(exp(log(30000) - preddf$GDP))

ggplot(preddf) + 
  geom_tile(aes(x=spei12, y=GDP, fill=pred_prob)) + 
  scale_fill_viridis_c() + 
  scale_y_continuous(labels = function(x){exp(log(30000) - x)}, 
                     breaks = seq(0, 5.5, 0.5))

# predmat <- preddf %>%
#   select(spei12, GDP, pred_prob) %>%
#   spread(GDP, pred_prob) %>%
#   select(-spei12) %>%
#   as.matrix
# 
# plot_ly(z = ~ predmat) %>% add_surface()

preddf$GDP_real[preddf$GDP_real == 368] <- "$350 - Niger"
preddf$GDP_real[preddf$GDP_real == 1494] <- "$1,500 - Kenya"
preddf$GDP_real[preddf$GDP_real == 4959] <- "$5,000 - Belize"
preddf$GDP_real[preddf$GDP_real == 8176] <- "$8,200 - Bulgaria"
preddf$GDP_real[preddf$GDP_real == 14898] <- "$15,000 - Panama"
preddf$GDP_real[preddf$GDP_real == 30000] <- "$30,000 - South Korea"

plotdf <- preddf %>%
  filter(grepl('-', GDP_real)) %>%
  group_by(GDP_real) %>%
  mutate(prob_change = (pred_prob/min(pred_prob) - 1)*100)

plotdf$GDP_real <- factor(plotdf$GDP_real, levels=c("$350 - Niger", "$1,500 - Kenya", "$5,000 - Belize",
                                                    "$8,200 - Bulgaria", "$15,000 - Panama", "$30,000 - South Korea"))

ggplot(plotdf) + 
  geom_line(aes(x=spei12, y=prob_change, color=GDP_real), size=1.5) + 
  theme_bw() + 
  #theme(legend.position=c(0.8, 0.8)) + 
  #scale_color_manual() + 
  labs(x="12 Month SPEI",
       y="Percent Change in Probability of Mortality",
       color="GDP Per Capita")

ggsave('G://My Drive/Mortality/SPEI-GDP-Relative.png', width=9, height=6)


ggplot(plotdf) + 
  geom_line(aes(x=spei12, y=pred_prob, color=GDP_real), size=1.5) + 
  theme_bw() + 
  #theme(legend.position=c(0.8, 0.8)) + 
  #scale_color_manual() + 
  labs(x="12 Month SPEI",
       y="Probability of Mortality in a Given Month",
       color="GDP Per Capita")

ggsave('G://My Drive/Mortality/SPEI-GDP-Absolute.png', width=9, height=6)





