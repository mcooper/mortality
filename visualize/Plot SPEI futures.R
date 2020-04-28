setwd('G://My Drive/Mortality/SPEI Pred')

library(tidyverse)

#########################
#SPEI3
########################
fs <- list.files(pattern='^rcp.*spei3.Rdata$')

df <- data.frame()
for (f in fs){
  load(f)
  
  RCP <- substr(f, 1, 5)
  
  GCM <- gsub('spei3.Rdata', '', gsub(paste0(RCP, '_'), '', f))
  
  sel <- spei3[ , , 841:960]
  
  samp <- sample(na.omit(as.vector(sel)), 1000)
  
  df <- bind_rows(df, data.frame(RCP, GCM, SPEI='SPEI3', data=samp))
  
}

load('ewembi_spei3.Rdata')

sel <- spei3[ , , 133:252]
samp <- sample(na.omit(as.vector(sel)), 1000)

df <- bind_rows(df, data.frame(RCP='1990s', GCM='1990s', SPEI='SPEI3', data=samp))

#########################
#SPEI36
########################
fs <- list.files(pattern='^rcp.*spei36.Rdata$')

for (f in fs){
  load(f)
  
  RCP <- substr(f, 1, 5)
  
  GCM <- gsub('spei36.Rdata', '', gsub(paste0(RCP, '_'), '', f))
  
  sel <- spei36[ , , 841:960]
  
  samp <- sample(na.omit(as.vector(sel)), 1000)
  
  df <- bind_rows(df, data.frame(RCP, GCM, SPEI='SPEI36', data=samp))
  
}

load('ewembi_spei36.Rdata')

sel <- spei36[ , , 133:252]
samp <- sample(na.omit(as.vector(sel)), 1000)

df <- bind_rows(df, data.frame(RCP='1990s', GCM='1990s', SPEI='SPEI36', data=samp))

df$comb <- paste0(df$RCP, df$GCM)

ggplot(df) + 
  geom_boxplot(aes(x=comb, y=data, linetype=GCM, color=RCP)) + 
  facet_grid(. ~ SPEI) + 
  theme_bw() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5)) + 
  labs(title = '1990s vs 2090s Global SPEI Values',
       y='',
       x='')

ggsave('G://My Drive/Mortality/SPEI Projections.png', width=10, height=5)



