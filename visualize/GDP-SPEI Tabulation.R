library(tidyverse)
library(Hmisc)

dat <- read_csv('/home/mattcoop/mortalityblob/Mortality-combined.csv') %>%
  select(alive, cc, spei3, spei36, gdp) %>%
  na.omit

dat$gdpbin <- cut2(dat$gdp, g=10)
dat$spei36bin <- cut2(dat$spei36, g=10)
dat$spei3bin <- cut2(dat$spei3, g=10)

dat <- dat

dat36 <- dat %>%
  group_by(gdpbin, spei36bin) %>%
  dplyr::summarize(mean=1 - mean(alive, na.rm=T),
            n=n(),
            surveys=length(unique(cc)))

dat3 <- dat %>%
  group_by(gdpbin, spei3bin) %>%
  dplyr::summarize(mean=1 - mean(alive, na.rm=T),
            n=n(),
            surveys=length(unique(cc)))

ggplot(dat36, aes(x=gdpbin, y=spei36bin)) + geom_tile(aes(fill=mean), width=1, height=1) + 
  geom_text(aes(label=signif(mean, 3))) + 
  scale_fill_gradient(low = "white", high = "red", space = "Lab", na.value = "gray90", guide = "colourbar") +
  scale_x_discrete(expand = c(0, 0), position = "top") +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("GDP PPP Per Capita At Location") +
  ylab("36-Month Standardized Precipitation Index") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA,color="gray90", size=0.5, linetype="solid"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill="gray90"),
        plot.background = element_rect(fill="gray90"),
        axis.text = element_text(color="black", size=12),
        axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(dat3, aes(x=gdpbin, y=spei3bin)) + geom_tile(aes(fill=mean), width=1, height=1) + 
  geom_text(aes(label=signif(mean, 3))) + 
  scale_fill_gradient(low = "white", high = "red", space = "Lab", na.value = "gray90", guide = "colourbar") +
  scale_x_discrete(expand = c(0, 0), position = "top") +
  scale_y_discrete(expand = c(0, 0)) +
  xlab("GDP PPP Per Capita At Location") +
  ylab("3-Month Standardized Precipitation Index") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA,color="gray90", size=0.5, linetype="solid"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill="gray90"),
        plot.background = element_rect(fill="gray90"),
        axis.text = element_text(color="black", size=12),
        axis.text.x = element_text(angle = 90, hjust = 1))
