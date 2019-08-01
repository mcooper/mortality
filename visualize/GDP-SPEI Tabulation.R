library(tidyverse)
library(Hmisc)

dat <- read_csv('/home/mattcoop/mortalityblob/Mortality-combined.csv') %>%
  select(alive, cc, spei3, spei36, gdp, months_before_survey, ind_code) %>%
  na.omit

dat$gdpbin <- cut2(dat$gdp, g=8)
dat$spei36bin <- cut(dat$spei36, breaks = c(-99, -1.75, -1.25, -0.75, -0.25, 0.25, 0.75, 1.25, 1.75, 99))
dat$spei3bin <- cut(dat$spei3, breaks = c(-99, -1.75, -1.25, -0.75, -0.25, 0.25, 0.75, 1.25, 1.75, 99))

dat36 <- dat %>%
  group_by(gdpbin, spei36bin) %>%
  dplyr::summarize(mean=1 - mean(alive, na.rm=T),
                   n=n(),
                   countries=length(unique(cc))) %>%
  na.omit %>%
  group_by(gdpbin) %>%
  mutate(rate = mean/mean[spei36bin=='(-0.25,0.25]'])

dat3 <- dat %>%
  group_by(gdpbin, spei3bin) %>%
  filter(months_before_survey <= 18) %>%
  dplyr::summarize(mean=1 - mean(alive, na.rm=T),
                   n=n(),
                   countries=length(unique(cc))) %>%
  na.omit %>%
  group_by(gdpbin) %>%
  mutate(rate = mean/mean[spei3bin=='(-0.25,0.25]'])

ggplot(dat36, aes(x=gdpbin, y=spei36bin)) + geom_tile(aes(fill=mean), width=1, height=1) + 
  geom_text(aes(label=signif(mean, 3))) + 
  scale_fill_gradient(low = "white", high = "red", space = "Lab", na.value = "gray90", guide = "colourbar") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA,color="gray90", size=0.5, linetype="solid"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill="gray90"),
        plot.background = element_rect(fill="gray90"),
        axis.text = element_text(color="black", size=12),
        #axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = 'none',
        plot.title = element_text(hjust = 0.5, size=15)) + 
  labs(title='Long-Term Precipitation and Probability of a Child Dying in A Given Month',
       caption='From 3,282,615 children and 242,070 deaths',
       x = 'GDP PPP Per Capita At Location',
       y = '36-Month Standardized Precipitation Index')
ggsave('~/Tabluate_SPEI36-Abs.png', width = 11, height = 7)

ggplot(dat3, aes(x=gdpbin, y=spei3bin)) + geom_tile(aes(fill=rate), width=1, height=1) + 
  geom_text(aes(label=signif(rate, 3))) + 
  scale_fill_gradient(low = "white", high = "#c51b8a", space = "Lab", na.value = "gray90", guide = "colourbar") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA,color="gray90", size=0.5, linetype="solid"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill="gray90"),
        plot.background = element_rect(fill="gray90"),
        axis.text = element_text(color="black", size=12),
        #axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = 'none',
        plot.title = element_text(hjust = 0.5, size=15)) + 
  labs(title='Short-Term Precipitation and Probability of a Child Dying in A Given Month',
       caption='From 1,265,317 children and 25,942 deaths',
       x = 'GDP PPP Per Capita At Location',
       y = '3-Month Standardized Precipitation Index')
ggsave('~/Tabluate_SPEI3-Abs.png', width = 11, height = 7)

ggplot(dat36, aes(x=gdpbin, y=spei36bin)) + geom_tile(aes(fill=rate), width=1, height=1) + 
  geom_text(aes(label=signif(rate, 3))) + 
  scale_fill_gradient2(low = "#999999", mid='white', high = "red", space = "Lab", na.value = "gray90", guide = "colourbar", midpoint=1) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA,color="gray90", size=0.5, linetype="solid"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill="gray90"),
        plot.background = element_rect(fill="gray90"),
        axis.text = element_text(color="black", size=12),
        #axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = 'none',
        plot.title = element_text(hjust = 0.5, size=15),
        plot.subtitle = element_text(hjust=0.5)) + 
  labs(title='Long-Term Precipitation and Probability of a Child Dying in A Given Month',
       subtitle = "Relative to Normal Rainfal At GDP Level",
       caption='From 3,282,615 children and 242,070 deaths',
       x = 'GDP PPP Per Capita At Location',
       y = '36-Month Standardized Precipitation Index')
ggsave('~/Tabluate_SPEI36-Rel.png', width = 11, height = 7)

ggplot(dat3, aes(x=gdpbin, y=spei3bin)) + geom_tile(aes(fill=rate), width=1, height=1) + 
  geom_text(aes(label=signif(rate, 3))) + 
  scale_fill_gradient2(low = "#999999", mid='white', high = "#c51b8a", space = "Lab", na.value = "gray90", guide = "colourbar", midpoint=1) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA,color="gray90", size=0.5, linetype="solid"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill="gray90"),
        plot.background = element_rect(fill="gray90"),
        axis.text = element_text(color="black", size=12),
        #axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = 'none',
        plot.title = element_text(hjust = 0.5, size=15),
        plot.subtitle = element_text(hjust=0.5)) + 
  labs(title='Short-Term Precipitation and Probability of a Child Dying in A Given Month',
       caption='From 1,265,317 children and 25,942 deaths',
       x = 'GDP PPP Per Capita At Location',
       y = '3-Month Standardized Precipitation Index')
ggsave('~/Tabluate_SPEI3-Rel.png', width = 11, height = 7)
