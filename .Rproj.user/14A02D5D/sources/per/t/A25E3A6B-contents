library(tidyverse)
library(patchwork)

mixdist <- readRDS('work/mixdist_function.rds')
theme_ochre <- readRDS('work/theme_ochre.rds')

r <- data.frame(age=seq(0,10,0.01),
                d1 = mixdist(age_mean = 2,
                             age_sd = 0.5,
                             range_min = 0,
                             range_max = 10,
                             range_step = 0.01)[,2],
                d2 = mixdist(age_mean = 5,
                             age_sd = 1.5,
                             range_min = 0,
                             range_max = 10,
                             range_step = 0.01)[,2],
                d3 = mixdist(age_mean = c(7,8,8.5),
                             age_sd = c(0.3,0.3,0.3),
                             range_min = 0,
                             range_max = 10,
                             range_step = 0.01,
                             mixcoeff = c(1/3,1/3,1/3))[,2],
                d4 = mixdist(age_mean = c(2,5,7,8,8.5),
                             age_sd = c(0.5,1.5,0.5,0.5,0.5),
                             range_min = 0,
                             range_max = 10,
                             range_step = 0.01,
                             mixcoeff = c(1/3,1/3,1/9,1/9,1/9))[,2],
                d5 = mixdist(age_mean = c(7),
                             age_sd = c(0.3),
                             range_min = 0,
                             range_max = 10,
                             range_step = 0.01,
                             mixcoeff = c(1/3))[,2]/3,
                d6 = mixdist(age_mean = c(8),
                             age_sd = c(0.3),
                             range_min = 0,
                             range_max = 10,
                             range_step = 0.01,
                             mixcoeff = c(1/3))[,2]/3,
                d7 = mixdist(age_mean = c(8.5),
                             age_sd = c(0.3),
                             range_min = 0,
                             range_max = 10,
                             range_step = 0.01,
                             mixcoeff = c(1/3))[,2]/3
) 

p1<- ggplot()+
  geom_errorbarh(aes(y=1, xmin=2-0.5*2.65, xmax=2+0.5*2.65, color='Type A',height = .02))+
  geom_errorbarh(aes(y=0.88, xmin=5-1.5*2.65, xmax=5+1.5*2.65, color='Type B',height = .02))+
  geom_errorbarh(aes(y=1, xmin=7-0.3*2.65, xmax=7+0.3*2.65, color='Type C',height = .02))+
  geom_errorbarh(aes(y=0.96, xmin=8-0.3*2.65, xmax=8+0.3*2.65, color='Type C',height = .02))+
  geom_errorbarh(aes(y=0.92, xmin=8.5-0.3*2.65, xmax=8.5+0.3*2.65, color='Type C',height = .02))+
  guides(color='none')+
  scale_color_manual(name='',
                     values=c('#C8503C', '#7DA54B', '#50AAC8'),
                     breaks=c('Type A','Type B', 'Type C'))+
  labs(x='', y='Age range')+
  scale_x_continuous(limits=c(0,10))+
  theme_void()+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())
plot(p1)

p2<- ggplot(r, aes(x=age))+
  geom_area(aes(y=d1, fill='Type A', color='Type A'),alpha=0.7)+
  geom_area(aes(y=d2, fill='Type B', color='Type B'),alpha=0.7)+
  geom_area(aes(y=d5, fill='Type C', color='Type C'),alpha=0.7)+
  geom_area(aes(y=d6, fill='Type C', color='Type C'),alpha=0.7)+
  geom_area(aes(y=d7, fill='Type C', color='Type C'),alpha=0.7)+
  geom_line(aes(y=d4,linetype = "Mixed density"))+
  guides(color='none', fill=guide_legend("Date types"), linetype=guide_legend(""))+
  scale_color_manual(name='',
                     values=c('#C8503C', '#7DA54B', '#50AAC8'),
                     breaks=c('Type A','Type B', 'Type C'))+
  scale_fill_manual(name='',
                     values=c('#C8503C', '#7DA54B', '#50AAC8'),
                     breaks=c('Type A','Type B', 'Type C'))+
  labs(x='Time', y='Density')+
  scale_x_continuous(limits=c(0,10))+
  theme_ochre()+
  theme(
    axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position="right")
plot(p2)

plt <- p2/p1+plot_layout(heights = c(6, 1))
plot(plt)

ggsave("results/theoretical_background.tiff", width = 17.4, height = 8, units = 'cm', dpi=600)
ggsave("results/theoretical_background.svg", width = 17.4, height = 8, units = 'cm', dpi=600)
