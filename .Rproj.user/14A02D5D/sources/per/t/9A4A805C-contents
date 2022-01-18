library(tidyverse)
library(scales)
#library(extrafont)
#font_import()

y#### Data ----
o <- readRDS('work/ochre.rds')
l <- readRDS('work/lithics.rds')
dist.ochre <- readRDS('work/dist.ochre.rds')
dist.lithics <- readRDS('work/dist.lithics.rds')
dist.ochre.region <- readRDS('work/dist.ochre.region.rds')
dist.lithics.region <- readRDS('work/dist.lithics.region.rds')
dist.ochre.smoothed <- readRDS('work/dist.ochre.smoothed.rds')




#### Theme ----
theme_ochre <-  function(){
  list(theme_bw(),
       theme(text=element_text(size=8), #change font size of all text
       axis.text=element_text(size=8), #change font size of axis text
       axis.title=element_text(size=10), #change font size of axis titles
       plot.title=element_text(size=8), #change font size of plot title
       legend.text=element_text(size=10), #change font size of legend text
       legend.title=element_text(size=10)))
}
saveRDS(theme_ochre, 'work/theme_ochre.rds')




#### Plot dating uncertainties ----
ol <- bind_rows('lithics'=l,'ochre'=o, .id=c('groups')) %>% mutate(groups=factor(groups, levels=c('lithics', 'ochre')))

plt_date_scatter <- ggplot(data=ol, aes(x=age_mean, y=age_difference))+
  geom_hline(yintercept=26000, color='black',linetype='dotted')+
  annotate('text', x=1000000, y=26000, label='precession', vjust=-0.5, size=2.86)+ # Size divided by 5/14
  geom_hline(yintercept=100000, color='black',linetype='dotted')+
  annotate('text', x=1000000, y=100000, label='eccentricity', vjust=-0.5, size=2.86)+
  geom_jitter(width=0.03, height=0.03, aes(color=groups), size=.5, show.legend=TRUE)+
  scale_color_manual(name='Legend',
                     values=c(lithics='#A4A3A9', ochre='#A51E37'),
                     breaks=c('lithics','ochre'),
                     guide='legend')+
  labs(x='Age (ka BP)', y='Age range (ka)')+
  theme_ochre()+
  theme(legend.position=c(0.8, 0.2),
        legend.background = element_rect(fill="white",
                                         size=0.1, linetype="solid", 
                                         colour ="black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  scale_x_log10(breaks = c(1000, 10000, 100000, 1000000),
                labels = c(1, 10, 100, 1000))+
  scale_y_log10(breaks = c(1000, 10000, 100000, 1000000),
                labels = c(1, 10, 100, 1000))+
  annotation_logticks()

plot(plt_date_scatter)

ggsave("results/ochre_age_ranges_climate.tiff", width = 8.4, height = 8.4, units = 'cm', dpi=600)




#### Plot dating uncertainties
plt_smoothing <- ggplot(data=dist.ochre.smoothed %>% 
         pivot_longer(-c(age), names_to = 'var', values_to = 'val'),
       aes(x=age, y=val, color=var, linetype=var)) +
  geom_line() +
  theme_ochre()+
  scale_x_continuous(limits=c(50000,500000),
                     labels = function(x) format(x/1000, scientific = FALSE))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  labs(x='Age (ka BP)', y='Density')+
  scale_color_discrete(name='Smoothing interval (ka)', 
                       labels=c('None',5,10,15,20,30,40),
                       breaks=c("density",'smooth5','smooth10','smooth15','smooth20','smooth30','smooth40'))+
  scale_linetype_manual(name='Smoothing interval (ka)', 
                        values=c('solid','solid','solid','solid','solid','solid','solid'),
                        labels=c('None',5,10,15,20,30,40),
                        breaks=c("density",'smooth5','smooth10','smooth15','smooth20','smooth30','smooth40'))+
  guides(colour = guide_legend(title.position = "top"))+
  theme(legend.position="right")
plot(plt_smoothing)
ggsave("results/ochre_density_smoothing.tiff", width = 17.4, height = 8, units = 'cm', dpi=600)
