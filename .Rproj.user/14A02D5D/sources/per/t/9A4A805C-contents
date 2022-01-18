library(tidyverse)
library(scales)
library(ggthemes)


y#### Data ----
o <- readRDS('work/ochre.rds')
l <- readRDS('work/lithics.rds')
dist.ochre <- readRDS('work/dist.ochre.rds')
dist.lithics <- readRDS('work/dist.lithics.rds')
dist.ochre.region <- readRDS('work/dist.ochre.region.rds')
dist.lithics.region <- readRDS('work/dist.lithics.region.rds')
dist.ochre.smoothed <- readRDS('work/dist.ochre.smoothed.rds')

orbital_parameters <- readRDS('work/orbital_parameters.rds')
pan_african_climate<- readRDS('work/pan_african_climate.rds')
benthic_isotope <- readRDS('work/benthic_isotope.rds')
insolation <- readRDS('work/insolation.rds')



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




#### Plot density smoothing
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




##### Plot regional varbiability ----

# Regional artefact distribution
region.labs <- c('N Africa', 'C+E Africa', 'S Africa', 'Overall')
names(region.labs) <- c('n_africa', 'ce_africa', 's_africa', 'overall')
regional_limits <- c(40000,500000)
regional_breaks <- seq(0,500000, 20000)
regional_labels <- function(x) format(x/1000, scientific = FALSE)



plt_regional <- ggplot()+
  geom_area(data=dist.lithics.region, aes(x=age,y=val, fill='Lithics', color='Lithics'), alpha=.6)+
  geom_area(data=dist.ochre.region, aes(x=age,y=val, fill='Ochre', color='Ochre'), alpha=.6)+
  facet_grid(var~.,
             labeller = labeller(var=region.labs))+
  scale_fill_manual(name='',
                    breaks = c('Lithics','Ochre'),
                    values = c('#A4A3A9','#A51E37'),
                    labels = c('Lithics', 'Ochre'))+
  scale_color_manual(name='',
                     breaks = c('Lithics','Ochre'),
                     values = c('#32414B','#A51E37'),
                     labels = c('Lithics', 'Ochre'))+
  
  scale_x_continuous(limits=regional_limits, breaks=regional_breaks, labels=regional_labels)+
  labs(x='Age ka BP',
       y='Density')+
  theme_ochre()+
  theme(axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank())

plot(plt_regional)


# Plot benthic isotopes
plt_benthic_isotope <- ggplot(data=benthic_isotope, aes(x=age, y=mean))+
  geom_ribbon(aes(ymin=mean-se, ymax=mean+se), fill='grey80')+
  geom_line(aes(color=mean))+
  scale_x_continuous(limits=regional_limits, breaks=regional_breaks, labels=regional_labels)+
  scale_y_reverse(limits=c(5,3))+
  scale_color_gradient2_tableau(palette = "Red-Blue Diverging", name='')+
  labs(x='Age (ka BP)',
       y='Benthic d18O (‰)')+
  theme_ochre()+
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())

plot(plt_benthic_isotope)


# Plot Insolation
plt_insolation <- ggplot(data=insolation, aes(x=age, y=val, linetype=month, color=var)) +
  geom_line()+
  scale_color_manual(name='',
                     breaks = c('N30','S30'),
                     #values = c('#D29600', '#C8503C'),
                     values = c('#415A8C', '#AF6E96'),
                     labels = c('Northern Summer','Southern Summer'))+
  scale_linetype_manual(name='',
                        breaks = c('Jun 21st','Dec 21st'),
                        #values = c('dashed', 'solid'),
                        values = c('solid', 'solid'),
                        labels = c('Northern Summer','Southern Summer'))+
  scale_x_continuous(limits=regional_limits, breaks=regional_breaks, labels=regional_labels)+
  labs(x='Age ka BP',
       y='Insolation (W/m²)')+
  theme_ochre()+
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())
plot(plt_insolation)


# Plot pan_african_climate
plt_climate <- ggplot(data=pan_african_climate, aes(x=age, y=PC1, color=PC1)) +
  geom_line()+
  scale_y_reverse()+
  scale_x_continuous(limits=regional_limits, breaks=regional_breaks, labels=regional_labels)+
  labs(x='Age ka BP',
       y='Hydroclimate')+
  scale_color_gradient2_tableau(palette = "Green-Blue Diverging", name='')+
  theme_ochre()+
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())
  
plot(plt_climate)

# Combine all plots
plt_ochre_climate <- plt_regional/plt_insolation/plt_climate/plt_benthic_isotope+plot_layout(heights = c(1, 0.3, 0.3, 0.3))
plot(plt_ochre_climate)
ggsave('results/ochre_climate.tiff', width = 17.4, height = 23.4, units = 'cm', dpi=600)

