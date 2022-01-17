library(tidyverse)
set.seed(2020)


#### Data Preparation ----

# Load data
o_data <- read_delim("data/all_ochre_dates.csv", ";", escape_double = FALSE, trim_ws = TRUE)  # ochre
l_data <- read_delim("data/all_lithics_dates.csv", ";", escape_double = FALSE, trim_ws = TRUE)  # lithics

# Clean ochre data
o <- o_data %>% mutate(age_mean = (query_age_min+query_age_max)/2, # Calculate age mean
                     age_difference = query_age_max-query_age_min,   # Calculate age age_difference
                     age_sd=(age_difference)/2.65) %>%               # calculate age standard deviation, assuming our range covers p=0.99
  unite('uid', c(assemblage.locality_idlocality, assemblage.name)) %>% 
  add_count(uid, name = 'split_assemblage') %>% 
  mutate(coeff = 1/length(unique(uid))/split_assemblage) %>% 
  mutate(region = case_when(locality.y > 23.5 ~ 'N Africa',
                            locality.y <= 23.5 & locality.y >= -23.5 ~ 'C+E Africa',
                            locality.y < -23.5 ~ 'S Africa')) %>% 
  mutate(region = factor(region, levels = c('N Africa', 'C+E Africa', 'S Africa')))

# Clean lithic data
l <- l_data %>% drop_na(query_age_min, query_age_max) %>% 
  mutate(age_mean = (query_age_min+query_age_max)/2,  # Calculate age mean
         age_difference = query_age_max-query_age_min,   # Calculate age age_difference
         age_sd=(age_difference)/2.65) %>%               # calculate age standard deviation, assuming our range covers p=0.99
  unite('uid', c(assemblage.locality_idlocality, assemblage.name)) %>% 
  add_count(uid, name = 'split_assemblage') %>% 
  mutate(coeff = 1/length(unique(uid))/split_assemblage) %>% 
  mutate(region = case_when(locality.y > 23.5 ~ 'N Africa',
                             locality.y <= 23.5 & locality.y >= -23.5 ~ 'C+E Africa',
                             locality.y < -23.5 ~ 'S Africa')) %>% 
  mutate(region = factor(region, levels = c('N Africa', 'C+E Africa', 'S Africa')))


# Load theme
theme_ochre <- readRDS('work/theme_ochre.rds')


#### Functions ----

# Gaussian mixture distribution
mixdist <- function(age_mean, age_sd, mixcoeff=vector(), range_min, range_max, range_step){
  ## Create list with normal distribution fuctions Norm() from the package distr
  require(distr)
  dist_list = list() # Create a list to be filled with the for loop
  for(i in seq(1,length(age_mean),1)){
    val <- distr::Norm(mean=age_mean[i],sd=age_sd[i])
    dist_list <- append(dist_list, val)
  }
  dist_list <- DistrList(Dlist=dist_list) # Create a DistrList from the list of individual Norm Dists
  
  ## Combinte the individual Norm distributions to a mixed distribution. The mixcoefficient compensates the effect of multiple dates per assemblage
  if(length(mixcoeff)==0){
    mix_dist <- UnivarMixingDistribution(Dlist = dist_list)
  }else{
    mix_dist <- UnivarMixingDistribution(Dlist = dist_list, mixCoeff = mixcoeff)
  }
  
  ## Create a table with densities
  mix_dist_table<-data.frame(age = seq(range_min,range_max,range_step),
                             density = mix_dist@d(seq(range_min,range_max,range_step)))
  return(mix_dist_table)
  
}
saveRDS(mixdist, 'work/mixdist_function.rds')

# Smoothing function
smoothdist <- function(data,smoothing_list){
  require(pracma)
  for(i in smoothing_list){
    data[,paste0("smooth", i)] <- movavg(data$density, n=i, type = 's')   
  }
  return(data)
}
saveRDS(smoothdist, 'work/smoothdist_function.rds')



#### Analysis ----

# Calculate mixture distribution
dist.ochre <- mixdist(age_mean = o$age_mean,
                      age_sd = o$age_sd,
                      range_min = 40000,
                      range_max = 500000,
                      range_step = 1000,
                      mixcoeff = o$coeff)
saveRDS(dist.ochre, 'work/dist.ochre.rds')

dist.lithics <- mixdist(age_mean = l$age_mean,
                      age_sd = l$age_sd,
                      range_min = 40000,
                      range_max = 500000,
                      range_step = 1000,
                      mixcoeff = l$coeff)
saveRDS(dist.lithics, 'work/dist.lithics.rds')




# Calculate regional mixture distributions
## Regional subsets
o.n_africa <- o %>% filter(region=='N Africa') %>% select(age_mean,age_sd, coeff)
o.ce_africa <- o %>% filter(region=='S Africa') %>% select(age_mean,age_sd, coeff)
o.s_africa <- o %>% filter(region=='C+E Africa') %>% select(age_mean,age_sd, coeff)
l.n_africa <- l %>% filter(region=='N Africa') %>% select(age_mean,age_sd, coeff)
l.ce_africa <- l %>% filter(region=='S Africa') %>% select(age_mean,age_sd, coeff)
l.s_africa <- l %>% filter(region=='C+E Africa') %>% select(age_mean,age_sd, coeff)

## Calculate regional distributions
dist.ochre.region <- data.frame(age=seq(40000,500000,1000),
                                 overall = mixdist(age_mean = o$age_mean,
                                                   age_sd = o$age_sd,
                                                   range_min = 40000,
                                                   range_max = 500000,
                                                   range_step = 1000,
                                                   mixcoeff = o$coeff)[,2],
                                 n_africa = mixdist(age_mean = o.n_africa$age_mean,
                                                    age_sd = o.n_africa$age_sd,
                                                    range_min = 40000,
                                                    range_max = 500000,
                                                    range_step = 1000,
                                                    mixcoeff = o.n_africa$coeff)[,2],
                                 ce_africa = mixdist(age_mean = o.ce_africa$age_mean,
                                                     age_sd = o.ce_africa$age_sd,
                                                     range_min = 40000,
                                                     range_max = 500000,
                                                     range_step = 1000,
                                                     mixcoeff = o.ce_africa$coeff)[,2],  
                                 s_africa = mixdist(age_mean = o.s_africa$age_mean,
                                                       age_sd = o.s_africa$age_sd,
                                                       range_min = 40000,
                                                       range_max = 500000,
                                                       range_step = 1000,
                                                       mixcoeff = o.s_africa$coeff)[,2]
                                ) %>% pivot_longer(-c(age), names_to = 'var', values_to = 'val')
dist.ochre.region$var <- factor(dist.ochre.region$var, levels=c('n_africa', 'ce_africa', 's_africa', 'overall'))
saveRDS(dist.ochre.region, 'work/dist.ochre.region.rds')


dist.lithics.region <- data.frame(age=seq(40000,500000,1000),
                                overall = mixdist(age_mean = l$age_mean,
                                                  age_sd = l$age_sd,
                                                  range_min = 40000,
                                                  range_max = 500000,
                                                  range_step = 1000,
                                                  mixcoeff = l$coeff)[,2],
                                n_africa = mixdist(age_mean = l.n_africa$age_mean,
                                                   age_sd = l.n_africa$age_sd,
                                                   range_min = 40000,
                                                   range_max = 500000,
                                                   range_step = 1000,
                                                   mixcoeff = l.n_africa$coeff)[,2],
                                ce_africa = mixdist(age_mean = l.ce_africa$age_mean,
                                                    age_sd = l.ce_africa$age_sd,
                                                    range_min = 40000,
                                                    range_max = 500000,
                                                    range_step = 1000,
                                                    mixcoeff = l.ce_africa$coeff)[,2],  
                                s_africa = mixdist(age_mean = l.s_africa$age_mean,
                                                   age_sd = l.s_africa$age_sd,
                                                   range_min = 40000,
                                                   range_max = 500000,
                                                   range_step = 1000,
                                                   mixcoeff = l.s_africa$coeff)[,2]
                                ) %>% pivot_longer(-c(age), names_to = 'var', values_to = 'val')
dist.lithics.region$var <- factor(dist.lithics.region$var, levels=c('n_africa', 'ce_africa', 's_africa', 'overall'))
saveRDS(dist.lithics.region, 'work/dist.ochre.region.rds')



# The effect of smoothing
## Apply smoothing
dist.ochre.smoothed <- smoothdist(data = dist.ochre, smoothing_list = c(5,10,15,20,30,40))
saveRDS(dist.ochre.smoothed, 'work/dist.ochre.smoothed.rds')
