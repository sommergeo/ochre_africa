library(tidyverse)
library(readxl)

#### Data ----
orbital_parameters <- read.delim("data/orbital_parameters.txt") %>% mutate(age=year/-1) %>% select(-year) # Computed with SAGA GIS after Berger (1978), J Atmos Sci
pan_african_climate <- read_excel("data/pnas.2018277118.sd01.xlsx", sheet = "PCA Scores") %>% rename(age=`Age (ka)`) %>% mutate(age=age*1000) # Baboth-Barh et al. (2021), Proc. Natl. Acad. Sci. U.S.A.
benthic_isotope <- read_delim("data/LR04stack.txt", "\t", escape_double = FALSE, trim_ws = TRUE, skip = 3) %>% rename(age=1, mean=2, se=3) %>% mutate(age=age*1000) # Lisiecki & Raymo (2005), Paleoceanogr Paleoclimatol
insolation_dec <- read.delim("data/Course of Daily Insolation on December 21.txt") %>% select(age=Year,S30=LAT_.30.0) %>% mutate(age=age*-1) %>% add_column(month='Dec 21st') %>% pivot_longer(c(S30), names_to = 'var', values_to = 'val')
insolation_jun <- read.delim("data/Course of Daily Insolation on June 21.txt") %>% select(age=Year, N30=LAT_30.0) %>% mutate(age=age*-1) %>% add_column(month='Jun 21st') %>% pivot_longer(c(N30), names_to = 'var', values_to = 'val')
insolation <- rbind(insolation_dec, insolation_jun)

#### Export ----
saveRDS(orbital_parameters, 'work/orbital_parameters.rds')
saveRDS(pan_african_climate, 'work/pan_african_climate.rds')
saveRDS(benthic_isotope, 'work/benthic_isotope.rds')
saveRDS(insolation, 'work/insolation.rds')