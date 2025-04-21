#
# Load in NHSN data
# 04-13-2025 Accessed
#  https://data.cms.gov/covid-19/covid-19-nursing-home-data

library(here())
source(here('src', 'setup.R'), echo=F)

d_nhsn = readRDS(file = here('dta', 'nhsn_23_24_agg')) %>%
  dplyr::filter(!is.na(c19_endpoint))

glimpse(d_nhsn)

i_facs = n_distinct(d_nhsn$ccn)

summary(d_nhsn$vacc)

summary(d_nhsn$c19_endpoint)
sum(d_nhsn$c19_endpoint, na.rm=T)

# experiment ----
  # sampling method to describe distribution of events
  i_boots = 500L
  i_nperarm=500L
  i_trialsize = i_nperarm*2
  
  d_c19_dist = 
  tibble(boot = 1:i_boots, 
         )
  
  d_c19_dist$events =
    replicate(i_boots, slice_sample(d_nhsn, n=i_trialsize, replace=F) %>%
                pull(c19_endpoint) %>% sum(., na.rm=T))

  quantile(d_c19_dist$events, c(0.025, 0.5, 0.975))
  

# Correlation plot ----
  d_nhsn %>%
  ggplot(., aes(x = vacc, y = c19_endpoint)) +
    geom_point(color = 'blue', shape = 7) +
    geom_smooth(color = 'red', se=F) + 
    labs(x = 'Vaccination rate', y = 'Composite endpoint') +
    theme_bw()
  
  d_nhsn %>%
    ggplot(., aes(x = resnum, y = c19_endpoint)) +
    geom_point(color = 'blue', shape = 7) +
    geom_smooth(color = 'red', se=F) + 
    labs(x = 'Resident count', y = 'Composite endpoint') +
    theme_bw()
  

  
