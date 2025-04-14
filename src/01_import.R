#
# Load in NHSN data
# 04-13-2025 Accessed
#  https://data.cms.gov/covid-19/covid-19-nursing-home-data

library(here())
source(here('src', 'setup.R'), echo=F)

d_covid <- read_csv(file = "dta/COVID_19_Nursing_Home_Data_Jan_05_2025.csv",
                     skip=0, guess_max=10000)

glimpse(d_covid)

colnames(d_covid) = c('date', 'ccn', 'state', 'zip5', 'data', 'qualitycheck', 
                      'ev_c19_wkly', 'ev_c19_tot', 
                      'ev_dths_wkly', 'ev_dths_tot',
                      'ev_c19dths_wkly', 'ev_c19dths_tot',
                      'totbeds', 'ev_c19hosp', 'resnum',
                      'c19vaccrate')

d_covid_2 = d_covid %>%
  select(date, ccn, state, zip5, totbeds, resnum, c19vaccrate,
         starts_with('ev'))

summary(d_covid_2$date) #dates 2023-2025
summary(d_covid_2$totbeds) #1 - 66 - 99 - 126 - 1748 
summary(d_covid_2$resnum) #0 - 51 - 77 - 106 - 1748 
summary(d_covid_2$c19vaccrate) #0 - 7.4 - 44 - 66 - 100 
n_distinct(d_covid_2$ccn) #14999

# Only 50+ res, vaccrate > 20% 
d_covid_3 = d_covid_2 %>%
  group_by(ccn) %>%
  dplyr::filter(mean(resnum) >= 50 & !is.na(resnum)) %>%
  dplyr::filter(median(c19vaccrate) >= 20 & ! is.na(c19vaccrate)) %>%
  ungroup %>%
  # summarize across seasons, 2024-25 season incomplete...
  mutate(season = case_when(
    date >= ymd('2023-10-01') & date <= ymd('2024-05-31') ~ '2023_2024',
    date >= ymd('2024-10-01') & date <= ymd('2025-05-31') ~ '2024_2025')
  ) %>%
  dplyr::filter(!is.na(season)) %>%
  group_by(state, ccn, season) %>%
  summarize(resnum = mean(resnum),
            vacc = mean(c19vaccrate),
            c19_events = sum(ev_c19_wkly),
            c19_dths = sum(ev_c19dths_wkly),
            c19_hosps = sum(ev_c19hosp),
            c19_endpoint = sum(ev_c19dths_wkly) + sum(ev_c19hosp), .groups='drop') 


n_distinct(d_covid_3$ccn) #7489

d_2324 = d_covid_3 %>%
  dplyr::filter(season == '2023_2024')

d_2425 = d_covid_3 %>%
  dplyr::filter(season == '2024_2025')

saveRDS(d_2324, file = here('dta', 'nhsn_23_24_agg'))
saveRDS(d_2425, file = here('dta', 'nhsn_24_25_agg'))

  
