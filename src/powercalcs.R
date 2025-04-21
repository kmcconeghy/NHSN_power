# Power Calculation"
# Kevin W. McConeghy"
# mRNA-1283 Nursing Home cRCT"
# 2025-02-10

# Setup  
library(clusterPower)
library(tidyverse)
library(here)
options(scipen=999)
options(pillar.print_max = 20)

# # Introduction  
# 
# Initial notes on power calculations for a proposed cluster-randomized nursing home study. Key elements of cRCT include:
#   
#   * Treatment: mRNA-1283 or Comirnaty (Pfizer vaccine)
# * Assignment: Nursing home level, not individual
# * Outcome: COVID-19 hospitalization, individual level
# * Follow-up: One winter season, October-May? 
#   * Analytical approach:
#   - Because of censoring issues, Failure-time? 
#   - Binary outcome logit model
# * Other issues:
#   - ICC of NHs
# - Vaccination rate is variable
# 
# # Notes regarding ICC  
# 
# We have a fair amount of data about influenza-related hosptialization rates and have manually computed ICC in the past, in general it was estimated to be fairly low and so prior calculations used a ranged of 0.005 - 0.05. However that was pre-COVID era and unknown if COVID-19 related hospitalizations are similar. 
# 
# # Notes regarding vaccination rate  
# 
# Vaccination rates were ignored in prior power calculations, vaccination was >80% so felt not an issue at the time. However, because these homes are considered to have a 30-40% vaccine rate, this is more problematic here. It will also likely vary significantly by site. I view this as essentially treatment effect heterogeneity, i.e. the % vaccinated varies by site and so VE varies by site. But a more sophisticated approach might try to frame it in causal interference terms. Since the trial is not comparing vaccination rates itself, I would view it as needing to hedge bets on the VE. So for example if you feel strongly the VE is 20% under perfect adherence, you might need to target power for a VE of 10%. I can perform simulations to give more exact answers but need more time to do that.
# 
# # Notes on outcome rate  
# 
# Focusing on COVID-19 hospitalization, our preliminary data suggested a 7% incidence of COVID-19-related hospitalizations in 2024. 

# Power calculations  

# Assume we will do at least 250, and not more than 1250

#baseline rates 
baseHosp = c(0.01, 0.03, 0.05, 0.075, 0.10, 0.15)

#percent reduction
effSize = c(0.10, 0.15, 0.2)

#intraclass correlations
intCorr = c(0.01, 0.05, 0.1)

nPerArm = 500

#alpha level
alphaV =0.05

#beta level
betaV = 0.8

#coefficient of variations for size
avSize = 100
CVval = 0.4 # average size in flublok was 100, but large variation

d_power = expand_grid(baseHosp, effSize, intCorr, nPerArm)


d_power$pwr = pmap(.l = list(d_power$baseHosp, 
                             d_power$effSize, 
                             d_power$intCorr,
                             d_power$nPerArm),
                   .f = ~cpa.binary(alpha=alphaV, 
                                    nsubjects = avSize, 
                                    nclusters = ..4,
                                    CV = CVval, 
                                    p1= ..1, 
                                    p2 = ..1 * (1 - ..2), 
                                    ICC= ..3)
) %>%
  unlist() 

d_power_2 = d_power %>%
  mutate(baseHosp = factor(d_power$baseHosp,
                           levels = baseHosp,
                           labels = paste0('Baseline ', scales::percent(baseHosp))),
         ,
         effSize = factor(d_power$effSize,
                          levels = effSize,
                          labels = paste0(scales::percent(effSize), ' reduction')),
         ICC = paste0('ICC: ', intCorr),
         `Sample Size  (per arm)` = nPerArm)


# save CSV ----

write_csv(d_power_2, file = 'mrna1283_nhNclust.csv')

# Figures ----

gg_1 = ggplot(d_power_2, aes(x = intCorr, y = pwr, color = baseHosp, group = baseHosp)) +
  geom_line(aes(linetype = baseHosp)) +
  geom_hline(aes(yintercept = 0.9), color = 'black', linetype=2) +
  geom_hline(aes(yintercept = betaV), color = 'red', linetype=2) +
  facet_wrap(vars(effSize)) +
  theme_bw() + 
  labs(x = 'ICC', y = 'Power') +
  theme(text=element_text(size=18))

ggsave('mrna1283_nhpower_500per.jpeg', plot = gg_1, dpi=150, width = 20, height=12)


