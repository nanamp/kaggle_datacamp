# In this project, I examine the clinical level data published by Dr Semmelweis
# in 1859 in which he compared maternal mortality rates from before and after handwashing 
# was instated. I will also look at hospital level data he analyzed, where he compared mortality 
# rates across two hospitals with different autopsy practices.

clinic_data <- read.csv('clinic_data.csv')

# year: each year from 1833 to 1858
# births: total number of births in the clinic
# deaths: number of maternal deaths in the clinic
# clinic: clinic (either clinic_1 or clinic_2). 
  # Doctors and midwives worked in Clinic 1, while only midwives worked in Clinic 2.

hospital_data <- read.csv('hospital_data.csv')

# year: each year from 1784 to 1848
# births: total number of births at the hospital
# deaths: number of maternal deaths at the hospital
# hospital: hospital (either Vienna or Dublin). 
  # At the Vienna General Hospital where Dr. Semmelweis worked, 
  # doctors began performing pathological autopsies in 1823. At the Dublin Rotunda Hospital, 
  # doctors did not perform pathological autopsies at all.

# 1. What were the child death rates for each year in both data sets
library(tidyverse)

clinic_data <- mutate(clinic_data,death_rate = deaths / births)
head(clinic_data,5)

hospital_data <- mutate(hospital_data,death_rate = deaths / births)
head(hospital_data,5)

# 2. In each clinic, what was the average death rate for the years before handwashing
    # was introduced in 1847
rate_by_clinic_pre_handwashing <- clinic_data %>%
    filter(year < 1847) %>%
    group_by(clinic) %>%
    summarise(avg_rate = mean(death_rate))
rate_by_clinic_pre_handwashing

# 3. What were the average death rates in the Vienna General Hospital both before 
    # and after pathological autopsies were introduced in 1823
rate_by_autopsies_introduced <- hospital_data %>%
  filter(hospital == "Vienna") %>%
  mutate(autopsies_introduced = ifelse(year >= 1823, TRUE, FALSE)) %>%
  group_by(autopsies_introduced) %>%
  summarise(avg_rate = mean(death_rate))
rate_by_autopsies_introduced

# Conclusions
# Clinic 1, where autopsies were performed had higher death rates than clinic 2 (0.08 vs 0.047)
# After autopsies were introduced in Vienna General Hospital, death rates increased from 0.012 to 0.059
  
                                      
