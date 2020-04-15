# source("1.data-preparation.R")
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('readr')) install.packages('readr'); library('readr')
if (!require('tidyr')) install.packages('tidyr'); library('tidyr')

df_ALL = read_rds("output/DF_all.rds")
df_JOINED = read_rds("output/df_JOINED.rds")



# General checks ----------------------------------------------------------
df_raw %>% 
  group_by(ResponseId) %>% 
  summarise(N = n())

df_ALL %>% 
  group_by(ResponseId) %>% 
  summarise(N = n())


# 1D ------------------------------------------------------------------------------------------
  
  df_ALL %>% 
    group_by(ResponseId, brochure) %>% 
    summarise(N = n())
  
  df_ALL %>% 
    group_by(ResponseId, presencePrevalence) %>% 
    summarise(N = n())

  df_ALL %>% 
    group_by(ResponseId, timeRecommendation) %>% 
    summarise(N = n())
  
  df_ALL %>% 
    group_by(ResponseId, age) %>% 
    summarise(N = n()) %>% arrange(desc(N))

  df_ALL %>% 
    group_by(ResponseId, disease) %>% 
    summarise(N = n()) %>% arrange(desc(N))


# 2D ------------------------------------------------------------------------------------------

  df_ALL %>% 
    group_by(brochure, timeRecommendation) %>% 
    summarise(N = n())
  
# 3D ------------------------------------------------------------------------------------------

  df_ALL %>% 
    group_by(ResponseId, brochure, presencePrevalence, timeRecommendation) %>% 
    summarise(N = n())


  df_ALL %>% 
    group_by(ResponseId, brochure, presencePrevalence, timeRecommendation, age) %>% 
    summarise(N = n())
  
  
# Equilibrium ------------------------------------------------------------------------------------------

  # ALL OK. Review in final set to make sure we have a balanced sample.

  df_ALL %>% 
    group_by(brochure, presencePrevalence, timeRecommendation, disease) %>% 
    summarise(N = n())
  
  df_ALL %>% 
    group_by(brochure, timeRecommendation, age) %>% 
    summarise(N = n())




# BETWEEN CONDITIONS ------------------------------------------------------

  df_ALL %>% distinct(ResponseId, .keep_all = TRUE) %>% 
    group_by(brochure) %>%
    summarise(N = n())
  
  df_ALL %>% distinct(ResponseId, .keep_all = TRUE) %>% 
    group_by(presencePrevalence) %>%
    summarise(N = n())
  
  df_ALL %>% distinct(ResponseId, .keep_all = TRUE) %>% 
    group_by(timeRecommendation) %>%
    summarise(N = n())
  
  
 df_ALL %>% distinct(ResponseId, .keep_all = TRUE) %>% 
    group_by(brochure, presencePrevalence, timeRecommendation) %>%
    summarise(N = n()) %>% 
    ungroup() %>% 
    mutate(diff = N - max(N)) #%>%
   filter(diff != 0)
 