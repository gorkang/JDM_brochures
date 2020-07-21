# Perform a few checks. ALL CHECKS SHOULD BE TRUE

# Libraries ---------------------------------------------------------------

library('dplyr')
library('readr')
library('tidyr')


# Read data ----------------------------------------------------------------

df_raw = read_csv("data/Gorka_2_201.csv") 
df_ALL = read_rds("output/data/DF_all.rds")
df_JOINED = read_rds("output/data/df_JOINED.rds")



# General checks ----------------------------------------------------------

# 1 row per participant
df_raw %>% 
  group_by(ResponseId) %>% 
  summarise(N = n()) %>% 
  pull(N) %>% 
  unique(.) == 1

# 2 items per participant
df_ALL %>% 
  group_by(ResponseId) %>% 
  summarise(N = n()) %>% 
  pull(N) %>% 
  unique(.) == 2



# 1D ------------------------------------------------------------------------------------------
  
# 2 items per participant, both in same brochure
df_ALL %>% 
  group_by(ResponseId, brochure) %>% 
  summarise(N = n()) %>% 
  pull(N) %>% 
  unique(.) == 2
  
# 2 items per participant, both in same presencePrevalence
df_ALL %>% 
  group_by(ResponseId, presencePrevalence) %>% 
  summarise(N = n()) %>% 
  pull(N) %>% 
  unique(.) == 2

# 2 items per participant, both in same timeRecommendation
df_ALL %>% 
  group_by(ResponseId, timeRecommendation) %>% 
  summarise(N = n()) %>% 
  pull(N) %>% 
  unique(.) == 2

# 1 items per participant for each age
df_ALL %>% 
  group_by(ResponseId, age) %>% 
  summarise(N = n()) %>% 
  arrange(desc(N)) %>% 
  pull(N) %>% 
  unique(.) == 1

# 1 items per participant for each disease
df_ALL %>% 
  group_by(ResponseId, disease) %>% 
  summarise(N = n()) %>% 
  arrange(desc(N)) %>% 
  pull(N) %>% 
  unique(.) == 1



# 2D ------------------------------------------------------------------------------------------

# 100 participants in each condition: brochure * timeRecommendation
df_ALL %>% 
  group_by(brochure, timeRecommendation) %>% 
  summarise(N = n()) %>% 
  pull(N) %>% 
  unique(.) == 100
  

# 3D ------------------------------------------------------------------------------------------

# 2 items per participant
df_ALL %>% 
  group_by(ResponseId, brochure, presencePrevalence, timeRecommendation) %>% 
  summarise(N = n()) %>% 
  pull(N) %>% 
  unique(.) == 2

# 1 items per participant
df_ALL %>% 
  group_by(ResponseId, brochure, presencePrevalence, timeRecommendation, age) %>% 
  summarise(N = n()) %>% 
  pull(N) %>% 
  unique(.) == 1
  
  
# Equilibrium ------------------------------------------------------------------------------------------

# ALL OK. To make sure we have a balanced sample.

df_ALL %>% 
  group_by(brochure, presencePrevalence, timeRecommendation, disease) %>% 
  summarise(N = n()) %>% 
  pull(N) %>% 
  unique(.) == 25
  
df_ALL %>% 
  group_by(brochure, timeRecommendation, age) %>% 
  summarise(N = n()) %>% 
  pull(N) %>% 
  unique(.) == 50




# BETWEEN CONDITIONS ------------------------------------------------------

df_ALL %>% distinct(ResponseId, .keep_all = TRUE) %>% 
  group_by(brochure) %>%
  summarise(N = n()) %>% 
  pull(N) %>% 
  unique(.) == 100


df_ALL %>% distinct(ResponseId, .keep_all = TRUE) %>% 
  group_by(presencePrevalence) %>%
  summarise(N = n()) %>% 
  pull(N) %>% 
  unique(.) == 100


df_ALL %>% distinct(ResponseId, .keep_all = TRUE) %>% 
  group_by(timeRecommendation) %>%
  summarise(N = n()) %>% 
  pull(N) %>% 
  unique(.) == 100



df_ALL %>% distinct(ResponseId, .keep_all = TRUE) %>% 
  group_by(brochure, presencePrevalence, timeRecommendation) %>%
  summarise(N = n()) %>% 
  ungroup() %>% 
  mutate(diff = N - max(N)) %>%
  # filter(diff != 0) %>% 
  pull(N) %>% 
  unique(.) == 25
 
