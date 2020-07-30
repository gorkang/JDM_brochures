# This script is called from "R/1.data-preparation.R". DO NOT run independently
# source("1.data-preparation.R")

# Demographic -------------------------------------------------------------

  # hist(as.numeric(df$`Duration (in seconds)`))
  df_demographic = df %>% 
    select(ResponseId, dem_gen, dem_age_1, dem_edu, dem_lan, dem_nationality) %>% 
    mutate(dem_edu_n = 
             case_when(
               dem_edu == "Primary education" ~ 1,
               dem_edu == "Lower Secondary Education" ~ 2,
               dem_edu == "Upper Secondary Education" ~ 3,
               dem_edu == "Post-Secondary Non-Tertiary Education" ~ 4,
               dem_edu == "Short-Cycle Tertiary Education" ~ 5,
               dem_edu == "Bachelor's or Equivalent level" ~ 6,
               dem_edu == "Master's or Equivalent level" ~ 7,
               dem_edu == "Doctoral or Equivalent level" ~ 8,
               dem_edu == "Not elsewhere classified" ~ 999))


# Emotion-Severity scales ------------------------------------------------
  
  df_emotionseverity = df %>% 
    select(ResponseId, matches("emosever_"), -matches("instru"), -starts_with("t_")) %>%
    gather(Item, Rating_Disease, 2:17) %>% 
    mutate(Rating_Disease = as.numeric(Rating_Disease)) %>%
    separate(Item, c("x", "Disease", "Item"), "_") %>%
    mutate(Disease = as.factor(paste0("ind_emotionseverity_", Disease))) %>%
    mutate(Item = as.numeric(Item)) %>%
    mutate(Item = 
             case_when(
               Item == 1 ~ "Severe",
               Item == 2 ~ "Serious",
               Item == 3 ~ "Unpleasant",
               Item == 4 ~ "Horrible",
             )) %>% 
    select(ResponseId, Item, Disease, Rating_Disease) %>% 
    group_by(ResponseId, Disease) %>% 
    summarise(Rating_Disease = mean(Rating_Disease)) %>% #, sd_Rating_Disease = sd(Rating_Disease))
    spread(Disease, Rating_Disease)
  

# Apriori screening  + trust and disconfort -------------------------------------------------------

  df_apriori = df %>% 
    select(ResponseId, matches("_apriori"), -starts_with("t_")) %>%
    mutate_if(is.character, as.numeric) %>% 
    mutate(ind_apriori = rowMeans(select(., matches("_apriori")), na.rm = TRUE)) %>% 
    select(ResponseId, ind_apriori)
  
  df_trust_disconfort = df %>% 
    select(ResponseId, matches("screen_trust_1"), matches("screen_disconfort_1"), -starts_with("t_")) %>%
    mutate_if(is.character, as.numeric) %>% 
    rename(ind_screening_trust = screen_trust_1,
           ind_screening_disconf = screen_disconfort_1)


# Agreeableness -----------------------------------------------------------

  df_agreableness = df %>% 
    select(ResponseId, matches("agreeableness"), -starts_with("t_")) %>%
    gather(item, response, 2:10) %>% 
    mutate(response = 
             case_when(
               response == "Disagree strongly" ~ 1L,
               response == "Disagree a little" ~ 2L,
               response == "Neither agree or disagree" ~ 3L,
               response == "Agree a little" ~ 4L,
               response == "Agree strongly" ~ 5L,
               TRUE ~ NA_integer_)) %>% 
    mutate(response = 
             case_when(
               item %in% c("agreeableness_1", "agreeableness_3", "agreeableness_6", "agreeableness_8") ~ (6L - response),
               TRUE ~ response)) %>% 
    group_by(ResponseId) %>% 
    summarise(ind_agreeableness = mean(response, na.rm = TRUE))
  


# Previous experience 1 & 2 -----------------------------------------------------

  df_prevexp_1 = df %>% 
    select(ResponseId, matches("PREVEXP_SC"), -starts_with("t_")) %>% 
    mutate_if(is.character, as.numeric) %>%
    rename(ind_prevexp1_similarproblems = PREVEXP_SCALE_1, 
           ind_prevexp1_prenataltest = PREVEXP_SCALE_2,
           ind_prevexp1_downsyndrome = PREVEXP_SCALE_3,
           ind_prevexp1_misscarriage = PREVEXP_SCALE_4,
           ind_prevexp1_abortion = PREVEXP_SCALE_5,
           ind_prevexp1_mammogram = PREVEXP_SCALE_6,
           ind_prevexp1_breastcancer = PREVEXP_SCALE_7,
           ind_prevexp1_mastectomy = PREVEXP_SCALE_8)
  
  df_prevexp_2 = df %>% 
    select(ResponseId, PREVEXP_BCANC, PREVEXP_DSYND, PREVEXP_MAMMO, PREVEXP_PRENAT) %>% 
    mutate(PREVEXP_BCANC = 
             case_when(
               PREVEXP_BCANC == "I have minimal or no experience with breast cancer." ~ 1,
               PREVEXP_BCANC == "Someone I know has/had breast cancer." ~ 2,
               PREVEXP_BCANC == "Someone very close to me has/had breast cancer." ~ 3,
               PREVEXP_BCANC == "I personally have/had breast cancer." ~ 4,)) %>% 
    mutate(PREVEXP_DSYND = 
             case_when(
               PREVEXP_DSYND == "I have minimal or no experience with pregnancies involving Down syndrome." ~ 1,
               PREVEXP_DSYND == "Someone I know has/had a pregnancy involving Down syndrome." ~ 2,
               PREVEXP_DSYND == "Someone very close to me has/had a pregnancy involving Down syndrome." ~ 3,
               PREVEXP_DSYND == "I personally have/had breast cancer." ~ 4,)) %>% 
    mutate(PREVEXP_MAMMO = 
             case_when(
               PREVEXP_MAMMO == "I have minimal or no experience with mammograms." ~ 1,
               PREVEXP_MAMMO == "Someone I know has undergone a mammogram." ~ 2,
               PREVEXP_MAMMO == "Someone very close to me has undergone a mammogram." ~ 3,
               PREVEXP_MAMMO == "I personally have undergone a mammogram." ~ 4,)) %>% 
    mutate(PREVEXP_PRENAT = 
             case_when(
               PREVEXP_PRENAT == "I have minimal or no experience with prenatal screenings." ~ 1,
               PREVEXP_PRENAT == "Someone I know has undergone a prenatal screening." ~ 2,
               PREVEXP_PRENAT == "Someone very close to me has undergone a prenatal screening." ~ 3,
               PREVEXP_PRENAT == "I personally have undergone a prenatal screening." ~ 4,)) %>% 
    rename(ind_prevexp2_breastcancer = PREVEXP_BCANC, 
           ind_prevexp2_downsyndrome = PREVEXP_DSYND, 
           ind_prevexp2_mammogram = PREVEXP_MAMMO, 
           ind_prevexp2_prenatalscreen = PREVEXP_PRENAT)

# Intuitive prevalences ---------------------------------------------------


  df_intuitive_prevalence = df %>% 
    select(ResponseId, matches("PREV_"), -starts_with("t_")) %>% 
    mutate_if(is.character, as.numeric) %>%
    mutate(ind_prevalence_BC_20 = PREV_BC_20_1 / PREV_BC_20_2,
           ind_prevalence_BC_40 = PREV_BC_40_1 / PREV_BC_40_2,
           ind_prevalence_DS_20 = PREV_DS_20_1 / PREV_DS_20_2,
           ind_prevalence_DS_40 = PREV_DS_40_1 / PREV_DS_40_2) %>% 
    select(ResponseId, matches("prevalence"))
  

# JOIN ALL ------------------------------------------------------------------------------------
  
  df_ALL_inddiff = df_demographic %>% 
    left_join(df_emotionseverity, by = c("ResponseId")) %>%
    left_join(df_apriori, by = c("ResponseId")) %>%
    left_join(df_trust_disconfort, by = c("ResponseId")) %>% 
    left_join(df_agreableness, by = c("ResponseId")) %>% 
    left_join(df_prevexp_1, by = c("ResponseId")) %>% 
    left_join(df_prevexp_2, by = c("ResponseId")) %>% 
    left_join(df_intuitive_prevalence, by = c("ResponseId")) #%>% left_join(df_UNCERTAINTY, by = c("ResponseId"))
  
  

# Save files --------------------------------------------------------------

  write_csv(df_ALL_inddiff, "output/data/DF_all_inddiff.csv")
  write_rds(df_ALL_inddiff, "output/data/DF_all_inddiff.rds")
  