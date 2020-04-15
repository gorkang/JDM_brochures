# Packages ---------------------------------------------------------------

# If this is the first time you run this script, make sure you have the necessary packages
# source("R/0.install-dependencies.R")

library('dplyr')
library('readr')
library('tidyr')
library('corrplot')


# Read data and create dictionary -----------------------------------------

  df_raw = read_csv("data/Gorka_2_201.csv") 
  df = df_raw %>% filter(dem_gen == "Female") %>% #| dem_gen == "Male"
    mutate(ResponseId = as.factor(ResponseId))

  # Column names dictionary
  df_diccionary_raw = df_raw %>% filter(Progress == "Progress") %>% t()
  df_diccionary = df_diccionary_raw %>% as_tibble() %>% mutate(column_names = rownames(df_diccionary_raw)) %>% rename(description = V1)
  
  
  #DF with main conditions  
  df_conditions = df %>% 
    select(ResponseId, brochure, timeRecommendation, presencePrevalence, age1, age2, disease1, disease2) %>% 
    gather(condition, value, 5:8) %>% 
    mutate(age_disease = gsub("[[:digit:]]","", condition),
           DIS_n = gsub("[^[:digit:]]", "", condition)) %>% 
    select(-condition) %>% 
    spread(age_disease, value)
         


# Screening -----------------------------------------------------------------------------------------

  df_SCREENING_raw = df %>% 
    select(ResponseId, matches("BFAFT"), matches("AFTER")) 
  
  
  # Screening - PPV --------------

    df_SCREENING_PPV = df_SCREENING_raw %>% 
      select(ResponseId, matches("PPV"), -starts_with("t_")) %>%
      gather(condition, PPV_screening, 2:5) %>% 
      drop_na(PPV_screening) %>% 
      mutate(DIS_n = stringr::str_sub(condition, 10, 10)) %>% 
      mutate(PPV_screening = as.numeric(PPV_screening)) %>% 
      select(-condition)
  
  
  # Screening - Confidence PPV --------------
    
    df_SCREENING_CONF_PPV = df_SCREENING_raw %>% 
      select(ResponseId, matches("CONF"), -starts_with("t_")) %>%
      gather(condition, CONF_PPV_screening, 2:5) %>% 
      drop_na(CONF_PPV_screening) %>% 
      mutate(DIS_n = stringr::str_sub(condition, 10, 10)) %>% 
      mutate(CONF_PPV_screening = as.numeric(CONF_PPV_screening)) %>% 
      select(-condition)
    
    
  # Screening - Recommendation --------------
  
    df_SCREENING_REC = df_SCREENING_raw %>% 
      select(ResponseId, matches("REC_"), -starts_with("t_")) %>%
      gather(condition, RECOMEND_screening, 2:7) %>% 
      drop_na(RECOMEND_screening) %>% 
      mutate(DIS_n = stringr::str_sub(condition, 10, 10)) %>% 
      mutate(RECOMEND_screening = as.numeric(
        case_when(
          RECOMEND_screening == "Yes, I would recommend it." ~ 1,
          RECOMEND_screening == "No, I would not recommend it." ~ 0,
          TRUE ~ NA_real_))) %>% 
      mutate(before_after_N = stringr::str_sub(condition, 16, 16)) %>% 
      select(-condition) %>% 
      spread(before_after_N, RECOMEND_screening) %>% 
      rename(RECOMEND_screening_AFTER = `1`,
             RECOMEND_screening_BEFORE = `2`)
      
    
    # Screening - Sure - Recommendation --------------
    
    df_SCREENING_SURE_REC = df_SCREENING_raw %>% 
      select(ResponseId, matches("SURE"), -starts_with("t_")) %>%
      gather(condition, SURE_RECOMEND_screening, 2:7) %>% 
      drop_na(SURE_RECOMEND_screening) %>% 
      mutate(DIS_n = stringr::str_sub(condition, 10, 10)) %>% 
      mutate(SURE_RECOMEND_screening = as.numeric(SURE_RECOMEND_screening)) %>% 
      mutate(before_after_N = stringr::str_sub(condition, 17, 17)) %>%
      select(-condition) %>% 
      spread(before_after_N, SURE_RECOMEND_screening) %>% 
      rename(SURE_RECOMEND_screening_AFTER = `1`,
             SURE_RECOMEND_screening_BEFORE = `2`)

    
    # Screening - Enough info --------------
    
    df_SCREENING_ENOUGH = df %>% 
      select(ResponseId, matches("ENO"), -starts_with("t_")) %>% 
      gather(condition, ENOUGH_screening, 2:9) %>% 
      drop_na(ENOUGH_screening) %>% 
      mutate(DIS_n = stringr::str_sub(condition, 16, 16)) %>%
      mutate(ENOUGH_screening = as.numeric(
               case_when(
                 ENOUGH_screening == "Yes." ~ 1,
                 ENOUGH_screening == "No, I needed the following information." ~ 0,
                 TRUE ~ NA_real_))) %>% 
      filter(!grepl("TEXT", condition)) %>% # REVIEW: WE FILTER THE REASONS FOR NOW.
      select(-condition)

  
    # Screening - Enough info EXPLANATION--------------
      
      df %>% 
        select(ResponseId, matches("ENO_4_TEXT"), -starts_with("t_")) %>% 
        gather(condition, ENOUGH_screening_TEXT, 2:5) %>% 
        drop_na(ENOUGH_screening_TEXT) 
  
    
# Follow Up ---------------------------------------------------------------

  df_FOLLOW_raw = df %>% 
    select(ResponseId, matches("FOLLOW")) 
  
  # Follow Up - Recommendation --------------
    
    df_FOLLOW_REC = df_FOLLOW_raw %>% 
      select(ResponseId, matches("REC"), -starts_with("t_")) %>%
      gather(condition, RECOMEND_followup, 2:3) %>% 
      drop_na(RECOMEND_followup) %>% 
      mutate(DIS_n = stringr::str_sub(condition, 11, 11)) %>% 
      mutate(RECOMEND_followup = as.numeric(
               case_when(
                 RECOMEND_followup == "Yes, I would recommend it." ~ 1,
                 RECOMEND_followup == "No, I would not recommend it." ~ 0,
                 TRUE ~ NA_real_))) %>% 
      select(-condition)
    
  
  
    # Follow up - Sure - Recommendation --------------
    
    df_FOLLOW_SURE_REC = df_FOLLOW_raw %>% 
      select(ResponseId, matches("SURE"), -starts_with("t_")) %>%
      gather(condition, SURE_RECOMEND_followup, 2:3) %>% 
      drop_na(SURE_RECOMEND_followup) %>% 
      mutate(DIS_n = stringr::str_sub(condition, 11, 11)) %>% 
      mutate(SURE_RECOMEND_followup = as.numeric(SURE_RECOMEND_followup)) %>% 
      # mutate(before_after_N = stringr::str_sub(condition, 17, 17)) %>% 
      select(-condition)
    

  df_FOLLOW_FACTORS = 
    df_FOLLOW_raw %>% 
    select(ResponseId, matches("FACTOR"), -starts_with("t_")) %>%
    gather(condition, FACTORS_followup, 2:17) %>% 
    drop_na(FACTORS_followup) %>% 
    separate(condition, c("x", "DIS_n", "x2", "FACTORS_FU_Item"), "_") %>% 
    select(-x, -x2) %>% 
    mutate(DIS_n = stringr::str_sub(DIS_n, 4, 4)) %>% 
    mutate(FACTORS_followup = as.numeric(FACTORS_followup)) %>% 
    spread(FACTORS_FU_Item, FACTORS_followup, sep = "_")
  
# CONTROL Q ---------------------------------------------------------------

  df_CONTROL_raw = df %>% 
    select(ResponseId, matches("CONTROL"), -starts_with("t_")) %>% 
    select(-matches("ENO")) %>% # see df_SCREENING_ENOUGH above
    gather(condition, response, 2:20) %>% 
    drop_na(response) %>% 
    filter(!grepl("Click", condition)) %>% 
    filter(!grepl("Page Submit", condition)) %>% 
    separate(condition, c("CONTROL", "brochure", "DIS_n", "item"), "_") %>% # Discarded 1 in PPV_1
    mutate(DIS_n = gsub("DIS", "", DIS_n))
  
  df_CONTROL = df_conditions %>% 
    left_join(df_CONTROL_raw,  by = c("ResponseId", "DIS_n", "brochure")) %>% 
    # filter(item == "PRE") %>% 
    mutate(CONTROL_PRE_OK = 
             case_when(
               #IN NOPREV "I DONT KNOW" IS OK!!???
               item == "PRE" & disease == "Breast cancer" & age == "20" & response == "1 in 4000" ~ 1,
               item == "PRE" & disease == "Breast cancer" & age == "40" & response == "1 in 50" ~ 1,
               item == "PRE" & disease == "Down syndrome" & age == "20" & response == "1 in 900" ~ 1,
               item == "PRE" & disease == "Down syndrome" & age == "40" & response == "1 in 60" ~ 1,
               item != "PRE" ~ NA_real_,
               TRUE ~ 0)) %>% 
    mutate(CONTROL_HIT_OK = 
             case_when(
               item == "HIT" & disease == "Breast cancer" & age == "20" & response == "99%" ~ 1,
               item == "HIT" & disease == "Breast cancer" & age == "40" & response == "99%" ~ 1,
               item == "HIT" & disease == "Down syndrome" & age == "20" & response == "90%" ~ 1,
               item == "HIT" & disease == "Down syndrome" & age == "40" & response == "90%" ~ 1,
               item != "HIT" ~ NA_real_,
               TRUE ~ 0)) %>% 
    mutate(CONTROL_FAL_OK = 
             case_when(
               item == "FAL" & disease == "Breast cancer" & age == "20" & response == "0,2%" ~ 1,
               item == "FAL" & disease == "Breast cancer" & age == "40" & response == "0,2%" ~ 1,
               item == "FAL" & disease == "Down syndrome" & age == "20" & response == "0,4%" ~ 1,
               item == "FAL" & disease == "Down syndrome" & age == "40" & response == "0,4%" ~ 1,
               item != "FAL" ~ NA_real_,
               TRUE ~ 0)) %>% 
    mutate(CONTROL_PPV = 
             case_when(
               item == "PPV" ~ as.numeric(response),
               TRUE ~ NA_real_))
  

  # PRE  # Prevalence
  # PPV  # How reliable
  # HIT  # Hit rate
  # FAL # FP
  
  df_CONTROL_EXP = df %>% 
    select(ResponseId, matches("CONTROL"), -starts_with("t_")) %>% 
    select(ResponseId, matches("EXP"))
  # EXP  # Explain how PPV changes with age
  


# UNCERTAINTY -------------------------------------------------------------

  # CHECK: Do DIS1 and DIS2 correspond really to DIS_n? ####
  
  df_UNCERTAINTY =  df %>% 
    select(ResponseId, matches("UNCERT"), -starts_with("t_")) %>% 
    gather(condition, UNCERTAINTY, 2:15) %>% 
    drop_na(UNCERTAINTY) %>% 
    mutate(DIS_n = stringr::str_sub(condition, 11, 11)) %>% 
    mutate(item = stringr::str_sub(condition, 13, 30)) %>% 
    mutate(UNCERTAINTY = as.numeric(UNCERTAINTY)) %>% 
    select(-condition) %>% 
    mutate(dimension = 
             case_when(
               item %in% c("ACCURAT_1", "ACCURAC_1", "RELIABIL_1") ~ "UNCERTAINTY_test_reliability_importance",
               item %in% c("TRUSTRE_1", "CERTAIN_1") ~ "UNCERTAINTY_screening_reliability",
               item %in% c("REGARDL_4", "RECOM_4") ~ "UNCERTAINTY_follow_up_decision",
               TRUE ~ NA_character_
             )) %>% 
    group_by(ResponseId, DIS_n, dimension) %>% summarise(UNCERTAINTY = sum(UNCERTAINTY)) %>% 
    spread(3, 4) %>% 
    mutate(UNCERTAINTY_all = UNCERTAINTY_follow_up_decision + UNCERTAINTY_screening_reliability + UNCERTAINTY_test_reliability_importance)
  
  
# JOIN ALL ------------------------------------------------------------------------------------

    df_ALL = df_conditions %>% 
      right_join(df_SCREENING_PPV, by = c("ResponseId", "DIS_n")) %>% 
      right_join(df_SCREENING_REC, by = c("ResponseId", "DIS_n")) %>% 
      right_join(df_SCREENING_SURE_REC, by = c("ResponseId", "DIS_n")) %>% 
      right_join(df_SCREENING_ENOUGH, by = c("ResponseId", "DIS_n")) %>% 
      right_join(df_SCREENING_CONF_PPV, by = c("ResponseId", "DIS_n")) %>% 
      right_join(df_FOLLOW_REC, by = c("ResponseId", "DIS_n")) %>% 
      right_join(df_FOLLOW_SURE_REC, by = c("ResponseId", "DIS_n")) %>% 
      right_join(df_FOLLOW_FACTORS, by = c("ResponseId", "DIS_n")) %>% 
      right_join(df_UNCERTAINTY, by = c("ResponseId", "DIS_n")) 
  
    
    
  threshold_PPV = 5
    
  df_ALL = df_ALL %>% 
    mutate(correct_PPV = 
      case_when(
        age == 20 & disease == "Breast cancer" ~ 11,
        age == 40 & disease == "Breast cancer" ~ 91,
        age == 20 & disease == "Down syndrome" ~ 20,
        age == 40 & disease == "Down syndrome" ~ 79.2,
      )) %>% 
    mutate(error_PPV = PPV_screening - correct_PPV) %>% 
    mutate(accuracy_PPV = 
             case_when(
               abs(error_PPV) <= threshold_PPV ~ 1,
               abs(error_PPV) > threshold_PPV ~ 0,
               TRUE ~ NA_real_)) %>%  
    mutate(RECOMEND_screening_AFTER = as.factor(
      case_when(
        RECOMEND_screening_AFTER == 1 ~ "Yes",
        RECOMEND_screening_AFTER == 0 ~ "No",
        TRUE ~ NA_character_))) %>% 
    mutate(RECOMEND_screening_BEFORE = as.factor(
      case_when(
        RECOMEND_screening_BEFORE == 1 ~ "Yes",
        RECOMEND_screening_BEFORE == 0 ~ "No",
        TRUE ~ NA_character_))) %>% 
    mutate(RECOMEND_followup = as.factor(
      case_when(
        RECOMEND_followup == 1 ~ "Yes",
        RECOMEND_followup == 0 ~ "No",
        TRUE ~ NA_character_))) %>% 
    mutate(ENOUGH_screening = as.factor(
      case_when(
        ENOUGH_screening == 1 ~ "Yes",
        ENOUGH_screening == 0 ~ "No",
        TRUE ~ NA_character_))) %>% 
    mutate(brochure = as.factor(brochure),
           age = as.factor(age)) %>% 
    
    # Calculate a continuous recommend var
    mutate(sure_recommend_screening_cont = 
             case_when(
               RECOMEND_screening_AFTER == "No" ~ - SURE_RECOMEND_screening_AFTER,
               TRUE ~ SURE_RECOMEND_screening_AFTER,
             )) %>% 
    mutate(sure_recommend_FU_cont = 
             case_when(
               RECOMEND_followup == "No" ~ - SURE_RECOMEND_followup,
               TRUE ~ SURE_RECOMEND_followup,
             ))
  

    
# Save files --------------------------------------------------------------

  write_csv(df_ALL, "output/DF_all.csv")
  write_rds(df_ALL, "output/DF_all.rds")
  
  # DT::datatable(df_ALL)
  
  

# Combine with ind diff -----------------------------------------------------------------------

source("R/2.data-preparation-ind-diff.R")  
  
  
  df_ALL = read_rds("output/DF_all.rds")
  df_ALL_inddiff = read_rds("output/DF_all_inddiff.rds")
  
  df_JOINED = df_ALL %>% left_join(df_ALL_inddiff, by = "ResponseId") %>% 
    mutate(ResponseId = as.factor(ResponseId)) %>% 
    mutate(brochure = as.factor(brochure)) %>%
    mutate(timeRecommendation = as.factor(timeRecommendation)) %>% 
    mutate(presencePrevalence = as.factor(presencePrevalence)) %>% 
    mutate(age = as.factor(age)) %>% 
    rename(n_item = DIS_n) %>% 
    mutate(Item_ID = as.factor(paste0(disease, "_", age)),
           disease = as.factor(disease),
           n_item = as.factor(n_item),
           ENOUGH_screening = as.factor(
             case_when(
               ENOUGH_screening == "No" ~ 0,
               ENOUGH_screening == "Yes" ~ 1
             ))) %>% 
    mutate(ENOUGH_accurate = 
             case_when(
               brochure == "NEW" & ENOUGH_screening == 1 ~ 1,
               presencePrevalence == "SIPREV" & ENOUGH_screening == 1 ~ 1,
               presencePrevalence == "NOPREV" & ENOUGH_screening == 0 ~ 1,
               TRUE ~ 0
             )) %>% 
    mutate(presencePrevalence = 
             as.factor(
               case_when(
                 presencePrevalence == "SIPREV" ~ "Yes",
                 presencePrevalence == "NOPREV" ~ "No"
               )))  
  
  write_csv(df_JOINED, "output/df_JOINED.csv")
  write_rds(df_JOINED, "output/df_JOINED.rds")
  