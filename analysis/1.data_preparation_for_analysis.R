# Environment Preparation -------------------------------------------------------------

options(pillar.sigfig = 5)


# __Packages and functions ---------------------------------------------------------------

# If this is the first time you run this script, make sure you have the necessary packages:
# source("R/0.install-dependencies.R")

library('dplyr')
library('forcats')
library('ggalluvial')
library('ggridges')
library('lme4')
library('patchwork')
library('performance')
library('psych')
library('purrr')
library('readr')
library('sjPlot')
library('tidyr')

source("R/helper-functions.R")



# __Data --------------------------------------------------------------------

df_JOINED = read_rds("output/data/df_JOINED.rds") %>% 
  mutate(brochure = 
           as.factor(
             case_when(
               brochure == "OLD" ~ "standard",
               brochure == "NEW" ~ "pictorial")),
         normative_PPV = 
           as.factor(
             case_when(
               age == 20 ~ "low",
               age == 40 ~ "high"))
         ) %>% 
  
  mutate(normative_PPV = forcats::fct_relevel(normative_PPV, "high", after = Inf)) %>% 
  
  mutate(HAD_Enough_Information = 
           case_when(
             brochure == "standard" & presencePrevalence == "No" ~ "HADNT_info",
             TRUE ~ "HAD_info"
           )) %>% 
  mutate(SAID_Enough_Information = 
           case_when(
             ENOUGH_screening == 1 ~ "SAID_yes",
             TRUE ~ "SAID_no"
           ))



# __Global parameters -------------------------------------------------------

# Set reference factor levels
df_JOINED <- within(df_JOINED, brochure <- relevel(brochure, ref = "standard"))
df_JOINED <- within(df_JOINED, timeRecommendation <- relevel(timeRecommendation, ref = "AFTER"))
df_JOINED <- within(df_JOINED, presencePrevalence <- relevel(presencePrevalence, ref = "Yes"))
df_JOINED <- within(df_JOINED, normative_PPV <- relevel(normative_PPV, ref = "high"))
df_JOINED <- within(df_JOINED, n_item <- relevel(n_item, ref = "2"))

# Contrast coding vs dummy coding: http://www.lrdc.pitt.edu/maplelab/slides/Simple_Main_Effects_Fraundorf.pdf
contrasts(df_JOINED$brochure) = named.contr.sum(levels(df_JOINED$brochure))
contrasts(df_JOINED$normative_PPV) = named.contr.sum(levels(df_JOINED$normative_PPV))
contrasts(df_JOINED$disease) = named.contr.sum(levels(df_JOINED$disease))
contrasts(df_JOINED$timeRecommendation) = named.contr.sum(levels(df_JOINED$timeRecommendation))
contrasts(df_JOINED$presencePrevalence) = named.contr.sum(levels(df_JOINED$presencePrevalence))
contrasts(df_JOINED$n_item) = named.contr.sum(levels(df_JOINED$n_item))
contrasts(df_JOINED$ENOUGH_screening) = named.contr.sum(levels(df_JOINED$ENOUGH_screening))




# ___Factors behind Follow up ---------------------------------------------------------------------------

df_FACTORS = df_JOINED %>% 
  gather(FACTORS_FU_Item, FACTORS_followup, FACTORS_FU_Item_1:FACTORS_FU_Item_8) %>%
  # select(matches("FACTORS")) %>%
  mutate(FACTORS_FU_Item2 = 
           case_when(
             FACTORS_FU_Item == "FACTORS_FU_Item_1" ~ "1. informative_test",
             FACTORS_FU_Item == "FACTORS_FU_Item_2" ~ "2. confident_results",
             FACTORS_FU_Item == "FACTORS_FU_Item_3" ~ "3. need_to_confirm",
             FACTORS_FU_Item == "FACTORS_FU_Item_4" ~ "4. seriosness_disease",
             FACTORS_FU_Item == "FACTORS_FU_Item_5" ~ "5. risk_complications_FU",
             FACTORS_FU_Item == "FACTORS_FU_Item_6" ~ "6. seriousness_complications_FU",
             FACTORS_FU_Item == "FACTORS_FU_Item_7" ~ "7. posibility_undetected",
             FACTORS_FU_Item == "FACTORS_FU_Item_8" ~ "8. posibility_FU_problem"
           ))

DF_PCA_RAW = df_FACTORS %>% 
  dplyr::select(ResponseId, Item_ID, brochure, normative_PPV, disease, n_item, RECOMEND_followup,  FACTORS_FU_Item2, FACTORS_followup, ind_apriori, ind_agreeableness, RECOMEND_screening_AFTER, PPV_screening, error_PPV, presencePrevalence) %>% 
  pivot_wider(names_from = FACTORS_FU_Item2, values_from = FACTORS_followup)



# ____PCA ---------------------------------------------------------------------

# Principal Components Analysis entering raw data and extracting PCs from the correlation matrix
a = DF_PCA_RAW %>% select(14:21)
fit <- princomp(a, cor = TRUE)
# summary(fit) # print variance accounted for
# loadings(fit) # pc loadings
# plot(fit,type="lines") # scree plot
# biplot(fit) 


# Varimax Rotated Principal Components
fit <- principal(a, nfactors = 3, rotate = "varimax")
# fit # print results 
DF_components = fit$scores %>% as_tibble()

DF_PCA = DF_PCA_RAW %>% 
  bind_cols(DF_components) %>% 
  rename(COMP_FollowUp = RC2,
         COMP_Condition = RC1,
         COMP_Screening = RC3)
