
# DECISIONS ---------------------------------------------------------------

  # brochure * age + disease  VS brochure * age * disease 
  # The triple interaction model predicts a bit more but complexity increases quite a bit. 


# Preparation -------------------------------------------------------------

source("analysis/1.data_preparation_for_analysis.R")

  # Cite packages used
  # citation()
  # report::cite_packages(session =  sessionInfo())

  
# RESULTS USED IN PAPER -----------------------------------------------------------


# Had Enough, Said Enough --------------------------------------------------------------------------------

  # Only OLD BROCHURE
  df_ENOUGH = df_JOINED %>% filter(brochure == "standard") 

  # Chi-sq test
  CHI = chisq.test(df_ENOUGH$presencePrevalence, df_ENOUGH$ENOUGH_accurate, correct = FALSE); CHI
  CHI[["observed"]]
  
  message("The relation between these variables was significant, X2 (", CHI$parameter  , "N = ", sum(CHI[["observed"]]),") = ", round(CHI$statistic, 3), ", p = ", CHI$p.value, ".")
  message("Participants were more likely to give an accurate response when the prevalence was present  [", CHI[["observed"]][3]/(CHI[["observed"]][3] + CHI[["observed"]][1]), "] and when it was not present [", CHI[["observed"]][4]/(CHI[["observed"]][4] + CHI[["observed"]][2]), "].")
  
  
  
# PPV ---------------------------------------------------------------------
  
  # __Presence of prevalence PPV ----------------------------------------------------------------------
  
    presence_prevalence_error = lmer(error_PPV ~ presencePrevalence * age  + (1|ResponseId) , df_JOINED %>% filter(brochure == "standard"))
    DF_presence_prevalence_error = parameters::p_value(presence_prevalence_error)[2,] %>% mutate(p = round(p, 3))
    # summary(presence_prevalence_error); sjPlot::tab_model(presence_prevalence_error, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE) # show.std = TRUE, show.stat = TRUE
    
    presence_prevalence_calculation = lmer(PPV_screening ~ presencePrevalence * age  + (1|ResponseId), df_JOINED %>% filter(brochure == "standard"))
    DF_presence_prevalence_calculation = parameters::p_value(presence_prevalence_calculation)[2,] %>% mutate(p = round(p, 3))
    # summary(presence_prevalence_calculation); sjPlot::tab_model(presence_prevalence_calculation, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE) # show.std = TRUE, show.stat = TRUE
    
    presence_prevalence_accuracy = glmer(accuracy_PPV ~ presencePrevalence * age +  (1|Item_ID), df_JOINED %>% filter(brochure == "standard"), family = "binomial")
    DF_presence_prevalence_accuracy = parameters::p_value(presence_prevalence_accuracy)[2,] %>% mutate(p = round(p, 3))
    # summary(presence_prevalence_calculation); sjPlot::tab_model(presence_prevalence_calculation, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE) # show.std = TRUE, show.stat = TRUE
    
    message("We then checked if the presence of prevalence would make any difference in the participant’s calculation (p = ", DF_presence_prevalence_calculation$p, "), error (p = ", DF_presence_prevalence_error$p, ") or accuracy (p = ", DF_presence_prevalence_accuracy$p, ") when assessing the PPV of the test with the standard brochure.")
    # Can say this as a justification to include all participants in COMPLETE MODEL?
    
    
    
    
  # __PPV error --------------------------------------------------------------------------

    # model = "error_PPV ~ brochure * age * disease + n_item + ind_apriori + ind_agreeableness + (1|ResponseId)"
    
    model_PPV = lmer(error_PPV ~ 
                       brochure * age + 
                       
                       disease +
                       n_item +
                       # presencePrevalence +
                       # timeRecommendation +
                       
                       # PPV_screening + # NO ES RAZONABLE AQUI!!!!!
                       
                       ind_apriori +
                       ind_agreeableness +
                       
                       (1|ResponseId) , df_JOINED)
    
    # performance::check_model(model_PPV)
    # report::report(model_PPV)
    
    
    summary(model_PPV); sjPlot::tab_model(model_PPV, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE) # show.std = TRUE, show.stat = TRUE  performance::model_performance(h0_error)
    performance::check_collinearity(model_PPV)
    performance::check_heteroscedasticity(model_PPV)
    performance::check_autocorrelation(model_PPV) # Warning: Autocorrelated residuals detected (p = 0.000)
    
    # sjPlot::plot_model(model_PPV) + theme_minimal() + sjPlot::plot_model(model_PPV, type = "int") + theme_minimal()
    
    DF_parameters = parameters::parameters(model_PPV)
    DF_performance = performance::performance(model_PPV)
    
    DF_parameters[1,]
    message("R2s"); DF_performance %>% select(R2_conditional, R2_marginal)
    
    report_result(model_PPV, variable_name = "brochure (standard - pictorial)")
    report_result(model_PPV, variable_name = "age (40 - 20)")
    report_result(model_PPV, variable_name = "brochure (standard - pictorial):age (40 - 20)")
    
    report_result(model_PPV, variable_name = "disease (Breast cancer - Down syndrome)")
    # report_result(model_PPV, variable_name = "age (40 - 20):disease (Breast cancer - Down syndrome)")
    
    

    report_result(model_PPV, has_the_word = "ind_apriori")
    report_result(model_PPV, has_the_word = "ind_agreeableness")
    
    # Other checks
      # Homogeneity if variances
      # car::leveneTest(residuals(h0_error) ~ df_JOINED$brochure)
      # car::leveneTest(residuals(h0_error) ~ df_JOINED$age)
      


# TODO: Confidence PPV estimation -----------------------------------------

    confidence_PPV = lmer(CONF_PPV_screening ~ brochure * age +
                            
                            PPV_screening +
                            
                            disease +
                            n_item  +
                            
                            ind_apriori + 
                            ind_agreeableness +
                            
                            (1|ResponseId) , df_JOINED) #+ (1|n_item)
    
    performance::check_collinearity(confidence_PPV)
    performance::check_heteroscedasticity(confidence_PPV) # Warning: Heteroscedasticity (non-constant error variance) detected (p = 0.000)
    performance::check_autocorrelation(confidence_PPV) # Warning: Autocorrelated residuals detected (p = 0.000)
    summary(confidence_PPV); sjPlot::tab_model(confidence_PPV, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE)   
    
  

# DECISIONS -----------------------------------------------------------------------------------

    
  # __SCREENING dico -------------------------------------------------------------------------
  

    model_screening = glmer(RECOMEND_screening_AFTER ~ 
                              brochure * age +
                              
                              disease +
                              n_item +
                              
                              # presencePrevalence + 
                              # timeRecommendation +
                              
                              PPV_screening + 
                              
                              ind_apriori +
                              ind_agreeableness +
                              
                              (1|ResponseId) , df_JOINED, family = "binomial", 
                            nAGQ = 11,
                            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    
    summary(model_screening); sjPlot::tab_model(model_screening, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE) # show.std = TRUE, show.stat = TRUE
    performance::check_collinearity(model_screening)
    performance::check_autocorrelation(model_screening)
    
    
    report_result(model_screening, variable_name = "brochure (standard - pictorial)")
    report_result(model_screening, variable_name = "age (40 - 20)")
    report_result(model_screening, variable_name = "disease (Breast cancer - Down syndrome)")
        
    report_result(model_screening, variable_name = "brochure (standard - pictorial):age (40 - 20)")
    
    # report_result(model_screening, variable_name = "presencePrevalence (Yes - No)")
    report_result(model_screening, has_the_word = "PPV_screening")
    report_result(model_screening, has_the_word = "ind_apriori")
  
    # report::report(model_screening)

    sjPlot::plot_model(model_screening, type = "pred", terms = c("brochure", "age")) + theme_minimal()
    # sjPlot::plot_model(model_screening, type = "pred", terms = c("disease", "age", "brochure")) + theme_minimal()
    sjPlot::plot_model(model_screening, type = "pred", terms = c("ind_apriori [all]", "age", "brochure")) + theme_minimal()
    sjPlot::plot_model(model_screening, type = "pred", terms = c("PPV_screening [all]", "age", "brochure")) + theme_minimal()
    
  

  
    # __FOLLOW UP dico -------------------------------------------------------------------------
    
    
    model_fu = glmer(RECOMEND_followup ~ 
                       brochure * age +
                       
                       disease +
                       n_item +
                       # presencePrevalence + 
                       # timeRecommendation +
                       
                       PPV_screening + 
                       RECOMEND_screening_AFTER +
                       
                       ind_apriori +
                       ind_agreeableness +
                       
                       (1|ResponseId) , df_JOINED, family = "binomial", 
                     nAGQ = 11,
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    
    
    summary(model_fu); sjPlot::tab_model(model_fu, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE) 
    performance::check_collinearity(model_fu)
    performance::check_autocorrelation(model_fu)
    sjPlot::plot_model(model_fu) + theme_minimal()
    
    report_result(model_fu, variable_name = "brochure (standard - pictorial)")
    report_result(model_fu, variable_name = "age (40 - 20)")
    report_result(model_fu, variable_name = "disease (Breast cancer - Down syndrome)")
    report_result(model_fu, has_the_word = "RECOMEND_screening_AFTER")
    
    
    
    # ___Reasons Follow Up -------------------------------------------------------------------------
    
    
    # _____Baseline ------------------------------------------------------
    
    # Same as model_fu (above)
    model_fu_reasons_baseline =  glmer(RECOMEND_followup ~ 
                                           brochure * age + 
                                           
                                           disease +
                                           n_item +
                                           # presencePrevalence + 
                                           # timeRecommendation +
                                           
                                           PPV_screening + 
                                           RECOMEND_screening_AFTER +
                                           
                                           ind_apriori +
                                           ind_agreeableness +
                                           
                                           # COMP_FollowUp +
                                           # COMP_Condition +
                                           # COMP_Screening +
                                           
                                           (1|ResponseId), DF_PCA, family = "binomial", 
                                         nAGQ = 11,
                                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    
    summary(model_fu_reasons_baseline); sjPlot::tab_model(model_fu_reasons_baseline, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE)
    
    
    # _____COMP_Screening ------------------------------------------------------
    
    model_fu_reasons_screening = glmer(RECOMEND_followup ~ 
                                         brochure * age + 
                                         
                                         disease +
                                         n_item +
                                         # presencePrevalence + 
                                         # timeRecommendation +
                                         
                                         PPV_screening + 
                                         RECOMEND_screening_AFTER +
                                         
                                         ind_apriori +
                                         ind_agreeableness +
                                         
                                         # COMP_FollowUp +
                                         # COMP_Condition +
                                         COMP_Screening +
                                         
                                         (1|ResponseId), DF_PCA, family = "binomial", 
                                       nAGQ = 11,
                                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    
    summary(model_fu_reasons_screening); sjPlot::tab_model(model_fu_reasons_screening, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE)

    
    
    # _____COMP_Condition ------------------------------------------------------
    
    model_fu_reasons_condition = glmer(RECOMEND_followup ~ 
                                         brochure * age +
                                         
                                         disease +
                                         n_item +
                                         # presencePrevalence + 
                                         # timeRecommendation +
                                         
                                         PPV_screening + 
                                         RECOMEND_screening_AFTER +
                                         
                                         ind_apriori +
                                         ind_agreeableness +
                                         
                                         # COMP_FollowUp +
                                         COMP_Condition +
                                         # COMP_Screening +
                                         
                                         (1|ResponseId), DF_PCA, family = "binomial", 
                                       nAGQ = 11,
                                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    
    summary(model_fu_reasons_condition); sjPlot::tab_model(model_fu_reasons_condition, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE)
    
    
    
    
    
    # _____COMP_FollowUp ------------------------------------------------------
    
    model_fu_reasons_fu = glmer(RECOMEND_followup ~ 
                                  brochure * age +
                                  
                                  disease +
                                  n_item +
                                  # presencePrevalence + 
                                  # timeRecommendation +
                                  
                                  PPV_screening + 
                                  RECOMEND_screening_AFTER +
                                  
                                  ind_apriori +
                                  ind_agreeableness +
                                  
                                  COMP_FollowUp +
                                  # COMP_Condition +
                                  # COMP_Screening +
                                  
                                  (1|ResponseId), DF_PCA, family = "binomial", 
                                nAGQ = 11,
                                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    
    summary(model_fu_reasons_fu); sjPlot::tab_model(model_fu_reasons_fu, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE)
    
    
    
    
    
    
    # _____COMP_Screening+COMP_Condition ------------------------------------------------------
    
    model_fu_reasons_screening_condition = glmer(RECOMEND_followup ~ 
                                                   brochure * age +
                                                   
                                                   disease +
                                                   n_item +
                                                   # presencePrevalence + 
                                                   # timeRecommendation +
                                                   
                                                   PPV_screening + 
                                                   RECOMEND_screening_AFTER +
                                                   
                                                   ind_apriori +
                                                   ind_agreeableness +
                                                   
                                                   # COMP_FollowUp +
                                                   COMP_Condition +
                                                   COMP_Screening +
                                                   
                                                   (1|ResponseId), DF_PCA, family = "binomial", 
                                                 nAGQ = 11,
                                                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    
    summary(model_fu_reasons_screening_condition); sjPlot::tab_model(model_fu_reasons_screening_condition, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE)
    
    
    
    
    
    
    # _____Complete model ------------------------------------------------------
    
    model_fu_reasons_complete = glmer(RECOMEND_followup ~ 
                                        brochure * age +
                                        
                                        disease +
                                        n_item +
                                        # presencePrevalence + 
                                        # timeRecommendation +
                                        
                                        PPV_screening + 
                                        RECOMEND_screening_AFTER +
                                        
                                        ind_apriori +
                                        ind_agreeableness +
                                        
                                        COMP_FollowUp +
                                        COMP_Condition +
                                        COMP_Screening + 
                                        
                                        (1|ResponseId), DF_PCA, family = "binomial", 
                                      nAGQ = 11,
                                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    
    summary(model_fu_reasons_complete); sjPlot::tab_model(model_fu_reasons_complete, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE)
    performance::check_collinearity(model_fu_reasons_complete)
    performance::check_autocorrelation(model_fu_reasons_complete)
    
    
    report_result(model_fu_reasons_complete, has_the_word = "RECOMEND_screening_AFTER")
    report_result(model_fu_reasons_complete, has_the_word = "n_item")
    
    report_result(model_fu_reasons_complete, has_the_word = "COMP_Screening")
    report_result(model_fu_reasons_complete, has_the_word = "COMP_Condition")
    report_result(model_fu_reasons_complete, has_the_word = "COMP_FollowUp")
    
    
    
    
    # _____Joined table -------------------------------------------------------------------
    
    sjPlot::tab_model(
      c(model_fu_reasons_baseline,
        model_fu_reasons_screening,
        model_fu_reasons_screening_condition,
        model_fu_reasons_complete),
      show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE)
    
    anova(model_fu_reasons_baseline,
      model_fu_reasons_screening,
      model_fu_reasons_screening_condition,
      model_fu_reasons_complete)
    
    
    DF_anova1 = anova(model_fu_reasons_baseline, model_fu_reasons_screening)
    message("Adding the Screening component does not improve the model X2(", DF_anova1$`Chi Df`[2], ") = ", round(DF_anova1$Chisq[2], 3), ", p = ", round(DF_anova1$`Pr(>Chisq)`[2], 3))
    
    DF_anova2 = anova(model_fu_reasons_screening, model_fu_reasons_screening_condition)
    message("Adding the Condition component does improve the model X2(", DF_anova2$`Chi Df`[2], ") = ", round(DF_anova2$Chisq[2], 3), ", p = ", DF_anova2$`Pr(>Chisq)`[2])
    
    DF_anova3 = anova(model_fu_reasons_screening_condition, model_fu_reasons_complete)
    message("Adding the Follow up component does improve the model X2(", DF_anova3$`Chi Df`[2], ") = ", round(DF_anova3$Chisq[2], 3), ", p = ", DF_anova3$`Pr(>Chisq)`[2])
    
    