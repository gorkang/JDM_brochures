# APPENDIX ----------------------------------------------------------------

# ** TODO: remember to put the final number for the tables! ------------------------



# Comprehension -----------------------------------------------------------

  # Main tables after filtering our COMP_Total == 0









# presencePrevalence and timeRecommendation -------------------------------

# We don't include presencePrevalence and timeRecommendation to avoid complexity


# PPV estimation
model_PPV = lmer(`error PPV` ~ 
                   brochure * `normative PPV` + 
                   
                   `medical condition` +
                   item +
                   
                   # Barely affect results and add complexity
                   presencePrevalence +
                   timeRecommendation +
                   
                   `a priori` +
                   agreeableness +
                   
                   (1|ResponseId) , df_JOINED)

# __TABLE 2 alternate ----------------
summary(model_PPV); sjPlot::tab_model(model_PPV, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE) # show.std = TRUE, show.stat = TRUE 



# SCREENING decision
model_screening = glmer(`recommend screening` ~ brochure * `normative PPV` + 
                          
                          `medical condition` + 
                          item +
                          
                          presencePrevalence +
                          timeRecommendation +
                          
                          `PPV screening` +
                          
                          `a priori` +
                          agreeableness +
                          
                          (1|ResponseId) , df_JOINED, family = "binomial", 
                        nAGQ = 11,
                        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# __TABLE 3 alternate ----------------
summary(model_screening); sjPlot::tab_model(model_screening, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE) # show.std = TRUE, show.stat = TRUE 



# FOLLOW UP decision
model_fu = glmer(`recommend follow up` ~ 
                   brochure * `normative PPV` +
                   
                   `medical condition` +
                   item +
                   
                   presencePrevalence +
                   timeRecommendation +
                   
                   `PPV screening` + 
                   `recommend screening` +
                   
                   `a priori` +
                   agreeableness +
                   
                   (1|ResponseId) , df_JOINED, family = "binomial", 
                 nAGQ = 11,
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))


# __TABLE 4 alternate ----------------
summary(model_fu); sjPlot::tab_model(model_fu, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE) 







# Global full model -------------------------------------------------------

model_PPV_complete = lmer(error_PPV ~ 
                   brochure * age + disease +
                   
                   n_item +
                   presencePrevalence +
                   timeRecommendation +
                   
                   # PPV_screening + # NO ES RAZONABLE AQUI!!!!!
                   
                   ind_apriori +
                   ind_agreeableness +
                   
                   (1|ResponseId) , df_JOINED)


summary(model_PPV_complete); sjPlot::tab_model(model_PPV_complete, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE) # show.std = TRUE, show.stat = TRUE  performance::model_performance(h0_error)


sjPlot::tab_model(c(model_PPV, model_PPV_complete), show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE)




# Global PPV estimation error model with calculation and accuracy --------------------------

  model_PPV_calculation = lmer(`PPV screening` ~ 
                                 brochure * `normative PPV` + 
                                 
                                 `medical condition` +
                                 item +
                                 
                                 # presencePrevalence +
                                 # timeRecommendation +
                              
                                 `a priori` +
                                 agreeableness +
                                 
                                 (1|ResponseId) , df_JOINED)

  
  # __TABLE XXXX estimation PPV ----------------
  
  summary(model_PPV_calculation); sjPlot::tab_model(model_PPV_calculation, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE)


  model_PPV_accuracy = glmer(accuracy_PPV ~ 
                               brochure * `normative PPV` + 
                               
                               `medical condition` +
                               item +
                               
                               # presencePrevalence +
                               # timeRecommendation +
                               
                               `a priori` +
                               agreeableness +
                               
                               (1|ResponseId) , df_JOINED, family = "binomial", 
                                nAGQ = 11,
                                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))


  # __TABLE XXXX accuracy PPV ----------------
  
  summary(model_PPV_accuracy); sjPlot::tab_model(model_PPV_accuracy, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE)
  
  

# # ____RECOMEND_screening_AFTER Common analysis both brochures ------------------------------------------
# 
# model_all = glmer(RECOMEND_screening_AFTER ~ brochure * age +
#                     # disease +
#                     # presencePrevalence + timeRecommendation +
#                     # PPV_screening +
#                     # error_PPV + # USING THIS CHANGES SOME OF THE RESULTS QUITE A BIT
#                     ind_apriori +
#                     ind_agreeableness +
#                     (1|ResponseId) + (1|disease), df_JOINED, family = "binomial")
# 
# model_all2 = try_all_optimizers(model_all)
# summary(model_all2); sjPlot::tab_model(model_all2, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE) # show.std = TRUE, show.stat = TRUE
# 
# # WHY THESE 2 SHOW DIFFERENT RESULTS?
# # Marginal effects of interaction terms in model.
# # sjPlot::plot_model(model_all2, type = "int") + theme_minimal()
# # Predicted values (marginal effects) for specific model terms. See ggpredict for details.
# # sjPlot::plot_model(model_all2, type = "pred", terms= c("age", "brochure")) + theme_minimal()
# 
# # performance::check_model(model_all2)
