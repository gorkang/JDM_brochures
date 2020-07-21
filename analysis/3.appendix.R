


# APPENDIX ----------------------------------------------------------------



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




# Global PPV model with calculation and accuracy --------------------------

  model_PPV_calculation = lmer(PPV_screening ~ 
                              brochure * age + disease +
                              
                              n_item +
                              # presencePrevalence +
                              # timeRecommendation +
                              
                              # PPV_screening + # NO ES RAZONABLE AQUI!!!!!
                              
                              ind_apriori +
                              ind_agreeableness +
                              
                              (1|ResponseId) , df_JOINED)

  summary(model_PPV_calculation); sjPlot::tab_model(model_PPV_calculation, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE) # show.std = TRUE, show.stat = TRUE  performance::model_performance(h0_error)


  model_PPV_accuracy = glmer(accuracy_PPV ~ 
                                 brochure * age + disease +
                                 
                                 n_item +
                                 # presencePrevalence +
                                 # timeRecommendation +
                                 
                                 # PPV_screening + # NO ES RAZONABLE AQUI!!!!!
                                 
                                 ind_apriori +
                                 ind_agreeableness +
                                 
                                 (1|ResponseId) , df_JOINED, family = "binomial", 
                                nAGQ = 11,
                                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

  summary(model_PPV_accuracy); sjPlot::tab_model(model_PPV_accuracy, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE) # show.std = TRUE, show.stat = TRUE  performance::model_performance(h0_error)


  

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
