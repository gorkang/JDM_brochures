# This document contains the code for all the results used in the paper


# Preparation -------------------------------------------------------------

  # Data preparation (avoiding packages start-up messages)
  suppressPackageStartupMessages(source("analysis/1.data_preparation_for_analysis.R"))



# CONTROL questions -------------------------------------------------------

# 21 participants did not get any of the control questions:
df_CONTROL_0 = df_JOINED %>% filter(CONTROL_total == 0) %>% count(ResponseId)


# **** CHECKING IF CONTROL QUESTIONS CHANGE ANYTHING ------------------------------

  # df_JOINED = df_JOINED %>% filter(CONTROL_total > 0)
    # Nothing seems to change qualitatively!
  # DF_PCA = DF_PCA %>% filter(CONTROL_total > 0)
    # The third model changes a bit!

# ---------------------------------------------------------------------------------
  
  
  # Cite packages used
    # citation()
    # report::cite_packages(session =  sessionInfo())

  
# Had Enough, Said Enough --------------------------------------------------------------------------------

  # https://docs.google.com/document/d/1GD_9pg9tRnJMX29UAVTsWOlxNa-azNqY5-chI8FjrL8/edit?pli=1#heading=h.utdgppz1hgiz

  # Only standard BROCHURE (in pictorial brochure all participants have enough information)
  df_ENOUGH = df_JOINED %>% filter(brochure == "standard") 
  df_ENOUGH %>% filter(brochure == "standard") %>% count(presencePrevalence, ENOUGH_screening)
  
  # Chi-sq test
  CHI = chisq.test(df_ENOUGH$presencePrevalence, df_ENOUGH$ENOUGH_accurate, correct = FALSE); CHI
  CHI[["observed"]]
  
  message("The relation between these variables was significant, X2 (", CHI$parameter  , ", N = ", sum(CHI[["observed"]]),") = ", round(CHI$statistic, 3), ", p = ", CHI$p.value, ".")
  message("Participants were more likely to give an accurate response when the prevalence was present  [", CHI[["observed"]][3]/(CHI[["observed"]][3] + CHI[["observed"]][1]), "] than when it was not present [", CHI[["observed"]][4]/(CHI[["observed"]][4] + CHI[["observed"]][2]), "].")
  
  
  # __FIGURE 2 ----------------

  df_alluvium_temp = df_JOINED %>% dplyr::select(brochure, HAD_Enough_Information, SAID_Enough_Information) %>% 
    group_by(brochure, HAD_Enough_Information, SAID_Enough_Information) %>% 
    summarise(Freq = n()) %>% ungroup() %>% 
    mutate(SAID_Enough_Information = forcats::fct_relevel(SAID_Enough_Information, c("SAID_yes", "SAID_no"))) %>%
    mutate(HAD_Enough_Information = forcats::fct_relevel(HAD_Enough_Information, c("HAD_info", "HADNT_info")))
  
  
  # NEW vs OLD ---
  plot_had_enough_said_enough_brochure = 
    df_alluvium_temp %>% 
    ggplot(aes(y = Freq, axis1 = HAD_Enough_Information, axis2 = SAID_Enough_Information)) +
    geom_alluvium(aes(fill = paste0(HAD_Enough_Information, SAID_Enough_Information)), width = 1/12) +
    geom_stratum(width = 1/12, fill = "black", color = "grey", alpha = .8) +
  
    # Individual labels for all the Freqs  
    # geom_label(stat = "stratum", aes(y = Freq, 
    #                                  axis1 = HAD_Enough_Information, 
    #                                  axis2 = interaction(HAD_Enough_Information, SAID_Enough_Information), 
    #                                  label = after_stat(count)), inherit.aes = FALSE,
    #            nudge_x = c(0, 0, -.1, -.1, -.1, -.1, 0, -.1, -.1)) +
               
    geom_label(stat = "stratum", 
               # aes(label = paste0(after_stat( (count / max(count) ) * 100 ), "%")), # Percentage
               aes(label = after_stat(count)),
               fill = c("grey", "grey", "white", "white", "grey", "white", "white")) +
    geom_label(stat = "stratum", 
               label = c("Did NOT\nhave\nenough", "HAD\nenough", NA, NA, "HAD\nenough", NA, NA), 
               nudge_x = c(-.0, -.0, 0, 0, -.0, .0, 0),
               nudge_y = c(-10, 10, 0, 0, 10, 0, 0),
               fill = "grey",
               color = "black", 
               alpha = 1) +
    geom_label(stat = "stratum",
               label = c(NA, NA, "\nNO\n", "\nYES\n", NA, "\nNO\n", "\nYES\n"),
               nudge_x = c(0, 0, -.075, -.075, 0, -.075, -.075),
               fill = "white") +
    scale_x_discrete(limits = c("CONDITION", "RESPONSE:\nDid you \nhave enough info?"), expand = c(.05, .05)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_brewer(type = "qual", palette = "Set1") +
    ggtitle("") +
    facet_grid(~ brochure) +
    theme_minimal() +
    theme(text = element_text(size = 16),
          legend.position = "none")
  
  
  plot_had_enough_said_enough_brochure
  ggsave("output/plots/Figure 2 - hadenough_saidenough.png", plot_had_enough_said_enough_brochure, width = 20, height = 12, dp = 300) # For the google doc
  ggsave("output/plots/Figure 2 - hadenough_saidenough.eps", plot_had_enough_said_enough_brochure, width = 20, height = 12, dp = 300, device=cairo_ps) # For the journal
  

  
# PPV estimation ---------------------------------------------------------------------
  
  # https://docs.google.com/document/d/1GD_9pg9tRnJMX29UAVTsWOlxNa-azNqY5-chI8FjrL8/edit?pli=1#heading=h.foheafml6yty
    
  
  # __Final model --------------------------------------------------------------------------

    # We replicate the same model for `error PPV`, `recommend screening` and `recommend follow up`, only adding the previous measure when appropriate (e.g. the `recommend screening` model includes the `PPV screening`)
    model_PPV = lmer(`error PPV` ~ 
                       brochure * `normative PPV` + 
                       
                       `medical condition` +
                       item +
                       
                       # Barely affect results and add complexity
                       # presencePrevalence +
                       # timeRecommendation +
                       
                       # These are included in the following models, one by one
                       # `PPV screening` + 
                       # `recommend screening` +
                       
                       `a priori` +
                       agreeableness +
                       
                       (1|ResponseId) , df_JOINED)
    
    
    
    # __TABLE 2 ----------------
    
    summary(model_PPV); sjPlot::tab_model(model_PPV, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE) # show.std = TRUE, show.stat = TRUE 
    performance::check_collinearity(model_PPV)
    performance::check_heteroscedasticity(model_PPV) # Warning: Heteroscedasticity (non-constant error variance) detected (p = 0.000).
    # performance::check_homogeneity(model_PPV, method = "fligner") 
    performance::check_autocorrelation(model_PPV) # Warning: Autocorrelated residuals detected (p = 0.000)
    
    DF_parameters = parameters::parameters(model_PPV)
    DF_performance = performance::performance(model_PPV)
    
    DF_parameters[1,]
    message("R2s"); DF_performance %>% select(R2_conditional, R2_marginal)
    
    report_result(model_PPV, variable_name = "brochure (standard - pictorial)")
    report_result(model_PPV, variable_name = "normative PPV (high - low)")
    report_result(model_PPV, variable_name = "brochure (standard - pictorial):normative PPV (high - low)")
    
    report_result(model_PPV, variable_name = "item (2 - 1)")
    
    report_result(model_PPV, variable_name = "medical condition (Breast cancer - Down syndrome)")
    # report_result(model_PPV, variable_name = "`normative PPV` (high - low):`medical condition` (Breast cancer - Down syndrome)")
    
    

    report_result(model_PPV, has_the_word = "a priori")
    report_result(model_PPV, has_the_word = "agreeableness")
    
    # Other checks
      # Homogeneity if variances
      # car::leveneTest(residuals(h0_error) ~ df_JOINED$brochure)
      # car::leveneTest(residuals(h0_error) ~ df_JOINED$`normative PPV`)
      
    
    # __FIGURE 3 ----------------
    
    # Adds PPV suffix to facet labels
    appender <- function(string, suffix = " PPV") {
      if (string[1] %in% c("high", "low")) {
        paste0(string, suffix)
      } else {
        paste0(string)
      }
    }
    
    DF_vlines = 
      tibble(
        INT = c(11, 20, 79, 91),
        `normative PPV` = c("low", "low", "high", "high")) %>% 
      group_by(`normative PPV`)
    

    plot_ridge_brochures_age = df_JOINED %>% 
      # Reorder `normative PPV`
      mutate(`normative PPV` = fct_reorder(`normative PPV`, desc(`normative PPV`))) %>%
      ggplot(aes(`PPV screening`, brochure, fill = brochure)) +
      ggridges::geom_density_ridges(stat = "binline", bins = 20, scale = 0.95, draw_baseline = TRUE, alpha = .2) +
      ggridges::geom_density_ridges(alpha = .5, point_alpha = .3, jittered_points = TRUE) +
      geom_vline(data = DF_vlines, mapping = aes(xintercept = INT), linetype = "dashed", color = "#2F4F4F") +
      scale_y_discrete(expand = c(0, 0)) +
      theme_minimal() +
      labs(x = "PPV estimation",
           caption = "Dashed lines show normative correct responses.") + 
      scale_fill_hue(l = 40) +
      theme(text = element_text(size = 22), legend.position = "none") +
      facet_grid(~ `normative PPV`, labeller = as_labeller(appender))
    
    plot_ridge_brochures_age
    ggsave("output/plots/Figure3 - PPV - brochure - `normative PPV`.png", plot_ridge_brochures_age, width = 18, height = 12, dp = 300)  # For the google doc
    ggsave("output/plots/Figure3 - PPV - brochure - `normative PPV`.eps", plot_ridge_brochures_age, width = 18, height = 12, dp = 300, device=cairo_ps)  # For the journal
    


        
# SCREENING decision -------------------------------------------------------------------------

    # https://docs.google.com/document/d/1GD_9pg9tRnJMX29UAVTsWOlxNa-azNqY5-chI8FjrL8/edit?pli=1#heading=h.7tlhv09d120h
    
    model_screening = glmer(`recommend screening` ~ 
                              brochure * `normative PPV` +
                              
                              `medical condition` +
                              item +
                              
                              # presencePrevalence + 
                              # timeRecommendation +
                              
                              `PPV screening` + 
                              # `recommend screening` +
                              
                              `a priori` +
                              agreeableness +
                              
                              (1|ResponseId) , df_JOINED, family = "binomial", 
                            nAGQ = 11,
                            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    
    
    # __TABLE 3 ----------------
    
    summary(model_screening); sjPlot::tab_model(model_screening, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE) # show.std = TRUE, show.stat = TRUE
    performance::check_collinearity(model_screening)
    performance::check_autocorrelation(model_screening)
    sjPlot::plot_model(model_screening) + theme_minimal()
    # gmodels::CrossTable(df_JOINED$`recommend screening`, interaction(df_JOINED$brochure, df_JOINED$`normative PPV`, df_JOINED$`medical condition`, df_JOINED$item), expected = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE, sresid = TRUE, format = "SPSS")
    
    report_result(model_screening, variable_name = "brochure (standard - pictorial)")
    report_result(model_screening, variable_name = "normative PPV (high - low)")
    report_result(model_screening, variable_name = "medical condition (Breast cancer - Down syndrome)")
        
    report_result(model_screening, variable_name = "brochure (standard - pictorial):normative PPV (high - low)")
    
    report_result(model_screening, has_the_word = "PPV screening")
    report_result(model_screening, has_the_word = "a priori")
    report_result(model_screening, has_the_word = "agreeableness")
  
    # sjPlot::plot_model(model_screening, type = "pred", terms = c("brochure", "`normative PPV`")) + theme_minimal()
    # sjPlot::plot_model(model_screening, type = "pred", terms = c("medical condition", "`normative PPV`", "brochure")) + theme_minimal()
    # sjPlot::plot_model(model_screening, type = "pred", terms = c("a priori [all]", "`normative PPV`", "brochure")) + theme_minimal()
    # sjPlot::plot_model(model_screening, type = "pred", terms = c("`PPV screening` [all]", "`normative PPV`", "brochure")) + theme_minimal()
    
  
    emmeans::emmeans(model_screening, list(pairwise ~ brochure * `normative PPV`), adjust = "tukey")
    
    
    # __FIGURE 4 ----------------
    
    df_alluvium_temp = df_JOINED %>% dplyr::select(brochure, `normative PPV`, `recommend screening`, `recommend follow up`) %>% 
      group_by(brochure, `normative PPV`, `recommend screening`, `recommend follow up`) %>% 
      summarise(Freq = n()) 
    
    # Adds PPV suffix to facet labels
    appender <- function(string, suffix = " PPV") {
      if (string[1] %in% c("high", "low")) {
        paste0(string, suffix)
      } else {
        paste0(string)
      }
    }
    
    plot_screening_followup_numbers =
      ggplot(df_alluvium_temp, aes(y = Freq, axis1 = `recommend screening`, axis2 = `recommend follow up`, label = Freq)) +
      geom_alluvium(aes(fill = `recommend screening`), width = 1/12) +
      geom_stratum(width = 1/12, fill = "black", color = "grey", alpha = .8) +
      geom_label(stat = "stratum", aes(label = after_stat(count))) + #, nudge_x = rep(c(.05, .075, -.075, -.075), 4)
      geom_label(stat = "stratum", 
                 label = rep(c("\nYES\n", "\nNO\n"), 8), 
                 nudge_y = .2, 
                 nudge_x = rep(c(.07, .07, -.07, -.07), 4)) +
      scale_x_discrete(limits = c("Screening\nrecommendation", "Follow up\nrecommendation"), expand = c(.05, .05)) +
      scale_fill_brewer(type = "qual", palette = "Set1") +
      ggtitle("") +
      facet_grid(`normative PPV` ~ brochure, labeller = as_labeller(appender)) +
      theme_minimal() +
      theme(text = element_text(size = 16),
            legend.position = "none")
    
    plot_screening_followup_numbers
    ggsave("output/plots/Figure 4 - screening - followup.png", plot_screening_followup_numbers, width = 18, height = 12, dp = 300) # For the google doc
    ggsave("output/plots/Figure 4 - screening - followup.eps", plot_screening_followup_numbers, width = 18, height = 12, dp = 300, device=cairo_ps) # For the journal
    
    
    
    
  
  # FOLLOW UP decision -------------------------------------------------------------------------
  
    # https://docs.google.com/document/d/1GD_9pg9tRnJMX29UAVTsWOlxNa-azNqY5-chI8FjrL8/edit?pli=1#heading=h.iiao2pk10h9z
    
    model_fu = glmer(`recommend follow up` ~ 
                       brochure * `normative PPV` +
                       
                       `medical condition` +
                       item +
                       
                       # presencePrevalence + 
                       # timeRecommendation +
                       
                       `PPV screening` + 
                       `recommend screening` +
                       
                       `a priori` +
                       agreeableness +
                       
                       (1|ResponseId) , df_JOINED, family = "binomial", 
                     nAGQ = 11,
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    
    
    # __TABLE 4 ----------------
    
    summary(model_fu); sjPlot::tab_model(model_fu, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE) 
    performance::check_collinearity(model_fu)
    performance::check_autocorrelation(model_fu)
    sjPlot::plot_model(model_fu) + theme_minimal()
    
    report_result(model_fu, variable_name = "brochure (standard - pictorial)")
    report_result(model_fu, variable_name = "normative PPV (high - low)")
    report_result(model_fu, variable_name = "medical condition (Breast cancer - Down syndrome)")
    report_result(model_fu, has_the_word = "recommend screening")
    
    
    
    
# Reasons Follow Up -------------------------------------------------------------------------
    
  # https://docs.google.com/document/d/1GD_9pg9tRnJMX29UAVTsWOlxNa-azNqY5-chI8FjrL8/edit?pli=1#heading=h.k3t3sou6qybz
    
    
    # ___1.Baseline ------------------------------------------------------
    
    # Same as model_fu (above)
    model_fu_reasons_baseline =  glmer(`recommend follow up` ~ 
                                           brochure * `normative PPV` + 
                                           
                                           `medical condition` +
                                           item +
                                           # presencePrevalence + 
                                           # timeRecommendation +
                                           
                                           `PPV screening` + 
                                           `recommend screening` +
                                           
                                           `a priori` +
                                           agreeableness +
                                           
                                           # `Follow up component` +
                                           # `Condition component` +
                                           # `Screening component` +
                                           
                                           (1|ResponseId), DF_PCA, family = "binomial", 
                                         nAGQ = 11,
                                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    
    summary(model_fu_reasons_baseline); sjPlot::tab_model(model_fu_reasons_baseline, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE)
    
    
    # ___2.`Screening component` ------------------------------------------------------
    
    model_fu_reasons_screening = glmer(`recommend follow up` ~ 
                                         brochure * `normative PPV` + 
                                         
                                         `medical condition` +
                                         item +
                                         # presencePrevalence + 
                                         # timeRecommendation +
                                         
                                         `PPV screening` + 
                                         `recommend screening` +
                                         
                                         `a priori` +
                                         agreeableness +
                                         
                                         # `Follow up component` +
                                         # `Condition component` +
                                         `Screening component` +
                                         
                                         (1|ResponseId), DF_PCA, family = "binomial", 
                                       nAGQ = 11,
                                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    
    summary(model_fu_reasons_screening); sjPlot::tab_model(model_fu_reasons_screening, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE)

    
    
    # ___3.`Condition component` ------------------------------------------------------
    
    model_fu_reasons_condition = glmer(`recommend follow up` ~ 
                                         brochure * `normative PPV` +
                                         
                                         `medical condition` +
                                         item +
                                         # presencePrevalence + 
                                         # timeRecommendation +
                                         
                                         `PPV screening` + 
                                         `recommend screening` +
                                         
                                         `a priori` +
                                         agreeableness +
                                         
                                         # `Follow up component` +
                                         `Condition component` +
                                         # `Screening component` +
                                         
                                         (1|ResponseId), DF_PCA, family = "binomial", 
                                       nAGQ = 11,
                                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    
    summary(model_fu_reasons_condition); sjPlot::tab_model(model_fu_reasons_condition, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE)
    
    
    
    
    
    # ___4.`Follow up component` ------------------------------------------------------
    
    model_fu_reasons_fu = glmer(`recommend follow up` ~ 
                                  brochure * `normative PPV` +
                                  
                                  `medical condition` +
                                  item +
                                  # presencePrevalence + 
                                  # timeRecommendation +
                                  
                                  `PPV screening` + 
                                  `recommend screening` +
                                  
                                  `a priori` +
                                  agreeableness +
                                  
                                  `Follow up component` +
                                  # `Condition component` +
                                  # `Screening component` +
                                  
                                  (1|ResponseId), DF_PCA, family = "binomial", 
                                nAGQ = 11,
                                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    
    summary(model_fu_reasons_fu); sjPlot::tab_model(model_fu_reasons_fu, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE)
    
    
    
    
    
    
    # ___5.`Screening component`+`Condition component` ------------------------------------------------------
    
    model_fu_reasons_screening_condition = glmer(`recommend follow up` ~ 
                                                   brochure * `normative PPV` +
                                                   
                                                   `medical condition` +
                                                   item +
                                                   # presencePrevalence + 
                                                   # timeRecommendation +
                                                   
                                                   `PPV screening` + 
                                                   `recommend screening` +
                                                   
                                                   `a priori` +
                                                   agreeableness +
                                                   
                                                   # `Follow up component` +
                                                   `Condition component` +
                                                   `Screening component` +
                                                   
                                                   (1|ResponseId), DF_PCA, family = "binomial", 
                                                 nAGQ = 11,
                                                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    
    summary(model_fu_reasons_screening_condition); sjPlot::tab_model(model_fu_reasons_screening_condition, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE)
    
    
    
    
    
    
    # ____Complete model ------------------------------------------------------
    
    model_fu_reasons_complete = glmer(`recommend follow up` ~ 
                                        brochure * `normative PPV` +
                                        
                                        `medical condition` +
                                        item +
                                        
                                        # presencePrevalence +
                                        # timeRecommendation +
                                        
                                        `PPV screening` + 
                                        `recommend screening` +
                                        
                                        `a priori` +
                                        agreeableness +
                                        
                                        `Follow up component` +
                                        `Condition component` +
                                        `Screening component` + 
                                        
                                        (1|ResponseId), DF_PCA, family = "binomial", 
                                      nAGQ = 11,
                                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    
    summary(model_fu_reasons_complete); sjPlot::tab_model(model_fu_reasons_complete, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE)
    performance::check_collinearity(model_fu_reasons_complete)
    performance::check_autocorrelation(model_fu_reasons_complete)
    
    
    report_result(model_fu_reasons_complete, has_the_word = "recommend screening")
    report_result(model_fu_reasons_complete, has_the_word = "item")
    
    report_result(model_fu_reasons_complete, has_the_word = "Screening component")
    report_result(model_fu_reasons_complete, has_the_word = "Condition component")
    report_result(model_fu_reasons_complete, has_the_word = "Follow up component")
    
    
    
    
    # __TABLE 6 ----------------
    
    # * TODO: change "`recommend follow up`" for a sensible title for each model? ##########
    
    sjPlot::tab_model(
      c(model_fu_reasons_baseline,
        model_fu_reasons_screening,
        model_fu_reasons_screening_condition,
        model_fu_reasons_complete),
      show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE, 
      # show.std = TRUE, # Standardized betas
      show.est = TRUE # Odds ratios
      )
    
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

    