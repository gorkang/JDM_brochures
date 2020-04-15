# Create and store (/output/plots) plots 


# Libraries and data ---------------------------------------------------------------

source("analysis/1.data_preparation_for_analysis.R")


# Parameters --------------------------------------------------------------

  pd = position_dodge(.05)

  
# Screening PPV ---------------------------------------------------------------------
  
  
  ##  ** NEW/OLD x 20/40 ----------------------
  
  screening_ppv_brochure_age_plot = df_JOINED %>% 
    ggplot(aes(age, PPV_screening, color = ResponseId, group = ResponseId)) +
    geom_point(size = 2, position = pd) +
    geom_line(alpha = .4, position = pd) +
    stat_summary(aes(age, PPV_screening, group = brochure),
                 size = 3, alpha = .5, fun.y = mean, geom = "line", position = pd, inherit.aes = FALSE) +
    stat_summary(aes(age, PPV_screening, group = brochure),
                 size = 3, alpha = .8, fun.y = mean, geom = "point", position = pd, inherit.aes = FALSE) +
    theme_minimal() +
    scale_y_continuous(breaks = seq(0,100, 10)) +
    facet_wrap(brochure ~ .) + #presencePrevalence
    
    labs(title = "Screening - PPV calculation",
         caption = "") +
    theme(legend.position = "none", 
          text = element_text(size = 16))
  
  screening_ppv_brochure_age_plot
  ggsave("output/plots/screening_ppv_brochure_age_plot.png", screening_ppv_brochure_age_plot, width = 18, height = 12, dp = 300)
  
  
  ## ** ggridges -----------------------
  
  plot_ridge_brochures = df_JOINED %>% 
    ggplot(aes(PPV_screening, brochure, fill = brochure)) + #, shape = age
    ggridges::geom_density_ridges(stat = "binline", bins = 20, scale = 0.95, draw_baseline = TRUE, alpha = .2) +
    ggridges::geom_density_ridges(alpha = .5, point_alpha = .3, jittered_points = TRUE) +
    theme_minimal() +
    scale_fill_hue(l = 40) +
    theme(text = element_text(size = 20))
  
  plot_ridge_brochures
  ggsave("output/plots/plot_ridge_brochures.png", plot_ridge_brochures, width = 18, height = 12, dp = 300)
  
  plot_ridge_brochures_age = df_JOINED %>% 
    ggplot(aes(PPV_screening, brochure, fill = brochure)) +
    ggridges::geom_density_ridges(stat = "binline", bins = 20, scale = 0.95, draw_baseline = TRUE, alpha = .2) +
    ggridges::geom_density_ridges(alpha = .5, point_alpha = .3, jittered_points = TRUE) +
    theme_minimal() +
    scale_fill_hue(l = 40) +
    theme(text = element_text(size = 20)) +
    facet_grid(~age) 
  
  plot_ridge_brochures_age
  ggsave("output/plots/plot_ridge_brochures_age.png", plot_ridge_brochures_age, width = 18, height = 12, dp = 300)
  
  
  
  
  # Error PPV ---------------------------------------------------------------------
  
  screening_error_brochure_age_plot = df_JOINED %>% 
    ggplot(aes(disease, error_PPV, color = age, shape = disease)) +
    geom_jitter(size = 2, width = .4, height = .5) +
    annotate(geom = "rect", xmin = -Inf, xmax = Inf, ymin = -5, ymax = 5, alpha = .2) +
    theme_minimal() +
    facet_wrap(age ~ brochure, scales = "free") +
    labs(title = "Screening - Error in PPV calculation",
         caption = "") +
    theme(legend.position = "none", 
          text = element_text(size = 16))
  
  screening_error_brochure_age_plot
  ggsave("output/plots/screening_error_brochure_age_plot.png", screening_error_brochure_age_plot, width = 18, height = 12, dp = 300)
  
   

  

# Recommendation ----------------------------------------------------------

  df_alluvium_temp = df_JOINED %>% dplyr::select(brochure, RECOMEND_screening_AFTER, RECOMEND_followup) %>% 
    # gather(test, recommendation, 3:4) %>% 
    group_by(brochure, RECOMEND_screening_AFTER, RECOMEND_followup) %>% 
    summarise(Freq = n()) 

  # NEW vs OLD ---
    plot_screening_followup_brochure = ggplot(df_alluvium_temp, aes(y = Freq, axis1 = RECOMEND_screening_AFTER, axis2 = RECOMEND_followup, label = Freq)) +
      geom_alluvium(aes(fill = RECOMEND_screening_AFTER), width = 1/12) +
      geom_stratum(width = 1/12, fill = "black", color = "grey", alpha = .8) +
      geom_label(stat = "stratum", label.strata = TRUE) +
      # geom_text(stat = "stratum", label = rep(c("SI", "NO"), 8), nudge_x = rep(c(-.075, -.075, .075, .075), 4)) +
      geom_label(stat = "stratum", label = rep(c("YES", "NO"), 4), nudge_x = rep(c(-.075, -.075, .075, .075), 4)) +
      
      # geom_text(aes(Freq, label = Freq)) +
      scale_x_discrete(limits = c("Screening", "Follow up"), expand = c(.05, .05)) +
      scale_fill_brewer(type = "qual", palette = "Set1") +
      ggtitle("") +
      facet_grid( ~ brochure) +
      theme_minimal() +
      theme(text = element_text(size = 16),
            legend.position = "none")
    
    plot_screening_followup_brochure
    ggsave("output/plots/plot_screening_followup_brochure.png", plot_screening_followup_brochure, width = 18, height = 12, dp = 300)
    
    
    
  
  # NEW vs OLD & Age ---

    # USE label = Freq to show numbers!
  
  df_alluvium_temp = df_JOINED %>% dplyr::select(brochure, age, RECOMEND_screening_AFTER, RECOMEND_followup) %>% 
    # gather(test, recommendation, 3:4) %>% 
    group_by(brochure, age, RECOMEND_screening_AFTER, RECOMEND_followup) %>% 
    summarise(Freq = n()) 
  
  
  plot_screening_followup_numbers = ggplot(df_alluvium_temp, aes(y = Freq, axis1 = RECOMEND_screening_AFTER, axis2 = RECOMEND_followup, label = Freq)) +
    geom_alluvium(aes(fill = RECOMEND_screening_AFTER), width = 1/12) +
    geom_stratum(width = 1/12, fill = "black", color = "grey", alpha = .8) +
    geom_label(stat = "stratum", label.strata = TRUE) +
    # geom_text(stat = "stratum", label = rep(c("SI", "NO"), 8), nudge_x = rep(c(-.075, -.075, .075, .075), 4)) +
    geom_label(stat = "stratum", label = rep(c("YES", "NO"), 8), nudge_x = rep(c(-.075, -.075, .075, .075), 4)) +
    
    # geom_text(aes(Freq, label = Freq)) +
    scale_x_discrete(limits = c("Screening", "Follow up"), expand = c(.05, .05)) +
    scale_fill_brewer(type = "qual", palette = "Set1") +
    ggtitle("") +
    facet_grid(age ~ brochure) +
    theme_minimal() +
    theme(text = element_text(size = 16),
          legend.position = "none")
  
  plot_screening_followup_numbers
  ggsave("output/plots/plot_screening_followup_numbers.png", plot_screening_followup_numbers, width = 18, height = 12, dp = 300)
  
  
# Correlations ------------------------------------------------------------

  df_JOINED %>% ggplot(aes(PPV_screening, sure_recommend_screening_cont)) +
    geom_jitter() +
    geom_smooth(method = "lm") +
    facet_grid( ~ brochure) +
    theme_minimal()
  

