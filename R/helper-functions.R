
# Name contrasts in models  -----------------------------------------------
named.contr.sum<-function(x, ...) {
  if (is.factor(x)) {
    x <- levels(x)
  } else if (is.numeric(x) & length(x)==1L) {
    stop("cannot create names with integer value. Pass factor levels")
  }
  x<-contr.sum(x, ...)
  colnames(x) <- apply(x,2,function(x) 
    paste0(" (", names(x[x>0]), " - ", names(x[x<0]), ")")
  )
  x
}




# Try all optimizers ------------------------------------------------------

try_all_optimizers <- function(model, maxfun_value = 2e5) {
  # https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
  # help("convergence")
  
  # DEBUG
  # model = model_old
  
  suppressWarnings({ gm_all <- allFit(model, maxfun = maxfun_value) })
  is.OK <- sapply(gm_all,is,"merMod")
  aa.OK <- gm_all[is.OK]
  ERRORS = lapply(aa.OK,function(x) x@optinfo$conv$lme4$messages)
  optimizers_without_messages = names(which(sapply(ERRORS, is.null))) 
  
  if (length(optimizers_without_messages) == 1) {
   model =  gm_all[optimizers_without_messages]
   
   message("\n\n  * ", optimizers_without_messages, " works!")
   
   model[[1]]
   
   
  } else if (length(optimizers_without_messages) > 1) {
    message("\n\n  * More than 1 optimizer")
    model =  gm_all[optimizers_without_messages]
    model
  } else {
    message("\n\n  * All optimizers gave warnings")
    message("  * Decreasing stopping tolerances...")
    
    
    ## FROM help("convergence") ------------------------------------ 
      
      ## 1. decrease stopping tolerances: 
      
      # a) CHANGE optimizer to nloptwrap
      if (grepl("^lmer", model@call[1])) {
        message("lmer model detected")
        strict_tol <- lmerControl(optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8))
        suppressMessages({ model <- update(model, control=lmerControl(optimizer="nloptwrap")) })
      } else if (grepl("glmer", model@call[1])) {
        message("    - glmer model detected")
        strict_tol <- glmerControl(optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8))
        suppressMessages({ model <- update(model, nAGQ = 11, control=glmerControl(optimizer="nloptwrap")) })
      } else {
        message("\n\n * Woooot? * \n\n")
      }
      
      # b) Try new parameters
      if (all(model@optinfo$optimizer == "nloptwrap")) {
        suppressWarnings({ model <- update(model, control = strict_tol) })
        ERRORS = model@optinfo$conv$lme4$messages
        
        if(is.null(ERRORS)) {
          message("    - decreasing stopping tolerances WORKED!")
          model
        } else {
          message("\n\n    *** Life is full of sadness and despair... ***\n\n")
        }
      }
  }
}



# Mixes models result report -------------------------------------------------
report_result <- function(model, variable_name = "", has_the_word = "") {
  
  # DEBUG
  # model = model_screening
  # variable_name = "brochure (OLD - NEW)"
  
  # model = global_simplified_model_ppv_error
  # has_the_word = ""
  # variable_name = "brochure (OLD - NEW)"
  # variable_name = "brochure (OLD - NEW):age (40 - 20)"
  
  # variable_name = ""
  # has_the_word = "ind_agreeableness"
  # has_the_word = "disease"
  
  # model = model_old
  # has_the_word = "age"
  # variable_name = ""
  
  # model = global_simplified_model_ppv_error
  # has_the_word = "ind_apriori"
  # variable_name = ""
  
  # model = model_old
  # has_the_word = "ind_apriori"
  # variable_name = ""
  
  
  
  not_significant = ""
  DF_parameters_temp = parameters::parameters(model) %>% as_tibble()
  names_to_round = names(DF_parameters_temp %>% select(-Parameter, -p))
  
  # Si es glmer, exponenciamos los coeficientes, tal y como hacen en sjPlot::tab_model()
  if (grepl("glmer", model@call[1])) DF_parameters_temp = DF_parameters_temp %>% mutate_at(c("Coefficient", "CI_low", "CI_high"), exp)  
  
  # Redondeamos a 3 decimales
  DF_parameters = DF_parameters_temp %>% mutate_at(vars(names_to_round), list(~ round(., 3)))
  
  
  
  # Get DF, calculate means per group
  DF_data = model@frame %>% as_tibble()
  names_factors_DF_data = names(which(sapply(DF_data, is.factor))) 
  summary_var = names(model@frame[1])
  is_interaction = grepl(":", variable_name)
  
  if (variable_name != "") {
    if (is_interaction == TRUE) {
      group_by_var = stringr::word(variable_name, 1)  
      group_by_var2 = stringr::word(gsub(".*:(.*)", "\\1", variable_name), 1)  
      # group_by_var = paste0(group_by_var, ", ", group_by_var2)
    } else {
      group_by_var = stringr::word(variable_name, 1)
      group_by_var2 = NULL
    }
    
  } else {
    group_by_var = has_the_word
    group_by_var2 = NULL
    
  }  
  
  

  # Summary DF --------------------------------------------------------------

  if (summary_var %in% names_factors_DF_data) {
    # If summary_var is a factor, COUNT

    DF_counts_raw = 
      DF_data %>%
      count_(c(group_by_var, group_by_var2, summary_var)) %>% 
      rename(VAR = 1, 
             N = n) 
      
    DF_counts = DF_counts_raw %>% 
      left_join(DF_counts_raw %>% group_by(VAR) %>% summarise(SUM = sum(N)),  by = "VAR") %>% 
      unite("VAR",  VAR:summary_var)
    
  } else {
  
    DF_means = DF_data %>% 
      group_by_at(vars(group_by_var, group_by_var2)) %>% 
      summarize(MEAN =  mean(!!sym(summary_var)),
                SD = sd(!!sym(summary_var))) %>% 
      ungroup() %>% 
      mutate_if(is.numeric, round, 3) %>% 
      rename(VAR = 1)
  
    if (!is.null(group_by_var2)) DF_means = DF_means %>% unite("VAR",  VAR:group_by_var2)
    
  }

  
  
  # global_simplified_model_ppv_error@frame[["age"]]
  # global_simplified_model_ppv_error
  # summary(global_simplified_model_ppv_error)
  
  if(variable_name != "") {
    DF_variable = DF_parameters %>% filter(Parameter == variable_name)  
  } else if (has_the_word != "") {
    DF_variable = DF_parameters %>% filter(grepl(has_the_word, Parameter, ignore.case = TRUE))  
  } else{
    
  }
  
  # DF_variable
  if (DF_variable$p > 0.05) { not_significant = "not " }
  
  message(paste0("The effect of ", DF_variable$Parameter ," was ", not_significant ,"significant (beta = ", DF_variable$Coefficient, ", 95% CI [", DF_variable$CI_low, ", ", DF_variable$CI_high, "], p = ", DF_variable$p, ") when predicting ", summary_var))
  
  # If IV is not a factor, do not show! 
  # if (DF_data %>% count(!! sym(group_by_var)) %>% nrow() < 4) {
  if (!group_by_var %in% names_factors_DF_data) {
    # Si group_by_var es una variable continua, no mostramos nada
    message(group_by_var, " es una variable continua")
    
  } else if (summary_var %in% names_factors_DF_data) {
    # SI summary_var es un factor, damos counts
    message(1:nrow(DF_counts) %>% map_chr( ~ paste0(" ",  DF_counts$VAR[.x], " ", group_by_var, "/", summary_var, " (N = ", DF_counts$N[.x], ", ", DF_counts$N[.x]/ DF_counts$SUM[.x], ")\n")))
    
  } else if (group_by_var %in% names_factors_DF_data) {
    # Si group_by_var es un factor, mostramos M (SD)
    message(1:nrow(DF_means) %>% map_chr( ~ paste0(" ",  DF_means$VAR[.x], " ", group_by_var, " (M = ", DF_means$MEAN[.x], ", SD = ", DF_means$SD[.x], ")\n")))
  
  }  
  
  
}
