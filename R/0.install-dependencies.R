
# Dependencies should be installed with renv ------------------------------

  # https://blog.rstudio.com/2019/11/06/renv-project-environments-for-r/
  # renv::init() # Initialize a project
  # renv::snapshot() # Create snapshot of packages

# If you are trying to run the scripts here for the first time:

  if (!require('renv')) install.packages('renv'); library('renv')
  renv::restore()


  
# Manually install dependencies -------------------------------------------

# If needed, you can manually install the dependencies
  
  # if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
  # if (!require('readr')) install.packages('readr'); library('readr')
  # if (!require('tidyr')) install.packages('tidyr'); library('tidyr')
   
  # if (!require('corrplot')) install.packages('corrplot'); library('corrplot')
  # if (!require('patchwork')) install.packages('patchwork'); library('patchwork')
  # if (!require('ggalluvial')) install.packages('ggalluvial'); library('ggalluvial')
  
  # if (!require('lme4')) install.packages('lme4'); library('lme4')
  # if (!require('sjPlot')) install.packages('sjPlot'); library('sjPlot')
  
  # devtools::install_github(c("easystats/insight",
  #                            "easystats/bayestestR",
  #                            "easystats/performance",
  #                            "easystats/parameters",
  #                            "easystats/correlation",
  #                            "easystats/estimate",
  #                            "easystats/see",
  #                            "easystats/report"))