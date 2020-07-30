library('readxl')
library('dplyr')
library('ggplot2')

# PLOT DS --------------------------------------------------------------------
  
  label_x_axis = "Age of the mother"
  df_BC = readxl::read_xls("MATERIALS/generate_plots/data/numbers_bayes_DS.xls", sheet = 1)
  
  
 plot_DS = ggplot(df_BC, aes(x=age, y=PPV_100)) +      # plot canvas
    scale_y_continuous(labels=function(x) paste0(x,"%"), # append % to y-axis value
                       limits = c(0,100)) +              # set y-axis limits
    geom_point(size = 5.5, color = "#009999", shape = 19) + # insert points with ppv value
    geom_line(aes(x=age, y=PPV_100), color = "#009999", size = 2) +
    theme_minimal() + # insert line bridging PPV-value points
    theme(axis.text = element_text(size = 25),                             # axis-numbers size
          axis.title = element_text(size = 25)) +                          # axis-labels size
    geom_text(aes(label = paste0(round(PPV_100, 0), "%"), #case_when(age %in% age_ppv_to_plot ~ paste0(round(PPV_100, 0), "%"), TRUE ~ paste0("")), # keep only ages previously set to be ploted
                  hjust = .4, vjust = 2.5), size = 8) + # (position) plot ppv-values above points set in "age_ppv_to_plot"
    labs(y = "Test reliability",
         x = label_x_axis)
  
 ggsave("MATERIALS/generate_plots/output/plot_DS.png", plot_DS,  width = 10, height = 6, dpi = 300)

 
# PLOT BC --------------------------------------------------------------------

label_x_axis = "Age of the woman"
df_BC = readxl::read_xls("MATERIALS/generate_plots/data/numbers_bayes_BC.xls", sheet = 1)




plot_BC = ggplot(df_BC, aes(x=age, y=PPV_100)) +      # plot canvas
  scale_y_continuous(labels=function(x) paste0(x,"%"), # append % to y-axis value
                     limits = c(0,100)) +              # set y-axis limits
  geom_point(size = 5.5, color = "#009999", shape = 19) + # insert points with ppv value
  geom_line(aes(x=age, y=PPV_100), color = "#009999", size = 2) +
  theme_minimal() + # insert line bridging PPV-value points
  theme(axis.text = element_text(size = 25),                             # axis-numbers size
        axis.title = element_text(size = 25)) +                          # axis-labels size
  geom_text(aes(label = paste0(round(PPV_100, 0), "%"), #case_when(age %in% age_ppv_to_plot ~ paste0(round(PPV_100, 0), "%"), TRUE ~ paste0("")), # keep only ages previously set to be ploted
                hjust = .4, vjust = 2.5), size = 8) + # (position) plot ppv-values above points set in "age_ppv_to_plot"
  labs(y = "Test reliability",
       x = label_x_axis)

ggsave("MATERIALS/generate_plots/output/plot_BC.png", plot_BC, width = 10, height = 6, dpi = 300)
