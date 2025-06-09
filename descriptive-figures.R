# This script generates Figures 1 in the paper
# Nima Dahir
# June 9, 2025

# Libraries:
library(tidyverse)
library(sf)
library(glue)
library(tidycensus)
library(broom)
library(splines)
library(ggpubr)
library(scales)

# Data:
cs_df <- read_csv(here::here("cs_replication_data.csv"))

# FIGURE 1:----
# Plot 1: Relationship between Percent Non-Hispanic Black and Camera Identification Rate

# Set text size:
theme_set(theme_minimal(base_size = 20))

black_fig <- cs_df %>%
  ggplot(aes(x = pnhblk, y = cam_count)) +                
  geom_smooth(                                            
    method = "lm",                                        
    formula = y ~ splines::ns(x, df = 3),                 
    se = TRUE,                                            
    fill = "red", color = "black", alpha = 0.2            
  ) +
  labs(x = "Percent Non-Hispanic Black",                  
       y = "Camera Identification Rate") +                
  scale_x_continuous(                                     # Format x-axis as percentage
    labels = label_percent(accuracy = 1, scale = 1)
  ) +
  theme_minimal()  

# Plot 2: Relationship between Percent Non-Hispanic White and Camera Identification Rate
white_fig <- cs_df %>%
  ggplot(aes(x = pnhwht, y = cam_count)) +                
  geom_smooth(                                           
    method = "lm",                                        
    formula = y ~ splines::ns(x, df = 2),                 
    se = TRUE,                                            
    fill = "red", color = "black", alpha = 0.2            
  ) +
  labs(x = "Percent Non-Hispanic White",                  
       y = "") +                                          
  scale_x_continuous(                                     
    labels = label_percent(accuracy = 1, scale = 1)
  ) +
  theme_minimal() 

# Plot 3: Relationship between Diversity Score Rank and Camera Identification Rate
div_fig <- cs_df %>%
  ggplot(aes(x = entropy_rank, y = cam_count)) +          
  geom_smooth(                                           
    method = "lm",                                        
    formula = y ~ splines::ns(x, df = 2),                 
    se = TRUE,                                            
    fill = "red", color = "black", alpha = 0.2            
  ) +
  labs(x = "Diversity Score Rank",                        
       y = "Camera Identification Rate") +                
  theme_minimal() 

# Combine all three plots into a single figure, arranged in 3 columns
desc_all <- ggpubr::ggarrange(black_fig, white_fig, div_fig,
                              ncol = 3)
