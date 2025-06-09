# This script generates Figure 2 in the paper
# Nima Dahir
# June 9, 2025

# Libraries:
library(tidyverse) 
library(pscl)
library(conflicted)
library(marginaleffects)


# Data:
df <- read_csv(here::here("cs_replication_data.csv"))

# Scale continuous measures:

df <- df %>%
  ungroup() %>%
  group_by(city) %>%
  mutate_at(c('pnhwht','pnhblk','mhmval','entropy','total_crime_rate','pop','hinc','pvac'), ~(scale(.) %>% as.vector)) 

# Model 1: Race + Crime:
# Fit a zero-inflated Poisson model for camera count using key predictors
m1 <- zeroinfl(
  cam_count ~ poly(pnhwht, 2) + poly(pnhblk, 2) + total_crime_rate + 
    factor(modal_zone) + pop + hinc + pvac + mhmval + factor(city) + 
    offset(log_road_length) | factor(city) + offset(log_road_length), 
  weights = log(image_count), # Use log of image count as weights
  dist = "poisson",           
  data = df                   
)

# Generate predictions and plot for Percent Non-Hispanic Black (Z-score)
m1_blk <- plot_predictions(m1, condition = list("pnhblk")) +
  xlim(-2, 2) +                 
  ylim(0.05, 0.12) +            
  theme_bw() +                  
  labs(
    x = "Percent Non-Hispanic Black (Z-Score)",
    y = "Predicted Camera Identification Rate"
  ) 

# Generate predictions and plot for Percent Non-Hispanic White (Z-score)
m1_wht <- plot_predictions(m1, condition = list("pnhwht")) +
  xlim(-2, 2) +
  ylim(0.05, 0.12) +
  theme_bw() +
  labs(
    x = "Percent Non-Hispanic White (Z-Score)",
    y = ""  
  ) 

# Generate predictions and plot for Total Crime Rate (Z-score)
m1_crime <- plot_predictions(m1, condition = list("total_crime_rate")) +
  xlim(-2, 3) +
  ylim(0.05, 0.12) +
  theme_bw() +
  labs(
    x = "Total Crime Rate (Z-Score)",
    y = ""  
  )

# Arrange the three plots into a single row
m1_plot <- ggarrange(m1_blk, m1_wht, m1_crime, ncol = 3)

# Add a title to the combined plot
m1_plot <- annotate_figure(
  m1_plot, 
  top = text_grob("Panel A: Camera Identification Rates, Racial Composition, and Neighborhood Diversity", 
                  face = "bold")
)

# Model 2: Diversity and Crime

# Fit a zero-inflated Poisson model for camera count using diversity (entropy) and crime rate as predictors
m2 <- zeroinfl(
  cam_count ~ entropy + total_crime_rate + factor(modal_zone) + pop + hinc + pvac + mhmval + 
    factor(city) + offset(log_road_length) | factor(city) + offset(log_road_length), 
  weights = log(image_count), 
  dist = "poisson",           
  data = df                  
)

# Generate predictions and plot for Diversity Measure (Z-score)
m2_entropy <- plot_predictions(m2, condition = list("entropy")) +
  xlim(-2, 2) +                 
  ylim(0.05, 0.12) +            
  theme_bw() +                  
  labs(
    x = "Diversity Measure (Z-Score)",       
    y = "Predicted Camera Identification Rate" 
  ) 

# Generate predictions and plot for Total Crime Rate (Z-score)
m2_crime <- plot_predictions(m2, condition = list("total_crime_rate")) +
  xlim(-2, 2.5) +              
  ylim(0.05, 0.12) +            
  theme_bw() +                  
  labs(
    x = "Total Crime Rate (Z-Score)",        
    y = ""                                   
  ) 

# Arrange the two plots into a single row
m2_plot <- ggarrange(m2_entropy, m2_crime, ncol = 2)

# Add a title to the combined plot
m2_plot <- annotate_figure(
  m2_plot, 
  top = text_grob("Panel B: Camera Identification Rates, Diversity, and Crime", 
                  face = "bold")
)

# Model 3: Residualized % Black and % White with Entropy as Moderator

# Step 1: Residualize % Black and % White with entropy
residual_black <- lm(pnhblk ~ entropy, data = df)$residuals
residual_white <- lm(pnhwht ~ entropy, data = df)$residuals

# Step 2: Add these residuals to the dataset
df$residual_black <- residual_black
df$residual_white <- residual_white

# Step 3: Fit a zero-inflated Poisson model with interaction between residuals and entropy
m3 <- zeroinfl(
  cam_count ~ poly(residual_black, degree = 2) * entropy + 
    poly(residual_white, degree = 2) * entropy + 
    total_crime_rate + factor(modal_zone) + factor(city) + 
    pop + hinc + pvac + mhmval + offset(log_road_length) | 
    factor(city) + offset(log_road_length), 
  weights = log(image_count),
  dist = "poisson",
  data = df
)

# Step 4: Generate prediction plots for residualized % Black with varying entropy levels
m3_blk <- plot_predictions(m3, condition = list("residual_black", "entropy" = c(-2, 0, 2))) + # Use mean, mean + 2SD, and mean - 2SD as focus
  xlim(-2, 2) +
  ylim(0.05, 0.2) +
  theme_bw() +
  labs(
    x = "Percent Non-Hispanic Black (Z-Score)",
    y = "Predicted Camera Identification Rate",
    color = "Diversity",
    fill = "Diversity"
  ) +
  scale_color_manual(
    values = c('-2' = "#F8766D", '0' = "#00BA38", '2' = "#619CFF"),
    labels = c('-2' = "-2SD", '0' = "Mean", '2' = "+2SD")
  ) +
  scale_fill_manual(
    values = c('-2' = "#F8766D", '0' = "#00BA38", '2' = "#619CFF"),
    labels = c('-2' = "-2SD", '0' = "Mean", '2' = "+2SD")
  ) 

# Step 5: Generate prediction plots for residualized % White with varying entropy levels
m3_wht <- plot_predictions(m3, condition = list("residual_white", "entropy" = c(-2, 0, 2))) +
  xlim(-2, 2) +
  theme_bw() +
  labs(
    x = "Percent Non-Hispanic White (Z-Score)",
    y = "",
    color = "Diversity",
    fill = "Diversity"
  ) +
  scale_color_manual(
    values = c('-2' = "#F8766D", '0' = "#00BA38", '2' = "#619CFF"),
    labels = c('-2' = "-2SD", '0' = "Mean", '2' = "+2SD")
  ) +
  scale_fill_manual(
    values = c('-2' = "#F8766D", '0' = "#00BA38", '2' = "#619CFF"),
    labels = c('-2' = "-2SD", '0' = "Mean", '2' = "+2SD")
  ) 

# Step 6: Arrange the two plots into a single row with a common legend at the bottom
m3_plot <- ggarrange(m3_blk, m3_wht, ncol = 2, common.legend = TRUE, legend = "bottom")

# Step 7: Add a title to the combined plot
m3_plot <- annotate_figure(
  m3_plot, 
  top = text_grob("Panel C: Camera Identification Rates, Racial Composition, and Neighborhood Diversity Interactions", 
                  face = "bold")
)

# Step 8: Combine all plots (m1_plot, m2_plot, m3_plot) into a single figure
full_cs_plot <- ggarrange(m1_plot, m2_plot, m3_plot, ncol = 1)