# This script generates Figure 3 in the paper
# Nima Dahir
# March 18, 2025

# Libraries:
library(tidyverse) 
library(pscl)
library(conflicted)
library(marginaleffects)
library(ggpubr)

# Data:
df <- read_csv(here::here("change_replication_data.csv"))

# Model 1: Change in entropy (diversity) 
e1 <- zeroinfl(
  cam_count ~ l_cam_count + c_entropy + l_entropy + 
    c_total_crime_rate + l_total_crime_rate + c_pop + l_pop + 
    c_hinc + l_hinc + c_pvac + l_pvac + c_mhmval + l_mhmval + 
    factor(city) + factor(modal_zone) + offset(log_road_length) |
    factor(city),
  data = df,
  weights = log(image_count),
  dist = "poisson"
)

# Create plot for Model 1:
entropy_fig <- plot_predictions(e1, condition = list("c_entropy")) +
  theme_bw() +
  labs(
    x = "Change in Diversity (Z-Score)",
    y = "Change in Camera Identification Rate"
  ) 

# Annotate the entropy plot
entropy_fig <- annotate_figure(entropy_fig, 
                               top = text_grob("Panel A: Change in Diversity & Change in Camera Identification Rates", face = "bold"))

# Model 2: White population changes by baseline non-White
wht_change <- zeroinfl(
  cam_count ~ l_cam_count + c_pnhwht * l_pnonwht + c_pnhblk + 
    c_total_crime_rate + l_total_crime_rate + c_pop + l_pop + 
    c_hinc + l_hinc + c_pvac + l_pvac + c_mhmval + l_mhmval + 
    factor(city) + factor(modal_zone) + offset(log_road_length) |
    factor(city),
  data = df,
  weights = log(image_count),
  dist = "poisson"
)

# Create plot for Model 2
wht_change_fig <- plot_predictions(wht_change, condition = list("c_pnhwht", "l_pnonwht" = c(-2, 0, 2)), type = "response") +
  theme_bw() +
  labs(
    x = "Change in Percent Non-Hispanic White (Z-Score)",
    y = "Change in Camera Identification Rate",
    color = "Baseline Share Non-White",
    fill = "Baseline Share Non-White"
  ) +
  scale_color_manual(values = c('-2' = "#F8766D", '0' = "#00BA38", '2' = "#619CFF"), labels = c('-2' = "-2SD", '0' = "Mean", '2' = "+2SD")) +
  scale_fill_manual(values = c('-2' = "#F8766D", '0' = "#00BA38", '2' = "#619CFF"), labels = c('-2' = "-2SD", '0' = "Mean", '2' = "+2SD")) 

# Model 3: Black population changes by baseline White
blk_change <- zeroinfl(
  cam_count ~ l_cam_count + c_pnhblk * l_pnhwht + l_pnhblk + 
    c_total_crime_rate + l_total_crime_rate + c_pop + l_pop + 
    c_hinc + l_hinc + c_pvac + l_pvac + c_mhmval + l_mhmval + 
    factor(city) + factor(modal_zone) + offset(log_road_length) |
    factor(city),
  data = df,
  weights = log(image_count),
  dist = "poisson"
)

# Create plot for Model 3 - 
blk_change_fig <- plot_predictions(blk_change, condition = list("c_pnhblk", "l_pnhwht" = c(-2, 0, 2)), type = "response") +
  theme_bw() +
  labs(
    x = "Change in Percent Non-Hispanic Black (Z-Score)",
    y = "Change in Camera Identification Rate",
    color = "Baseline Share White",
    fill = "Baseline Share White"
  ) +
  scale_color_manual(values = c('-2' = "#F8766D", '0' = "#00BA38", '2' = "#619CFF"), labels = c('-2' = "-2SD", '0' = "Mean", '2' = "+2SD")) +
  scale_fill_manual(values = c('-2' = "#F8766D", '0' = "#00BA38", '2' = "#619CFF"), labels = c('-2' = "-2SD", '0' = "Mean", '2' = "+2SD")) 

# Combine the racial change plots (Model 2 and Model 3)
race_plot <- ggpubr::ggarrange(wht_change_fig, blk_change_fig, ncol = 2, legend = "bottom")

# Annotate the combined racial change plot
race_plot <- annotate_figure(race_plot, 
                             top = text_grob("Panel B: Racial Change & Change in Camera Identification Rates", face = "bold"))

# Combine the entropy plot and racial change plots into a full figure
full_change_plot <- ggarrange(entropy_fig, race_plot, ncol = 1)