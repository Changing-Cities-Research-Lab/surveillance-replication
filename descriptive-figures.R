# This script generates Figures 1 and 5 in the paper
# Nima Dahir
# March 18, 2025

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
map_df <- read_csv(here::here("map_data.csv")) %>% 
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4269,
    agr = "constant"
  )

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

# FIGURE 5:----
## Population and road length info for use later (source: Sheng et al. 2021) ---- 
city_info <- tibble(
  city = c("Baltimore", "Boston", "Chicago", "Los Angeles", "Milwaukee", 
           "New York", "Philadelphia", "San Francisco", "Seattle", "Washington"),
  population = c("621,000", "618,000", "2,696,000", "3,793,000", "595,000", 
                 "8,175,000", "1,526,000", "805,000", "609,000", "602,000"),
  road_length_km = c("3,746", "2,589", "10,449", "21,095", "4,899", 
                     "16,362", "6,759", "3,101", "5,569", "3,262")
)

## Functions:----
### Function to load road network
load_road_network <- function(city_name) {
  stopifnot(city_name %in% map_df$city)
  path <- here::here("road_network", city_name, "edges.shp") # These should be downloaded to your working directory.
  read_sf(path)
}

### Function to generate point maps for detected cameras
generate_detected_point_map <- function(df, city_name, population, road_length_km) {
  # Load road network for the city
  road_network <- load_road_network(city_name)
  
  # Get CRS and bounding box of the road network
  road_network_crs <- st_crs(road_network)$epsg
  bbox <- st_bbox(road_network)
  
  # Plot the road network and detected cameras
  road_network %>%
    ggplot() +
    geom_sf(fill = "white", color = "gray", alpha = 0.6, size = 0.05) +
    geom_sf(data = df %>%
              filter(city == city_name, camera_count > 0) %>%
              st_as_sf(coords = c("lon", "lat"), crs = road_network_crs, agr = "constant"), 
            color = "red", size = 0.5, shape = 16, alpha = 1) +
    coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax)) +
    labs(caption = glue("{city_name}\nPopulation: {population}\nRoad Length: {road_length_km} KM")) +
    theme(axis.text = element_blank(), 
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 20),
          plot.caption = element_text(hjust = 0.5, size = rel(1.2)),
          text = element_text(family = "Times"))
}


# Generate maps for each city
maps <- city_info %>%
  pmap(~ generate_detected_point_map(map_df, ..1, ..2, ..3))

# Combine maps into a grid
map <- ggpubr::ggarrange(plotlist = maps, ncol = 5, nrow = 2, align = "v")