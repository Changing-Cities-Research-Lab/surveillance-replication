# Higher Prevalence of Surveillance Cameras in Racially Diverse Neighborhoods across 10 US Cities

This repository contains code and scripts used in the paper **"Higher Prevalence of Surveillance Cameras in Racially Diverse Neighborhoods across 10 US Cities"**. The study leverages computer vision techniques to analyze surveillance camera prevalence across various neighborhoods in 10 U.S. cities, focusing on understanding the relationship between camera prevalence and neighborhood racial diversity.

---

## System Requirements

### Software Dependencies
This analysis requires R and the following packages (specific versions tested on indicated):
- `tidyverse`: 1.3.2  
- `pscl`: 1.5.5  
- `conflicted`: 1.2.0  
- `marginaleffects`: 0.17.0  
- `ggpubr`: 0.6.0  
- `sf`: 1.0.12  
- `glue`: 1.6.2  
- `tidycensus`: 1.2.3  
- `broom`: 1.0.4  
- `splines`: 4.2.2  
- `scales`: 1.2.1  

### Operating System Compatibility
The repository has been tested on:
- macOS Monterey  

---

## Installation Guide

### Instructions
1. Ensure that R is installed on your system (version **4.2.2 or higher** recommended).
2. Install the required R packages by running the following command in your R console:
   ```R
   install.packages(c("tidyverse", "pscl", "conflicted", "marginaleffects", 
                      "ggpubr", "sf", "glue", "tidycensus", "broom", "splines", "scales"))
3. Clone this repository to your local machine.

## Repository Structure

This repository contains three R scripts used to generate the figures in the paper. Below is a description of each script:

- **`descriptive-figures.R`**: Generates the descriptive statistics and visualizations, specifically Figures 1 and 5 in the paper.
- **`cs-results.R`**: Produces the results and figures highlighting the relationship between neighborhood racial composition and surveillance camera prevalence.
- **`change-results.R`**: Produces the results and figures for findings related to changes in surveillance camera prevalence.

## Data

The datasets and codebook used in this study are publicly available in the Stanford Digital Repository. The data can be accessed at the following link:

[https://doi.org/10.25740/jr882ny4955](https://doi.org/10.25740/jr882ny4955)

The repository includes:

- **Datasets**: Camera location data, neighborhood demographic data, and related variables.
- **Codebook**: Detailed descriptions of the variables and data sources used in the study.

## Camera Detection Code

All code for camera detection is [here](https://github.com/stanford-policylab/surveilling-surveillance).

## How to Run

1. Clone this repository:
2. Download the datasets from the [Stanford Digital Repository](https://doi.org/10.25740/jr882ny4955).
3. Place the downloaded files in the data/ directory of the cloned repository.
4. Run the provided R scripts.
   
All figures will be saved in the outputs/ directory.

### Expected Runtime
Each analysis script takes approximately 2-5 minutes to execute.

