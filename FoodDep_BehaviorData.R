# Project: Food Deprivation Pilot Project
# Behavior Data
# Load packages ----
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# load dataset ----
f2 <- "https://raw.githubusercontent.com/slcornett/FoodDepBehaviorPilot/main/FoodDep_Pilot_Behavior_Master.csv"
d2 <- read_csv(f2, col_names = TRUE)
print(d2)

# organize dataset ----
