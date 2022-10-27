# Project: Food Deprivation Project
# Behavior Data
library(tidyverse)
library(dplyr)
library(ggplot2)
f2 <- "https://raw.githubusercontent.com/slcornett/FoodDepBehaviorPilot/main/FoodDep_Pilot_Behavior_Master.csv"
d2 <- read_csv(f2, col_names = TRUE)
print(d2)
