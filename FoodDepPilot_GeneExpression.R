# FDP: Gene Expression Analysis from qPCR Data
# Load Packages----
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(ggsci) # science theme for ggplot2
#library(cowplot)
library(car) #anova
# load dataset----
f2 <- "https://raw.githubusercontent.com/slcornett/FoodDepBehaviorPilot/main/fdp_datasets/2023-02-08_GnRHR2GAPDH.csv"
df2 <- read_csv(f2, col_names = TRUE)
print(df2)
