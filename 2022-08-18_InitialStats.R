# Project: Summer2022 Food Deprivation Behavior Pilot
# Initial Statistics
library(tidyverse)
library(dplyr)
library(ggplot2)
f1 <- "https://raw.githubusercontent.com/slcornett/FoodDepBehaviorPilot/main/Summer22_FoodDepPilot_Fish_Dissection_Record.csv"
d1 <- read_csv(f1, col_names = TRUE)
print(d1)
h1 <- ggplot(data = d1, aes(x = MorphSex, colour = MorphSex)) +
  geom_histogram(stat = "count") +
  facet_wrap(~Condition) +
  ggtitle("Members of Each MorphSex per Food Condition")
h1
