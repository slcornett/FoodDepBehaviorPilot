# Project: Summer2022 Food Deprivation Behavior Pilot
# Initial Statistics
library(tidyverse)
library(dplyr)
library(ggplot2)
f <- "https://raw.githubusercontent.com/slcornett/FoodDepBehaviorPilot/main/Summer22_FoodDepPilot_Fish_Dissection_Record.csv"
d <- read_csv(f, col_names = TRUE)
print(d)
h1 <- ggplot(data = d, aes(x = MorphSex, colour = MorphSex)) +
  geom_histogram(stat = "count") +
  facet_wrap(~Condition) +
  ggtitle("Members of Each MorphSex per Food Condition")
h1
