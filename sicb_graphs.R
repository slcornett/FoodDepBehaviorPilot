#SICB Poster Data
#Packages needed
library(tidyverse)
library(dplyr)
library(ggplot2)
#spreadsheet prepared in excel
f<-"https://raw.githubusercontent.com/slcornett/FoodDepBehaviorPilot/main/SICB_Poster_Data.csv"
d <- read_csv(f, col_names = TRUE)
print(d)
#difference between day 14 and day 1: Day14 - Day 1 (occurances)
d <- d %>% mutate(Diff_Chases = Day14_Chase-Day1_Chase,
                  Diff_Charges = Day14_Charge - Day1_Charge,
                  Diff_Initiatings = Day14_InitiatingMatingBehaviors - Day1_InitiatingMatingBehaviors,
                  Diff_Responses = Day14_ResponseMatingBehaviors - Day1_ResponseMatingBehaviors)
print(d)



initiating <- ggplot(data = d, aes(x = Diff_Initiatings, y = FoodCondition, color = MorphSex)) +
  geom_boxplot()
initiating

