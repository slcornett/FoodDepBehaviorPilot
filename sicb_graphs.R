#SICB Poster Data
#Packages needed
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggsci) # science theme for ggplot2
#spreadsheet prepared in excel
f<-"https://raw.githubusercontent.com/slcornett/FoodDepBehaviorPilot/main/SICB_Poster_Data.csv"
d <- read_csv(f, col_names = TRUE)
print(d)
#difference between day 14 and day 1: Day14 - Day 1
d <- d %>% mutate(Diff_Chases = Day14_Chase - Day1_Chase, #count
                  Diff_Chases_s = Day14_Chase_s - Day1_Chase_s, # seconds
                  Diff_Charges = Day14_Charge - Day1_Charge,
                  Diff_Initiatings = Day14_InitiatingMatingBehaviors - Day1_InitiatingMatingBehaviors,
                  Diff_Responses = Day14_ResponseMatingBehaviors - Day1_ResponseMatingBehaviors,
                  # both initiating and response parallel swim bc otherwise biased to OMs
                  Diff_Parallel_s = (`Day14_ParallelSwim-I_s`+`Day14_ParallelSwim-R_s`)-(`Day1_ParallelSwim-I_s`+`Day1_ParallelSwim-R_s`),
                  Diff_Refuge_s = Day14_Refuge_s - Day1_Refuge_s
                  )
print(d)
#boxplot initiating behaviors count
initiating <- ggplot(data = d, aes(x = FoodCondition,
                                   y = Diff_Initiatings,
                                  fill = MorphSex)) +
  scale_color_startrek()+
  geom_point(position=position_jitterdodge()) +
  geom_boxplot(outlier.shape = NA ) +
  labs(x = "Food Condition", y = "Day14 - Day1 Initiating Behaviors (Count)") +
  ggtitle("Effects of Food Condition on the Occurance of Initiating Mating Behaviors") +
  theme_classic()
initiating
#boxplot response behaviors count
response <- ggplot(data = d, aes(x = FoodCondition,
                                 y = Diff_Responses,
                                 fill = MorphSex)) +
  scale_color_startrek()+
  geom_point(position=position_jitterdodge()) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Food Condition", y = "Day14 - Day1 Response Behaviors (Count)") +
  ggtitle("Effects of Food Condition on the Occurance of Responding Mating Behaviors") +
  theme_classic()
response
#boxplot parallel swim time (s)
Parallel_s <- ggplot(data = d, aes(x = FoodCondition,
                                 y = Diff_Parallel_s,
                                 fill = MorphSex)) +
  scale_color_startrek()+
  geom_point(position=position_jitterdodge()) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Food Condition", y = "Day14 - Day1 Parallel Swim (seconds)") +
  ggtitle("Effects of Food Condition on Time(s) Spent Participating in Parallel Swim") +
  theme_classic()
Parallel_s

