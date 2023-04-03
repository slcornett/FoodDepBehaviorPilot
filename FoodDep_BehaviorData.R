# Project: Food Deprivation Pilot Project
# Behavior Data
library(tidyverse)
library(dplyr)
library(ggplot2)
f2 <- "https://raw.githubusercontent.com/slcornett/FoodDepBehaviorPilot/main/FoodDep_Pilot_Behavior_Master.csv"
d2 <- read_csv(f2, col_names = TRUE)
print(d2)
#Could possibly make difference calcs easier by making 2 data frames, day1 and day14
Day1 <- select(d2,
               Fish,
               MorphSex,
               Population,
               FoodCondition,
               TotalTime_s,
               `Day1_Chase-SameSex`,
               `Day1_Chase-SameSex_s`,
               `Day1_Chase-DiffSex`,
               `Day1_Chase-DiffSex_s`,
               `Day1_ParallelSwim-SameSexDorsalFin-I`,
               `Day1_ParallelSwim-SameSexDorsalFin-I_s`,
               `Day1_ParallelSwim-SameSexNoFin-I`,
               `Day1_ParallelSwim-SameSexNoFin-I_s`,
               `Day1_ParallelSwim-DiffSexDorsalFin-I`,
               `Day1_ParallelSwim-DiffSexDorsalFin-I_s`,
               `Day1_ParallelSwim-DiffSexNoFin-I`,
               `Day1_ParallelSwim-DiffSexNoFin-I_s`,
               `Day1_TransverseApproach-DorsalFin`,
               `Day1_TransverseApproach-NoFin`,
               Day1_AttemptedCopulation,
               `Day1_SexDisp_Cwrap-DorsalFin`,
               `Day1_SexDisp_Cwrap-NoFin`,
               `Day1_SexDisp_FrontShimmy-DorsalFin`,
               `Day1_SexDisp_FrontShimmy-NoFin`,
               Day1_Nip,
               Day1_Charge,
               Day1_Stay,
               Day1_Backup,
               Day1_Retreat,
               Day1_Glide,
               Day1_Dart,
               `Day1_ParallelSwim-SameSex-R`,
               `Day1_ParallelSwim-SameSex-R_s`,
               `Day1_ParallelSwim-DiffSex-R`,
               `Day1_ParallelSwim-DiffSex-R_s`,
               Day1_Refuge,
               Day1_Refuge_s,
               Day1_OutofFrame,
               Day1_OutofFrame_s
               )
# same for day 14
Day14 <-select(d2,
               Fish,
               MorphSex,
               Population,
               FoodCondition,
               TotalTime_s,
               `Day14_Chase-SameSex`,
               `Day14_Chase-SameSex_s`,
               `Day14_Chase-DiffSex`,
               `Day14_Chase-DiffSex_s`,
               `Day14_ParallelSwim-SameSexDorsalFin-I`,
               `Day14_ParallelSwim-SameSexDorsalFin-I_s`,
               `Day14_ParallelSwim-SameSexNoFin-I`,
               `Day14_ParallelSwim-SameSexNoFin-I_s`,
               `Day14_ParallelSwim-DiffSexDorsalFin-I`,
               `Day14_ParallelSwim-DiffSexDorsalFin-I_s`,
               `Day14_ParallelSwim-DiffSexNoFin-I`,
               `Day14_ParallelSwim-DiffSexNoFin-I_s`,
               `Day14_TransverseApproach-DorsalFin`,
               `Day14_TransverseApproach-NoFin`,
               Day14_AttemptedCopulation,
               `Day14_SexDisp_Cwrap-DorsalFin`,
               `Day14_SexDisp_Cwrap-NoFin`,
               `Day14_SexDisp_FrontShimmy-DorsalFin`,
               `Day14_SexDisp_FrontShimmy-NoFin`,
               Day14_Nip,
               Day14_Charge,
               Day14_Stay,
               Day14_Backup,
               Day14_Retreat,
               Day14_Glide,
               Day14_Dart,
               `Day14_ParallelSwim-SameSex-R`,
               `Day14_ParallelSwim-SameSex-R_s`,
               `Day14_ParallelSwim-DiffSex-R`,
               `Day14_ParallelSwim-DiffSex-R_s`,
               Day14_Refuge,
               Day14_Refuge_s,
               Day14_OutofFrame,
               Day14_OutofFrame_s
               )

d2 <- d2 %>% mutate(Day1_SexDisplays = paste(`Day1_ParallelSwim-SameSexDorsalFin-I`+
                                              #`Day1_ParallelSwim-SameSexDorsalFin-I_s`+
                                              `Day1_ParallelSwim-SameSexNoFin-I`+
                                              #`Day1_ParallelSwim-SameSexNoFin-I_s`+
                                              `Day1_ParallelSwim-DiffSexDorsalFin-I`+
                                              #`Day1_ParallelSwim-DiffSexDorsalFin-I_s`+
                                              `Day1_ParallelSwim-DiffSexNoFin-I`+
                                              #`Day1_ParallelSwim-DiffSexNoFin-I_s +
                                              `Day1_TransverseApproach-DorsalFin`+
                                              `Day1_TransverseApproach-NoFin`+
                                              Day1_AttemptedCopulation +
                                              `Day1_SexDisp_Cwrap-DorsalFin`+
                                              `Day1_SexDisp_Cwrap-NoFin`+
                                              `Day1_SexDisp_FrontShimmy-DorsalFin`+
                                              `Day1_SexDisp_FrontShimmy-NoFin`)
                    )
d2 <- d2 %>% mutate(Day14_SexDisplays = paste(`Day14_ParallelSwim-SameSexDorsalFin-I`+
                                                 #`Day14_ParallelSwim-SameSexDorsalFin-I_s`+
                                                 `Day14_ParallelSwim-SameSexNoFin-I`+
                                                 #`Day14_ParallelSwim-SameSexNoFin-I_s`+
                                                 `Day14_ParallelSwim-DiffSexDorsalFin-I`+
                                                 #`Day14_ParallelSwim-DiffSexDorsalFin-I_s`+
                                                 `Day14_ParallelSwim-DiffSexNoFin-I`+
                                                 #`Day14_ParallelSwim-DiffSexNoFin-I_s +
                                                 `Day14_TransverseApproach-DorsalFin`+
                                                 `Day14_TransverseApproach-NoFin`+
                                                 Day14_AttemptedCopulation +
                                                 `Day14_SexDisp_Cwrap-DorsalFin`+
                                                 `Day14_SexDisp_Cwrap-NoFin`+
                                                 `Day14_SexDisp_FrontShimmy-DorsalFin`+
                                                 `Day14_SexDisp_FrontShimmy-NoFin`)
                    )
class(d2$Day1_SexDisplays) # character
d2 <- mutate(d2, Day1_SexDisplays = as.numeric(Day1_SexDisplays))
class(d2$Day1_SexDisplays) #numeric
class(d2$Day14_SexDisplays) # character
d2 <- mutate(d2, Day14_SexDisplays = as.numeric(Day14_SexDisplays))
class(d2$Day14_SexDisplays) #numeric

sexdisp <- ggplot(data = d2, aes(x = FoodCondition, y = (Day1_SexDisplays-Day14_SexDisplays)))
sexdisp <- sexdisp + geom_violin()
sexdisp

# LMs <- filter(d2, MorphSex == "LM")
