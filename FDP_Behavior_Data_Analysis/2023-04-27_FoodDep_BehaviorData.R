# Project: Food Deprivation Pilot Project
# Behavior Data
# Load packages ----
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(skimr)

# load dataset ----
f2 <- "https://raw.githubusercontent.com/slcornett/FoodDepBehaviorPilot/main/fdp_datasets/2023-04-27_FoodDep_Pilot_Behavior.csv"
d2 <- read_csv(f2, col_names = TRUE)
print(d2)

# organize dataset: initiating behaviors ----
## initiating behaviors: select the initiating behaviors for Day1 and Day14
initiating <- d2 %>% select(Fish,
                            MorphSex,
                            FoodCondition,
                            Day1_Chase,
                            Day1_ParallelSwim.I,
                            Day1_TransverseApproach,
                            Day1_AttemptedCopulation,
                            Day1_SexDispCwrap,
                            Day1_SexDispFrontShimmy,
                            Day1_Nip,
                            Day14_Chase,
                            Day14_ParallelSwim.I,
                            Day14_TransverseApproach,
                            Day14_AttemptedCopulation,
                            Day14_SexDispCwrap,
                            Day14_SexDispFrontShimmy,
                            Day14_Nip)

## initiating behaviors: collapse behaviors into single column
ib.d1.14 <- initiating %>%
  pivot_longer(cols=starts_with("Day"),
               names_to='Behavior',
               values_to='IB_Frequency',
               values_drop_na = FALSE)
## initiating behaviors: separate Day#_ from the behavior
ib.d1.14 <- ib.d1.14 %>%
  separate_wider_delim(col = Behavior,
                       delim = "_",
                        names = c("Day", "Behavior")
                       # too_few = "debug",
                       # too_many = "debug"
                       )

## initiating behaviors 2: selecting specific initiating behaviors for molly, Day 1 and 14
initiating2 <- d2 %>% select(Fish,
                             MorphSex,
                             FoodCondition,
                             Day1_Chase,
                             Day1_TransverseApproach,
                             Day1_SexDispCwrap,
                             Day1_SexDispFrontShimmy,
                             Day14_Chase,
                             Day14_TransverseApproach,
                             Day14_SexDispCwrap,
                             Day14_SexDispFrontShimmy)
## initiating behaviors 2: collapse behaviors into single column
ib.d1.14_2 <- initiating2 %>%
  pivot_longer(cols=starts_with("Day"),
               names_to='Behavior',
               values_to='IB_Frequency',
               values_drop_na = FALSE)
## initiating behaviors 2: separate Day#_ from the behavior
ib.d1.14_2 <- ib.d1.14_2 %>%
  separate_wider_delim(col = Behavior,
                       delim = "_",
                       names = c("Day", "Behavior")
                       # too_few = "debug",
                       # too_many = "debug"
  )
# initiating behaviors 3: remove chase and c-wrap because no trends, remove small males
initiating3 <- d2 %>% select(Fish,
                             MorphSex,
                             FoodCondition,
                             Day1_TransverseApproach,
                             Day1_SexDispFrontShimmy,
                             Day14_TransverseApproach,
                             Day14_SexDispFrontShimmy)
## initiating behaviors 3: filter out small males (4 SM removed)
initiating3 <- initiating3 %>% filter(MorphSex == "F"|MorphSex == "OM")
## initiating behaviors 3: collapse behaviors into single column
ib.d1.14_3 <- initiating3 %>%
  pivot_longer(cols=starts_with("Day"),
               names_to='Behavior',
               values_to='IB_Frequency',
               values_drop_na = FALSE)
## initiating behaviors 3: separate Day#_ from the behavior
ib.d1.14_3 <- ib.d1.14_3 %>%
  separate_wider_delim(col = Behavior,
                       delim = "_",
                       names = c("Day", "Behavior")
                       # too_few = "debug",
                       # too_many = "debug"
  )

# organize dataset: responding behaviors ----
## responding behaviors: select the responding behaviors for Day1 and Day14
responding <- d2 %>% select(Fish,
                            MorphSex,
                            FoodCondition,
                            Day1_Stay,
                            Day1_Backup,
                            Day1_Dart,
                            Day1_Glide,
                            Day1_ParallelSwim.R,
                            Day1_Retreat,
                            Day14_Stay,
                            Day14_Backup,
                            Day14_Dart,
                            Day14_Glide,
                            Day14_ParallelSwim.R,
                            Day14_Retreat)

## responding behaviors: collapse behaviors into single column
rb.d1.14 <- responding %>%
  pivot_longer(cols=starts_with("Day"),
               names_to='Behavior',
               values_to='RB_Frequency',
               values_drop_na = FALSE)
## responding behaviors: separate Day#_ from the behavior
rb.d1.14 <- rb.d1.14 %>%
  separate_wider_delim(col = Behavior,
                       delim = "_",
                       names = c("Day", "Behavior")
                       # too_few = "debug",
                       # too_many = "debug"
  )

## responding behaviors 2: selecting specific responding behaviors for molly, Day 1 and 14
responding2 <- d2 %>% select(Fish,
                             MorphSex,
                             FoodCondition,
                             Day1_Stay,
                             Day1_Dart,
                             Day14_Stay,
                             Day14_Dart)

## responding behaviors 2: collapse behaviors into single column
rb.d1.14_2 <- responding2 %>%
  pivot_longer(cols=starts_with("Day"),
               names_to='Behavior',
               values_to='RB_Frequency',
               values_drop_na = FALSE)
## responding behaviors 2: separate Day#_ from the behavior
rb.d1.14_2 <- rb.d1.14_2 %>%
  separate_wider_delim(col = Behavior,
                       delim = "_",
                       names = c("Day", "Behavior")
                       # too_few = "debug",
                       # too_many = "debug"
  )

# responding behaviors 3: remove small males
responding3 <- d2 %>% select(Fish,
                             MorphSex,
                             FoodCondition,
                             Day1_Stay,
                             Day1_Dart,
                             Day14_Stay,
                             Day14_Dart)

## responding behaviors 3: filter out small males (4 SM removed)
responding3 <- responding3 %>% filter(MorphSex == "F"|MorphSex == "OM")

## responding behaviors 2: collapse behaviors into single column
rb.d1.14_3 <- responding3 %>%
  pivot_longer(cols=starts_with("Day"),
               names_to='Behavior',
               values_to='RB_Frequency',
               values_drop_na = FALSE)
## responding behaviors 2: separate Day#_ from the behavior
rb.d1.14_3 <- rb.d1.14_3 %>%
  separate_wider_delim(col = Behavior,
                       delim = "_",
                       names = c("Day", "Behavior")
                       # too_few = "debug",
                       # too_many = "debug"
  )

# BEHAVIOR CATEGORIES COMPARISON----
## plot initiations: wrap by behaviors + food condition
pIB.b <- ggplot(data = ib.d1.14, aes(x = Day,
                                    y = IB_Frequency,
                                    color = MorphSex))+
  scale_color_manual(values = c("#4B0055", "#1F948C", "#FDE333")) +
  #scale_color_startrek(alpha = 0.75) + # so can see overlapping points
  # use the group aesthetic to map a different line for each subject.
  geom_line(aes(group=Fish), color="darkgrey", linewidth=0.5) + # group = {Subject}, the individual linking the two data points
  geom_point(aes(color=MorphSex), position=position_jitter(0.05), size=4, alpha=0.75) + #, position=position_jitter(0.1) # off-sets the data points
  theme_classic() +
  #scale_y_continuous(breaks=pretty(Day1.14_ib$InitiatingBehaviors_Count, n=10)) +
  labs(title = "Initiating Behaviors",
       x ="Food Deprivation Day",
       y="Initiating Behavior Count (over 20min)") +
  theme(plot.title = element_text(size = 28),
        axis.title.x = element_text(size = 14), # x-axis
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14), # y-axis
        axis.text.y = element_text(size = 14)) +
  facet_wrap(~ Behavior + FoodCondition)
pIB.b

## plot initiations 2: wrap by behaviors + food condition
pIB.b2 <- ggplot(data = ib.d1.14_2, aes(x = Day,
                                        y = IB_Frequency,
                                        color = MorphSex))+
  scale_color_manual(values = c("#4B0055", "#1F948C", "#FDE333")) +
  #scale_color_startrek(alpha = 0.75) + # so can see overlapping points
  # use the group aesthetic to map a different line for each subject.
  geom_line(aes(group=Fish), color="darkgrey", linewidth=0.5) + # group = {Subject}, the individual linking the two data points
  geom_point(aes(color=MorphSex), position=position_jitter(0.08), size=4, alpha=0.75) + #, position=position_jitter(0.1) # off-sets the data points
  theme_classic() +
  #scale_y_continuous(breaks=pretty(Day1.14_ib$InitiatingBehaviors_Count, n=10)) +
  labs(title = "Initiating Behaviors",
       x ="Food Deprivation Day",
       y="Initiating Behavior Count (over 20min)") +
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 14, color = "black"), # x-axis
        axis.text.x = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 14, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black")) +
  facet_wrap(ncol = 4, ~ Behavior + FoodCondition)
pIB.b2

## plot initiations 3: wrap by behaviors + food condition
pIB.b3 <- ggplot(data = ib.d1.14_3, aes(x = Day,
                                        y = IB_Frequency,
                                        color = MorphSex))+
  scale_color_manual(values = c("#4B0055", "#1F948C")) +
  #scale_color_startrek(alpha = 0.75) + # so can see overlapping points
  # use the group aesthetic to map a different line for each subject.
  geom_line(aes(group=Fish), color="darkgrey", linewidth=0.5) + # group = {Subject}, the individual linking the two data points
  geom_point(aes(color=MorphSex), position=position_jitter(0.08), size=4, alpha=0.75) + #, position=position_jitter(0.1) # off-sets the data points
  theme_classic() +
  labs(title = "Initiating Behaviors",
       x ="Food Deprivation Day",
       y="Initiating Behavior Count (over 20min)") +
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 14, color = "black"), # x-axis
        axis.text.x = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 14, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black")) +
  facet_wrap( ~ Behavior + FoodCondition) #ncol = 4,
pIB.b3

## plot responses: behavior + food
pRB.b <- ggplot(data = rb.d1.14, aes(x = Day,
                                     y = RB_Frequency,
                                     color = MorphSex))+
  scale_color_manual(values = c("#4B0055", "#1F948C", "#FDE333")) +
  #scale_color_startrek(alpha = 0.75) + # so can see overlapping points
  # use the group aesthetic to map a different line for each subject.
  geom_line(aes(group=Fish), color="darkgrey", linewidth=0.5) + # group = {Subject}, the individual linking the two data points
  geom_point(aes(color=MorphSex), position=position_jitter(0.08), size=4, alpha=0.75) + #, position=position_jitter(0.1) # off-sets the data points
  theme_classic() +
  #scale_y_continuous(breaks=pretty(Day1.14_ib$InitiatingBehaviors_Count, n=10)) +
  labs(title = "Responding Behaviors",
       x ="Food Deprivation Day",
       y="Responding Behavior Count (over 20min)") +
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 14, color = "black"), # x-axis
        axis.text.x = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 14, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black")) +
  facet_wrap(~ Behavior + FoodCondition)
pRB.b

## plot responses 2: behavior + food
pRB.b2 <- ggplot(data = rb.d1.14_2, aes(x = Day,
                                        y = RB_Frequency,
                                        color = MorphSex))+
  scale_color_manual(values = c("#4B0055", "#1F948C", "#FDE333")) +
  #scale_color_startrek(alpha = 0.75) + # so can see overlapping points
  # use the group aesthetic to map a different line for each subject.
  geom_line(aes(group=Fish), color="darkgrey", linewidth=0.5) + # group = {Subject}, the individual linking the two data points
  geom_point(aes(color=MorphSex), position=position_jitter(0.05), size=4, alpha=0.75) + #, position=position_jitter(0.1) # off-sets the data points
  theme_classic() +
  #scale_y_continuous(breaks=pretty(Day1.14_ib$InitiatingBehaviors_Count, n=10)) +
  labs(title = "Responding Behaviors",
       x ="Food Deprivation Day",
       y="Responding Behavior Count (over 20min)") +
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 14, color = "black"), # x-axis
        axis.text.x = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 14, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black")) +
  facet_wrap(~ Behavior + FoodCondition)
pRB.b2

## plot responses 2: behavior + food
pRB.b3 <- ggplot(data = rb.d1.14_3, aes(x = Day,
                                        y = RB_Frequency,
                                        color = MorphSex))+
  scale_color_manual(values = c("#4B0055", "#1F948C")) +
  #scale_color_startrek(alpha = 0.75) + # so can see overlapping points
  # use the group aesthetic to map a different line for each subject.
  geom_line(aes(group=Fish), color="darkgrey", linewidth=0.5) + # group = {Subject}, the individual linking the two data points
  geom_point(aes(color=MorphSex), position=position_jitter(0.08), size=4, alpha=0.75) + #, position=position_jitter(0.1) # off-sets the data points
  theme_classic() +
  labs(title = "Responding Behaviors",
       x ="Food Deprivation Day",
       y="Responding Behavior Count (over 20min)") +
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 14, color = "black"), # x-axis
        axis.text.x = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 14, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black")) +
  facet_wrap(~ Behavior + FoodCondition)
pRB.b3
