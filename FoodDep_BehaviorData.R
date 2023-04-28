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
## select the initiating behaviors for Day1 and Day14
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

## collapse behaviors into single column
ib.d1.14 <- initiating %>%
  pivot_longer(cols=starts_with("Day"),
               names_to='Behavior',
               values_to='IB_Frequency',
               values_drop_na = FALSE)
## separate Day#_ from the behavior
ib.d1.14 <- ib.d1.14 %>%
  separate_wider_delim(col = Behavior,
                       delim = "_",
                        names = c("Day", "Behavior")
                       # too_few = "debug",
                       # too_many = "debug"
                       )
# organize dataset: responding behaviors ----
## select the responding behaviors for Day1 and Day14
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

## responding collapse behaviors into single column
rb.d1.14 <- responding %>%
  pivot_longer(cols=starts_with("Day"),
               names_to='Behavior',
               values_to='RB_Frequency',
               values_drop_na = FALSE)
## responding separate Day#_ from the behavior
rb.d1.14 <- rb.d1.14 %>%
  separate_wider_delim(col = Behavior,
                       delim = "_",
                       names = c("Day", "Behavior")
                       # too_few = "debug",
                       # too_many = "debug"
  )
# BEHAVIOR CATEGORIES COMPARISON----
## plot initiations: wrap by behaviors
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
       y="Initiating Behavior Count (over 30min)") +
  theme(plot.title = element_text(size = 28),
        axis.title.x = element_text(size = 18), # x-axis
        axis.text.x = element_text(size = 18),
        axis.title.y = element_text(size = 18), # y-axis
        axis.text.y = element_text(size = 18)) +
  facet_wrap(~ Behavior + FoodCondition)
pIB.b

# plot responses: behavior + food
pRB.b <- ggplot(data = rb.d1.14, aes(x = Day,
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
       y="Responding Behavior Count (over 30min)") +
  theme(plot.title = element_text(size = 28),
        axis.title.x = element_text(size = 18), # x-axis
        axis.text.x = element_text(size = 18),
        axis.title.y = element_text(size = 18), # y-axis
        axis.text.y = element_text(size = 18)) +
  facet_wrap(~ Behavior + FoodCondition)
pRB.b

