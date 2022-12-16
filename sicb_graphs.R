#SICB Poster Data
#Load Packages
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci) # science theme for ggplot2
library(cowplot)
## linear models
library(lmodel2)
library(broom)
## skim stats
library(skimr)
## covariance heatmaps
library(pvclust)
library(pheatmap)

# DATA ANAKYSIS TIME
## spreadsheet prepared in excel
f<-"https://raw.githubusercontent.com/slcornett/FoodDepBehaviorPilot/main/SICB_Poster_Data.csv"
d <- read_csv(f, col_names = TRUE)
print(d)
## difference between day 14 and day 1: Day14 - Day 1
d <- d %>% mutate(#Diff_Chases = Day14_Chase - Day1_Chase, #count
                  #Diff_Chases_s = Day14_Chase_s - Day1_Chase_s, # seconds
                  #Diff_Charges = Day14_Charge - Day1_Charge,
                  #Diff_Initiatings = Day14_InitiatingMatingBehaviors - Day1_InitiatingMatingBehaviors,
                  #Diff_Responses = Day14_ResponseMatingBehaviors - Day1_ResponseMatingBehaviors,
                  # both initiating and response parallel swim bc otherwise biased to OMs
                  #Diff_Parallel_s = (`Day14_ParallelSwim-I_s`+`Day14_ParallelSwim-R_s`) -
                  #  (`Day1_ParallelSwim-I_s`+`Day1_ParallelSwim-R_s`),
                  #Diff_Refuge_s = Day14_Refuge_s - Day1_Refuge_s,
                  Day1_Aggression = Day1_Charge + Day1_Chase,
                  Day14_Aggression = Day14_Charge + Day14_Chase
                  )
print(d)
skim(d) # preliminary scan of data.

# BOXPLOTS OF DIFFERENCE DATA # Hans recommends tabling this for now.
## boxplot initiating behaviors count
initiating <- ggplot(data = d, aes(x = MorphSex,
                                   y = Diff_Initiatings,
                                  fill = FoodCondition)) +
  scale_color_startrek() +
  geom_boxplot(outlier.shape = NA ) +
  geom_point(position=position_jitterdodge()) +
  labs(x = "Morphological Sex", y = "Day14 - Day1 Initiating Behaviors (Count)") +
  ggtitle("Effects of Food Condition on the Occurance of Initiating Mating Behaviors") +
  theme_classic()+
  theme(plot.title = element_text(size = 20, color = "dark green", face = "bold"),
        axis.title.x = element_text(size = 18, color = "dark green"),
        axis.title.y = element_text(size = 18, color = "dark green"))
initiating
## boxplot response behaviors count
response <- ggplot(data = d, aes(x = MorphSex,
                                 y = Diff_Responses,
                                 fill = FoodCondition)) +
  scale_color_startrek()+
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge()) +
  labs(x = "Morphological Sex", y = "Day14 - Day1 Response Behaviors (Count)") +
  ggtitle("Effects of Food Condition on the Occurance of Responding Mating Behaviors") +
  theme_classic() +
  theme(plot.title = element_text(size = 20, color = "dark green", face = "bold"),
        axis.title.x = element_text(size = 18, color = "dark green"),
        axis.title.y = element_text(size = 18, color = "dark green"))
response
## boxplot parallel swim time (s)
Parallel_s <- ggplot(data = d, aes(x = MorphSex,
                                 y = Diff_Parallel_s,
                                 fill = FoodCondition)) +
  scale_color_startrek()+
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge()) +
  labs(x = "Morphological Sex", y = "Day14 - Day1 Parallel Swim (seconds)") +
  ggtitle("Effects of Food Condition on Time(s) Spent Participating in Parallel Swim") +
  theme_classic() +
  theme(plot.title = element_text(size = 20, color = "dark green", face = "bold"),
        axis.title.x = element_text(size = 18, color = "dark green"),
        axis.title.y = element_text(size = 18, color = "dark green"))
Parallel_s
## boxplot chase
Chases_s <- ggplot(data = d, aes(x = FoodCondition,
                                   y = Diff_Chases_s,
                                   fill = MorphSex)) +
  scale_color_startrek()+
  #geom_point(position=position_jitterdodge()) + # not needed here because the dataset so small
  geom_boxplot() + #outlier.shape = NA
  labs(x = "Food Condition", y = "Day14 - Day1 Parallel Swim (seconds)") +
  ggtitle("Effects of Food Condition on Time(s) Spent Chasing") +
  theme_classic()+
  theme(plot.title = element_text(size = 10, color = "dark green", face = "bold"),
        axis.title.x = element_text(size = 8, color = "dark green"),
        axis.title.y = element_text(size = 8, color = "dark green"))
Chases_s

# SCATTER PLOTS
## scatter plot of initiating behavior Day14 vs Day1
initiating_scat <- ggplot(data = d, aes(x = Day1_InitiatingMatingBehaviors,
                                        y = Day14_InitiatingMatingBehaviors,
                                        color = MorphSex))+
  geom_jitter() +
  theme_classic() +
  facet_wrap(~ FoodCondition)
initiating_scat
## scatter plot of response behaviors day 1 vs day 14
responding_scat <- ggplot(data = d, aes(x = Day1_ResponseMatingBehaviors,
                                        y = Day14_ResponseMatingBehaviors,
                                        color = MorphSex))+
  geom_jitter() +
  theme_classic() +
  facet_wrap(~ FoodCondition)
#facet_wrap(~MorphSex)
responding_scat

charge_scat <- ggplot(data = d, aes(x = Day1_Charge,
                                        y = Day14_Charge,
                                        color = MorphSex))+
  geom_jitter() +
  theme_classic() +
  facet_wrap(~ FoodCondition)
charge_scat
## scatterplot body size by diff initiating behaviors
# plot(data = d,  Diff_Responses ~ Diff_Initiatings)

#plot_grid(initiating, response, Parallel_s, ncol = 1) #group figured in print

# BEHAVIOR CATEGORIES COMPARISON--------
## selecting data by day1 and day14:
### select initiating behaviors
Day1.14_ib <- d %>% select(Fish,
                    Population,
                    FoodCondition,
                    Day1_InitiatingMatingBehaviors,
                    #Day1_ResponseMatingBehaviors,
                    #Day1_Aggression,
                    Day14_InitiatingMatingBehaviors,
                    #Day14_ResponseMatingBehaviors,
                    #Day14_Aggression
                    )
Day1.14_ib
## use pivot_longer() to collapse days into single column, with Day ID as a new categorical data column
### initiating behaviors
Day1.14_ib <- Day1.14_ib %>%
  pivot_longer(cols=c('Day1_InitiatingMatingBehaviors', 'Day14_InitiatingMatingBehaviors'),
               names_to='Day',
               values_to='InitiatingBehaviors_Count')
Day1.14_ib
### select response behaviors
Day1.14_rb <- d %>% select(Fish,
                           Population,
                           FoodCondition,
                           #Day1_InitiatingMatingBehaviors,
                           Day1_ResponseMatingBehaviors,
                           #Day1_Aggression,
                           #Day14_InitiatingMatingBehaviors,
                           Day14_ResponseMatingBehaviors,
                           #Day14_Aggression
                           )
### response behaviors collapse data
Day1.14_rb <- Day1.14_rb %>%
  pivot_longer(cols=c('Day1_ResponseMatingBehaviors', 'Day14_ResponseMatingBehaviors'),
               names_to='Day',
               values_to='ResponseBehaviors_Count')
Day1.14_rb
### select aggressive behaviors
Day1.14_ab <- d %>% select(Fish,
                           Population,
                           FoodCondition,
                           #Day1_InitiatingMatingBehaviors,
                           #Day1_ResponseMatingBehaviors,
                           Day1_Aggression,
                           #Day14_InitiatingMatingBehaviors,
                           #Day14_ResponseMatingBehaviors,
                           Day14_Aggression
)
### aggressive behaviors collapse data
Day1.14_ab <- Day1.14_ab %>%
  pivot_longer(cols=c('Day1_Aggression', 'Day14_Aggression'),
               names_to='Day',
               values_to='Aggro_Count')
Day1.14_ab

## plot behavior categories by population
### plot initiations
pIB <- ggplot(data = Day1.14_ib, aes(x = Day,
                          y = InitiatingBehaviors_Count,
                          color = FoodCondition))+
  scale_color_startrek(alpha = 0.75) + # so can see overlapping points
  # use the group aesthetic to map a different line for each subject.
  geom_line(aes(group=Fish), color = "gray", linewidth=0.5) + # group = {Subject}, the individual linking the two data points
  geom_point(aes(color=FoodCondition), position=position_jitter(0.05), size=4) + #, position=position_jitter(0.1) # off-sets the data points
  theme_classic() +
  labs(x="Food Deprivation Day",
       y="Initiating Behavior Count (over 30min)") +
  facet_wrap(~ Population)
pIB
### plot responses
pRB <- ggplot(data = Day1.14_rb, aes(x = Day,
                                     y = ResponseBehaviors_Count,
                                     color = FoodCondition))+
  scale_color_startrek(alpha = 0.75) + # so can see overlapping points
  # use the group aesthetic to map a different line for each subject.
  geom_line(aes(group=Fish), color = "gray", linewidth=0.5) + # group = {Subject}, the individual linking the two data points
  geom_point(aes(color=FoodCondition), position=position_jitter(0.05), size=4) + #, position=position_jitter(0.1) # off-sets the data points
  theme_classic() +
  labs(x="Food Deprivation Day",
       y="Response Behavior Count (over 30min)") +
  facet_wrap(~ Population)
pRB
### plot aggression
pAB <- ggplot(data = Day1.14_ab, aes(x = Day,
                                     y = Aggro_Count,
                                     color = FoodCondition))+
  scale_color_startrek(alpha = 0.75) + # so can see overlapping points
  # use the group aesthetic to map a different line for each subject.
  geom_line(aes(group=Fish), color = "gray", linewidth=0.5) + # group = {Subject}, the individual linking the two data points
  geom_point(aes(color=FoodCondition), position=position_jitter(0.05), size=4) + #, position=position_jitter(0.1) # off-sets the data points
  theme_classic() +
  labs(x="Food Deprivation Day",
       y="Aggressive Behavior Count (over 30min)") +
  facet_wrap(~ Population)
pAB

# COVARIENCE MATRIX PLOTS
## Selecting for Day1
Day1cov_input<- d %>% select(Day1_Chase,
                             Day1_Chase_s,
                             Day1_Charge,
                             `Day1_ParallelSwim-I`,
                             `Day1_ParallelSwim-I_s`,
                             `Day1_ParallelSwim-R`,
                             `Day1_ParallelSwim-R_s`,
                             Day1_TransverseApproach,
                             Day1_SexDisp_Cwrap,
                             Day1_SexDisp_FrontShimmy,
                             Day1_AttemptedCopulation,
                             Day1_Nip,
                             Day1_Stay,
                             Day1_Backup,
                             Day1_Retreat,
                             Day1_Glide,
                             Day1_Dart)
## Selecting for Day14
Day14cov_input<- d %>% select(Day14_Chase,
                             Day14_Chase_s,
                             Day14_Charge,
                             `Day14_ParallelSwim-I`,
                             `Day14_ParallelSwim-I_s`,
                             `Day14_ParallelSwim-R`,
                             `Day14_ParallelSwim-R_s`,
                             Day14_TransverseApproach,
                             Day14_SexDisp_Cwrap,
                             Day14_SexDisp_FrontShimmy,
                             Day14_AttemptedCopulation,
                             Day14_Nip,
                             Day14_Stay,
                             Day14_Backup,
                             Day14_Retreat,
                             Day14_Glide,
                             Day14_Dart)
## selecting for all behaviors
Allcov_input <- d %>% select(Day1_Chase, # all Day 1
                            Day1_Chase_s,
                            Day1_Charge,
                            `Day1_ParallelSwim-I`,
                            `Day1_ParallelSwim-I_s`,
                            `Day1_ParallelSwim-R`,
                            `Day1_ParallelSwim-R_s`,
                            Day1_TransverseApproach,
                            Day1_SexDisp_Cwrap,
                            Day1_SexDisp_FrontShimmy,
                            Day1_AttemptedCopulation,
                            Day1_Nip,
                            Day1_Stay,
                            Day1_Backup,
                            Day1_Retreat,
                            Day1_Glide,
                            Day1_Dart,
                            # all day 14
                            Day14_Chase,
                            Day14_Chase_s,
                            Day14_Charge,
                            `Day14_ParallelSwim-I`,
                            `Day14_ParallelSwim-I_s`,
                            `Day14_ParallelSwim-R`,
                            `Day14_ParallelSwim-R_s`,
                            Day14_TransverseApproach,
                            Day14_SexDisp_Cwrap,
                            Day14_SexDisp_FrontShimmy,
                            Day14_AttemptedCopulation,
                            Day14_Nip,
                            Day14_Stay,
                            Day14_Backup,
                            Day14_Retreat,
                            Day14_Glide,
                            Day14_Dart)
##Hierarchical clustering with bootstrap, using pvclust: https://github.com/shimo-lab/pvclust#####
pvclust<-pvclust(#{input data set}}, ## WHAT CAN I PUT HERE IF I WANT TO DO TWO TIME POINTS AGAINST EACH OTHER?
                 method.dist="cor",
                 method.hclust="complete",
                 nboot=1000)
### Plot the dendrogram result
plot(pvclust)
###Box out the significant ## p value calculation
pvrect(pvclust, alpha=0.95)

########Calculate the covariance matrix heatmap######
# X = dataset (if y = Null, don't need it explicitly denoted)
# Y = null because for a covariance of data points, we don't care about individuals.
# the column headers used automatically
cor.Daycov_input<- cor(x= Day1cov_input,
                       y = Day14cov_input,
                       use = "everything",
                       method = c("pearson"))
#Visualize the covariance matrix using pheatmap (a more advanced package for ploting heatmaps),
#columns and rows are sorted as the hierarchical clustering results
pheatmap(cor.Daycov_input)
#         cluster_cols = pvclust$hclust, # bootstrap values of covariance
#         cluster_rows = pvclust$hclust)

