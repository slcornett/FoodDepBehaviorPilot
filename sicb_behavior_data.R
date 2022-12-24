#SICB Poster Data, Behavior
#Load Packages----
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci) # science theme for ggplot2
library(cowplot)
## skim stats
#library(skimr)
## covariance heatmaps
library(pvclust)
library(pheatmap)
## correlation matrix
library(corrplot)
library(PerformanceAnalytics) # for correlation matrix
library(RColorBrewer) # for pheatmap colors

# DATA ANAKYSIS TIME
## spreadsheet prepared in excel
f<-"https://raw.githubusercontent.com/slcornett/FoodDepBehaviorPilot/main/SICB_Behavior_Data.csv"
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
  ## Aggression Category of Behavior
                  Day1_Aggression = Day1_Charge + Day1_Chase,
                  Day14_Aggression = Day14_Charge + Day14_Chase
                  )
print(d)
#skim(d) # preliminary scan of data.
# SCATTERPLOT WITH OUT-OF-FRAME----
OoF <- d %>% select(Fish,
                    MorphSex,
                    Population,
                    FoodCondition,
                    Day1_InitiatingMatingBehaviors,
                    Day1_ResponseMatingBehaviors,
                    Day1_Aggression,
                    Day1_OutofFrame_s,
                    Day14_InitiatingMatingBehaviors,
                    Day14_ResponseMatingBehaviors,
                    Day14_Aggression,
                    Day14_OutofFrame_s) %>%
  mutate(InitiatingMatingBehaviors = Day1_InitiatingMatingBehaviors + Day14_InitiatingMatingBehaviors,
         ResponseMatingBehaviors = Day1_ResponseMatingBehaviors + Day14_ResponseMatingBehaviors,
         Aggression = Day1_Aggression + Day14_Aggression,
         OutOfFrame = Day1_OutofFrame_s + Day14_OutofFrame_s)
# OoF v Initiating Behaviors
OofI <- ggplot(data = OoF, aes(x = OutOfFrame,
                               y = InitiatingMatingBehaviors,
                               color = FoodCondition)) +
  scale_color_startrek(alpha = 0.75) +
  geom_point(size=6) + #, position=position_jitter(0.1) # off-sets the data points
  theme_classic() +
  labs(title = "Out of Frame (s) vs Initiating Behaviors",
       x ="Out of Frame (s)",
       y="Initiating Behaviors Count (over 30min)")
OofI

# OoF v Responding Behaviors
OofR <- ggplot(data = OoF, aes(x = OutOfFrame,
                               y = ResponseMatingBehaviors,
                               color = FoodCondition)) +
  scale_color_startrek(alpha = 0.75) +
  geom_point(size=6) + #, position=position_jitter(0.1) # off-sets the data points
  theme_classic() +
  labs(title = "Out of Frame (s) vs Response Behaviors",
       x ="Out of Frame (s)",
       y="Response Behaviors Count (over 30min)")
OofR

# OoF v Aggressive Behaviors
OofA <- ggplot(data = OoF, aes(x = OutOfFrame,
                               y = Aggression,
                               color = FoodCondition)) +
  scale_color_startrek(alpha = 0.75) +
  geom_point(size=6) + #, position=position_jitter(0.1) # off-sets the data points
  theme_classic() +
  labs(title = "Out of Frame (s) vs Aggressive Behaviors",
       x ="Out of Frame (s)",
       y="Aggressive Behaviors Count (over 30min)")
OofA
# BOXPLOTS OF DIFFERENCE DATA -------
# Hans recommends tabling this for now.
## boxplot initiating behaviors count
#initiating <- ggplot(data = d, aes(x = MorphSex,
#                                   y = Diff_Initiatings,
#                                  fill = FoodCondition)) +
#  scale_color_startrek() +
#  geom_boxplot(outlier.shape = NA ) +
#  geom_point(position=position_jitterdodge()) +
#  labs(x = "Morphological Sex", y = "Day14 - Day1 Initiating Behaviors (Count)") +
#  ggtitle("Effects of Food Condition on the Occurance of Initiating Mating Behaviors") +
#  theme_classic()+
#  theme(plot.title = element_text(size = 20, color = "dark green", face = "bold"),
#        axis.title.x = element_text(size = 18, color = "dark green"),
#        axis.title.y = element_text(size = 18, color = "dark green"))
#initiating
## boxplot response behaviors count
#response <- ggplot(data = d, aes(x = MorphSex,
#                                 y = Diff_Responses,
#                                 fill = FoodCondition)) +
#  scale_color_startrek()+
#  geom_boxplot(outlier.shape = NA) +
#  geom_point(position=position_jitterdodge()) +
#  labs(x = "Morphological Sex", y = "Day14 - Day1 Response Behaviors (Count)") +
#  ggtitle("Effects of Food Condition on the Occurance of Responding Mating Behaviors") +
#  theme_classic() +
#  theme(plot.title = element_text(size = 20, color = "dark green", face = "bold"),
#        axis.title.x = element_text(size = 18, color = "dark green"),
#        axis.title.y = element_text(size = 18, color = "dark green"))
#response
## boxplot parallel swim time (s)
#Parallel_s <- ggplot(data = d, aes(x = MorphSex,
#                                 y = Diff_Parallel_s,
#                                 fill = FoodCondition)) +
#  scale_color_startrek()+
#  geom_boxplot(outlier.shape = NA) +
#  geom_point(position=position_jitterdodge()) +
#  labs(x = "Morphological Sex", y = "Day14 - Day1 Parallel Swim (seconds)") +
#  ggtitle("Effects of Food Condition on Time(s) Spent Participating in Parallel Swim") +
#  theme_classic() +
#  theme(plot.title = element_text(size = 20, color = "dark green", face = "bold"),
#        axis.title.x = element_text(size = 18, color = "dark green"),
#        axis.title.y = element_text(size = 18, color = "dark green"))
#Parallel_s

## boxplot chase
#Chases_s <- ggplot(data = d, aes(x = FoodCondition,
#                                   y = Diff_Chases_s,
#                                   fill = MorphSex)) +
#  scale_color_startrek()+
#  #geom_point(position=position_jitterdodge()) + # not needed here because the dataset so small
#  geom_boxplot() + #outlier.shape = NA
#  labs(x = "Food Condition", y = "Day14 - Day1 Parallel Swim (seconds)") +
#  ggtitle("Effects of Food Condition on Time(s) Spent Chasing") +
#  theme_classic()+
#  theme(plot.title = element_text(size = 10, color = "dark green", face = "bold"),
#        axis.title.x = element_text(size = 8, color = "dark green"),
#        axis.title.y = element_text(size = 8, color = "dark green"))
#Chases_s

# SCATTER PLOTS
## scatter plot of initiating behavior Day14 vs Day1
#initiating_scat <- ggplot(data = d, aes(x = Day1_InitiatingMatingBehaviors,
#                                        y = Day14_InitiatingMatingBehaviors,
#                                        color = MorphSex))+
#  geom_jitter() +
#  theme_classic() +
#  facet_wrap(~ FoodCondition)
#initiating_scat

## scatter plot of response behaviors day 1 vs day 14
#responding_scat <- ggplot(data = d, aes(x = Day1_ResponseMatingBehaviors,
#                                        y = Day14_ResponseMatingBehaviors,
#                                        color = MorphSex))+
#  geom_jitter() +
#  theme_classic() +
#  facet_wrap(~ FoodCondition)
#facet_wrap(~MorphSex)
#responding_scat

#charge_scat <- ggplot(data = d, aes(x = Day1_Charge,
#                                        y = Day14_Charge,
#                                        color = MorphSex))+
#  geom_jitter() +
#  theme_classic() +
#  facet_wrap(~ FoodCondition)
#charge_scat

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
### plot initiations: all by day
pIB.sex <- ggplot(data = Day1.14_ib, aes(x = Day,
                                     y = InitiatingBehaviors_Count,
                                     color = FoodCondition))+
  scale_color_startrek(alpha = 0.75) + # so can see overlapping points
  # use the group aesthetic to map a different line for each subject.
  geom_line(aes(group=Fish), color = "gray", linewidth=0.5) + # group = {Subject}, the individual linking the two data points
  geom_point(aes(color=FoodCondition), position=position_jitter(0.05), size=6) + #, position=position_jitter(0.1) # off-sets the data points
  theme_classic() +
  scale_y_continuous(breaks=pretty(Day1.14_ib$InitiatingBehaviors_Count, n=10)) +
  labs(title = "Initiating Behaviors",
       x ="Food Deprivation Day",
       y="Initiating Behavior Count (over 30min)") +
  theme(plot.title = element_text(size = 28),
        axis.title.x = element_text(size = 18), # x-axis
        axis.text.x = element_text(size = 18),
        axis.title.y = element_text(size = 18), # y-axis
        axis.text.y = element_text(size = 18)) +
  facet_wrap(~ MorphSex)
pIB.sex
### plot responses
pRB <- ggplot(data = Day1.14_rb, aes(x = Day,
                                     y = ResponseBehaviors_Count,
                                     color = FoodCondition))+
  scale_color_startrek(alpha = 0.75) + # so can see overlapping points
  # use the group aesthetic to map a different line for each subject.
  geom_line(aes(group=Fish), color = "gray", linewidth=0.5) + # group = {Subject}, the individual linking the two data points
  geom_point(aes(color=FoodCondition), position=position_jitter(0.05), size=6) + #, position=position_jitter(0.1) # off-sets the data points
  theme_classic() +
  scale_y_continuous(breaks=pretty(Day1.14_rb$ResponseBehaviors_Count, n=10)) +
  labs(title = "Responding Behaviors",
       x="Food Deprivation Day",
       y="Response Behavior Count (over 30min)") +
  theme(plot.title = element_text(size = 28),
        axis.title.x = element_text(size = 18), # x-axis
        axis.text.x = element_text(size = 18),
        axis.title.y = element_text(size = 18), # y-axis
        axis.text.y = element_text(size = 18)) +
  facet_wrap(~ Population)
pRB
### plot aggression
pAB <- ggplot(data = Day1.14_ab, aes(x = Day,
                                     y = Aggro_Count,
                                     color = FoodCondition))+
  scale_color_startrek(alpha = 0.75) + # so can see overlapping points
  # use the group aesthetic to map a different line for each subject.
  geom_line(aes(group=Fish), color = "gray", linewidth=0.5) + # group = {Subject}, the individual linking the two data points
  geom_point(aes(color=FoodCondition), position=position_jitter(0.05), size=6) + #, position=position_jitter(0.1) # off-sets the data points
  theme_classic() +
  scale_y_continuous(breaks=pretty(Day1.14_ab$Aggro_Count, n=5)) +
  labs(title = "Aggressive Behaviors",
       x="Food Deprivation Day",
       y="Aggressive Behavior Count (over 30min)") +
  #increase font size for poster
  theme(plot.title = element_text(size = 28),
        axis.title.x = element_text(size = 18), # x-axis
        axis.text.x = element_text(size = 18),
        axis.title.y = element_text(size = 18), # y-axis
        axis.text.y = element_text(size = 18)) +
  facet_wrap(~ Population)
pAB

# SCATTERPLOTS OF OUT-OF-FRAME----
## selecting for behavior categories and out-of-frame

# COVARIENCE MATRIX PLOT 1: ALL BEHAVIORS, D1 V D14-------
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
                             Day1_Dart,
                             Day1_Refuge_s)
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
                             Day14_Dart,
                             Day14_Refuge_s)
## Hierarchical clustering with bootstrap, using pvclust: https://github.com/shimo-lab/pvclust
### Day1
Day1_pvclust<-pvclust(Day1cov_input,
                 method.dist="cor",
                 method.hclust="complete",
                 nboot=1000)
### Day14
Day14_pvclust<-pvclust(Day14cov_input,
                      method.dist="cor",
                      method.hclust="complete",
                      nboot=1000)

## Plot the dendrogram result
### Day1
plot(Day1_pvclust)
### Day14
plot(Day14_pvclust)

## Box out the significant ## p value calculation
### Day1
pvrect(Day1_pvclust, alpha=0.95)
### Day14
pvrect(Day14_pvclust, alpha=0.95)

## Calculate the covariance matrix heatmap
# X = dataset (if y = Null, don't need it explicitly denoted)
# Y = null because for a covariance of data points, we don't care about individuals.
# the column headers used automatically
cor.Daycov_input<- cor(x= Day1cov_input,
                       y = Day14cov_input,
                       use = "everything",
                       method = c("pearson"))
#Visualize the covariance matrix using pheatmap (a more advanced package for ploting heatmaps),
#columns and rows are sorted as the hierarchical clustering results
range1 <- max(abs(cor.Daycov_input))
pheatmap(cor.Daycov_input,
         # customize covarience legend so makes more sense
         legend = TRUE,
         breaks = seq(-range1, range1, length.out = 150),
         # BLUE-YELLOW palette: hcl.colors(100, "BluYl")
         color = paletteer_c("grDevices::ag_GrnYl", 150),
         # GREEN-PURPLE palette: # n = the saturation/darkness/closeness of the two ends of the color spectrum
         #colorRampPalette(rev(brewer.pal(n = 10, name = "PRGn")))(100),
         border_color = "black",
         cluster_cols = Day1_pvclust$hclust, # bootstrap values of covariance
         cluster_rows = Day14_pvclust$hclust,
         main = "Covariance of X. nigrensis Behaviors on Day 1 vs Day 14")

# COVARIANCE MATRIX PLOT 2: ALL FEMALES-------
## Selecting for all Females - DAY 1
F.D1cov_input<- d %>% filter(MorphSex == "F") %>%
  select(Day1_Chase,
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
         Day1_Refuge_s)
### F -  DAY 14
F.D14cov_input<- d %>% filter(MorphSex == "F") %>%
  select(Day14_Chase,
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
         Day14_Dart,
         Day14_Refuge_s)

## FEMALES Hierarchical clustering with bootstrap, using pvclust: https://github.com/shimo-lab/pvclust
### F Day1
F.D1_pvclust<-pvclust(F.D1cov_input, # WORKS WITHOUT ERROR
                      method.dist="cor",
                      method.hclust="complete",
                      nboot=1000)
### F Day14
# RETURNING AN ERROR: Error in hclust(distance, method = method.hclust) :
#  NA/NaN/Inf in foreign function call (arg 10)
# In addition: Warning message:
#  In cor(x, method = "pearson", use = use.cor) :
#  the standard deviation is zero
F.D14_pvclust<-pvclust(F.D14cov_input, # ERROR
                       method.dist="cor",
                       method.hclust="complete",
                       nboot=1000)
# STOPPED HERE FOR NOW BECAUSE OF ERROR. CHECK TO SEE IF OCCURS IN POPULATION FILTER

# COVARIANCE MATRIX PLOT 3:F+SM Population -------
## Selecting for F+SM Pop - DAY 1
FSM.D1cov_input<- d %>% filter(Population == "F+SM") %>%
  select(Day1_Chase,
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
         Day1_Refuge_s)
### F+OM Pop -  DAY 14
FSM.D14cov_input<- d %>% filter(Population == "F+SM") %>%
  select(Day14_Chase,
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
         Day14_Dart,
         Day14_Refuge_s)
## F+SM Hierarchical clustering with bootstrap, using pvclust: https://github.com/shimo-lab/pvclust
### F+SM Day1
FSM.D1_pvclust<-pvclust(FSM.D1cov_input, # gives the same error
                        method.dist="cor",
                        method.hclust="complete",
                        nboot=1000)
### F+SM Day14
FSM.D14_pvclust<-pvclust(FSM.D14cov_input, # gives the same error
                        method.dist="cor",
                        method.hclust="complete",
                        nboot=1000)

## Plot the dendrogram result
### F+SM Day1
plot(FSM.D1_pvclust)
### F+SM Day14
plot(FSM.D14_pvclust)

# ## Box out the significant ## p value calculation ---- # gotta fix error first
# ### Day1
# pvrect(FSM.D1_pvclust, alpha=0.95)
# ### Day14
# pvrect(FSM.D14_pvclust, alpha=0.95)
#
# ## Calculate the covariance matrix heatmap
# # X = dataset (if y = Null, don't need it explicitly denoted)
# # Y = null because for a covariance of data points, we don't care about individuals.
# # the column headers used automatically
# cor.Daycov_input<- cor(x= FSM.D1cov_input,
#                        y = FSM.D14cov_input,
#                        use = "everything",
#                        method = c("pearson"))
# #Visualize the covariance matrix using pheatmap (a more advanced package for ploting heatmaps),
# #columns and rows are sorted as the hierarchical clustering results
# pheatmap(cor.Daycov_input,
#          color = colorRampPalette(rev(brewer.pal(n = 10, name = "PRGn")))(100), # n = the saturation/darkness/closeness of the two ends of the color spectrum
#          border_color = "black",
#          cluster_cols = FSM.D1_pvclust$hclust, # bootstrap values of covariance
#          cluster_rows = FSM.D14_pvclust$hclust)




# CORRELATION MATRIX PLOT ----
#https://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software#use-chart.correlation-draw-scatter-plots
## exploratory data
## matrix correlation graph
iraBehavs <- d %>%
  mutate(InitiatingMatingBehaviors = Day1_InitiatingMatingBehaviors + Day14_InitiatingMatingBehaviors,
         ResponseMatingBehaviors = Day1_ResponseMatingBehaviors + Day14_ResponseMatingBehaviors,
         AggressionBehaviors = Day1_Aggression + Day14_Aggression)
iraBehavs <- iraBehavs %>%
  select(InitiatingMatingBehaviors,
         ResponseMatingBehaviors,
         AggressionBehaviors)

chart.Correlation(iraBehavs,
                  method = c("pearson"),
                  histogram = TRUE,
                  pch = 19) # do not know her

## Combining Day 1 and 14 for correlation
behaviors <- d %>%
  mutate(Chase = Day1_Chase + Day14_Chase,
         Chase_s = Day1_Chase_s + Day14_Chase_s,
         Charges = Day1_Charge + Day14_Charge,
         ParallelSwim_I = `Day1_ParallelSwim-I` + `Day14_ParallelSwim-I`,
         ParallelSwim_Is = `Day1_ParallelSwim-I_s` + `Day14_ParallelSwim-I_s`,
         ParallelSwim_R = `Day1_ParallelSwim-R` + `Day14_ParallelSwim-R`,
         ParallelSwim_Rs = `Day1_ParallelSwim-R_s` + `Day14_ParallelSwim-R_s`,
         TransverseApproach = Day1_TransverseApproach + Day14_TransverseApproach,
         SexDisp_Cwrap = Day1_SexDisp_Cwrap + Day14_SexDisp_Cwrap,
         SexDisp_FrontShimmy = Day1_SexDisp_FrontShimmy + Day1_SexDisp_FrontShimmy,
         AttemptedCopulation = Day1_AttemptedCopulation + Day14_AttemptedCopulation,
         Nip = Day1_Nip + Day1_Nip,
         Stay = Day1_Stay + Day14_Stay,
         Backup = Day1_Backup + Day14_Backup,
         Retreat = Day1_Retreat + Day14_Retreat,
         Glide = Day1_Glide + Day14_Glide,
         Dart = Day1_Dart + Day14_Dart,
         Refuge_s = Day1_Refuge_s + Day14_Refuge_s)
# selecting relevant columns of data
behaviors <- behaviors %>%
  select(Fish, MorphSex, Population, FoodCondition,
         Chase, Chase_s,
         Charges,
         ParallelSwim_I, ParallelSwim_Is,
         ParallelSwim_R, ParallelSwim_Rs,
         TransverseApproach,
         SexDisp_Cwrap,
         SexDisp_FrontShimmy,
         AttemptedCopulation,
         Nip,
         Stay,
         Backup,
         Retreat,
         Glide,
         Dart,
         Refuge_s)

## COR PLOT 1: FOOD CORRELATION ----
FoodBehav <- behaviors %>%
  filter(FoodCondition == "Food") %>%
  select(Chase, Chase_s,
         Charges,
         ParallelSwim_I, ParallelSwim_Is,
         ParallelSwim_R, ParallelSwim_Rs,
         TransverseApproach,
         SexDisp_Cwrap,
         SexDisp_FrontShimmy,
         AttemptedCopulation,
         Nip,
         Stay,
         Backup,
         Retreat,
         Glide,
         Dart,
         Refuge_s)

## #correlation calc
FoodBcor <- cor(FoodBehav)
### round to 3 decimal points
round(FoodBcor, 3)
## CORRELATION MATRIX:
### The correlation matrix is reordered according to the correlation coefficient using “hclust” method.
### tl.col (for text label color) and tl.srt (for text label string rotation) are used to change text colors and rotations.
### Possible values for the argument type are : “upper”, “lower”, “full”
corrplot(FoodBcor,
         type = "upper",
         order = "hclust",
         tl.col = "black",
         col = paletteer_c("grDevices::ag_GrnYl", 100))

## COR PLOT 2: NO FOOD CORRELATION-----
NoFoodBehav <- behaviors %>%
  filter(FoodCondition == "NoFood") %>%
  select(Chase, Chase_s,
         Charges,
         ParallelSwim_I, ParallelSwim_Is,
         ParallelSwim_R, ParallelSwim_Rs,
         TransverseApproach,
         SexDisp_Cwrap,
         SexDisp_FrontShimmy,
         AttemptedCopulation,
         Nip,
         Stay,
         Backup,
         Retreat,
         Glide,
         Dart,
         Refuge_s)
## #correlation calc
NoFoodBcor <- cor(NoFoodBehav)
### round to 3 decimal points
round(NoFoodBcor, 3)
## CORRELATION MATRIX:
### The correlation matrix is reordered according to the correlation coefficient using “hclust” method.
### tl.col (for text label color) and tl.srt (for text label string rotation) are used to change text colors and rotations.
### Possible values for the argument type are : “upper”, “lower”, “full”
corrplot(NoFoodBcor,
         type = "upper",
         order = "hclust",
         tl.col = "black",
         col = paletteer_c("grDevices::ag_GrnYl", 100))

## COR PLOT 3: F+OM CORRELATION ----
FOMBehav <- behaviors %>%
  filter(Population == "F+OM") %>%
  select(Chase, Chase_s,
         Charges,
         ParallelSwim_I, ParallelSwim_Is,
         ParallelSwim_R, ParallelSwim_Rs,
         TransverseApproach,
         SexDisp_Cwrap,
         SexDisp_FrontShimmy,
         AttemptedCopulation,
         Nip,
         Stay,
         Backup,
         Retreat,
         Glide,
         Dart,
         Refuge_s)
## #correlation calc
FOMBcor <- cor(FOMBehav)
### round to 3 decimal points
round(FOMBcor, 3)
## CORRELATION MATRIX:
### The correlation matrix is reordered according to the correlation coefficient using “hclust” method.
### tl.col (for text label color) and tl.srt (for text label string rotation) are used to change text colors and rotations.
### Possible values for the argument type are : “upper”, “lower”, “full”
corrplot(FOMBcor,
         type = "upper",
         order = "hclust",
         tl.col = "black",
         #tl.srt = 45,
         col = paletteer_c("grDevices::ag_GrnYl", 100))

## COR PLOT 4: F+SM CORRELATION---
FSMBehav <- behaviors %>%
  filter(Population == "F+SM") %>%
  select(Chase, Chase_s,
         Charges,
         ParallelSwim_I, ParallelSwim_Is,
         ParallelSwim_R, ParallelSwim_Rs,
         TransverseApproach,
         SexDisp_Cwrap,
         SexDisp_FrontShimmy,
         AttemptedCopulation,
         Nip,
         Stay,
         Backup,
         Retreat,
         Glide,
         Dart,
         Refuge_s)
## #correlation calc
FSMBcor <- cor(FSMBehav, use = "complete.obs") # standard deviation is 0
### round to 3 decimal points
round(FSMBcor, 3)
## CORRELATION MATRIX:
### The correlation matrix is reordered according to the correlation coefficient using “hclust” method.
### tl.col (for text label color) and tl.srt (for text label string rotation) are used to change text colors and rotations.
### Possible values for the argument type are : “upper”, “lower”, “full”
corrplot(FSMBcor,
         type = "upper",
         order = "hclust",
         tl.col = "black",
         col = paletteer_c("grDevices::ag_GrnYl", 100))
