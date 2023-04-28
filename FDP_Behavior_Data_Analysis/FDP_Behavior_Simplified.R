# Project: Food Deprivation Pilot
# FoodDep Behavior Data: Simplied to Categories
library(tidyverse)
library(dplyr)
library(ggplot2)
library(skimr)
## covariance heatmaps
library(pvclust)
library(pheatmap)
## correlation matrix
library(corrplot)
library(PerformanceAnalytics) # for correlation matrix
library(RColorBrewer) # for pheatmap colors
library(paletteer) # color palettes

# Dataframe----
f3 <- "https://raw.githubusercontent.com/slcornett/FoodDepBehaviorPilot/main/fdp_datasets/FoodDep_Pilot_Behavior_Master.csv"
d3 <- read_csv(f3, col_names = TRUE)
print(d3)

# all day 14 is character data for some reason
skim(d3)
d3 <- d3 %>% mutate(`Day14_Chase-SameSex`= as.numeric(`Day14_Chase-SameSex`),
                    `Day14_Chase-SameSex_s` = as.numeric(`Day14_Chase-SameSex_s`),
                    `Day14_Chase-DiffSex` = as.numeric(`Day14_Chase-DiffSex`),
                    `Day14_Chase-DiffSex_s` = as.numeric(`Day14_Chase-DiffSex_s`),
                    `Day14_ParallelSwim-SameSexDorsalFin-I` = as.numeric(`Day14_ParallelSwim-SameSexDorsalFin-I`),
                    `Day14_ParallelSwim-SameSexDorsalFin-I_s` = as.numeric(`Day14_ParallelSwim-SameSexDorsalFin-I_s`),
                    `Day14_ParallelSwim-SameSexNoFin-I`= as.numeric(`Day14_ParallelSwim-SameSexNoFin-I`),
                    `Day14_ParallelSwim-SameSexNoFin-I_s`= as.numeric(`Day14_ParallelSwim-SameSexNoFin-I_s`),
                    `Day14_ParallelSwim-DiffSexDorsalFin-I`= as.numeric(`Day14_ParallelSwim-DiffSexDorsalFin-I`),
                    `Day14_ParallelSwim-DiffSexDorsalFin-I_s`= as.numeric(`Day14_ParallelSwim-DiffSexDorsalFin-I_s`),
                    `Day14_ParallelSwim-DiffSexNoFin-I`= as.numeric(`Day14_ParallelSwim-DiffSexNoFin-I`),
                    `Day14_ParallelSwim-DiffSexNoFin-I_s`= as.numeric(`Day14_ParallelSwim-DiffSexNoFin-I_s`),
                    `Day14_TransverseApproach-DorsalFin`= as.numeric(`Day14_TransverseApproach-DorsalFin`),
                    `Day14_TransverseApproach-NoFin`= as.numeric(`Day14_TransverseApproach-NoFin`),
                    Day14_AttemptedCopulation= as.numeric(Day14_AttemptedCopulation),
                    `Day14_SexDisp_Cwrap-DorsalFin`= as.numeric(`Day14_SexDisp_Cwrap-DorsalFin`),
                    `Day14_SexDisp_Cwrap-NoFin`= as.numeric(`Day14_SexDisp_Cwrap-NoFin`),
                    `Day14_SexDisp_FrontShimmy-DorsalFin`= as.numeric(`Day14_SexDisp_FrontShimmy-DorsalFin`),
                    `Day14_SexDisp_FrontShimmy-NoFin`= as.numeric(`Day14_SexDisp_FrontShimmy-NoFin`),
                    Day14_Nip= as.numeric(Day14_Nip),
                    Day14_Charge= as.numeric(Day14_Charge),
                    Day14_Stay= as.numeric(Day14_Stay),
                    Day14_Backup= as.numeric(Day14_Backup),
                    Day14_Retreat= as.numeric(Day14_Retreat),
                    Day14_Glide= as.numeric(Day14_Glide),
                    Day14_Dart= as.numeric(Day14_Dart),
                    `Day14_ParallelSwim-SameSex-R`= as.numeric(`Day14_ParallelSwim-SameSex-R`),
                    `Day14_ParallelSwim-SameSex-R_s`= as.numeric(`Day14_ParallelSwim-SameSex-R_s`),
                    `Day14_ParallelSwim-DiffSex-R`= as.numeric(`Day14_ParallelSwim-DiffSex-R`),
                    `Day14_ParallelSwim-DiffSex-R_s`= as.numeric(`Day14_ParallelSwim-DiffSex-R_s`),
                    Day14_Refuge= as.numeric(Day14_Refuge),
                    Day14_Refuge_s= as.numeric(Day14_Refuge_s),
                    Day14_OutofFrame= as.numeric(Day14_OutofFrame),
                    Day14_OutofFrame_s= as.numeric(Day14_OutofFrame_s)
                    )
skim(d3)
# Behavior Category Set Up----
## collapse behaviors from ethogram, removing discrete details, in day 1 and day 14 (frequency only)
d3 <- d3 %>% mutate(Day1_Chase = (`Day1_Chase-SameSex`+ # Day1 Chase
                                    `Day1_Chase-DiffSex`),
                    #Day1 parallel swim (I)
                    Day1_ParallelSwim_I = (`Day1_ParallelSwim-SameSexDorsalFin-I`+
                                             `Day1_ParallelSwim-SameSexNoFin-I`+
                                             `Day1_ParallelSwim-DiffSexDorsalFin-I`+
                                             `Day1_ParallelSwim-DiffSexNoFin-I`),
                    #Day 1 Transverse Approach
                    Day1_TransverseApproach = (`Day1_TransverseApproach-DorsalFin` +
                                                 `Day1_TransverseApproach-NoFin`),
                    #Day1 SexDisplay
                    Day1_SexDisplay = (`Day1_SexDisp_Cwrap-DorsalFin`+
                                         `Day1_SexDisp_Cwrap-NoFin`+
                                         `Day1_SexDisp_FrontShimmy-DorsalFin`+
                                         `Day1_SexDisp_FrontShimmy-NoFin`),
                    #Day1 Parallel Swim R
                    Day1_ParallelSwim_R = (`Day1_ParallelSwim-SameSex-R`+
                                             `Day1_ParallelSwim-DiffSex-R`),
                    # Day14 Chase
                    Day14_Chase = (`Day14_Chase-SameSex`+
                                     `Day14_Chase-DiffSex`),
                    #Day14 parallel swim (I)
                    Day14_ParallelSwim_I = (`Day14_ParallelSwim-SameSexDorsalFin-I`+
                                              `Day14_ParallelSwim-SameSexNoFin-I`+
                                              `Day14_ParallelSwim-DiffSexDorsalFin-I`+
                                              `Day14_ParallelSwim-DiffSexNoFin-I`),
                    #Day 14 Transverse Approach
                    Day14_TransverseApproach = (`Day14_TransverseApproach-DorsalFin` +
                                                  `Day14_TransverseApproach-NoFin`),
                    #Day14 SexDisplay
                    Day14_SexDisplay = (`Day14_SexDisp_Cwrap-DorsalFin`+
                                          `Day14_SexDisp_Cwrap-NoFin`+
                                          `Day14_SexDisp_FrontShimmy-DorsalFin`+
                                          `Day14_SexDisp_FrontShimmy-NoFin`),
                    #Day14 Parallel Swim R
                    Day14_ParallelSwim_R = (`Day14_ParallelSwim-SameSex-R`+
                                              `Day14_ParallelSwim-DiffSex-R`))
## create behavior categories within full dataframe
d3 <- d3 %>% mutate(Day1_Initiations = (Day1_Chase+ # Day 1 initiation behaviors (frequency)
                                          Day1_ParallelSwim_I+
                                          Day1_TransverseApproach+
                                          Day1_SexDisplay+
                                          Day1_AttemptedCopulation+
                                          Day1_Nip),
                    #day 14 initiation behaviors (frequency)
                    Day14_Initiations = (Day14_Chase+
                                          Day14_ParallelSwim_I+
                                          Day14_TransverseApproach+
                                          Day14_SexDisplay+
                                          Day14_AttemptedCopulation+
                                          Day14_Nip),
                    #day1 response behaviors (frequency)
                    Day1_Responses = (Day1_Backup+
                                        Day1_Stay+
                                        Day1_ParallelSwim_R+
                                        Day1_Glide+
                                        Day1_Dart+
                                        Day1_Retreat),
                    #day 14 reponse behaviors (frequency)
                    Day14_Responses = (Day14_Backup+
                                        Day14_Stay+
                                        Day14_ParallelSwim_R+
                                        Day14_Glide+
                                        Day14_Dart+
                                        Day14_Retreat),
                    #day 1 aggression
                    Day1_Aggression = Day1_Charge,
                    #day 14 aggression
                    Day14_Aggression = Day14_Charge
                    )
## new data frame of behavior categories
ira <- d3 %>% select(Fish,
                     MorphSex,
                     PreSize_mm,
                     PostSize_mm,
                     Tank,
                     Population,
                     FoodCondition,
                     Day1_Initiations,
                     Day14_Initiations,
                     Day1_Responses,
                     Day14_Responses,
                     Day1_Aggression,
                     Day14_Aggression,
                     Day1_Refuge,
                     Day14_Refuge)
print(ira)

#covarience: NOT ENOUGH DATA----
## Selecting for Day1
Day1cov_input<- ira %>% select(Day1_Initiations,
                               Day1_Responses,
                               Day1_Aggression,
                               Day1_Refuge)
## Selecting for Day14
Day14cov_input<- ira %>% select(Day14_Initiations,
                                Day14_Responses,
                                Day14_Aggression,
                                Day14_Refuge)
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
                       method = c("spearman")) # spearman's rank bc pearson's is for normal distribution
#Visualize the covariance matrix using pheatmap (a more advanced package for ploting heatmaps),
#columns and rows are sorted as the hierarchical clustering results
range1 <- max(abs(cor.Daycov_input))
pheatmap(cor.Daycov_input,
         # customize covarience legend so makes more sense
         legend = TRUE,
         breaks = seq(-range1, range1, length.out = 100),
         # BLUE-YELLOW palette: hcl.colors(100, "BluYl")
         color = paletteer_c("grDevices::Viridis", 100),
         # GREEN-PURPLE palette: # n = the saturation/darkness/closeness of the two ends of the color spectrum
         #colorRampPalette(rev(brewer.pal(n = 10, name = "PRGn")))(100),
         border_color = "black",
         cluster_cols = Day1_pvclust$hclust, # bootstrap values of covariance
         cluster_rows = Day14_pvclust$hclust)
#Correlations with Behavior Categories: NOT ENOUGH DATA----
# ##CorPlot1: Day1 Correlations
# IRA.d1 <- ira %>%
#   select(Day1_Initiations,
#          Day1_Responses,
#          Day1_Aggression)
# ## #correlation calc
# cor1_IRA.d1 <- cor(IRA.d1,
#                    method = c("spearman"),
#                    use = "everything")
# ### round to 3 decimal points
# round(cor1_IRA.d1, 3)
# ## CORRELATION MATRIX:
# ### The correlation matrix is reordered according to the correlation coefficient using “hclust” method.
# ### tl.col (for text label color) and tl.srt (for text label string rotation) are used to change text colors and rotations.
# ### Possible values for the argument type are : “upper”, “lower”, “full”
# corrplot(cor1_IRA.d1,
#          type = "upper",
#          order = "hclust",
#          tl.col = "black",
#          col = paletteer_c("grDevices::Plasma", 100))
#
# ##CorPlot2: Day14 Correlations
# IRA.d14 <- ira %>%
#   select(Day14_Initiations,
#          Day14_Responses)
# ## #correlation calc
# cor2_IRA.d14 <- cor(IRA.d14,
#                     method = c("spearman"),
#                     use = "everything")
# ### round to 3 decimal points
# round(cor2_IRA.d14, 3)
# ## CORRELATION MATRIX:
# ### The correlation matrix is reordered according to the correlation coefficient using “hclust” method.
# ### tl.col (for text label color) and tl.srt (for text label string rotation) are used to change text colors and rotations.
# ### Possible values for the argument type are : “upper”, “lower”, “full”
# corrplot(cor2_IRA.d14,
#          type = "upper",
#          order = "hclust",
#          tl.col = "black",
#          col = paletteer_c("grDevices::Plasma", 100))
