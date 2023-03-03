# FDP Gene Expression Analysis (qPCR) - GnRHRA + GAPDH
# Load Packages----
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(ggsci) # science theme for ggplot2
library(cowplot)
library(car) #anova
# load dataset----
f2 <- "https://raw.githubusercontent.com/slcornett/FoodDepBehaviorPilot/main/fdp_datasets/2023-02-08_GnRHR2GAPDH_qPCR.csv"
df2 <- read_csv(f2, col_names = TRUE)
print(df2)

## calculate ∆Ct
df2 <- df2 %>% mutate(R2.GAPDH.deltaCt = GnRHR2_CtAvg - GAPDH_CtAvg,
                      R2.GAPDH.deltaCtSD = GnRHR2_CtSD - GAPDH_CtSD)
print(df2)

## Filter for Each Population (R2 = GnRHR2 dataset)
### females + ornamented males
R2.fom_pop <- df2 %>% filter(Population == "F+OM")
### females + small males
R2.fsm_pop <- df2 %>% filter(Population == "F+SM")

## Filter for each morph sex
### females
R2.sex.f <- df2 %>% filter(MorphSex == "F")
### ornamented males
R2.sex.om <- df2 %>% filter(MorphSex == "OM")
### ornamented males
R2.sex.sm <- df2 %>% filter(MorphSex == "SM")

# GNRHR2 deltaCT morph sex graphs----
## females food v no food
R2fem.p <- ggplot(data = R2.sex.f, aes(x = FoodCondition,
                                  y = R2.GAPDH.deltaCt,
                                  fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size=3) +
  labs(title = "GnRHR2 Expression in Females",
       x = "Food Condition",
       y = "Whole Brain GnRHR2 Expression Relative to GAPDH") +
  theme_classic() +
  scale_y_continuous(limits = c(0,7.0),
                     n.breaks = 10) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
R2fem.p

## FEMALES AVG CT GNRHR2
R2fem.p2 <- ggplot(data = R2.sex.f, aes(x = FoodCondition,
                                   y = GnRHR2_CtAvg,
                                   fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size=3) +
  labs(title = "GnRHR2 Expression in Females",
       x = "Food Condition",
       y = "Whole Brain GnRHR2 Expression (Avg Ct)") +
  theme_classic() +
  scale_y_continuous(limits = c(0,32.0),
                     n.breaks = 10) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
R2fem.p2

## Ornamented Males food v no food
R2om.p <- ggplot(data = R2.sex.om, aes(x = FoodCondition,
                                  y = R2.GAPDH.deltaCt,
                                  fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size=3) +
  labs(title = "GnRHR2 Expression in Ornamented Males",
       x = "Food Condition",
       y = "Whole Brain GnRHR2 Expression Relative to GAPDH") +
  theme_classic() +
  scale_y_continuous(limits = c(0,7.0),
                     n.breaks = 10) + # breaks=pretty(sex.om$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend
R2om.p

### ORNAMENTED MALES Avg Ct GNRHR2
R2om.p2 <- ggplot(data = R2.sex.om, aes(x = FoodCondition,
                                   y = GnRHR2_CtAvg,
                                   fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size=3) +
  labs(title = "GnRHR2 Expression in Ornamented Males",
       x = "Food Condition",
       y = "Whole Brain GnRHR2 Expression (Avg Ct)") +
  theme_classic() +
  scale_y_continuous(limits = c(0.0,32.0),
                     n.breaks = 10) + # breaks=pretty(sex.om$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend
R2om.p2

## Small Males food v no food
R2sm.p <- ggplot(data = R2.sex.sm, aes(x = FoodCondition,
                                      y = R2.GAPDH.deltaCt,
                                      fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge(), size = 3) +
  labs(title = "GnRHR2 Expression in Small Males",
       x = "Food Condition",
       y = "Whole Brain GnRHR2 Expression Relative to GAPDH") +
  theme_classic() +
  scale_y_continuous(limits = c(0,7.0),
                     n.breaks = 10) + # breaks=pretty(sex.sm$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend
R2sm.p

### SMALL MALES Avg Ct GNRHR2
R2sm.p2 <- ggplot(data = R2.sex.sm, aes(x = FoodCondition,
                                    y = GnRHR2_CtAvg,
                                    fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge(), size = 3) +
  labs(title = "GnRHR2 Expression in Small Males",
       x = "Food Condition",
       y = "Whole Brain GnRHR2 Expression Relative to GAPDH") +
  theme_classic() +
  scale_y_continuous(limits = c(0,32.0),
                     n.breaks = 10) + # breaks=pretty(sex.sm$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend
R2sm.p2


# Cowplot
plot_grid(R2fem.p,
          R2om.p,
          R2sm.p,
          ncol = 3)
