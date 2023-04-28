# FDP Gene Expression Analysis (qPCR) - GnRH2 + EarlyB
# load packages----
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(ggsci) # science theme for ggplot2
#library(cowplot)
library(car) #anova

# load dataset----
f1 <- "https://raw.githubusercontent.com/slcornett/FoodDepBehaviorPilot/main/fdp_datasets/2022-12-12_GnRH2EarlyB_qPCR.csv"
df1 <- read_csv(f1, col_names = TRUE)
print(df1)

## calculate ∆Ct
df1 <- df1 %>% mutate(GnRH2.eB.deltaCt = GnRH2_CtAvg - EarlyB_CtAvg,
                      GnRH2.eB.deltaCtSD = GnRH2_CtSD - EarlyB_CtSD)
print(df1)

## Filter for Each Population (g2 = GnRH2 dataset)
### females + ornamented males
g2.fom_pop <- df1 %>% filter(Population == "F+OM")
### females + small males
g2.fsm_pop <- df1 %>% filter(Population == "F+SM")

## Filter for each morph sex
### females
g2.sex.f <- df1 %>% filter(MorphSex == "F")
### ornamented males
g2.sex.om <- df1 %>% filter(MorphSex == "OM")
### ornamented males
g2.sex.sm <- df1 %>% filter(MorphSex == "SM")

# GNRHR2 deltaCT morph sex graphs----
## females food v no food
g2fem.p <- ggplot(data = g2.sex.f, aes(x = FoodCondition,
                                       y = GnRH2.eB.deltaCt,
                                       fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size=3) +
  labs(title = "GnRH2 Expression in Females",
       x = "Food Condition",
       y = "Whole Brain GnRH2 Expression Relative to EarlyB") +
  theme_classic() +
  scale_y_continuous(limits = c(0,7.0),
                     n.breaks = 10) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
g2fem.p

## FEMALES AVG CT GNRH2
g2fem.p2 <- ggplot(data = g2.sex.f, aes(x = FoodCondition,
                                       y = GnRH2_CtAvg,
                                       fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size=3) +
  labs(title = "GnRH2 Expression in Females",
       x = "Food Condition",
       y = "Whole Brain GnRH2 Expression (Avg Ct)") +
  theme_classic() +
  scale_y_continuous(limits = c(0,32.0),
                     n.breaks = 10) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
g2fem.p2

## Ornamented Males food v no food
g2om.p <- ggplot(data = g2.sex.om, aes(x = FoodCondition,
                                       y = GnRH2.eB.deltaCt,
                                       fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size=3) +
  labs(title = "GnRH2 Expression in Ornamented Males",
       x = "Food Condition",
       y = "Whole Brain GnRH2 Expression Relative to EarlyB") +
  theme_classic() +
  scale_y_continuous(limits = c(0,7.0),
                     n.breaks = 10) + # breaks=pretty(sex.om$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend
g2om.p

### ORNAMENTED MALES Avg Ct GNRH2
g2om.p2 <- ggplot(data = g2.sex.om, aes(x = FoodCondition,
                                        y = GnRH2_CtAvg,
                                        fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size=3) +
  labs(title = "GnRH2 Expression in Ornamented Males",
       x = "Food Condition",
       y = "Whole Brain GnRH2 Expression (Avg Ct)") +
  theme_classic() +
  scale_y_continuous(limits = c(0.0,32.0),
                     n.breaks = 10) + # breaks=pretty(sex.om$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend
g2om.p2

## Small Males food v no food
g2sm.p <- ggplot(data = g2.sex.sm, aes(x = FoodCondition,
                                      y = GnRH2.eB.deltaCt,
                                      fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge(), size = 3) +
  labs(title = "GnRH2 Expression in Small Males",
       x = "Food Condition",
       y = "Whole Brain GnRH2 Expression Relative to EarlyB") +
  theme_classic() +
  scale_y_continuous(limits = c(0,7.0),
                     n.breaks = 10) + # breaks=pretty(sex.sm$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend
g2sm.p

### SMALL MALES Avg Ct GNRH2
g2sm.p2 <- ggplot(data = g2.sex.sm, aes(x = FoodCondition,
                                       y = GnRH2_CtAvg,
                                       fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge(), size = 3) +
  labs(title = "GnRH2 Expression in Small Males",
       x = "Food Condition",
       y = "Whole Brain GnRH2 Expression (Avg Ct)") +
  theme_classic() +
  scale_y_continuous(limits = c(0,32.0),
                     n.breaks = 10) + # breaks=pretty(sex.sm$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend
g2sm.p2

## Cowplot
plot_grid(g2fem.p,
          g2om.p,
          g2sm.p,
          ncol = 3)
