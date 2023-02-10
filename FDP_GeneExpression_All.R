# FDP Gene Expression Analysis (qPCR) - All Genes
# load packages----
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(ggsci) # science theme for ggplot2
#library(cowplot)
library(car) #anova

# NOT SURE I TRUST THIS METHOD ACTUALLY :(
# load dataset----
g <-"https://raw.githubusercontent.com/slcornett/FoodDepBehaviorPilot/main/fdp_datasets/FDP_GeneExpression.csv?token=GHSAT0AAAAAAB6AIFGRF5TCLB6FG7TFR5J4Y7GS57Q"
ge <- read_csv(g, col_names = TRUE) # ge = gene expression
print(ge)

# Dataset Calculations ----
## combined housekeeping genes Avg Ct
ge <- ge %>% mutate(Control_AvgCt = (EarlyB_CtAvg+ # gnrh2 control
                                     GAPDH_CtAvg)/2, # gnrhr2 control
                    Control_CtSD = (EarlyB_CtSD+ # gnrh2 control
                                      GAPDH_CtSD)/2 # gnrhr2 control
                    )

## ∆Ct for each gene
ge <- ge %>% mutate(g2.deltaCt = GnRH2_CtAvg - Control_AvgCt, # gnrh2
                    R2.deltaCt = GnRHR2_CtAvg - Control_AvgCt) # gnrhr2
print(ge)

## Filter for each morph sex
### females
sex.f <- ge %>% filter(MorphSex == "F")
### ornamented males
sex.om <- ge %>% filter(MorphSex == "OM")
### ornamented males
sex.sm <- ge %>% filter(MorphSex == "SM")

# FEMALES deltaCT morph sex graphs----
## females food v no food: GnRH2
GnRH2.fem.p <- ggplot(data = sex.f, aes(x = FoodCondition,
                                        y = g2.deltaCt,
                                        fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "GnRH2 Expression in Females",
       x = "Food Condition",
       y = "Relative Whole Brain GnRH2 Expression") +
  theme_classic() +
  scale_y_continuous(limits = c(0,7.0),
                     n.breaks = 10) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
GnRH2.fem.p

## females food v no food: GnRHR2
GnRHR2.fem.p <- ggplot(data = sex.f, aes(x = FoodCondition,
                                         y = R2.deltaCt,
                                         fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "GnRHR2 Expression in Females",
       x = "Food Condition",
       y = "Relative Whole Brain GnRHR2 Expression") +
  theme_classic() +
  scale_y_continuous(limits = c(0,7.0),
                     n.breaks = 10) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
GnRHR2.fem.p

# Ornamented  Males deltaCT morph sex graphs----
## Ornamented Males food v no food: GnRH2
GnRH2.om.p <- ggplot(data = sex.om, aes(x = FoodCondition,
                                        y = g2.deltaCt,
                                        fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "GnRH2 Expression in Ornamented Males",
       x = "Food Condition",
       y = "Relative Whole Brain GnRH2 Expression") +
  theme_classic() +
  scale_y_continuous(limits = c(0,7.0),
                     n.breaks = 10) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
GnRH2.om.p

## Ornamented Males food v no food: GnRHR2
GnRHR2.om.p <- ggplot(data = sex.om, aes(x = FoodCondition,
                                        y = R2.deltaCt,
                                        fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "GnRHR2 Expression in Ornamented Males",
       x = "Food Condition",
       y = "Relative Whole Brain GnRHR2 Expression") +
  theme_classic() +
  scale_y_continuous(limits = c(0,7.0),
                     n.breaks = 10) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
GnRHR2.om.p

# Small Males deltaCT morph sex graphs----
## Small Males food v no food: GnRH2
GnRH2.sm.p <- ggplot(data = sex.sm, aes(x = FoodCondition,
                                        y = g2.deltaCt,
                                        fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "GnRH2 Expression in Small Males",
       x = "Food Condition",
       y = "Relative Whole Brain GnRH2 Expression") +
  theme_classic() +
  scale_y_continuous(limits = c(0,7.0),
                     n.breaks = 10) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
GnRH2.sm.p

## Small Males food v no food: GnRHR2
GnRHR2.sm.p <- ggplot(data = sex.sm, aes(x = FoodCondition,
                                         y = R2.deltaCt,
                                         fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "GnRHR2 Expression in Small Males",
       x = "Food Condition",
       y = "Relative Whole Brain GnRHR2 Expression") +
  theme_classic() +
  scale_y_continuous(limits = c(0,7.0),
                     n.breaks = 10) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
GnRHR2.sm.p
