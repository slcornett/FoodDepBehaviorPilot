# FDP Gene Expression Analysis (qPCR) - All Genes
# load packages----
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(ggsci) # science theme for ggplot2
library(cowplot)
library(car) #anova

# load dataset----
g <-"https://raw.githubusercontent.com/slcornett/FoodDepBehaviorPilot/main/fdp_datasets/FDP_GeneExpression.csv"
ge <- read_csv(g, col_names = TRUE) # ge = gene expression
print(ge)

# Dataset Calculations ----
## combined housekeeping genes Avg Ct
ge <- ge %>% mutate(g2.eB.deltaCt = (GnRH2_CtAvg - EarlyB_CtAvg), # same plate control
                    R2.eb.deltaCt = (GnRHR2_CtAvg - EarlyB_CtAvg), # diff plate control
                    R2.GAP.deltaCt = (GnRHR2_CtAvg - GAPDH_CtAvg), # same plate control
                    g2.GAP.deltaCt = (GnRH2_CtAvg - GAPDH_CtAvg) # diff plate control
                    # Control_AvgCt = (EarlyB_CtAvg + # gnrh2 control
                    #                  GAPDH_CtAvg)/2, # gnrhr2 control
                    # Control_CtSD = (EarlyB_CtSD + # gnrh2 control
                    #                   GAPDH_CtSD)/2 # gnrhr2 control
                    )

# NOT SURE I TRUST THIS METHOD ACTUALLY :(
## ∆Ct for each gene -> no cannot do this
# ge <- ge %>% mutate(g2.deltaCt = GnRH2_CtAvg - Control_AvgCt, # gnrh2
#                     R2.deltaCt = GnRHR2_CtAvg - Control_AvgCt) # gnrhr2
print(ge)

## Filter for each morph sex
### females
sex.f <- ge %>% filter(MorphSex == "F")
### ornamented males
sex.om <- ge %>% filter(MorphSex == "OM")
### ornamented males
sex.sm <- ge %>% filter(MorphSex == "SM")

# Females deltaCT morph sex graphs----
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


# Females Avg Gene Expression----
## GnRH2
GnRH2.fem.p2 <- ggplot(data = sex.f, aes(x = FoodCondition,
                                        y = GnRH2_CtAvg,
                                        fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
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
GnRH2.fem.p2

## GnRHR2
GnRHR2.fem.p2 <- ggplot(data = sex.f, aes(x = FoodCondition,
                                         y = GnRHR2_CtAvg,
                                         fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
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
GnRHR2.fem.p2

## Cowplot: fem gnrh genes
plot_grid(GnRH2.fem.p2,
          GnRHR2.fem.p2,
          control.fem.p2,
          ncol = 3)

## CONTROL: EarlyB
eB.fem.p2 <- ggplot(data = sex.f, aes(x = FoodCondition,
                                          y = EarlyB_CtAvg,
                                          fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "EarlyB Expression in Females",
       x = "Food Condition",
       y = "Whole Brain EarlyB Expression (Avg Ct)") +
  theme_classic() +
  scale_y_continuous(limits = c(0,32.0),
                     n.breaks = 10) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
eB.fem.p2

## CONTROL: GAPDH
GAPDH.fem.p2 <- ggplot(data = sex.f, aes(x = FoodCondition,
                                      y = GAPDH_CtAvg,
                                      fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "GAPDH Expression in Females",
       x = "Food Condition",
       y = "Whole Brain GAPDH Expression (Avg Ct)") +
  theme_classic() +
  scale_y_continuous(limits = c(0,32.0),
                     n.breaks = 10) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
GAPDH.fem.p2

## CONTROL: control genes combined
control.fem.p2 <- ggplot(data = sex.f, aes(x = FoodCondition,
                                           y = Control_AvgCt,
                                           fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "Control Gene Expression in Females",
       x = "Food Condition",
       y = "Whole Brain Control Expression (Avg Ct)") +
  theme_classic() +
  scale_y_continuous(limits = c(0,32.0),
                     n.breaks = 10) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
control.fem.p2

## Cowplot: fem control genes
plot_grid(eB.fem.p2,
          GAPDH.fem.p2,
          control.fem.p2,
          ncol = 3)

# Ornamented Males Avg Gene Expression----
## GnRH2
GnRH2.om.p2 <- ggplot(data = sex.om, aes(x = FoodCondition,
                                         y = GnRH2_CtAvg,
                                         fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "GnRH2 Expression in Ornamented Males",
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
GnRH2.om.p2

## GnRHR2
GnRHR2.om.p2 <- ggplot(data = sex.om, aes(x = FoodCondition,
                                          y = GnRHR2_CtAvg,
                                          fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "GnRHR2 Expression in Ornamented Males",
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
GnRHR2.om.p2

## Cowplot: om gnrh genes
plot_grid(GnRH2.om.p2,
          GnRHR2.om.p2,
          control.om.p2,
          ncol = 3)

## CONTROL: EarlyB
eB.om.p2 <- ggplot(data = sex.om, aes(x = FoodCondition,
                                      y = EarlyB_CtAvg,
                                      fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "EarlyB Expression in Ornamented Males",
       x = "Food Condition",
       y = "Whole Brain EarlyB Expression (Avg Ct)") +
  theme_classic() +
  scale_y_continuous(limits = c(0,32.0),
                     n.breaks = 10) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
eB.om.p2

## CONTROL: GAPDH
GAPDH.om.p2 <- ggplot(data = sex.om, aes(x = FoodCondition,
                                         y = GAPDH_CtAvg,
                                         fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "GAPDH Expression in Ornamented Males",
       x = "Food Condition",
       y = "Whole Brain GAPDH Expression (Avg Ct)") +
  theme_classic() +
  scale_y_continuous(limits = c(0,32.0),
                     n.breaks = 10) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
GAPDH.om.p2

## CONTROL: control genes combined
control.om.p2 <- ggplot(data = sex.om, aes(x = FoodCondition,
                                           y = Control_AvgCt,
                                           fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "Control Gene Expression in Ornamented Males",
       x = "Food Condition",
       y = "Whole Brain Control Expression (Avg Ct)") +
  theme_classic() +
  scale_y_continuous(limits = c(0,32.0),
                     n.breaks = 10) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
control.om.p2

## Cowplot: om control genes
plot_grid(eB.om.p2,
          GAPDH.om.p2,
          control.om.p2,
          ncol = 3)

# Small Males Avg Gene Expression----
## GnRH2
GnRH2.sm.p2 <- ggplot(data = sex.sm, aes(x = FoodCondition,
                                         y = GnRH2_CtAvg,
                                         fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "GnRH2 Expression in Small Males",
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
GnRH2.sm.p2

## GnRHR2
GnRHR2.sm.p2 <- ggplot(data = sex.sm, aes(x = FoodCondition,
                                          y = GnRHR2_CtAvg,
                                          fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "GnRHR2 Expression in Small Males",
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
GnRHR2.sm.p2

## Cowplot: om gnrh genes
plot_grid(GnRH2.sm.p2,
          GnRHR2.sm.p2,
          control.sm.p2,
          ncol = 3)

## CONTROL: EarlyB
eB.sm.p2 <- ggplot(data = sex.sm, aes(x = FoodCondition,
                                      y = EarlyB_CtAvg,
                                      fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "EarlyB Expression in Small Males",
       x = "Food Condition",
       y = "Whole Brain EarlyB Expression (Avg Ct)") +
  theme_classic() +
  scale_y_continuous(limits = c(0,32.0),
                     n.breaks = 10) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
eB.sm.p2

## CONTROL: GAPDH
GAPDH.sm.p2 <- ggplot(data = sex.sm, aes(x = FoodCondition,
                                         y = GAPDH_CtAvg,
                                         fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "GAPDH Expression in Small Males",
       x = "Food Condition",
       y = "Whole Brain GAPDH Expression (Avg Ct)") +
  theme_classic() +
  scale_y_continuous(limits = c(0,32.0),
                     n.breaks = 10) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
GAPDH.sm.p2

## CONTROL: control genes combined
control.sm.p2 <- ggplot(data = sex.sm, aes(x = FoodCondition,
                                           y = Control_AvgCt,
                                           fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "Control Gene Expression in Small Males",
       x = "Food Condition",
       y = "Whole Brain Control Expression (Avg Ct)") +
  theme_classic() +
  scale_y_continuous(limits = c(0,32.0),
                     n.breaks = 10) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
control.sm.p2

## Cowplot: om control genes
plot_grid(eB.sm.p2,
          GAPDH.sm.p2,
          control.sm.p2,
          ncol = 3)


# Expression Trends----
## GAPDH vs earlyB
GAP.eB <- ggplot(data = ge, aes(x = GAPDH_CtAvg,
                                y = EarlyB_CtAvg,
                                #color = FoodCondition
                                )) +
  geom_point(position = "identity", size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Control Genes Expression",
       x = "GAPDH (Avg Ct)",
       y = "EarlyB (Avg Ct)") +
  theme_classic()
GAP.eB
## earlyB vs GAPDH
eB.GAP <- ggplot(data = ge, aes(x = EarlyB_CtAvg,
                                y = GAPDH_CtAvg,
                                color = MorphSex
)) +
  geom_point(position = "identity", size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Control Genes Expression",
       x = "EarlyB (Avg Ct)",
       y = "GAPDH (Avg Ct)") +
  theme_classic()
eB.GAP

## GnRH2 vs GnRHR2
### average ct
p1.g2.R2 <- ggplot(data = ge, aes(x = GnRH2_CtAvg,
                                y = GnRHR2_CtAvg,
                                color = FoodCondition
                               )) +
  geom_point(position = "identity", size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "GnRH Genes Expression",
       x = "GnRH2 (Avg Ct)",
       y = "GnRHR2 (Avg Ct)") +
  theme_classic()
p1.g2.R2
### delta Ct with respective controls
p2.g2.R2 <- ggplot(data = ge, aes(x = g2.eB.deltaCt,
                                  y = R2.GAP.deltaCt,
                                  #color = FoodCondition
                                  )) +
  geom_point(outlier.shape = NA, position = "identity", size = 3, ) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "GnRH Genes Expression",
       x = "GnRH2 (relative to EarlyB)",
       y = "GnRHR2 (relative to GAPDH)") +
  theme_classic() +
  scale_y_continuous(limits = c(0,8),
                     n.breaks = 5) +
  scale_x_continuous(limits = c(0,8),
                     n.breaks = 5)
p2.g2.R2
### delta Ct with GAPDH for both (regardless of separate plates)
p3.g2.R2 <- ggplot(data = ge, aes(x = g2.GAP.deltaCt,
                                  y = R2.GAP.deltaCt,
                                  color = FoodCondition
)
) +
  geom_point(position = "identity", size = 3, ) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "GnRH Genes Expression",
       x = "GnRH2 (relative to GAPDH*)",
       y = "GnRHR2 (relative to GAPDH)") +
  theme_classic() +
  # scale_y_continuous(limits = c(0,8),
  #                    n.breaks = 5) +
  # scale_x_continuous(limits = c(0,8),
  #                    n.breaks = 5)
  facet_wrap(~MorphSex)
p3.g2.R2
#Hans's Feedback:
## look at GnRH2 in Fems v body size (standard length)
## look at GAPDH in Fems v body size (sl)
## look at just females by population, and food; 18S primers for sanity check later

### delta Ct with EarlyB for both (regardless of separate plates)
#### filter outliers
earlyB.ge <- ge %>% select(Fish,
                           MorphSex,
                           FoodCondition,
                           GnRH2_CtAvg,
                           GnRHR2_CtAvg,
                           EarlyB_CtAvg,
                           EarlyB_CtSD,
                           g2.eB.deltaCt,
                           R2.eb.deltaCt) %>%
  # filtering out two high SD data points to remove the outliers from deltaCt calculations
  filter(EarlyB_CtSD < 1.0,
         GAPDH_CtSD)

p4.g2.R2 <- ggplot(data = earlyB.ge, aes(x = g2.eB.deltaCt,
                                         y = R2.eb.deltaCt,
                                         #color = FoodCondition
                                         )) +
  geom_point(position = "identity", size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "GnRH Genes Expression",
       x = "GnRH2 (relative to EarlyB)",
       y = "GnRHR2 (relative to EarlyB*)") +
  theme_classic()
# scale_y_continuous(limits = c(0,8),
#                    n.breaks = 5) +
# scale_x_continuous(limits = c(0,8),
#                    n.breaks = 5)
p4.g2.R2
