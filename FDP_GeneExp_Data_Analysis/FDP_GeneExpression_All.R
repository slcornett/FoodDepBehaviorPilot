# FDP Gene Expression Analysis (qPCR) - All Genes
# load packages----
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(ggsci) # science theme for ggplot2
library(cowplot)
library(patchwork) # combining plots
library(car) #anova

# load dataset----
g <-"https://raw.githubusercontent.com/slcornett/FoodDepBehaviorPilot/main/fdp_datasets/FDP_GeneExpression.csv"
ge <- read_csv(g, col_names = TRUE) # ge = gene expression
print(ge)

# Dataset Calculations ----
## combining GnRH1 plate GAPDH with GnRHRA plate GAPDH
ge <- ge %>% mutate(GAPDH_CtAvg = (g1.GAPDH_CtAvg + RA.GAPDH_CtAvg)/2
                     )
## Checking consistent results bw GnRH1 plate and GNRHRA plate GAPDH samples
GAPDH.g1.ra <- ggplot(data = ge, aes(x = RA.GAPDH_CtAvg,
                                     y = g1.GAPDH_CtAvg
                                     #color = FoodCondition
)) +
  geom_point(position = "identity", size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "GAPDH Genes Expression",
       x = "GnRHRA Plate GAPDH (Avg Ct)",
       y = "GnRH1 Plate GAPDH (Avg Ct)") +
  theme_classic()
GAPDH.g1.ra

## housekeeping genes Avg Ct
ge <- ge %>% mutate(g2.eB.deltaCt = (GnRH2_CtAvg - EarlyB_CtAvg), # same plate control
                    RA.eB.deltaCt = (GnRHRA_CtAvg - EarlyB_CtAvg), # diff plate control
                    RA.GAP.deltaCt = (GnRHRA_CtAvg - RA.GAPDH_CtAvg), # same plate control
                    g2.GAP.deltaCt = (GnRH2_CtAvg - GAPDH_CtAvg), # diff plate control
                    g1.GAP.deltaCt = (GnRH1_CtAvg - g1.GAPDH_CtAvg)
                    )

# NOT SURE I TRUST THIS METHOD ACTUALLY :(
## ∆Ct for each gene -> no cannot do this
# ge <- ge %>% mutate(g2.deltaCt = GnRH2_CtAvg - Control_AvgCt, # gnrh2
#                     RA.deltaCt = GnRHRA_CtAvg - Control_AvgCt) # gnrhRA
print(ge)
# Food v NoFood Comparisons----
## Gnrh1
FvNF.GnRH1.p <- ggplot(data = ge , aes(x = MorphSex,
                                      y = g1.GAP.deltaCt,
                                      fill = MorphSex)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  # scale_fill_manual(values = c("#08A47F", "#E78140")) + #fill = food condition
  scale_fill_manual(values = c("#392682", "#3F86BC", "#83DDE0")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  geom_abline(aes(intercept = 0, slope = 0)) +
  labs(title = "GnRH1",
       x = "Morphological Sex",
       y = "Relative Whole Brain Expression (GAPDH)") +
  theme_classic() +
  scale_y_continuous(limits = c(-5.0,8.0),
                     n.breaks = 12) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position = "none") + #hide legend)
#facet_wrap(~MorphSex) # fill = food condition
  facet_wrap(~FoodCondition) # fill = MorphSex
FvNF.GnRH1.p

## Gnrh2 (GAPDH)
FvNF.GnRH2.p1 <- ggplot(data = ge , aes(x = MorphSex,
                                       y = g2.GAP.deltaCt,
                                       fill = MorphSex)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  # scale_fill_manual(values = c("#08A47F", "#E78140")) + #fill = food condition
  scale_fill_manual(values = c("#392682", "#3F86BC", "#83DDE0")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3,
             #aes(colour = factor(Population))
             ) +
  geom_abline(aes(intercept = 0, slope = 0)) +
  labs(title = "GnRH2",
       x = "Morphological Sex",
       y = "Relative Whole Brain Expression (GAPDH)") +
  theme_classic() +
  scale_y_continuous(limits = c(-5.0,8.0),
                     n.breaks = 12) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position = "none") + #hide legend
  #facet_wrap(~MorphSex) # fill = food condition
  facet_wrap(~FoodCondition) # fill = MorphSex
FvNF.GnRH2.p1

## Gnrh2 (EarlyB)
FvNF.GnRH2.p2 <- ggplot(data = ge , aes(x = FoodCondition,
                                        y = g2.eB.deltaCt,
                                        fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) + #fill = food condition
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3,
             #aes(colour = factor(Population))
  ) +
  labs(title = "GnRH2 Expression in Food vs No Food",
       x = "Food Condition",
       y = "Relative Whole Brain GnRH2 Expression (EarlyB)") +
  theme_classic() +
  scale_y_continuous(limits = c(0,8.0),
                     n.breaks = 12) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position = "none") + #hide legend
  facet_wrap(~ MorphSex)
FvNF.GnRH2.p2

## GnrhrA (GAPDH)
FvNF.GnRHRA.p <- ggplot(data = ge , aes(x = MorphSex,
                                       y = RA.GAP.deltaCt,
                                       fill = MorphSex)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  # scale_fill_manual(values = c("#08A47F", "#E78140")) + #fill = food condition
  scale_fill_manual(values = c("#392682", "#3F86BC", "#83DDE0")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3,
             #aes(colour = factor(Population))
             ) +
  geom_abline(aes(intercept = 0, slope = 0)) +
  labs(title = "GnRHR(A)",
       x = "Morphological Sex",
       y = "Relative Whole Brain Expression (GAPDH)") +
  theme_classic() +
  scale_y_continuous(limits = c(-5.0,8.0),
                     n.breaks = 12) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position = "none")+ #hide legend
  #facet_wrap(~MorphSex) # fill = food condition
  facet_wrap(~FoodCondition) # fill = MorphSex
FvNF.GnRHRA.p

# patchwork alignment
FvNF.GnRH1.p + FvNF.GnRH2.p1 + FvNF.GnRHRA.p + plot_annotation(tag_levels = "A")

## Filter for each morph sex----
### females
sex.f <- ge %>% filter(MorphSex == "F")
### ornamented males
sex.om <- ge %>% filter(MorphSex == "OM")
### ornamented males
sex.sm <- ge %>% filter(MorphSex == "SM")

# Females deltaCT morph sex graphs----
## females food v no food: GnRH1
GnRH1.fem.p <- ggplot(data = sex.f, aes(x = FoodCondition,
                                        y = g1.GAP.deltaCt,
                                        fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "GnRH1 Expression in Females",
       x = "Food Condition",
       y = "Relative Whole Brain GnRH1 Expression") +
  theme_classic() +
  scale_y_continuous(limits = c(-2.0,8.0),
                     n.breaks = 10) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position = "none") #hide legend)

GnRH1.fem.p

## females food v no food: GnRH2
GnRH2.fem.p <- ggplot(data = sex.f, aes(x = FoodCondition,
                                        y = g2.GAP.deltaCt,
                                        fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "GnRH2 Expression in Females",
       x = "Food Condition",
       y = "Relative Whole Brain GnRH2 Expression") +
  theme_classic() +
  scale_y_continuous(limits = c(-3.0,8.0),
                     n.breaks = 12) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
GnRH2.fem.p

## females food v no food: GnRHRA
GnRHRA.fem.p <- ggplot(data = sex.f, aes(x = FoodCondition,
                                         y = RA.GAP.deltaCt,
                                         fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "GnRHRA Expression in Females",
       x = "Food Condition",
       y = "Relative Whole Brain GnRHRA Expression") +
  theme_classic() +
  scale_y_continuous(limits = c(0,8.0),
                     n.breaks = 10) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
GnRHRA.fem.p

## Cowplot Females
plot_grid(GnRH1.fem.p,
          GnRH2.fem.p,
          GnRHRA.fem.p,
          ncol = 3)

# Ornamented  Males deltaCT morph sex graphs----
## Ornamented Males food v no food: GnRH1
GnRH1.om.p <- ggplot(data = sex.om, aes(x = FoodCondition,
                                        y = g1.GAP.deltaCt,
                                        fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "GnRH1 Expression in Ornamented Males",
       x = "Food Condition",
       y = "Relative Whole Brain GnRH1 Expression") +
  theme_classic() +
  scale_y_continuous(limits = c(-2.0,8.0),
                     n.breaks = 10) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
GnRH1.om.p

## Ornamented Males food v no food: GnRH2
GnRH2.om.p <- ggplot(data = sex.om, aes(x = FoodCondition,
                                        y = g2.GAP.deltaCt,
                                        fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "GnRH2 Expression in Ornamented Males",
       x = "Food Condition",
       y = "Relative Whole Brain GnRH2 Expression") +
  theme_classic() +
  scale_y_continuous(limits = c(-4.0,8.0),
                     n.breaks = 12) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
GnRH2.om.p

## Ornamented Males food v no food: GnRHRA
GnRHRA.om.p <- ggplot(data = sex.om, aes(x = FoodCondition,
                                        y = RA.GAP.deltaCt,
                                        fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "GnRHRA Expression in Ornamented Males",
       x = "Food Condition",
       y = "Relative Whole Brain GnRHRA Expression") +
  theme_classic() +
  scale_y_continuous(limits = c(0,8.0),
                     n.breaks = 10) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
GnRHRA.om.p

## Cowplot OMales
plot_grid(GnRH1.om.p,
          GnRH2.om.p,
          GnRHRA.om.p,
          ncol = 3)

# Small Males deltaCT morph sex graphs----
## Small Males food v no food: GnRH1
GnRH1.sm.p <- ggplot(data = sex.sm, aes(x = FoodCondition,
                                        y = g1.GAP.deltaCt,
                                        fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "GnRH1 Expression in Small Males",
       x = "Food Condition",
       y = "Relative Whole Brain GnRH1 Expression") +
  theme_classic() +
  scale_y_continuous(limits = c(-2.0,8.0),
                     n.breaks = 12) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
GnRH1.sm.p

## Small Males food v no food: GnRH2
GnRH2.sm.p <- ggplot(data = sex.sm, aes(x = FoodCondition,
                                        y = g2.GAP.deltaCt,
                                        fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "GnRH2 Expression in Small Males",
       x = "Food Condition",
       y = "Relative Whole Brain GnRH2 Expression") +
  theme_classic() +
  scale_y_continuous(limits = c(-5.0,8.0),
                     n.breaks = 12) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
GnRH2.sm.p

## Small Males food v no food: GnRHRA
GnRHRA.sm.p <- ggplot(data = sex.sm, aes(x = FoodCondition,
                                         y = RA.GAP.deltaCt,
                                         fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size = 3) +
  labs(title = "GnRHRA Expression in Small Males",
       x = "Food Condition",
       y = "Relative Whole Brain GnRHRA Expression") +
  theme_classic() +
  scale_y_continuous(limits = c(0,8.0),
                     n.breaks = 12) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
GnRHRA.sm.p

## Cowplot Smales
plot_grid(GnRH1.sm.p,
          GnRH2.sm.p,
          GnRHRA.sm.p,
          ncol = 3)

# Expression Trends----
## GAPDH vs earlyB
GAP.eB <- ggplot(data = ge, aes(x = GAPDH_CtAvg,
                                y = EarlyB_CtAvg,
                                #color = FoodCondition
                                )) +
  geom_point(position = "identity", size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Control Genes Expression",
       x = "GAPDH (Avg Ct)",
       y = "EarlyB (Avg Ct)") +
  theme_classic() +
  scale_x_continuous(limits = c(20,28),
                     n.breaks = 12) +
  scale_y_continuous(limits = c(16,28),
                     n.breaks = 12) # breaks=pretty(sex.f$deltaCt, n=15)
GAP.eB

## average ct: not informative
# p1.g2.RA <- ggplot(data = ge, aes(x = GnRH2_CtAvg,
#                                 y = GnRHRA_CtAvg,
#                                 color = FoodCondition
#                                )) +
#   geom_point(position = "identity", size = 3) +
#   geom_smooth(method = "lm", se = FALSE) +
#   labs(title = "GnRH Genes Expression",
#        x = "GnRH2 (Avg Ct)",
#        y = "GnRHRA (Avg Ct)") +
#   theme_classic()
# p1.g2.RA

## deltaCt GnRH1 and GnRH2----
p.g2.g1 <- ggplot(data = ge, aes(x = g2.GAP.deltaCt,
                                 y = g1.GAP.deltaCt,
                                 color = MorphSex #FoodCondition #
                                 ))+
  scale_color_manual(values = c("#4B0055", "#1F948C", "#FDE333")) +
  geom_point(position = "identity", size = 4, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "GnRH Genes Expression",
        x = "Relative GnRH2 (GAPDH)",
        y = "Relative GnRH1 (GAPDH)") +
  theme_classic() +
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 14, color = "black"), # x-axis
        axis.text.x = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 14, color = "black"), # y-axis
        axis.text.y = element_text(size = 12, color = "black")) +
  facet_wrap(~ FoodCondition)
p.g2.g1

## deltaCt GnRH2 and GnRHRA
p.g2.RA <- ggplot(data = ge, aes(x = g2.GAP.deltaCt,
                                 y = RA.GAP.deltaCt,
                                 color = MorphSex
)) +
  geom_point(position = "identity", size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "GnRH Genes Expression",
       x = "Relative GnRH2 (GAPDH)",
       y = "Relative GnRHRA (GAPDH)") +
  theme_classic() +
  facet_wrap(~ FoodCondition)
p.g2.RA

## deltaCt GnRH1 and GnRHRA
p.g1.RA <- ggplot(data = ge, aes(x = g1.GAP.deltaCt,
                                 y = RA.GAP.deltaCt,
                                 color = MorphSex
)) +
  geom_point(position = "identity", size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "GnRH Genes Expression",
       x = "∆Ct GnRH1",
       y = "∆Ct GnRHRA") +
  theme_classic() +
  facet_wrap(~ FoodCondition)
p.g1.RA

## ∆Ct v GnRH1
p.g1.SL <- ggplot(data = ge, aes(x = SL_mm,
                                  y = g1.GAP.deltaCt,
                                  color = MorphSex
)) +
  geom_point(position = "identity", size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Body Size vs GnRH1 Genes Expression",
       x = "Standard Length (mm)",
       y = "∆Ct GnRH1") +
  theme_classic() +
  facet_wrap(~ FoodCondition)
p.g1.SL
#Note: gestational state: looking at oocyte #/fry (count and weigh, get dev state) and repro organs
## SL v ∆Ct GnRH2
p.g2.SL <- ggplot(data = ge, aes(x = SL_mm,
                                 y = g2.GAP.deltaCt,
                                 color = MorphSex
)) +
  geom_point(position = "identity", size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Body Size vs GnRH2 Genes Expression",
       x = "Standard Length (mm)",
       y = "∆Ct GnRH2") +
  theme_classic()+
  facet_wrap(~ FoodCondition)
p.g2.SL

## SL v ∆Ct GnRHRA
p.RA.SL <- ggplot(data = ge, aes(x = SL_mm,
                                 y = RA.GAP.deltaCt,
                                 color = MorphSex
)) +
  geom_point(position = "identity", size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Body Size vs GnRHRA Genes Expression",
       x = "Standard Length (mm)",
       y = "∆Ct GnRHRA") +
  theme_classic()+
  facet_wrap(~ FoodCondition)
p.RA.SL

## delta Ct with respective controls
p2.g2.RA <- ggplot(data = ge, aes(x = g2.eB.deltaCt,
                                  y = RA.GAP.deltaCt,
                                  #color = FoodCondition
                                  )) +
  geom_point(outlier.shape = NA, position = "identity", size = 3, ) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "GnRH Genes Expression",
       x = "GnRH2 (relative to EarlyB)",
       y = "GnRHRA (relative to GAPDH)") +
  theme_classic() +
  scale_y_continuous(limits = c(0,8),
                     n.breaks = 5) +
  scale_x_continuous(limits = c(0,8),
                     n.breaks = 5)
p2.g2.RA
## delta Ct with GAPDH for both (regardless of separate plates)
p3.g2.RA <- ggplot(data = ge, aes(x = g2.GAP.deltaCt,
                                  y = RA.GAP.deltaCt,
                                  color = FoodCondition
)) +
  geom_point(position = "identity", size = 3, ) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "GnRH Genes Expression",
       x = "GnRH2 (relative to GAPDH*)",
       y = "GnRHRA (relative to GAPDH)") +
  theme_classic() +
  # scale_y_continuous(limits = c(0,8),
  #                    n.breaks = 5) +
  # scale_x_continuous(limits = c(0,8),
  #                    n.breaks = 5)
  facet_wrap(~MorphSex)
p3.g2.RA
# Hans's Feedback:
## look at GnRH2 in Fems v body size (standard length)
## look at GAPDH in Fems v body size (sl)
## look at just females by population, and food; 18S primers for sanity check later

## delta Ct with EarlyB for both (regardless of separate plates)----
#### filter outliers
earlyB.ge <- ge %>% select(Fish,
                           MorphSex,
                           FoodCondition,
                           GnRH2_CtAvg,
                           GnRHRA_CtAvg,
                           EarlyB_CtAvg,
                           EarlyB_CtSD,
                           g2.eB.deltaCt,
                           RA.eb.deltaCt) %>%
  # filtering out two high SD data points to remove the outliers from deltaCt calculations
  filter(EarlyB_CtSD < 1.0)

p4.g2.RA <- ggplot(data = earlyB.ge, aes(x = g2.eB.deltaCt,
                                         y = RA.eB.deltaCt,
                                         #color = FoodCondition
                                         )) +
  geom_point(position = "identity", size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "GnRH Genes Expression",
       x = "GnRH2 (relative to EarlyB)",
       y = "GnRHRA (relative to EarlyB*)") +
  theme_classic()
# scale_y_continuous(limits = c(0,8),
#                    n.breaks = 5) +
# scale_x_continuous(limits = c(0,8),
#                    n.breaks = 5)
p4.g2.RA
