#SICB Poster Data, gene expression
# load packages----
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(ggsci) # science theme for ggplot2
#library(cowplot)
library(car) #anova
# load dataset
f1 <- "https://raw.githubusercontent.com/slcornett/FoodDepBehaviorPilot/main/fdp_datasets/2022-12-12_GnRH2EarlyB_qPCR.csv"
df <- read_csv(f1, col_names = TRUE)
print(df)

# ∆Ct method graphing results----
## Filter for Each Population
### females + ornamented males
fom_pop <- df %>% filter(Population == "F+OM")
### females + small males
fsm_pop <- df %>% filter(Population == "F+SM")

## filter for each morph sex
### females
sex.f <- df %>% filter(MorphSex == "F")
### ornamented males
sex.om <- df %>% filter(MorphSex == "OM")
### ornamented males
sex.sm <- df %>% filter(MorphSex == "SM")

## deltaCT population graphs----
### f+om population
fom.p1 <- ggplot(data = fom_pop, aes(x = MorphSex,
                               y = deltaCt,
                               fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_color_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA ) +
  geom_point(position=position_jitterdodge()) +
  labs(title = "GnRH2 qPCR Expression in Female + Ornamented Male Populations",
       x = "Morphological Sex",
       y = "Relative Whole Brain GnRH2 Expression to EarlyB") +
  theme_classic() +
  scale_y_continuous(breaks=pretty(fom_pop$deltaCt, n=15)) +
  #  theme(plot.title = element_text(size = 20, color = "dark green", face = "bold"),
  #        axis.title.x = element_text(size = 18, color = "dark green"),
  #        axis.title.y = element_text(size = 18, color = "dark green"))
  facet_wrap(~Population)
fom.p1

### f+sm population
fsm.p1 <- ggplot(data = fsm_pop, aes(x = MorphSex,
                                     y = deltaCt,
                                     fill = FoodCondition)) +
  scale_color_startrek() +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge()) +
  labs(title = "GnRH2 qPCR Expression in Female + Small Male Populations",
       x = "Morphological Sex",
       y = "Relative Whole Brain GnRH2 Expression to EarlyB") +
  theme_classic() +
  scale_y_continuous(breaks=pretty(fsm_pop$deltaCt, n=15)) + # makes the number of major gridlines = n
  #  theme(plot.title = element_text(size = 20, color = "dark green", face = "bold"),
  #        axis.title.x = element_text(size = 18, color = "dark green"),
  #        axis.title.y = element_text(size = 18, color = "dark green"))
  facet_wrap(~Population)
fsm.p1

## deltaCT morph sex graphs----
### females food v no food
fem.p <- ggplot(data = sex.f, aes(x = FoodCondition,
                                     y = deltaCt,
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
fem.p
### AVG CT GNRH2
fem.p2 <- ggplot(data = sex.f, aes(x = FoodCondition,
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
  scale_y_continuous(limits = c(0,26.0),
                     n.breaks = 15) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend)
fem.p2

### Ornamented Males food v no food
om.p <- ggplot(data = sex.om, aes(x = FoodCondition,
                                  y = deltaCt,
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
om.p
### AVG CT GNRH2
om.p2 <- ggplot(data = sex.om, aes(x = FoodCondition,
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
  scale_y_continuous(limits = c(20.0,26.0),
                      n.breaks = 14) + # breaks=pretty(sex.om$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"), # x-axis
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position="none") #hide legend
om.p2

### Small Males food v no food
sm.p <- ggplot(data = sex.sm, aes(x = FoodCondition,
                                  y = deltaCt,
                                  fill = FoodCondition)) +
  # food : green from sg_GrnYl (#08A47F) # No food: orange from plasma (#E78140)
  scale_fill_manual(values = c("#08A47F", "#E78140")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(), size=3) +
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
sm.p

# Average GnRH2 Expression----
### avg GnRH2 expression
p2 <- ggplot(data = df, aes(x = MorphSex,
                            y = GnRH2_CtAvg,
                            color = FoodCondition)) +
  geom_point() +
  theme_classic()
p2
### avg GnRH2 expression by pop: F_OM
fom.p2 <- ggplot(data = fom_pop, aes(x = MorphSex,
                                     y = GnRH2_CtAvg,
                                     fill = FoodCondition)) +
  scale_color_startrek() +
  geom_boxplot(outlier.shape = NA ) +
  geom_point(position=position_jitterdodge()) +
  labs(title = "GnRH2 qPCR Expression in the Female + Ornamented Male Populations",
       x = "Morphological Sex",
       y = "Whole Brain GnRH2 Expression") +
  theme_classic() +
  scale_y_continuous(breaks=pretty(fom_pop$GnRH2_CtAvg, n=15)) + # makes the number of major gridlines = n
  #  theme(plot.title = element_text(size = 20, color = "dark green", face = "bold"),
  #        axis.title.x = element_text(size = 18, color = "dark green"),
  #        axis.title.y = element_text(size = 18, color = "dark green"))
  facet_wrap(~Population)
fom.p2

### avg GnRH2 expression by pop: F+SM
fsm.p2 <- ggplot(data = fsm_pop, aes(x = MorphSex,
                                     y = GnRH2_CtAvg,
                                     fill = FoodCondition)) +
  scale_color_startrek() +
  geom_boxplot(outlier.shape = NA ) +
  geom_point(position=position_jitterdodge()) +
  labs(title = "GnRH2 qPCR Expression in the Female + Small Male Populations",
       x = "Morphological Sex",
       y = "Avg Whole Brain GnRH2 Expression") +
  theme_classic() +
  scale_y_continuous(breaks=pretty(fsm_pop$GnRH2_CtAvg, n=8)) + # makes the number of major gridlines = n
  #  theme(plot.title = element_text(size = 20, color = "dark green", face = "bold"),
  #        axis.title.x = element_text(size = 18, color = "dark green"),
  #        axis.title.y = element_text(size = 18, color = "dark green"))
  facet_wrap(~Population)
fsm.p2

# Average EarlyB Expression----
p3 <- ggplot(data = df, aes(x = MorphSex,
                            y = EarlyB_CtAvg,
                            fill = FoodCondition)) +
  scale_color_startrek() +
  geom_boxplot(outlier.shape = NA ) +
  geom_point(position=position_jitterdodge()) +
  labs(title = "EarlyB qPCR Results",
       x = "Morphological Sex",
       y = "Avg Whole Brain Avg EarlyB Expression")
p3
### avg EarlyB expression by pop: F+OM
fom.p3<- ggplot(data = fom_pop, aes(x = MorphSex,
                                    y = EarlyB_CtAvg,
                                    fill = FoodCondition)) +
  scale_color_startrek() +
  geom_boxplot(outlier.shape = NA ) +
  geom_point(position=position_jitterdodge()) +
  labs(title = "EarlyB qPCR Expression in Female + Ornamented Male Populations",
       x = "Morphological Sex",
       y = "Avg Whole Brain EarlyB Expression") +
  theme_classic() +
  scale_y_continuous(breaks=pretty(fom_pop$EarlyB_CtAvg, n=15)) + # makes the number of major gridlines = n
  #  theme(plot.title = element_text(size = 20, color = "dark green", face = "bold"),
  #        axis.title.x = element_text(size = 18, color = "dark green"),
  #        axis.title.y = element_text(size = 18, color = "dark green"))
  facet_wrap(~Population)
fom.p3

### avg EarlyB expression by pop: F+SM
fsm.p3<- ggplot(data = fsm_pop, aes(x = MorphSex,
                                    y = EarlyB_CtAvg,
                                    fill = FoodCondition)) +
  scale_color_startrek() +
  geom_boxplot(outlier.shape = NA ) +
  geom_point(position=position_jitterdodge()) +
  labs(title = "EarlyB qPCR Expression in Female + Small Male Populations",
       x = "Morphological Sex",
       y = "Avg Whole Brain EarlyB Expression") +
  theme_classic() +
  scale_y_continuous(breaks=pretty(fsm_pop$EarlyB_CtAvg, n=15)) + # makes the number of major gridlines = n
  #  theme(plot.title = element_text(size = 20, color = "dark green", face = "bold"),
  #        axis.title.x = element_text(size = 18, color = "dark green"),
  #        axis.title.y = element_text(size = 18, color = "dark green"))
  facet_wrap(~Population)
fsm.p3

# RELATIONSHIPS AND INTERACTIONS OF CATEGORIES WITH GNRH2 ----
# in ANOVA and lm, the colon (:) operator includes specific interaction terms
# in ANOVA and lm, asterisk (*) operator includes all interaction terms

## sex anova
### GnRH2 Ct
asex.GnRH2 <- aov(data = df, GnRH2_CtAvg ~ MorphSex)
asex.GnRH2
summary(asex.GnRH2)
### ∆Ct
asex.dCt <- aov(data = df, deltaCt ~ MorphSex)
asex.dCt
summary(asex.dCt)

## pop anova
apop.GnRH2 <- aov(data = df, GnRH2_CtAvg ~ Population)
apop.GnRH2
summary(apop.GnRH2)
### ∆Ct
apop.dCt <- aov(data = df, deltaCt ~ Population)
apop.dCt
summary(apop.dCt)

## food anova
afood.GnRH2 <- aov(data = df, GnRH2_CtAvg ~ FoodCondition)
afood.GnRH2
summary(afood.GnRH2)
### ∆Ct
afood.dCt <- aov(data = df, deltaCt ~ FoodCondition)
afood.dCt
summary(afood.dCt)
## interaction anova: food and sex
a1.GnRH2 <- aov(data = df, GnRH2_CtAvg ~ FoodCondition:MorphSex)
a1.GnRH2
summary(a1.GnRH2)
## interaction anova: food and pop
a2.GnRH2 <- aov(data = df, GnRH2_CtAvg ~ FoodCondition:Population)
a2.GnRH2
summary(a2.GnRH2)
## ANOVA effect of food condition and effect of morphsex combined
a3.GnRH2 <- aov(data = df, GnRH2_CtAvg ~ FoodCondition+MorphSex)
a3.GnRH2
summary(a3.GnRH2)
## ANOVA effect of food condition and effect of Population combined
a4.GnRH2 <- aov(data = df, GnRH2_CtAvg ~ FoodCondition+Population)
a4.GnRH2
summary(a4.GnRH2)
## ANOVA effect of food condition, of Population, and of morphsex combined
a5.GnRH2 <- aov(data = df, GnRH2_CtAvg ~ Population * MorphSex + FoodCondition)
a5.GnRH2
summary(a5.GnRH2)

# Linear Models
## sex linear model -> SIGNIFICANT @ MORPHSEX SM AND INTERCEPT
lmsex.GnRH2 <- lm(data = df, GnRH2_CtAvg ~ MorphSex)
lmsex.GnRH2
summary(lmsex.GnRH2)
plot(lmsex.GnRH2)
## pop linear model -> SIGNIFICANT @ INTERCEPT of F+SM and F+OM
lmpop.GnRH2 <- lm(data = df, GnRH2_CtAvg ~ Population)
lmpop.GnRH2
summary(lmpop.GnRH2)
plot(lmpop.GnRH2)
## food linear model -> SIGNIFICANT @ intercept of No
lmfood.GnRH2 <- lm(data = df, GnRH2_CtAvg ~ FoodCondition)
lmfood.GnRH2
summary(lmfood.GnRH2)
## interaction linear model: food sex
lm1.GnRH2 <- lm(data = df, GnRH2_CtAvg ~ FoodCondition:MorphSex)
lm1.GnRH2
summary(lm1.GnRH2)
## interaction linear model: food pop
lm2.GnRH2 <- lm(data = df, GnRH2_CtAvg ~ FoodCondition:Population)
lm2.GnRH2
summary(lm2.GnRH2)
## combined effect of food condition and of morphsex
lm3.GnRH2 <- lm(data = df, GnRH2_CtAvg ~ FoodCondition+MorphSex)
lm3.GnRH2
summary(lm3.GnRH2)
## combined effect linear model: food pop
lm4.GnRH2 <- lm(data = df, GnRH2_CtAvg ~ FoodCondition+Population)
lm4.GnRH2
summary(lm4.GnRH2)
## combined effects linear model: food + pop + morphsex
lm5.GnRH2 <- lm(data = df, GnRH2_CtAvg ~ FoodCondition+Population+MorphSex)
lm5.GnRH2
summary(lm5.GnRH2)
