#SICB Poster Data, gene expression
# load packages----
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci) # science theme for ggplot2
library(cowplot)
library(MASS) #anova
# load dataset
f1 <- "https://raw.githubusercontent.com/slcornett/FoodDepBehaviorPilot/main/2022-12-12_GnRH2EarlyB_qPCR.csv"
df <- read_csv(f1, col_names = TRUE)
print(df)

# ∆Ct method graphing results----
## Filter for Each Population
### females
fom_pop <- df %>% filter(Population == "F+OM")
### small males
fsm_pop <- df %>% filter(Population == "F+SM")

## deltaCT
### f+om population
fom.p1 <- ggplot(data = fom_pop, aes(x = MorphSex,
                               y = deltaCt,
                               fill = FoodCondition)) +
  scale_color_startrek() +
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
  geom_boxplot(outlier.shape = NA ) +
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

## EarlyB Expression
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
