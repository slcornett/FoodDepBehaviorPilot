#SICB Poster Data, gene expression
# load packages----
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci) # science theme for ggplot2
library(cowplot)
# load dataset
f1 <- "https://raw.githubusercontent.com/slcornett/FoodDepBehaviorPilot/main/2022-12-12_GnRH2EarlyB_qPCR.csv"
df <- read_csv(f1, col_names = TRUE)
print(df)

# ∆Ct method graphing results----
## Filter for Each MorphSex
### females
fem <- df %>% filter(MorphSex == "F")
### small males
sm <- df %>% filter(MorphSex == "SM")
### ornamented males
om <- df %>% filter(MorphSex == "OM")

## deltaCT
### all
p1 <- ggplot(data = df, aes(x = MorphSex,
                               y = deltaCt,
                               fill = FoodCondition)) +
  scale_color_startrek() +
  geom_boxplot(outlier.shape = NA ) +
  geom_point(position=position_jitterdodge()) +
  labs(title = "GnRH2 qPCR Results",
       x = "Morphological Sex",
       y = "Relative Whole Brain GnRH2 Expression to EarlyB") +
  theme_classic() +
  #  theme(plot.title = element_text(size = 20, color = "dark green", face = "bold"),
  #        axis.title.x = element_text(size = 18, color = "dark green"),
  #        axis.title.y = element_text(size = 18, color = "dark green"))
  facet_wrap(~Population)
p1

#### female
f.p1 <- ggplot(data = fem, aes(x = Population,
                            y = deltaCt,
                            fill = FoodCondition)) +
  geom_boxplot() +
  theme_classic()
f.p1

#### small male
sm.p1 <- ggplot(data = sm, aes(x = Population,
                               y = deltaCt,
                               fill = FoodCondition)) +
  geom_boxplot() +
  theme_classic()
sm.p1

### ornamented males
om.p1 <- ggplot(data =om, aes(x = Population,
                               y = deltaCt,
                               fill = FoodCondition)) +
  geom_boxplot() +
  theme_classic()
om.p1



p2 <- ggplot(data = df, aes(x = MorphSex,
                            y = GnRH2_CtAvg,
                            color = FoodCondition)) +
  geom_point() +
  theme_classic()
p2

p3 <- ggplot(data = df, aes(x = FoodCondition,
                            y = EarlyB_CtAvg,
                            color = FoodCondition)) +
  geom_point() +
  facet_wrap(~Population)
p3
