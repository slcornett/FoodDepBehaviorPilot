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

p1 <- ggplot(data = df, aes(x = FoodCondition,
                            y = deltaCt,
                            fill = FoodCondition)) +
  geom_boxplot() +
  facet_wrap(~MorphSex)
p1

