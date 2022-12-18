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

# ∆Ct and ∆Ct sd
df <- df %>% mutate(deltaCt = GnRH2_CtAvg - EarlyB_CtAvg,
                    deltaCtSD = GnRH2_CtSD - EarlyB_CtSD)



#by_pop <- df %>% group_by(Population)
p1 <- ggplot(data = df, aes(x = MorphSex,
                            y = GnRH2_CtAvg,
                            fill = FoodCondition)) +
  geom_violin() +
  facet_wrap(~Population)
p1

