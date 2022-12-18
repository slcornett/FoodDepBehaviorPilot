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

# ∆Ct method ----
# Avg and std dev Ct of target (GnRH2) and control (earlyB)
## must do it the long way for it to work rowwise
df <- df %>% mutate(GnRH2_CtAvg = ((GnRH2Ct_RowA + GnRH2Ct_RowB + GnRH2Ct_RowC)/3),
                    GnRH2_CtSD = sqrt((GnRH2Ct_RowA^2 + GnRH2Ct_RowB^2 + GnRH2Ct_RowC^2)/3),
                    EarlyB_CtAvg = ((EarlyBCt_RowD + EarlyBCt_RowE + EarlyBCt_RowF)/3), #control
                    EarlyB_CtSD = sqrt((EarlyBCt_RowD^2 + EarlyBCt_RowE^2 + EarlyBCt_RowF^2)/3)
                    )

# ∆Ct and ∆Ct sd
df <- df %>% mutate(deltaCt = GnRH2_CtAvg - EarlyB_CtAvg,
                    deltaCtSD = GnRH2_CtSD - EarlyB_CtSD)



#by_pop <- df %>% group_by(Population)
p1 <- ggplot(data = df, aes(x = MorphSex,
                            y = GnRH2_CtAvg,
                            color = MorphSex)) +
  geom_point() +
  facet_wrap(~FoodCondition)
p1

