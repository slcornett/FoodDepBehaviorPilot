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

# STEP 1 AVG Ct
df <- df %>%
  drop_na() %>%
  mutate(GnRH2_CtAvg = ((GnRH2Ct_RowA + GnRH2Ct_RowB + GnRH2Ct_RowC)/3),
                    GnRH2_CtSD = sqrt((GnRH2Ct_RowA^2 + GnRH2Ct_RowB^2 + GnRH2Ct_RowC^2)/3),
                    EarlyB_CtAvg = ((EarlyBCt_RowD + EarlyBCt_RowE + EarlyBCt_RowF)/3), #control
                    EarlyB_CtSD = sqrt((EarlyBCt_RowD^2 + EarlyBCt_RowE^2 + EarlyBCt_RowF^2)/3)
                    )
