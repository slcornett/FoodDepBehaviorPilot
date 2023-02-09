# FDP Gene Expression Analysis (qPCR) - All Genes
# load packages----
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(ggsci) # science theme for ggplot2
#library(cowplot)
library(car) #anova

# load dataset----
g <-"https://raw.githubusercontent.com/slcornett/FoodDepBehaviorPilot/main/fdp_datasets/FDP_GeneExpression.csv"
ge <- read_csv(g, col_names = TRUE) # ge = gene expression
print(ge)

# Dataset Calculations ----
## combined housekeeping genes Avg Ct
ge <- ge %>% mutate(Control_AvgCt = mean(EarlyBCt_RowD, EarlyBCt_RowE, EarlyBCt_RowF, # gnrh2 control
                                         GAPDHCt_RowD, GAPDHCt_RowE, GAPDHCt_RowF) # gnrhr2 control
                    )

## ∆Ct for each gene
ge <- ge %>% mutate(g2.deltaCt = GnRH2_CtAvg - Control_AvgCt, # gnrh2
                    R2.deltaCt = GnRHR2_CtSD - Control_AvgCt) # gnrhr2
print(ge)
