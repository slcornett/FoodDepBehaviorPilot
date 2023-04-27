# SAILFIN MOLLY RNASeq GENES OF INTEREST
## data by Sarah Price and Becca Young

# load packages----
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(ggsci) # science theme for ggplot2

# load dataset----
f3<- "https://raw.githubusercontent.com/slcornett/FoodDepBehaviorPilot/main/fdp_datasets/SailfinTPM_GOIs_14fish.csv"
d3 <- read_csv(f3, col_names = TRUE)

# graph read counts:
## peptides
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
