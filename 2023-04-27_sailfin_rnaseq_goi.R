# SAILFIN MOLLY RNASeq GENES OF INTEREST
## data by Sarah Price and Becca Young

# load packages----
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(paletteer)
library(patchwork)
#library(ggsci) # science theme for ggplot2

# load dataset----
f3<- "https://raw.githubusercontent.com/slcornett/FoodDepBehaviorPilot/main/fdp_datasets/SailfinTPM_GOIs_14fish.csv"
d3 <- read_csv(f3, col_names = TRUE)

# collapse columns----
## use pivot_longer() to collapse days into single column, with Day ID as a new categorical data column
RNA.reads <- d3 %>%
  pivot_longer(cols=c('FPL1',
                      'FPL10',
                      'FPL11',
                      'FPL12',
                      'FPL13',
                      'FPL17',
                      'FPL18',
                      'FPL19',
                      'FPL2',
                      'FPL3',
                      'FPL4',
                      'FPL5',
                      'FPL8',
                      'FPL9'),
               names_to='subject',
               values_to='read_count')
RNA.reads
## peptides only: gene_name column == gnrh1, gnrh2, or gnrh3
RNA.reads.pep <- RNA.reads %>% filter(gene_name == "GnRH1" | # | = or
                                        gene_name == "GnRH2"|
                                        gene_name == "GnRH3")
## receptors only: gene_name column == gnrhr(A), gnrhr(B), gnrhr(C), MC4R
RNA.reads.receptor <- RNA.reads %>% filter(gene_name == "GnRHR(A)" | # | = or
                                             gene_name == "GnRHR(B)"|
                                             gene_name == "GnRHR(C)"|
                                             gene_name == "MC4R")

# graph read counts----
## peptides
pep.p <- ggplot(data = RNA.reads.pep , aes(x = gene_name,
                                       y = read_count,
                                       fill = gene_name)) +
  scale_fill_discrete(type = paletteer_d("ggthemes::excel_Green")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge(), size = 3) +
  labs(x = "Gene of Interest",
       y = "Reads") +
  theme_classic() +
  scale_y_continuous(limits = c(0,300),
                     n.breaks = 12) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 14, color = "black", face = "bold"), # x-axis
        axis.text.x = element_text(size = 14, color = "black", face = "italic"),
        axis.title.y = element_text(size = 14, color = "black", face = "bold"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position = "none") #hide legend)
pep.p

## receptors
receptor.p <- ggplot(data = RNA.reads.receptor , aes(x = gene_name,
                                                     y = read_count,
                                                     fill = gene_name)) +
  scale_fill_discrete(type = paletteer_d("ggthemes::excel_Blue")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge(), size = 3) +
  labs(x = "Gene of Interest",
       y = "Reads") +
  theme_classic() +
  scale_y_continuous(limits = c(0,15.0),
                     n.breaks = 15) + # breaks=pretty(sex.f$deltaCt, n=15)
  theme(plot.title = element_text(size = 28, color = "black"),
        axis.title.x = element_text(size = 14, color = "black", face = "bold"), # x-axis
        axis.text.x = element_text(size = 14, color = "black", face = "italic"),
        axis.title.y = element_text(size = 14, color = "black", face = "bold"), # y-axis
        axis.text.y = element_text(size = 14, color = "black"),
        legend.position = "none") #hide legend)
receptor.p

pep.p + receptor.p
