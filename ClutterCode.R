# CodeNotWorking ----
# data = d = SICB_behavior_data.csv

# COVARIENCE MATRIX PLOT 2: FOOD AND NO FOOD BEHAVIORS----
## Selecting for Day1 FOOD
foodD14cov_input<- d %>% filter(FoodCondition == "Food") %>%
  select(Day14_Chase,
         Day14_Chase_s,
         Day14_Charge,
         `Day14_ParallelSwim-I`,
         `Day14_ParallelSwim-I_s`,
         `Day14_ParallelSwim-R`,
         `Day14_ParallelSwim-R_s`,
         Day14_TransverseApproach,
         Day14_SexDisp_Cwrap,
         Day14_SexDisp_FrontShimmy,
         Day14_AttemptedCopulation,
         Day14_Nip,
         Day14_Stay,
         Day14_Backup,
         Day14_Retreat,
         Day14_Glide,
         Day14_Dart,
         Day14_Refuge_s)
## Selecting for Day14 No food
nofoodD14cov_input<- d %>% filter(FoodCondition == "NoFood") %>%
  select(Day14_Chase,
         Day14_Chase_s,
         Day14_Charge,
         `Day14_ParallelSwim-I`,
         `Day14_ParallelSwim-I_s`,
         `Day14_ParallelSwim-R`,
         `Day14_ParallelSwim-R_s`,
         Day14_TransverseApproach,
         Day14_SexDisp_Cwrap,
         Day14_SexDisp_FrontShimmy,
         Day14_AttemptedCopulation,
         Day14_Nip,
         Day14_Stay,
         Day14_Backup,
         Day14_Retreat,
         Day14_Glide,
         Day14_Dart,
         Day14_Refuge_s)
## Hierarchical clustering with bootstrap, using pvclust: https://github.com/shimo-lab/pvclust
### Day1 food
foodD14_pvclust<-pvclust(foodD14cov_input,
                         method.dist="cor",
                         method.hclust="complete",
                         nboot=1000)
### Day14 food
nofoodD14_pvclust<-pvclust(nofoodD14cov_input,
                           method.dist ="cor",
                           method.hclust="complete",
                           nboot=1000)

## Plot the dendrogram result
### Day1
plot(foodD14cov_input)
### Day14
plot(nofoodD14cov_input)

## Box out the significant ## p value calculation
### Day1
pvrect(foodD14cov_input, alpha=0.95)
### Day14
pvrect(nofoodD14cov_input, alpha=0.95)

## Calculate the covariance matrix heatmap
# ERROR RETURNED:
# Error in cor.test.default(x = foodD14cov_input, y = nofoodD14cov_input,  :
# 'x' must be a numeric vector
cor.foodcov_input <- cor(x = foodD14cov_input,
                         y = nofoodD14cov_input,
                         use = "everything",
                         method = c("pearson"))
#Visualize the covariance matrix using pheatmap (a more advanced package for ploting heatmaps),
#columns and rows are sorted as the hierarchical clustering results
range1 <- max(abs(cor.foodcov_input))
pheatmap(cor.foodcov_input,
         # customize covarience legend so makes more sense
         legend = TRUE,
         breaks = seq(-range1, range1, length.out = 100),
         # BLUE-YELLOW palette: hcl.colors(100, "BluYl")
         color = paletteer_c("grDevices::ag_GrnYl", 100),
         # GREEN-PURPLE palette: # n = the saturation/darkness/closeness of the two ends of the color spectrum
         #colorRampPalette(rev(brewer.pal(n = 10, name = "PRGn")))(100),
         border_color = "black",
         cluster_cols = foodD14cov_input$hclust, # bootstrap values of covariance
         cluster_rows = nofoodD14cov_input$hclust,
         main = "Covariance of X. nigrensis Behaviors Between Food vs No Food Conditions (Day14)")
