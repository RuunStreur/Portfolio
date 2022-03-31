library(dendextend)


samp <- cbind(bowie_all[36], bowie_clean)
bowieclean2 <- samp[,-1]
rownames(bowieclean2) <- samp[,1]

samp2 <- cbind(TH_all[36], TH_clean)
THclean2 <- samp2[,-1]
rownames(THclean2) <- samp2[,1]

# install.packages("dendextend")
library(dendextend)

dend <- as.dendrogram(hclust(dist(bowieclean2[1])))
# Like: 
# dend <- small_iris[,-5] %>% dist %>% hclust %>% as.dendrogram

# By default, the dend has no colors to the labels
labels_colors(dend)
## NULL
par(mfrow = c(1,2))
plot(dend, main = "Original dend")

# Let's add some color:
colors_to_use <- as.numeric(bowieclean2[,1])
colors_to_use
## [1] 1 2 3 1 2 3
# But sort them based on their order in dend:
colors_to_use <- colors_to_use[order.dendrogram(dend)]
colors_to_use
## [1] 1 1 2 2 3 3
# Now we can use them
labels_colors(dend) <- colors_to_use
# Now each state has a color
labels_colors(dend) 

plot(dend, main = "A color for every Species")
