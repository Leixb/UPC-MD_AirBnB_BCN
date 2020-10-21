library(ggplot2)
library(ggdendro)

dd <- readRDS('data/20-data_na.Rda')

num_vars <- names(Filter(is.numeric, dd))

dcon <-  dd[,num_vars]

d  <- dist(dcon)
h2 <- hclust(d, method = "ward.D")  # NOTICE THE COST
plot(h2)

# # Nicer plot
 dendo <- ggdendrogram(h2, leaf_labels = F, labels = F)
 dendo +
   geom_hline(yintercept = 0.9e6, linetype = "dashed", color = "red")
   # ggtitle('Cluster Dendogram')

ggsave('plots/dendogram.pdf', width = 11, height = 7.6)

saveRDS(h2, 'data/21-clust.Rda')

c <- cutree(h2, 3)

table(c)
plot(c)
