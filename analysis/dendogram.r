library(ggplot2)
library(ggdendro)

dd <- readRDS('data/20-data_na.Rda')

source('shared.r')
save_dendo_plot <-
  function(p, ...)
    save_pdf(p, 'dendo', ..., w = 11, h = 7.6)

num_vars <- names(Filter(is.numeric, dd))

dcon <-  dd[, num_vars]

d  <- dist(dcon)
h2 <- hclust(d, method = "ward.D")  # NOTICE THE COST
plot(h2)

# # Nicer plot
dendo <- ggdendrogram(h2, leaf_labels = F, labels = F)
dendo <- dendo +
  geom_hline(yintercept = 0.7e6,
             linetype = "dashed",
             color = "red")
# ggtitle('Cluster Dendogram')

save_dendo_plot(dendo)

saveRDS(h2, 'data/21-clust.Rda')

c <- cutree(h2, 3)

df <- cbind(dd, data.frame(h2$order))

table(c)
plot(c)
