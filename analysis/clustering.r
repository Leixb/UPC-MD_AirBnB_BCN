library(ggplot2)
library(ggdendro)
library(GGally)
library(FactoMineR)

dd <- readRDS('data/20-data_na.Rda')

names(dd)

num_vars <- names(Filter(is.numeric, dd))

dcon <-  dd[,num_vars]

d  <- dist(dcon)
h2 <- hclust(d, method = "ward.D2")  # NOTICE THE COST

c <- cutree(h2, 3)

dd <- cbind(dd, data.frame(cluster=c))

dd$cluster <- as.factor(dd$cluster)

ggpairs(dd, columns = c(7,8,11,16,21), ggplot2::aes(colour=cluster))


qsup <- names(Filter(function(x)
  is.factor(x) | is.logical(x), dd))
qqsup <- c()
for (i in qsup)
  qqsup <- c(qqsup, which(colnames(dd) == i))

### Perform PCA
res.pca <- PCA(dd,
               ncp = 4,
               quali.sup = qqsup,
               graph = FALSE)


############################################################
# CLUSTERING WITH THE PRINCIPAL COMPONENTS

# Compute distances
Psi <- res.pca$ind$coord
D = dist(Psi)

# Hierarchical clustering
hc <- hclust(D, method = "ward.D2")

plot(hc)

c2 <- cutree(hc, 3)

dd <- cbind(dd, data.frame(cluster=c2))

dd$cluster <- as.factor(dd$cluster)

ggpairs(dd, columns = c(28,18,21), ggplot2::aes(colour=cluster, alpha=0.7))


####################################################
#### Hierarchical Clustering on Principle Components with FactoMineR

dd2 <- dd

res.pca <- PCA(dd2,
               ncp = 4,
               quali.sup = qqsup,
               graph = FALSE)

res.hcpc <- HCPC(res.pca, nb.clust = -1)

class(res.hcpc)
summary(res.hcpc)

dd2 <- cbind(dd2, data.frame(cluster=res.hcpc$data.clust$clust))

dd2$cluster <- as.factor(dd2$cluster)

dd2 <- dd2[dd2$price <= 100,]


#ggpairs(dd2, columns = c(14,15,16,17,18,21), ggplot2::aes(colour=cluster, alpha=0.7))
ggpairs(dd2, columns = c(8,11,16,21,22,23), ggplot2::aes(colour=cluster, alpha=0.7))

# Write to file
write.csv(dd2, "csv/bcn_listings_cluster.csv", row.names = FALSE)

# Serialize object
saveRDS(dd2, 'data/30-data_cluster.Rda')

