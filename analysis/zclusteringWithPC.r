library(FactoMineR)
library(factoextra)
library(ggplot2)
library(cluster)

dd <- readRDS('data/20-data_na.Rda')

source('shared.r')
save_pcaFact_plot <- function(p, ...) save_pdf(p, 'pca_fact', ..., w = 10, h = 7)

# squared plot for variables (unit circle)
save_pcaFact_plot_var <- function(p, ...) save_pdf(p, 'pca_fact', ..., w = 6, h = 6)

save_pcaFact_plot_contrib <- function(p, ...) save_pdf(p, 'pca_fact', ..., w = 6, h = 2.5)

quanti.sup.names <- c(
  #"accommodates"                ,
  "bedrooms"                    ,
  "beds"                        ,
  #"availability_30"             ,
  "availability_60"             ,
  "availability_90"             ,
  "availability_365"            ,
  #"number_of_reviews"           ,
  "number_of_reviews_ltm"       ,
  "number_of_reviews_l30d"      ,
  #"review_scores_rating"        ,
  "review_scores_accuracy"      ,
  "review_scores_cleanliness"   ,
  "review_scores_location"      ,
  "review_scores_value"         ,
  #"maximum_nights_avg_ntm",
  #"minimum_nights_avg_ntm",
  "number_of_reviews"
)
quanti.sup.names <- c()

quali.sup.names <- names(Filter(function(x)
  is.factor(x) | is.logical(x), dd))

quali.sup <- c()
for (i in quali.sup.names)
  quali.sup <- c(quali.sup, which(colnames(dd) == i))

quanti.sup <- c()
for (i in quanti.sup.names)
  quanti.sup <- c(quanti.sup, which(colnames(dd) == i))


### Perform PCA
res.pca <- PCA(dd,
               ncp = 4,
               quali.sup = quali.sup,
               quanti.sup = quanti.sup,
               graph = FALSE)
res.pca

# Eigenvalues
res.pca$eig

tab_eig <- data.frame(res.pca$eig[1:10,])
colnames(tab_eig) <- c('Eigenvalue', 'Var. %', 'Cum. Var. %' )
save_table(tab_eig, 'pca_fact', 'eig')

# Variable contribution
tab_contrib <- data.frame(res.pca$var$contrib)
save_table(tab_contrib, 'pca_fact', 'contrib')

tab_cor <- data.frame(res.pca$var$cor)
save_table(tab_cor, 'pca_fact', 'cor')

# Supplementary Variable correlation
tab_scor <- data.frame(res.pca$quanti.sup$cor)
save_table(tab_scor, 'pca_fact', 'qtsup', 'cor')

### PCA graphs
# Screeplot

scree <- fviz_screeplot(res.pca, addlabels = T, ncp = 20)
save_pcaFact_plot(scree, 'screeplot')

plane_plots <- function(i, j) {
  var <-
    fviz_pca_var(
      res.pca,
      axes = c(i, j),
      select.var = list(cos2 = 0.25),
      alpha.quanti.sup = 0.1,
      repel = T
    )
  ind <-
    fviz_pca_ind(
      res.pca,
      axes = c(i, j),
      geom = c('point'),
      col.ind = "steelblue",
      alpha.ind = 0.2,
    )
  bi <-
    fviz_pca_biplot(
      res.pca,
      axes = c(i, j),
      geom = c('point'),
      repel = T,
      alpha.ind = 0.1,
      alpha.quanti.sup = 0.1,
      col.ind = "steelblue",
      col.var = "black"
    )
  contrib <- fviz_contrib(res.pca, 'var', axes = c(i, j))
  contrib$layers[[2]]$data$yintercept <- 2.5

  plane <- sprintf('plane_%d_%d', i, j)

  save_pcaFact_plot_var(var, plane, 'var')
  save_pcaFact_plot(ind, plane, 'ind')
  save_pcaFact_plot(bi,  plane, 'bi')
  save_pcaFact_plot_contrib(contrib,  plane, 'contrib')
}

### Plot for the variables
run_planes <- function(func, planes=4) {
  for (i in 1:planes) {
    mn <- i + 1
    if (mn > planes) next
    for (j in mn:planes)  func(i, j)
  }
}

dcat <- names(Filter(is.factor, dd))

for (cat in dcat) {
  run_planes(function(i, j) {
    ind <- fviz_pca_biplot(
      title = NULL,
      res.pca,
      axes = c(i, j),
      geom = c('point'),
      repel = T,
      alpha.ind = 0,
      pointsize = 2,
      pointshape = 19,
      habillage = cat,
      addEllipses = T,
      ellipse.alpha = 0,
      alpha.var = 0.1,
      select.var = list(cos2 = 0.3),
      col.var = "black"
    )
    ind$layers[1] <- NULL # Delete points

    plane <- sprintf('plane_%d_%d', i, j)
    save_pcaFact_plot(ind, cat, plane)
  })
}

run_planes(plane_plots)

############################################################
# CLUSTERING WITH THE PRINCIPAL COMPONENTS

# Compute distances
Psi <- res.pca$ind$coord
D = dist(Psi)

# Hierarchical clustering
hc <- hclust(D, method = "ward.D2")

plot(hc, labels = F)
# fviz_dend(hc, show_labels = F, rect = T, k = 3)
barplot(hc$height, main = "Aggregated distance at each iteration")

# How many clusters?
abline(h = 0.12, col = "red", lwd = 2)
abline(h = 0.08, col = "red", lty = 2)

# Number of clusters
nc = 3

# Cut tree
clus1 <- cutree(hc, nc)
table(clus1)

# Representation of the clusters on the first principal plane
plot(Psi, col = as.numeric(clus1))
abline(h = 0, v = 0, col = "gray")
legend("topright",
       c("c1", "c2", "c3"),
       pch = 1,
       col = c(1:3))

# The quality of partition
cdg <- aggregate(as.data.frame(Psi), list(clus1), mean)[, 2:7]
Bss <- sum(rowSums(cdg ^ 2) * as.numeric(table(clus1)))
Tss <- sum(rowSums(Psi ^ 2))
100 * Bss / Tss

# Consolidate the partition
clus2 <- kmeans(Psi, centers = cdg)
clus2$size

Bss <- sum(rowSums(clus2$centers ^ 2) * clus2$size)
Wss <- sum(clus2$withinss)
100 * Bss / (Bss + Wss)

# Clusters on the first principal plane after consolidation
plot(Psi, col = as.numeric(clus2$cluster))
abline(h = 0, v = 0, col = "gray")
legend("topright",
       c("c1", "c2", "c3"),
       pch = 1,
       col = c(1:3))

####################################################
#### Hierarchical Clustering on Principle Components with FactoMineR

? HCPC
res.hcpc <- HCPC(res.pca, nb.clust = -1)
names(res.hcpc)

### Description of the partition

### desc.var ###
### A. The description of the clusters by the variables ###
names(res.hcpc$desc.var)
? catdes

### desc.var$test.chi2 ###
### A.1. The categorical variables which characterizes the clusters ###
res.hcpc$desc.var$test.chi2
RoomTypeXClus <-
  table(res.hcpc$data.clust$room_type, res.hcpc$data.clust$clust)
sweep(RoomTypeXClus, 2, apply(RoomTypeXClus, 2, sum), "/")

### desc.var$category ###
### A.2. The description of each cluster by the categories ##
res.hcpc$desc.var$category
privateRoomXclust1 <-
  table(res.hcpc$data.clust$room_type, res.hcpc$data.clust$clust)[2, 1]
positiu <-
  sum(res.hcpc$data.clust$room_type == "Private room", na.rm = TRUE)
clust1 <- sum(res.hcpc$data.clust$clust == "1")
N <- nrow(res.hcpc$data.clust)
privateRoomXclust1 / positiu ###Cla/Mod
privateRoomXclust1 / clust1  ###Mod/Cla
positiu / N ###Global


###TODO D'AQU´I CAP AVALL
### desc.var$quanti.var ###
### A.3. The quantitative variables which characterizes the clusters ###
res.hcpc$desc.var$quanti.var
summary(lm(res.hcpc$data.clust$Plazo ~ res.hcpc$data.clust$clust))
summary(lm(
  res.hcpc$data.clust$Importe.solicitado ~ res.hcpc$data.clust$clust
))

### desc.var$quanti ###
### A.4. The description of each cluster by the quantitative variables ###
res.hcpc$desc.var$quanti
mean(res.hcpc$data.clust$Edad[res.hcpc$data.clust$clust == 1])
mean(res.hcpc$data.clust$Edad)
sd(res.hcpc$data.clust$Edad[res.hcpc$data.clust$clust == 1])
sd(res.hcpc$data.clust$Edad)

### desc.axes ###
### B. The description of the clusters by the axes ###
names(res.hcpc$desc.axes)
res.hcpc$desc.axes$quanti.var
res.hcpc$desc.axes$quanti

### desc.ind ###
### C. The description of the clusters by the individuals ###
names(res.hcpc$desc.ind)
res.hcpc$desc.ind$para
res.hcpc$desc.ind$dist

para1 <-
  which(rownames(res.pca$ind$coord) %in% names(res.hcpc$desc.ind$para[[1]]))
para2 <-
  which(rownames(res.pca$ind$coord) %in% names(res.hcpc$desc.ind$para[[2]]))
para3 <-
  which(rownames(res.pca$ind$coord) %in% names(res.hcpc$desc.ind$para[[3]]))
dist1 <-
  which(rownames(res.pca$ind$coord) %in% names(res.hcpc$desc.ind$dist[[1]]))
dist2 <-
  which(rownames(res.pca$ind$coord) %in% names(res.hcpc$desc.ind$dist[[2]]))
dist3 <-
  which(rownames(res.pca$ind$coord) %in% names(res.hcpc$desc.ind$dist[[3]]))

points(
  res.pca$ind$coord[para1, 1],
  res.pca$ind$coord[para1, 2],
  col = "blue",
  cex = 2,
  pch = 16
)
points(
  res.pca$ind$coord[dist1, 1],
  res.pca$ind$coord[dist1, 2],
  col = "orange",
  cex = 2,
  pch = 16
)
points(
  res.pca$ind$coord[para2, 1],
  res.pca$ind$coord[para2, 2],
  col = "blue",
  cex = 2,
  pch = 16
)
points(
  res.pca$ind$coord[dist2, 1],
  res.pca$ind$coord[dist2, 2],
  col = "orange",
  cex = 2,
  pch = 16
)
points(
  res.pca$ind$coord[para3, 1],
  res.pca$ind$coord[para3, 2],
  col = "blue",
  cex = 2,
  pch = 16
)
points(
  res.pca$ind$coord[dist3, 1],
  res.pca$ind$coord[dist3, 2],
  col = "orange",
  cex = 2,
  pch = 16
)

### Characteristics of the more typical and more rare individuals
res.hcpc$data.clust[which(rownames(res.hcpc$data.clust) %in% names(res.hcpc$desc.ind$para[[1]])), ]
res.hcpc$data.clust[which(rownames(res.hcpc$data.clust) %in% names(res.hcpc$desc.ind$dist[[1]])), ]
res.hcpc$data.clust[which(rownames(res.hcpc$data.clust) %in% names(res.hcpc$desc.ind$para[[2]])), ]
res.hcpc$data.clust[which(rownames(res.hcpc$data.clust) %in% names(res.hcpc$desc.ind$dist[[2]])), ]
res.hcpc$data.clust[which(rownames(res.hcpc$data.clust) %in% names(res.hcpc$desc.ind$para[[3]])), ]
res.hcpc$data.clust[which(rownames(res.hcpc$data.clust) %in% names(res.hcpc$desc.ind$dist[[3]])), ]

### call ###
### Other parameters and objects of HCPC ###
names(res.hcpc$call)

### call$t ###
### Results for the hierarchical tree ###
names(res.hcpc$call$t)
### Results for the PCA ###
res.hcpc$call$t$res
### The suggested level to cut the tree  ###
res.hcpc$call$t$nb.clust
### Within inertias ###
res.hcpc$call$t$within[1:5]
### Ratio between within inertias ###
res.hcpc$call$t$quot[1:5]
### Inertia gain ###
res.hcpc$call$t$inert.gain[1:5]

##########################################################
### Suggested level to cut tree (Original space)
dcon <- dd[, c(2, 4, 5, 9:16)]
d  <- dist(dcon)
h1 <- hclust(d, method = "ward.D")
plot(h1)

actives <- c(2:16)
dissimMatrix <- daisy(dd[, actives], metric = "gower", stand = TRUE)
distMatrix <- dissimMatrix ^ 2
h2 <- hclust(distMatrix, method = "ward.D")
plot(h2)

### Function suggested level
suggested.level <- function(hc, min = 3, max = 10) {
  if (min < 2)
    stop("Min should be equal or higher than 2")
  intra <- rev(cumsum(hc$height))
  quot <- intra[min:(max)] / intra[(min - 1):(max - 1)]
  nb.clust = which.min(quot) + min - 1
  return(nb.clust)
}

###
suggested.level(h1)
suggested.level(h2)


