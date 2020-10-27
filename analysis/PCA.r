library(FactoMineR)
library(factoextra)
library(ggplot2)
library(cluster)

dd <- readRDS('data/20-data_na.Rda')

source('shared.r')
save_pcaFact_plot <-
  function(p, ...)
    save_pdf(p, 'pca_fact', ..., w = 10, h = 7)

# squared plot for variables (unit circle)
save_pcaFact_plot_var <-
  function(p, ...)
    save_pdf(p, 'pca_fact', ..., w = 6, h = 6)

save_pcaFact_plot_contrib <-
  function(p, ...)
    save_pdf(p, 'pca_fact', ..., w = 6, h = 2.5)

# Wont be used for PCA
quanti.sup.names <- c(
  #"accommodates"                ,
  #"bedrooms"                    ,
  #"beds"                        ,
  #"availability_30"             ,
  "availability_60"             ,
  "availability_90"             ,
  #"availability_365"            ,
  #"number_of_reviews"           ,
  "number_of_reviews_ltm"       ,
  "number_of_reviews_l30d"      ,
  #"review_scores_rating"        ,
  "review_scores_accuracy"      ,
  "review_scores_cleanliness"
  #"review_scores_location"      ,
  #"review_scores_value"
  #"maximum_nights_avg_ntm",
  #"minimum_nights_avg_ntm",
)

quali.sup.names <- names(Filter(function(x)
  is.factor(x) | is.logical(x), dd))

quali.sup <- c()
for (i in quali.sup.names)
  quali.sup <- c(quali.sup, which(colnames(dd) == i))

quanti.sup <- c()
for (i in quanti.sup.names)
  quanti.sup <- c(quanti.sup, which(colnames(dd) == i))


planes <- 4

### Perform PCA
res.pca <- PCA(
  dd,
  ncp = planes,
  quali.sup = quali.sup,
  quanti.sup = quanti.sup,
  graph = FALSE
)
res.pca

# Eigenvalues
res.pca$eig

# Func Save tables
tables <- function() {
  tab_eig <- data.frame(res.pca$eig[1:min(dim(res.pca$eig)[1]), ])
  colnames(tab_eig) <- c('Eigenvalue', 'Var. %', 'Cum. Var. %')
  save_table(tab_eig, 'pca_fact', 'eig')

  # Variable contribution
  tab_contrib <- data.frame(res.pca$var$contrib)
  save_table(tab_contrib, 'pca_fact', 'contrib')

  tab_cor <- data.frame(res.pca$var$cor)
  save_table(tab_cor, 'pca_fact', 'cor')

  # Supplementary Variable correlation
  tab_scor <- data.frame(res.pca$quanti.sup$cor)
  save_table(tab_scor, 'pca_fact', 'qtsup', 'cor')
}



comp_centroids <- function() {
  planes <- dim(res.pca$quali.sup$coord)[2]

  centroids <- data.frame()
  count <- 1
  for (cat in quali.sup.names) {
    for (l in levels(as.factor(dd[, cat]))) {
      print(sprintf("%s %s -> %s",
                    cat,
                    l,
                    rownames(res.pca$quali.sup$coord)[count]))
      coord <- res.pca$quali.sup$coord[count, ]
      c <- t(data.frame(r = c(cat, l, coord)))
      centroids <- rbind(centroids, c)
      count <- count + 1
    }
  }
  cols <- c("variable", "value")
  for (i in 1:planes)
    cols <- c(cols, paste0("PC", i))
  colnames(centroids) <- cols
  planes <- planes + 2
  centroids[, 3:planes] <- sapply(centroids[, 3:planes], as.numeric)
  return(centroids)
}

# Compute centroids
centroids <- comp_centroids()

### PCA graphs

# Screeplot
scree_plot <- function() {
  scree <- fviz_screeplot(res.pca, addlabels = T, ncp = 20)
  save_pcaFact_plot(scree, 'screeplot')
}

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

  cent <- plot_centroids(i, j)

  plane <- sprintf('plane_%d_%d', i, j)

  save_pcaFact_plot_var(var, plane, 'var')
  save_pcaFact_plot(ind, plane, 'ind')
  save_pcaFact_plot(bi,  plane, 'bi')
  save_pcaFact_plot(cent,  plane, 'cent')
  save_pcaFact_plot_contrib(contrib,  plane, 'contrib')
}

### Plot for the variables
run_planes <- function(func, planes = 4) {
  for (i in 1:planes) {
    mn <- i + 1
    if (mn > planes)
      next
    for (j in mn:planes)
      func(i, j)
  }
}

cat_plots <- function(dcat) {
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
        invisible = "quanti.sup",
        col.var = "black"
      )
      ind$layers[1] <- NULL # Delete points

      plane <- sprintf('plane_%d_%d', i, j)
      save_pcaFact_plot(ind, cat, plane)
    })
  }
}

plot_centroids <- function(i, j, geom='arrow', ...) {
  x <- names(centroids)[i + 2]
  y <- names(centroids)[j + 2]
  p <-
    fviz_pca_var(
      title = '',
      geom = geom,
      res.pca,
      axes = c(1, 2),
      alpha = 0.2,
      invisible = 'quanti.sup',
      ...
    )
  p <-
    p + geom_point(
      data = centroids,
      mapping = aes_string(
        x = x,
        y = y,
        colour = 'variable',
        shape = 'variable'
      )
    ) +
    scale_shape_manual(values = 1:11)
  p
}

plot_centroid_single <- function(i, j, v) {
  x <- names(centroids)[i + 2]
  y <- names(centroids)[j + 2]

  cen <- centroids[centroids$variable == v, ]

  axisx <-
    geom_vline(
      xintercept = 0,
      linetype = "dotted",
      color = "black",
      size = 1.5
    )
  axisy <-
    geom_hline(
      yintercept = 0,
      linetype = "dotted",
      color = "black",
      size = 1.5
    )

  ggplot(cen, aes_string(x = x, y = y, colour = 'value')) + geom_point() + axisx + axisy
}

##################################################

tables()
scree_plot()
run_planes(plane_plots)
cat_plots(quali.sup.names)

saveRDS(res.pca, 'data/30-res_pca.Rda')
