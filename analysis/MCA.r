library(FactoMineR)
library(factoextra)
library(ggplot2)
library(cluster)

dd <- readRDS('data/20-data_na.Rda')

source('shared.r')
FILE_PRE <- 'mca_fact'
save_mcaFact_plot <- function(p, ...) save_pdf(p, FILE_PRE, ..., w = 10, h = 7)

# squared plot for variables (unit circle)
save_mcaFact_plot_var <- function(p, ...) save_pdf(p, FILE_PRE, ..., w = 6, h = 6)

save_mcaFact_plot_contrib <- function(p, ...) save_pdf(p, FILE_PRE, ..., w = 6, h = 2.5)

quanti.sup.names <- names(Filter(is.numeric, dd))

quanti.sup <- c()
for (i in quanti.sup.names)
  quanti.sup <- c(quanti.sup, which(colnames(dd) == i))


### Perform mca
res.mca <- MCA(dd,
               quanti.sup = quanti.sup,
               graph = FALSE)
res.mca

# Eigenvalues
res.mca
$eig

tab_eig <- data.frame(res.mca$eig[1:10,])
colnames(tab_eig) <- c('Eigenvalue', 'Var. %', 'Cum. Var. %' )
save_table(tab_eig, FILE_PRE, 'eig')

# Variable contribution
tab_contrib <- data.frame(res.mca$var$contrib)
save_table(tab_contrib, FILE_PRE, 'contrib')

tab_cor <- data.frame(res.mca$var$cor)
save_table(tab_cor, FILE_PRE, 'cor')

# Supplementary Variable correlation
tab_scor <- data.frame(res.mca$quanti.sup$cor)
save_table(tab_scor, FILE_PRE, 'qtsup', 'cor')

### mca graphs
# Screeplot

scree <- fviz_screeplot(res.mca, addlabels = T, ncp = 20)
save_mcaFact_plot(scree, 'screeplot')

plane_plots <- function(i, j) {
  var <-
    fviz_mca_var(
      res.mca,
      axes = c(i, j),
      select.var = list(cos2 = 0.25),
      alpha.quanti.sup = 0.1,
      repel = T
    )
  ind <-
    fviz_mca_ind(
      res.mca,
      axes = c(i, j),
      geom = c('point'),
      col.ind = "steelblue",
      alpha.ind = 0.2,
    )
  bi <-
    fviz_mca_biplot(
      res.mca,
      axes = c(i, j),
      geom = c('point'),
      repel = T,
      alpha.ind = 0.1,
      alpha.quanti.sup = 0.1,
      col.ind = "steelblue",
      col.var = "black"
    )
  contrib <- fviz_contrib(res.mca, 'var', axes = c(i, j))
  contrib$layers[[2]]$data$yintercept <- 2.5

  plane <- sprintf('plane_%d_%d', i, j)

  save_mcaFact_plot_var(var, plane, 'var')
  save_mcaFact_plot(ind, plane, 'ind')
  save_mcaFact_plot(bi,  plane, 'bi')
  save_mcaFact_plot_contrib(contrib,  plane, 'contrib')
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
    ind <- fviz_mca_biplot(
      title = NULL,
      res.mca,
      axes = c(i, j),
      geom = c('point'),
      repel = T,
      alpha.ind = 0,
      pointsize = 2,
      pointshape = 19,
      habillage = 'room_type',
      addEllipses = T,
      ellipse.alpha = 0,
      alpha.var = 0.1,
      select.var = list(cos2 = 0.3),
      col.var = "black"
    )
    ind$layers[1] <- NULL # Delete points

    plane <- sprintf('plane_%d_%d', i, j)
    save_mcaFact_plot(ind, cat, plane)
  })
}

run_planes(plane_plots)
