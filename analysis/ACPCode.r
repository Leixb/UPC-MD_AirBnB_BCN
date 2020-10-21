library(ggplot2)

dd <- readRDS('data/20-data_na.Rda')

#
# VISUALISATION OF DATA
#
# PRINCIPAL COMPONENT ANALYSIS OF CONTINUOUS VARIABLES, WITH Dictamen PROJECTED AS ILLUSTRATIVE
#

# set a list of numerical variables
dcon <- Filter(is.numeric, dd)

# PRINCIPAL COMPONENT ANALYSIS OF dcon
pc1 <- prcomp(dcon, scale = TRUE)
class(pc1)
attributes(pc1)

print(pc1)

# WHICH PERCENTAGE OF THE TOTAL INERTIA IS REPRESENTED IN SUBSPACES?
pc1$sdev
inerProj <- pc1$sdev^2
inerProj
totalIner <- sum(inerProj)
totalIner
pinerEix <- 100 * inerProj / totalIner
pinerEix
barplot(pinerEix)

# Cummulated Inertia in subspaces, from first principal component to the 11th dimension subspace
barplot(100 * cumsum(pc1$sdev[1:dim(dcon)[2]]^2) / dim(dcon)[2])
percInerAccum <- 100 * cumsum(pc1$sdev[1:dim(dcon)[2]]^2) / dim(dcon)[2]
percInerAccum

# SELECTION OF THE SINGIFICNT DIMENSIONS (keep 80% of total inertia)
nd <- 4

# STORAGE OF THE EIGENVALUES, EIGENVECTORS AND PROJECTIONS IN THE nd DIMENSIONS
Psi <- pc1$x[, 1:nd]

# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES
iden <- row.names(dcon)
etiq <- names(dcon)
ze <- rep(0, length(etiq)) # WE WILL NEED THIS VECTOR AFTERWARDS FOR THE GRAPHICS

# Helper function for axis
build_axis <- function(color = 'black')  for (i in 1:4) axis(side = i, pos = 0, labels = F, col = color)

# 3D projections
# library(rgl)
# plot3d(Psi[, 1], Psi[, 2], Psi[, 3])

# Projection of variables
Phi <- cor(dcon, Psi)

plot_plane <- function(i, j, plot_title) {
    arrow_col <- 'blue'
    arrow_txt <- 'darkblue'

    X <- Phi[, i]
    Y <- Phi[, j]

    plot(Psi[, i], Psi[, j], type = "n", xlim = c(-1, 1), ylim = c(-1, 1),
         xlab = i, ylab = j, main = plot_title)
    build_axis()

    arrows(ze, ze, X, Y, length = 0.07, col = arrow_col)
    text(X, Y, labels = etiq, col = arrow_txt, cex = 0.7)
}

plot_all_planes <- function(varcat, plot_title) {
  point_col <- 'orange'
  point_txt <- 'red'
  offset <- 0.05

  for (i in 1:4) {
    mn <- i + 1
    if (mn > 4) next
    for (j in mn:4) {
      plot_plane(i, j, plot_title)

      fdic1 <- tapply(Psi[, i], varcat, mean)
      fdic2 <- tapply(Psi[, j], varcat, mean)

      points(fdic1, fdic2, pch = 16, col = point_col, labels = levels(varcat))
      text(fdic1, fdic2 + offset, labels = levels(varcat), col = point_txt, cex = 0.7)
    }
  }
}

plot_all_cat <- function(eje1, eje2, cat_names) {
  ax_col <- 'cyan'
  plot(Psi[, eje1], Psi[, eje2], type = "n", ylim = c(-1, 1), xlim = c(-1, 1),
       xlab = eje1, ylab = eje2)
  build_axis(ax_col)

  colors <- rainbow(length(cat_names), alpha = 1)

  c <- 1
  for (k in cat_names) {
    seguentColor <- colors[c]
    fdic1 <- tapply(Psi[, eje1], dd[, k], mean)
    fdic2 <- tapply(Psi[, eje2], dd[, k], mean)

    # points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(dd[,k]))
    # connect modalities of qualitative variables
    lines(fdic1, fdic2, pch = 16, col = seguentColor)
    text(fdic1, fdic2, labels = levels(dd[, k]), col = seguentColor, cex = 0.6)
    c <- c + 1
    #col <- col + 1
    legend("bottomleft", cat_names, pch = 1, col = colors, cex = 0.6)
  }
}

dcat <- Filter(is.factor, dd)

# All variables on all planes
for (f in names(dcat)) plot_all_planes(dd[, f], f)

# All qualitative together
for (i in 1:4) {
  mn <- i + 1
  if (mn > 4) next
  for (j in mn:4) plot_all_cat(i, j, names(dcat))
}

# TODO

# determine zoom level
# use the scale factor or not depending on the position of centroids
# ES UN FACTOR D'ESCALA PER DIBUIXAR LES FLETXES MES VISIBLES EN EL GRAFIC
# fm = round(max(abs(Psi[,1])))
fm <- 20

# scale the projected variables
X <- fm * U[, eje1]
Y <- fm * U[, eje2]

# represent numerical variables in background
plot(Psi[, eje1], Psi[, eje2], type = "n", xlim = c(-1, 1), ylim = c(-3, 1))
# plot(X,Y,type="none",xlim=c(min(X,0),max(X,0)))
build_axis('cyan')

# add projections of numerical variables in background
arrows(ze, ze, X, Y, length = 0.07, col = "lightgray")
text(X, Y, labels = etiq, col = "gray", cex = 0.7)

# add centroids
c <- 1
for (k in names(dcat)) {
  seguentColor <- colors[c]

  fdic1 <- tapply(Psi[, eje1], dd[, k], mean)
  fdic2 <- tapply(Psi[, eje2], dd[, k], mean)

  # points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(dd[,k]))
  text(fdic1, fdic2, labels = levels(dd[, k]), col = seguentColor, cex = 0.6)
  c <- c + 1
}

legend("bottomleft", names(dcat), pch = 1, col = colors, cex = 0.6)


# add ordinal qualitative variables. Ensure ordering is the correct

dordi <- c(8)


levels(dd[, dordi[1]])
# reorder modalities: when required
dd[, dordi[1]] <- factor(dd[, dordi[1]], ordered = TRUE, levels = c("WorkingTypeUnknown", "altres sit", "temporal", "fixe", "autonom"))
levels(dd[, dordi[1]])

c <- 1
for (k in dordi) {
  seguentColor <- colors[col]
  fdic1 <- tapply(Psi[, eje1], dd[, k], mean)
  fdic2 <- tapply(Psi[, eje2], dd[, k], mean)

  # points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(dd[,k]))
  # connect modalities of qualitative variables
  lines(fdic1, fdic2, pch = 16, col = seguentColor)
  text(fdic1, fdic2, labels = levels(dd[, k]), col = seguentColor, cex = 0.6)
  c <- c + 1
  col <- col + 1
}
legend("topleft", names(dd)[dordi], pch = 1, col = colors[1:length(dordi)], cex = 0.6)



# PROJECTION OF ILLUSTRATIVE qualitative variables on individuals' map
# PROJECCI? OF INDIVIDUALS DIFFERENTIATING THE Dictamen
# (we need a numeric Dictamen to color)

varcat <- dd[, 1]
plot(Psi[, 1], Psi[, 2], col = varcat)
build_axis('darkgray')
legend("bottomleft", levels(varcat), pch = 1, col = c(1, 2), cex = 0.6)


# Overproject THE CDG OF  LEVELS OF varcat
fdic1 <- tapply(Psi[, 1], varcat, mean)
fdic2 <- tapply(Psi[, 2], varcat, mean)

text(fdic1, fdic2, labels = levels(varcat), col = "cyan", cex = 0.75)

# GG

Psi2= as.data.frame(Psi)
Psi2["dict"]=dd$room_type
Psi2["viv"]=dd$host_since_season
Psi2["Importe"]=dd$host_since_year
ggplot(Psi2, aes(x=PC1, y=PC2, color=dict)) +
  geom_point()
ggplot(Psi2, aes(x=PC1, y=PC2, color=viv)) +
  geom_point()
ggplot(Psi2, aes(x=PC1, y=PC2, color=Importe)) +
  geom_point() +
  scale_color_gradient(low="blue", high="red")
