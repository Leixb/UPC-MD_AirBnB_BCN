library(dplyr)
library(ggplot2)
library(extrafont)
library(showtext)

source('save_plot.r')
save_prof_plot <- function(p, ...)  save_plot(p, 'prof', ...)

dd <- readRDS('data/20-data_na.Rda')

#Calcula els valor test de la variable Xnum per totes les modalitats del factor P
ValorTestXnum <- function(Xnum, P) {
   #freq dis of fac
   nk <- as.vector(table(P))

   n <- sum(nk)

   #mitjanes x grups
   xk <- tapply(Xnum, P, mean)

   #valors test
   txk <- (xk - mean(Xnum)) / (sd(Xnum) * sqrt((n - nk) / (n * nk)))

   #p-values
   pxk <- pt(txk, n - 1, lower.tail = F)

   for (c in 1:length(levels(as.factor(P)))) {
      if (pxk[c] > 0.5) {
         pxk[c] <- 1 - pxk[c]
      }
   }
   return (pxk)
}

ValorTestXquali <- function(P, Xquali) {
   taula <- table(P, Xquali)

   n <- sum(taula)

   pk <- apply(taula, 1, sum) / n

   pj <- apply(taula, 2, sum) / n

   pf <- taula / (n * pk)

   pjm <- matrix(data = pj,
                 nrow = dim(pf)[1],
                 ncol = dim(pf)[2])

   dpf <- pf - pjm

   dvt <- sqrt(((1 - pk) / (n * pk)) %*% t(pj * (1 - pj)))

   #i hi ha divisions iguals a 0 dona NA i no funciona
   zkj <- dpf
   zkj[dpf != 0] <- dpf[dpf != 0] / dvt[dpf != 0]

   pzkj <- pnorm(zkj, lower.tail = F)

   for (c in 1:length(levels(as.factor(P)))) {
      for (s in 1:length(levels(Xquali))) {
         if (pzkj[c, s] > 0.5) {
            pzkj[c, s] <- 1 - pzkj[c, s]
         }
      }
   }
   return (list(
      rowpf = pf,
      vtest = zkj,
      pval = pzkj
   ))
}


dades <- dd
K <- dim(dades)[2]

P <- dd$room_type # TODO change this for cluster

nc <- length(levels(factor(P)))
pvalk <-
   matrix(
      data = 0,
      nrow = nc,
      ncol = K,
      dimnames = list(levels(P), names(dades))
   )
nameP <- "Cluster"
n <- dim(dades)[1]


# Numeriques
plot_num <- function(v, cluster = cluster, df = dd) {
   v <- enquo(v)
   cluster <- enquo(cluster)

   bp <-
      ggplot(df, aes(!!v, !!cluster, fill = !!cluster)) + geom_boxplot(show.legend = F)
   vi <-
      ggplot(df, aes(!!v, !!cluster, fill = !!cluster)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), show.legend = F)

   intercept <- mean(pull(df, !!v))

   meanp <-
      ggplot(df) +  geom_bar(aes(!!cluster, !!v, fill = !!cluster),
                             stat = "summary",
                             fun = "mean",
                             show.legend = F) +
      geom_hline(yintercept = intercept,
                 linetype = "dashed",
                 color = "black")


   name <- substring(deparse(v), 2)
   save_prof_plot(bp, name, 'bp')
   save_prof_plot(vi, name, 'vi')
   save_prof_plot(meanp, name, 'meanp')

   return(c(bp, vi, meanp))
}

# Categoriques
plot_cat <- function(v, cluster = cluster, df = dd) {
   v <- enquo(v)
   cluster <- enquo(cluster)

   stack <-
      ggplot(df, aes(!!v, fill = !!cluster)) + geom_bar(position = 'stack')
   side <-
      ggplot(df, aes(!!v, fill = !!cluster)) + geom_bar(position = 'dodge')
   percent <-
      ggplot(df, aes(!!v, fill = !!cluster)) + geom_bar(position = 'fill')

   name <- substring(deparse(v), 2)
   save_prof_plot(stack, name, 'stack')
   save_prof_plot(side, name, 'side')
   save_prof_plot(percent, name, 'percent')

   return(c(stack, side, percent))
}

pvalues <- read.csv(text = 'ANOVA,Kruskal-Wallis,Chi square')

for (k in names(dd)) {
   if (is.numeric(dd[, k])) {
      r <- plot_num(!!sym(k), room_type) # TODO borrar parametre
      for (j in r)
         print(r)

      print(paste("Analisi per classes de la Variable:", k))

      print("Estadistics per groups:")
      for (s in levels(as.factor(P))) {
         print(summary(dades[P == s, k]))
      }
      o <- oneway.test(dades[, k] ~ P)
      print(paste("p-valueANOVA:", o$p.value))
      kw <- kruskal.test(dades[, k] ~ P)
      print(paste("p-value Kruskal-Wallis:", kw$p.value))
      pvalk[, k] <- ValorTestXnum(dades[, k], P)
      print("p-values ValorsTest: ")
      print(pvalk[, k])
      pvalues[k, ] = c(o, kw, NA)
   } else {
      r <- plot_cat(!!sym(k), room_type) # TODO borrar parametre
      for (j in r)
         print(r)

      print(paste("Variable", k))
      table <- table(P, dades[, k])

      rowperc <- prop.table(table, 1)
      colperc <- prop.table(table, 2)
      print("Distribucions condicionades a files")
      print(rowperc)

      dades[, k] <- as.factor(dades[, k])

      #condicionades a classes
      print(append("Categories=", levels(dades[, k])))

      table <- table(dades[, k], P)
      print("Cross Table:")
      print(table)
      print("Distribucions condicionades a columnes:")
      print(colperc)

      chi <- chisq.test(dades[, k], as.factor(P))
      chi_pval <- chi$p.value
      test_pval <- ValorTestXquali(P, dades[, k])

      pvalues[k, ] = c(NA, NA, chi_pval)

      print("Test Chi quadrat: ")
      print(chi_pval)
      print("valorsTest:")
      print(test_pval)
   }
}

# pvalues per class

for (c in 1:length(levels(as.factor(P)))) {
   if (!is.na(levels(as.factor(P))[c])) {
      print(paste("P.values per class:", levels(as.factor(P))[c]))

      print(sort(pvalk[c,]), digits = 3)
   }
}

pvalk <- data.frame(t(pvalk))

save_table(pvalk, 'prof', 'pvalues')
save_table(pvalues, 'prof', 'pvalues', 'extra')
