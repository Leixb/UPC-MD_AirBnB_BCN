library(ggplot2)
library(gridExtra)
library(grid)
library(cowplot)

dd_pre <- readRDS('data/10-data_pre.Rda')

dd_na <- readRDS('data/20-data_na.Rda')

dd <- dd_na

n <- dim(dd)[1]

source('shared.r')

save_desc_plot <- function(p, ..., width = plotWidth, height = plotHeight) {
  save_pdf(p, 'desc', w = width, h = height, ...)
}

## Summary of numerical variables

num_summary <- function(df, name) {
  tab <- t(summary(Filter(is.numeric, df)))
  tab <- apply(tab, 2, function(x)
    sub(".*:", "", x))

  cnames <- c("Min", "1st Q.", "Median", "Mean", "3rd Q.", "Max", "NA's")

  if (length(colnames(tab)) < length(cnames)) {
    colnames(tab) <- head(cnames, -1)
  } else {
    colnames(tab) <- cnames
    tab[is.na(tab[,"NA's"]), "NA's"] <- 0
  }

  save_table(tab, 'desc', name)
}

# Basic descriptive analysis for numerical variables
plot_num <- function(k, bins = 100, df = dd, nounique = F) {
  if (!nounique) bins <- min(bins, length(unique(df[, k])))

  var_s <- sym(k)

  hi <-
    ggplot(df, aes(x = !!var_s)) + geom_histogram(bins = bins)

  bp <-
    ggplot(df, aes(x = "", y = !!var_s)) + geom_boxplot() + coord_flip() +
      labs(x = NULL, y = NULL, fill = NULL)

  plot_grid(
    bp,
    hi,
    align = "v",
    nrow = 2,
    rel_heights = c(1 / 4, 3 / 4)
  )
}

# Basic descriptive analysis for factorial variables
plot_fact <- function(k, frecs, df = dd) {
  proportions <- frecs / n

  dfc <- as.data.frame(proportions)
  colnames(dfc)[1] <- k

  var_s <- sym(k)

  p <- ggplot(dfc, aes("", Freq, fill = !!var_s)) +
    geom_bar(
      width = 1,
      size = 1,
      color = "white",
      stat = "identity",
    ) +
    coord_polar("y") +
    geom_text(aes(label = paste0(round(Freq * 100), "%")),
              position = position_stack(vjust = 0.5),
              family = "LM Roman") +
    labs(x = NULL, y = NULL, fill = NULL) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank()
    )

  b <-
    ggplot(df, aes(x = !!var_s, fill = !!var_s)) + geom_bar(width = 0.7) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = -90, hjust = 0))

  save_desc_plot(p, k, 'pie', height = 3.5, width = 3.5)
  save_desc_plot(b, k, 'bar', height = 3.5, width = 3.5)
}


plot_all <- function() {
  for (k in names(dd)) {
    if (is.numeric(dd[, k])) {
      p <- plot_num(k)

      save_desc_plot(p, k, 'hi_bp')

      tab <- data.frame(rbind(t(summary(dd[, k]))))

      # Change col names (remove last if no NA's)
      cnames <-
        c('Min', 'Q1', 'Median', 'Mean', 'Q3', 'Max', "NA's")
      if (length(colnames(tab)) < length(cnames))
        cnames <- head(cnames, -1)
      colnames(tab) <- cnames

      # Add sd and vc columns
      tab$SD <- c(sd(dd[, k], na.rm = T))
      tab$VC <- sd(dd[, k], na.rm = T) / mean(dd[, k], na.rm = T)

      save_table(tab, 'desc', k, 'ext_sum')

    } else {
      frecs <- table(as.factor(dd[, k]), useNA = "ifany")
      plot_fact(k, frecs)

      tab <- sort(frecs, decreasing = T)
      dtab <- data.frame(tab)
      rownames(dtab) <- paste(dtab$Var1)
      dtab$Var1 <- NULL
      dtab['Total',] <- sum(dtab$Freq)
      dtab <- cbind(dtab, Perc = round(dtab$Freq / dtab['Total','Freq'], digits = 4) * 100)
      save_table(dtab, 'desc', k, 'freq')
    }
  }
}

# Additional plots with tweaks
extra_plots <- function() {
  p <- plot_num('price', df = dd[dd$price < 500,])

  save_desc_plot(p, 'price', 'hi_bp-tallat500')

  p <- plot_num('host_listings_count', df = dd[dd$host_listings_count < 50,], nounique = T, bins = 50)

  save_desc_plot(p, 'host_listings_count', 'hi_bp-tallat100')
}

num_summary(dd_pre, 'num_summary')
num_summary(dd_na, 'num_summary_na')
plot_all()
extra_plots()
