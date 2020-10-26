library(ggplot2)
library(gridExtra)
library(grid)
library(cowplot)

dd <- readRDS('data/10-data_pre.Rda')
n <- dim(dd)[1]

source('save_plot.r')

save_desc_plot <- function(p, width = plotWidth, height = plotHeight, ...) {
  save_plot(p, w = width, h = height, 'desc', ...)
}

## Summary of numerical variables
num_summary <- function() {
  tab <- t(summary(Filter(is.numeric, dd)))
  tab <- apply(tab, 2, function(x)
    sub(".*:", "", x))
  colnames(tab) <-
    c("Min", "1st Q.", "Median", "Mean", "3rd Q.", "Max", "NA's")

  save_table(tab, 'num_summary')
}

# Basic descriptive analysis for numerical variables
plot_num <- function(k, bins = 100, df = dd) {
  bins <- min(bins, length(unique(df[, k])))

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
      stat = "identity"
    ) +
    coord_polar("y") +
    geom_text(aes(label = paste0(round(Freq * 100), "%")),
              position = position_stack(vjust = 0.5),
              family = "LM Roman") +
    labs(x = NULL, y = NULL, fill = NULL) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_classic() +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )

  b <-
    ggplot(df, aes(x = !!var_s, fill = !!var_s)) + geom_bar(width = 0.7) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = -90, hjust = 0))  + theme_font

  save_desc_plot(p, k, 'pie', height = 2.5, width = 3.5)
  save_desc_plot(p, k, 'bar', height = 2.5, width = 3.5)
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

      save_table(tab, k, 'ext_sum')

    } else {
      frecs <- table(as.factor(df[, k]), useNA = "ifany")
      plot_fact(k, frecs)
      tab <- sort(frecs, decreasing = T)
      save_table(tab, k, 'freq')
    }
  }
}

# Additional plots with tweaks
extra_plots <- function() {
  p <- plot_num('price', df = dd[dd$price < 500,])

  save_desc_plot(p, 'price', 'hi_bp-tallat500')

  p <- plot_num('host_listings_count', df = dd[dd$host_listings_count < 100,])

  save_desc_plot(p, 'host_listings_count', 'hi_bp-tallat100')
}

num_summary()
plot_all()
extra_plots()
