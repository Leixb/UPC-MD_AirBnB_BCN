library(ggplot2)
library(kableExtra)
library(gridExtra)
library(grid)
library(extrafont)
library(showtext)
library(cowplot)

dd <- readRDS('data/10-data_pre.Rda')
n <- dim(dd)[1]

theme_font <- theme()

font_init <- function() {
  loadfonts(quiet = T)
  font_add("LM Roman", regular = "latinmodern-math.otf")

  theme_font <<- theme(text = element_text(family = "LM Roman"))

  showtext_auto()
}

## Summary of numerical variables
num_summary <- function() {

  tab <- t(summary(Filter(is.numeric, dd)))
  tab <- apply(tab, 2, function(x)
    sub(".*:", "", x))
  colnames(tab) <-
    c("Min", "1st Q.", "Median", "Mean", "3rd Q.", "Max", "NA's")

  cat(kbl(tab, booktabs = T, format = 'latex') %>% #landscape()
        kable_styling(latex_options = c("scale_down")),
      file = 'tables/num_summary.tex')

}

# Basic descriptive analysis for numerical variables
plot_num <- function(k, bins=100, df=dd) {
  bins <- min(bins, length(unique(df[, k])))

  var_s <- sym(k)

  hi <- ggplot(df, aes(x = !!var_s)) + geom_histogram(bins = bins) + theme_font

  bp <-
    ggplot(df, aes(x = "", y = !!var_s)) + geom_boxplot() + coord_flip() +
      theme_font + labs(x = NULL, y = NULL, fill = NULL)

  plot_grid(bp, hi, align = "v", nrow = 2, rel_heights = c(1/4, 3/4))
}

# Basic descriptive analysis for factorial variables
plot_fact <- function(k, frecs, df=dd) {

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

      theme_classic() + theme_font +
      theme(
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )

    ggsave(plot = p,
           sprintf('plots/%s-pie.pdf', k),
           height = 2.5, width = 3.5)

    b <-
      ggplot(df, aes(x = !!var_s, fill = !!var_s)) + geom_bar(width = 0.7) +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = -90, hjust = 0))  + theme_font

    ggsave(plot = b,
           sprintf('plots/%s-bar.pdf', k),
           height = 2.5, width = 3.5)

}

plot_save <- function(p, name, tail, width=5.5, height = 4) {
  ggsave(plot = p,
         file = sprintf('plots/%s-%s.pdf', name, tail),
         device = 'pdf',
         width = width,
         height = height)
}

plot_all <- function() {
  for (k in names(dd)) {
    if (is.numeric(dd[, k])) {

      p <- plot_num(k)

      plot_save(p, k, 'hi_bp')

      tab <- data.frame(rbind(t(summary(dd[, k]))))

      # Change col names (remove last if no NA's)
      cnames <- c('Min', 'Q1', 'Median', 'Mean', 'Q3', 'Max', "NA's")
      if (length(colnames(tab)) < length(cnames))
        cnames <- head(cnames, -1)
      colnames(tab) <- cnames

      # Add sd and vc columns
      tab$SD <- c(sd(dd[, k], na.rm = T))
      tab$VC <- sd(dd[, k], na.rm = T) / mean(dd[, k], na.rm = T)

      cat(kbl(tab, booktabs = T, format = 'latex') %>% #landscape()
            kable_styling(latex_options = c("scale_down")),
          file = sprintf('tables/%s-ext_sum.tex', k))

    } else {
      frecs <- table(as.factor(df[, k]), useNA = "ifany")

      plot_fact(k, frecs)

      table <-
        kbl(sort(frecs, decreasing = T),
            booktabs = T,
            format = 'latex') %>%
        kable_styling(position = "center")
      cat(table, file = sprintf('tables/%s-freq.tex', k))

    }
  }
}

# Additional plots with tweaks
extra_plots <- function() {
  p <- plot_num('price', df = dd[dd$price < 500, ])

  plot_save(p, 'price', 'hi_bp-tallat500')
}

num_summary()
font_init()
plot_all()
extra_plots()
