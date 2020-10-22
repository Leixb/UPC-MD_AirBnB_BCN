library(ggplot2)
library(kableExtra)
library(grid)
library(extrafont)
library(showtext)

#loadfonts()
font_add("LM Roman", regular = "latinmodern-math.otf")

showtext_auto()

dd <- readRDS('data/10-data_pre.Rda')

n <- dim(dd)[1]

## Summary of numerical variables

tab <- t(summary(Filter(is.numeric, dd)))
tab <- apply(tab, 2, function(x)
  sub(".*:", "", x))
colnames(tab) <-
  c("Min", "1st Q.", "Median", "Mean", "3rd Q.", "Max", "NA's")

cat(kbl(tab, booktabs = T, format = 'latex') %>% #landscape()
      kable_styling(latex_options = c("scale_down")),
    file = 'tables/num_summary.tex')

# Basic descriptive analysis for numerical variables

theme_font <- theme(text = element_text(family = "LM Roman"))

for (k in names(dd)) {
  var_s <- sym(k)
  if (!(is.numeric(dd[, k]) || class(dd[, k]) == "Date")) {
    frecs <- table(as.factor(dd[, k]), useNA = "ifany")
    proportions <- frecs / n

    dfc <- as.data.frame(proportions)
    colnames(dfc)[1] <- k

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
      #scale_fill_manual(values = c("#ffd700", "#bcbcbc", "#ffa500", "#254290")) +
      theme_classic() + theme_font +
      theme(
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
    ggsave(plot = p,
           sprintf('plots/%s-pie.pdf', k),
           height = 5)

    p <-
      ggplot(dd, aes(x = !!var_s, fill = !!var_s)) + geom_bar(width = 0.7) +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = -90, hjust = 0))  + theme_font
    ggsave(plot = p,
           sprintf('plots/%s-bar.pdf', k),
           height = 5)

    table <-
      kbl(sort(frecs, decreasing = T),
          booktabs = T,
          format = 'latex') %>%
      kable_styling(position = "center")
    cat(table, file = sprintf('tables/%s-freq.tex', k))

  } else {
    if (class(dd[, k]) == "Date") {
      print(summary(dd[, k]))
      print(sd(dd[, k]))

      p <-
        ggplot(dd, aes(x = !!var_s)) + geom_histogram()  + theme_font
      ggsave(plot = p,
             sprintf('plots/%s-hist.tex', k),
             height = 5)

    } else {
      hi <- ggplot(dd, aes(x = !!var_s)) + geom_histogram()  + theme_font

      bp <-
        ggplot(dd, aes(x = "", y = !!var_s)) + geom_boxplot() + coord_flip() +
          theme_font + labs(x = NULL, y = NULL, fill = NULL)

      pdf(sprintf('plots/%s-hi_bp.pdf', k))
      grid.newpage()
      grid.draw(rbind(ggplotGrob(bp), ggplotGrob(hi), size = "last"))
      dev.off()

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
    }
  }
}
