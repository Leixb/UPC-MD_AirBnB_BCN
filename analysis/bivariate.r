library(ggplot2)
library(kableExtra)
library(grid)
#library(extrafont)
#library(showtext)

#loadfonts()
#font_add("LM Roman", regular = "latinmodern-math.otf")

#showtext_auto()

#theme_font <- theme(text = element_text(family = "LM Roman"))

theme_font <- theme()

dd <- readRDS('data/20-data_na.Rda')

saveplot <- function(p) {
  x <- p$labels$x
  y <- p$labels$y
  ggsave(plot = p, sprintf('plots/bivar-%s-%s.pdf', x, y))
}

bivarplot <- function(x, y, df=dd, save=F, geo=geom_point()) {
  p <- ggplot(df, aes_string(x, y)) + geo + theme_font
  if (save) saveplot(p)
  return(p)
}

bivarplot('room_type', 'minimum_nights_avg_ntm', geo = geom_boxplot(), df = dd[dd$minimum_nights_avg_ntm < 10,], save = T)

dim(dd)
