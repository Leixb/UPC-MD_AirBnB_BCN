library(extrafont)  # LM font
library(showtext)   # LM font on pdf device
library(ggplot2)    # Plots
library(kableExtra) # Tables

if (!exists("fontsLoaded")) {
  fontsLoaded <- T

  loadfonts(quiet = T)
  font_add("LM Roman", regular = "latinmodern-math.otf")
  showtext_auto()

  theme_update(text = element_text(family = "LM Roman"))
}

if (!exists("savePlots"))   savePlots <- T
if (!exists("printPlots"))  printPlots <- interactive()
if (!exists("plotFolder"))  plotFolder <- "plots"
if (!exists("plotWidth"))   plotWidth <- 5.5
if (!exists("plotHeight"))  plotHeight <- 4

if (!exists("save_pdf")) {
  save_pdf <- function(p, name, ..., w = plotWidth, h = plotHeight) {
    title <- paste0(c(name, ...), collapse = "-")
    filename <- sprintf("%s/%s.pdf", plotFolder, title)
    if (savePlots) {
      print(filename)
      ggsave(
        filename = filename,
        plot = p,
        device = "pdf",
        width = w,
        height = h,
      )
    }
    if (printPlots) print(p)
  }
}

if (!exists("saveTables"))  saveTables <- T
if (!exists("printTables")) printTables <- T
if (!exists("tableFolder")) tableFolder <- "tables"

if (!exists("save_table")) {
  save_table <- function(t, name, ...) {
    title <- paste0(c(name, ...), collapse = "-")
    filename <- sprintf("%s/%s.tex", tableFolder, title)
    if (saveTables) {
      print(filename)
      cat(kbl(t, booktabs = T, format = 'latex'),
          file = filename)
    }
    if (printTables) print(t)
  }
}
