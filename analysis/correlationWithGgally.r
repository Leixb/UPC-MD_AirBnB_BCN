library(ggplot2)
library(GGally)

dd <- readRDS('data/20-data_na.Rda')

source('save_plot.r')
save_corr_plot <- function(p, ...) {
  save_pdf(p, 'corr', ...)
}

ourggpairs <- function(columnes, save=F, df=dd, color = NULL) {
  simcolor <- sym(color)
  p <- ggpairs(df,columns = columnes, aes(colour=!!simcolor, alpha=0.5))
  if (save) save_corr_plot(p)
  return(p)
}

reviews <- data.frame( review_scores_rating = dd$review_scores_rating, review_scores_cleanliness = dd$review_scores_cleanliness, review_scores_location = dd$review_scores_location, review_scores_accuracy = dd$review_scores_accuracy, review_scores_value = dd$review_scores_value, room_type = dd$room_type)

ourggpairs(c('review_scores_rating', 'review_scores_cleanliness', 'review_scores_location', 'review_scores_accuracy', 'review_scores_value') , save = T, color = "room_type")

