library(ggplot2)
library(grid)

dd <- readRDS("data/20-data_na.Rda")

source("save_plot.r")
save_bivar_plot <- function(p) {
  x <- p$labels$x
  y <- p$labels$y
  save_plot(p, 'bivar', x, y)
}

bivarplot <- function(x, y, color = NULL, df = dd, save = F, geo = geom_point()) {
  p <- ggplot(df, aes_string(x, y, colour = color)) + geo
  if (save) save_bivar_plot(p)
  return(p)
}

bivarplot("minimum_nights_avg_ntm", "number_of_reviews", df = dd[dd$minimum_nights_avg_ntm < 10, ], save = T)
bivarplot("minimum_nights_avg_ntm", "reviews_per_month", df = dd[dd$minimum_nights_avg_ntm < 10, ], save = T)
bivarplot("minimum_nights_avg_ntm", "number_of_reviews_l30d", save = T)

bivarplot("room_type", "minimum_nights_avg_ntm", geo = geom_boxplot(), df = dd[dd$minimum_nights_avg_ntm < 10, ], save = T)
bivarplot("number_of_reviews", "review_scores_rating", df = dd[dd$minimum_nights_avg_ntm < 10, ], save = T)
bivarplot("reviews_per_month", "review_scores_rating", color = "price", df = dd[dd$minimum_nights_avg_ntm < 10 & dd$price < 500, ], save = T)

bivarplot("price", "review_scores_rating", df = dd[dd$price < 1000, ], save = T)
bivarplot("room_type", "minimum_nights_avg_ntm", geo = geom_boxplot(), df = dd[dd$minimum_nights_avg_ntm < 10, ], save = T)
bivarplot("neighbourhood_group_cleansed", "price", geo = geom_boxplot(), df = dd[dd$price < 1000, ], save = T)
bivarplot("neighbourhood_group_cleansed", "review_scores_rating", geo = geom_boxplot(), save = T)

bivarplot("host_since_year", "host_listings_count", geo = geom_boxplot(), df = dd[dd$host_listings_count < 200, ], save = T)
bivarplot("host_since_year", "price", geo = geom_boxplot(), df = dd[dd$price < 1000, ], save = T)

dim(dd)
