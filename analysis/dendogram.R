#dd <- read.csv("../bcn_listings_clean.csv")

num_vars <- names(Filter(is.numeric, dd))

dcon <-  dd[,num_vars]

d  <- dist(dcon[1:50,])
h1 <- hclust(d,method="ward.D")  # NOTICE THE COST
plot(h1)

d  <- dist(dcon)
h2 <- hclust(d,method="ward.D")  # NOTICE THE COST
plot(h2)

c <- cutree(h2, 5)

table(c)

plot(c)
