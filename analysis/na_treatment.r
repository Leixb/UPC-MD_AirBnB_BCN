library(class)

dd <- read.csv("../bcn_listings_clean.csv")

# Convert years to factors
dd$host_since_year <- as.factor(dd$host_since_year)

many_nulls <- dd[rowSums(is.na(dd)) > 10, ]

categorical_vars <- names(Filter(is.factor, dd))

# Imputation of categorical variables
for (cat in categorical_vars) {
    if (any(is.na(dd[, cat]))) {
        levels(dd[, cat]) <- c(levels(dd[, cat]), "Unknown")
        dd[, cat][is.na(dd[, cat])] <- "Unknown"
    }
}

# Imputation of numerical variables (knn)

dd$maximum_nights_avg_ntm[dd$maximum_nights_avg_ntm > 1125] <- NA

uncomplete_vars <- names(Filter(function(x) is.numeric(x) && any(is.na(x)), dd))
complete_vars <- names(Filter(function(x) is.numeric(x) && !any(is.na(x)), dd))

dd_complete <- dd[, complete_vars]
dim(dd_complete)
names(dd_complete)

for (k in uncomplete_vars) {
    aux1 <- dd_complete[!is.na(dd[, k]), ]
    dim(aux1)
    aux2 <- dd_complete[is.na(dd[, k]), ]
    dim(aux2)

    ref <- dd[!is.na(dd[, k]), k]
    knn_values <- knn(aux1, aux2, ref)

    dd[is.na(dd[, k]), k] <- as.numeric(as.character(knn_values))
    complete_vars <- c(complete_vars, k)
    dd_complete <- dd[, complete_vars]
}

# Remove
dd <- dd[!is.na(dd$host_identity_verified), ]

dim(dd)
summary(dd)

# Write to file
write.csv(dd, "../bcn_listings_na_treated.csv", row.names = FALSE)
