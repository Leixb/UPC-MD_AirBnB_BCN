# Read file
dd <- read.csv(gzfile("../listings.csv.gz"), na.strings = c("", "N/A"))

class(dd)

dim(dd)
n <- dim(dd)[1]
K <- dim(dd)[2]

n
K

names(dd)

# Select variables

actives <- c(
    "host_response_time",
    "host_response_rate",
    "host_acceptance_rate",
    "host_is_superhost",
    "host_listings_count",
    "host_has_profile_pic",
    "host_identity_verified",
    "neighbourhood_group_cleansed",
    "room_type",
    "accommodates",
    "bedrooms",
    "beds",
    "price",
    "minimum_nights_avg_ntm",
    "maximum_nights_avg_ntm",
    "availability_30",
    "availability_60",
    "availability_90",
    "availability_365",
    "number_of_reviews",
    "number_of_reviews_ltm",
    "number_of_reviews_l30d",
    "review_scores_rating",
    "review_scores_accuracy",
    "review_scores_cleanliness",
    "review_scores_location",
    "review_scores_value",
    "instant_bookable",
    "reviews_per_month"
)

# Parse dates
dd$host_since <- as.Date(dd$host_since, format = "%Y-%m-%d")

# Parse prices
dd$price <- as.numeric(sub("\\,", "", sub("\\$", "", dd$price)))

# Parse percentages
dd$host_response_rate <- as.numeric(sub("%", "", dd$host_response_rate))
dd$host_acceptance_rate <- as.numeric(sub("%", "", dd$host_acceptance_rate))

# Convert boolean to logical
booleans <- c(
    "host_is_superhost", "host_has_profile_pic",
    "host_identity_verified", "instant_bookable"
)
dd[, booleans] <- dd[, booleans] == "t"

# Convert to factor and numerical
dd$host_since_year <- factor(sub("-[0-9-]+", "", dd$host_since))
dd$host_since_mm_dd <- as.Date(sub("[0-9]+[-]", "", dd$host_since),
    format = "%m-%d"
)

# Create new categories
breaks <- as.Date(c("01-01", "03-21", "06-21", "09-21", "12-21", "12-31"),
    format = "%m-%d"
)
dd$host_since_season <- cut(dd$host_since_mm_dd,
    labels = c("Winter", "Spring", "Summer", "Autumn", "Winter"),
    breaks = breaks
)

actives <- c(actives, "host_since_season", "host_since_year")

categories <- c("host_response_rate", "host_acceptance_rate")

for (k in categories) {
    newfield <- paste(sep = "", k, "_cat")
    dd[, newfield] <- cut(dd[, k],
        breaks = c(-1, 20, 40, 60, 80, 100),
        labels = c(
            "very low",
            "low",
            "average",
            "high",
            "very high"
        )
    )

    # remove old category from actives
    actives <- actives[actives != k]

    # add new category to actives
    actives <- c(actives, newfield)
}
actives <- unique(actives)

# Mark as factors
factors <- c("neighbourhood_group_cleansed", "host_response_time", "room_type")
dd[, factors] <- lapply(dd[, factors], factor)

# Save only the columns we need
dd_clean <- dd[, actives]

# Write to file
write.csv(dd_clean, "../bcn_listings_clean.csv", row.names = FALSE)
