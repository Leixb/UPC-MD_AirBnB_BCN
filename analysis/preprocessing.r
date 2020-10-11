
dd <- read.csv(gzfile("../listings.csv.gz"), na.strings = c("", "N/A"))

dataset <- "airbnb"

class(dd)

dim(dd)
n<-dim(dd)[1]
K<-dim(dd)[2]

n
K

names(dd)

actives<-c(12, 15, 16, 17, 18, 22, 25, 26, 29, 33, 34, 37, 38, 40, 47, 48, 51,
           52, 53, 54, 56, 57, 58, 61, 62, 63, 66, 67, 69, 74)

names(dd[,actives])

dd$host_since <- as.Date(dd$host_since, format="%Y-%m-%d")

dd$price <- as.numeric(sub("\\,", "", sub("\\$", "", dd$price)))

dd$host_response_rate <- as.numeric(sub("%", "", dd$host_response_rate))
dd$host_acceptance_rate <- as.numeric(sub("%", "", dd$host_acceptance_rate))

booleans <- c("host_is_superhost", "host_has_profile_pic",
              "host_identity_verified", "instant_bookable")
dd[,booleans] <- dd[,booleans] == "t"

dd$host_since_year <- factor(sub("-[0-9-]+","", dd$host_since))
dd$host_since_mm_dd <- as.Date(sub("[0-9]+[-]","", dd$host_since),
                               format="%m-%d")

breaks <- as.Date(c("01-01","03-21","06-21","09-21", "12-21", "12-31"), 
                  format="%m-%d")
dd$host_since_season <- cut(dd$host_since_mm_dd,
                            labels=c("Winter", "Spring",
                                     "Summer", "Autumn", "Winter"),
                            breaks=breaks)

index_new <- grep("host_since_season", colnames(dd))
actives <- c(actives, index_new)
index_new <- grep("host_since_year", colnames(dd))
actives <- c(actives, index_new)

categories <- c("host_response_rate", "host_acceptance_rate")

for (k in categories) {
  newfield <- paste(sep = "", k, "_cat")
  dd[,newfield] <- cut(dd[,k],
                       breaks=c(-1,20,40,60,80,100),
                       labels=c("very low", "low", "average",
                                "high", "very high"))

  # remove old category from actives
  actives <- actives[names(dd)[actives]!=k]

  # add new category to actives
  index_new <- grep(newfield, colnames(dd))
  actives <- c(actives, index_new)
}
actives <- unique(actives)

factors <- c("neighbourhood_group_cleansed", "host_response_time", "room_type")
dd[,factors] <- lapply(dd[,factors], factor)

dd_clean <- dd[,actives]

write.csv(dd_clean,"../bcn_listings_clean.csv", row.names = FALSE)

