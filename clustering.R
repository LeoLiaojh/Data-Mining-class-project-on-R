# clustering users
# selected attributes: user_watching, user_completed, user_onhold, user_dropped, user_plantowatch,
# user_days_spent_watching, stats_mean_score, stats_rewatched, stats_episodes

# get selected attributes from dataset
user.cluster <- users[,c(3:9,14:16)]
# remove scores lower than 1 or larger than 10
user.cluster <- user.cluster[user.cluster$stats_mean_score >= 1 & user.cluster$stats_mean_score <= 10,]
# add up gender class to 0 and 1 values
user.cluster$genderNum[user.cluster$gender == "Female"] <- 0
user.cluster$genderNum[user.cluster$gender == "Male"] <- 1
user.lm <- lm(genderNum ~.-gender, data = user.cluster)
summary(user.lm)
# remove attributes which are not significant at the level of 0.001
# remove gender as well
user.cluster.sig <- user.cluster[,c(1:3,5,8)]

# function that normalizing data by z-score
normalization <- function(rawData) {
  # store nomalized data to a new data.frame
  Data.normalized <- rawData
  for (i in 1:ncol(rawData)) {
    # Read data column by column
    Data.normalized[,i] <- (Data.normalized[,i]-mean(Data.normalized[,i]))/sd(Data.normalized[,i])
  }
  return(Data.normalized)
}

user.cluster.norm <- normalization(user.cluster.sig)

# implement k-means
user.kmeans <- kmeans(user.cluster.norm, 5)
print(user.kmeans)

# Compare clusters with class label (gender)
table(user.cluster$gender, user.kmeans$cluster)

# Visualize the clusters
plot(user.cluster[c("user_completed", "stats_mean_score")], col = user.kmeans$cluster, pch = user.cluster$genderNum)
points(user.kmeans$centers[,c("user_completed", "stats_mean_score")], col = 1:3, pch = 8, cex=2) 





