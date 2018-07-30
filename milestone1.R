# Load data from the csv files
animes <- read.csv("E:/CLU_Files/Summer18/IT531/project/dataset/anime_filtered.csv", header = TRUE, sep = ",")
users <- read.csv("E:/CLU_Files/Summer18/IT531/project/dataset/users_filtered.csv", header = TRUE, sep = ",")
usersAnimes <- read.csv("E:/CLU_Files/Summer18/IT531/project/dataset/animelists_filtered.csv", header = TRUE, sep = ",")

# Check structures of each table
str(animes)
str(users)
str(usersAnimes)

# mean of the socres and number of the members have scored of anime
mean(animes$score)    # 6.144
mean(animes$scored_by)    # 11463.19

# top 5 studio
head(sort(table(animes$studio, exclude = c(NULL, "", NA)), decreasing = TRUE), 5)

# standard deviation of user's day watched and mean score
user_day_watched.sd = sd(users$user_days_spent_watching)
user_day_watched.sd    # 7636.053
user_mean_score.sd = sd(users$stats_mean_score)
user_mean_score.sd    # 1.539

# plot user day watched and mean score
user_day_watched.normalized = (users$user_days_spent_watching - mean(users$user_days_spent_watching))/user_day_watched.sd
user_mean_score_normalized = (users$stats_mean_score - mean(users$stats_mean_score)/user_mean_score.sd)
plot(user_day_watched.normalized, user_mean_score_normalized)
plot(users$user_days_spent_watching, users$stats_mean_score)

# histogram of scores
hist(animes$score)

# boxplot of members number based on types
boxplot(members ~ type, data=animes, xlab="Types", ylab="Members")

# Scatter plot compares scores and members
with(animes, plot(score, members, col=type, pch=as.numeric(type)))

