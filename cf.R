# Collaborative Filtering
# Analyzing collaborative filtering on the attributes of user_id, anime_id, my_score
# reference: http://www.salemmarafi.com/code/collaborative-filtering-r/

# collect data that I want
user_anime <- usersAnimes[,c(1:2,6)]
user_anime <- user_anime[user_anime$my_score >= 1 & user_anime$my_score <= 10,]    # remove score lower than 1 or larger than 10
user_anime.sorted <- user_anime[order(user_anime$anime_id),]
user_anime.train <- user_anime.sorted[1:100000,]
head(sort(table(user_anime.train$anime_id), decreasing = TRUE))    # check how many anime in the train data

user_anime.train <- merge(user_anime.train, users[,1:2], by = "username", all.x = TRUE)
user_anime.train <- user_anime.train[,2:4]
colnames(user_anime.train) <- c("anime", "score", "user")

barplot(table(user_anime.train$score))

# create a score matrix
library(reshape2)

user_anime.acast <- acast(user_anime.train, user ~ anime, value.var = "score")
class(user_anime.acast)
user_anime.acast[is.na(user_anime.acast)] <- 0    # repalce NA to 0
View(user_anime.acast)

# create function to get cosine similarity
getCosine <- function(x,y) 
{
  this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}

# create simialrity matrix from item to item
user_anime.similarity  <- matrix(NA, nrow=ncol(user_anime.acast),
                                       ncol=ncol(user_anime.acast),
                                       dimnames=list(colnames(user_anime.acast),
                                                     colnames(user_anime.acast)))
# Loop through the columns
for(i in 1:ncol(user_anime.acast)) {
  # Loop through the columns for each column
  for(j in 1:ncol(user_anime.acast)) {
    # Fill in placeholder with cosine similarities
    user_anime.similarity[i,j] <- getCosine(as.matrix(user_anime.acast[,i]),as.matrix(user_anime.acast[,j]))
  }
}

# Back to dataframe
user_anime.similarity <- as.data.frame(user_anime.similarity)

# get top 3 neighbors
user_anime.neighbours <- matrix(NA, nrow=ncol(user_anime.similarity),
                                  ncol=4,
                                  dimnames=list(colnames(user_anime.similarity)))

# find neighbours
for(i in 1:ncol(user_anime.acast)) {
  user_anime.neighbours[i,] <- (t(head(n=4,
                                       rownames(user_anime.similarity[order(user_anime.similarity[,i],decreasing=TRUE),][i]))))
}

user_anime.neighbours <- as.data.frame(user_anime.neighbours)
user_anime.neighbours <- user_anime.neighbours[,2:4]
colnames(user_anime.neighbours) <- c("top1", "top2", "top3")
for (i in 1:length(rownames(user_anime.neighbours))) {
  # replace anime_id to its title
  row.name <- strtoi(rownames(user_anime.neighbours)[i])
  rownames(user_anime.neighbours)[i] <- as.character(animes$title[animes$anime_id == row.name])
}
for (j in 1:ncol(user_anime.neighbours)) {
  # replace anime_id to its title
  user_anime.neighbours[,j] <- as.character(user_anime.neighbours[,j])
  for (i in 1:nrow(user_anime.neighbours)) {
    user_anime.neighbours[i,j] <- as.character(animes$title[animes$anime_id == user_anime.neighbours[i,j]])
  }
}




