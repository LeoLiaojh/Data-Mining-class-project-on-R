## Classification of anime scores
## Decision tree attributes: type, source, genre; class: intervals of scores

# Get attributes we needed for the classification
anime.cls <- animes[,c(3,4,12,24)]

anime.cls <- anime.cls[anime.cls$genre != "",]    # remove rows of empty genre

sort(table(anime.cls$type), decreasing = TRUE)    # look for the values in this attribute
anime.cls <- anime.cls[anime.cls$type != 'Unknown' & anime.cls$type != 'Music'
                       & anime.cls$type != 'ONA',]    # only keep the top 4 types
anime.cls$type <- as.factor(as.character(anime.cls$type))    # re-assign levels

sort(table(anime.cls$source), decreasing = TRUE)    # look for the values in this attribute
anime.cls <- anime.cls[anime.cls$source != 'Unknown' & anime.cls$source != 'Other'
                       & anime.cls$source != 'Book' & anime.cls$source != 'Picture book'
                       & anime.cls$source != 'Music' & anime.cls$source != 'Game'
                       & anime.cls$source != 'Card game' & anime.cls$source != 'Radio',]    # remove sources that have low frequencies or not usefule
anime.cls$source[(anime.cls$source == "4-koma manga") | (anime.cls$source == "Web manga") 
                | (anime.cls$source == "Digital manga")] <- "Manga"    # assign all manga related sources to "Manga"
anime.cls$source[anime.cls$source == "Visual novel" | anime.cls$source == "Light novel"] <- "Novel"    # assign all novel related sources to "Novel"
anime.cls$source <- as.factor(as.character(anime.cls$source))    # re-assign levels

# divide genre by comma
anime.cls.atom <- data.frame()   # create new data frame to store results
for (i in 1:nrow(anime.cls)) {
  # divide genre by comma and remove white space
  genre.group <- gsub('\\s+', '', unlist(strsplit(as.character(anime.cls$genre[i]), "[,]")))
  for (j in 1:length(genre.group)) {
    anime.cls.atom <- rbind(anime.cls.atom, cbind(anime.cls[i,1:3], genre.group[j], stringsAsFactors = FALSE), stringsAsFactors = FALSE)
  }
}
colnames(anime.cls.atom) <- colnames(anime.cls)    # assign col names
sort(table(anime.cls.atom$genre), decreasing = TRUE)    # look for the values in this attribute
anime.cls.atom <- anime.cls.atom[anime.cls.atom$genre == "Comedy" | anime.cls.atom$genre == "Action"
                                 | anime.cls.atom$genre == "Drama",]    # only keep the top 3 frequent genre
anime.cls.atom$genre <- as.factor(as.character(anime.cls.atom$genre))    # re-assign levels

# for the class attribute score, first remove the score that lower than 1 or larger than 10
anime.cls.atom <- anime.cls.atom[anime.cls.atom$score >= 1 & anime.cls.atom$score <= 10,]
anime.cls.scoreMedian <- median(anime.cls.atom$score)   # looking for socre median
anime.cls.scoreMedian   # because the median score is 7.06, I decide to convert scores into the intervals: [1,7) and [7,10]

# add up a new column descibe the intervals
anime.cls.atom$score.interval[findInterval(anime.cls.atom$score, c(1, 7, 10)) == 1] <- "[1,7)"
anime.cls.atom$score.interval[findInterval(anime.cls.atom$score, c(1, 7, 10)) == 2] <- "[7,10]"
findInterval(anime.cls.atom$score, c(1, 7, 10))
anime.cls.atom$score.interval <- as.factor(anime.cls.atom$score.interval)

# perform decision tree
library(party)
set.seed(1234)
# genreate train and test data
ind <- sample(2, nrow(anime.cls.atom), replace=TRUE, prob=c(0.7, 0.3))
anime.cls.train <- anime.cls.atom[ind==1,c(1:2,4:5)]
anime.cls.test <- anime.cls.atom[ind==2,c(1:2,4:5)]

# implement ctree
anime.cls.formula <- score.interval ~ .
anime.cls.ctree <- ctree(anime.cls.formula, data = anime.cls.train)

# plot result
plot(anime.cls.ctree, type = "simple")

# test model
anime.cls.pred <- predict(anime.cls.ctree, newdata = anime.cls.test)
table(anime.cls.pred, anime.cls.test$score.interval)




