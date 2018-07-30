# Association rule of interesting rules of attributes
# I did the same preprocessing job as classification, but use the following attributes now:
# type, source, studio, genre

# Get attributes we needed for the classification
anime.arules <- animes[,c(3,4,23,24)]

anime.arules <- anime.arules[anime.arules$genre != "",]    # remove rows of empty genre
anime.arules <- anime.arules[anime.arules$studio != "",]    # remove rows of empty studio

anime.arules <- anime.arules[anime.arules$type != 'Unknown',]    # remove unknown type
anime.arules$type <- as.factor(as.character(anime.arules$type))    # re-assign levels

sort(table(anime.arules$source), decreasing = TRUE)    # look for the values in this attribute
anime.arules <- anime.arules[anime.arules$source != 'Unknown',]    # remove unknown source
anime.arules$source[(anime.arules$source == "4-koma manga") | (anime.arules$source == "Web manga") 
                 | (anime.arules$source == "Digital manga")] <- "Manga"    # assign all manga related sources to "Manga"
anime.arules$source[anime.arules$source == "Visual novel" | anime.arules$source == "Light novel"] <- "Novel"    # assign all novel related sources to "Novel"
anime.arules$source[(anime.arules$source == "Picture book")] <- "Book"    # assign all book related sources to "Book"
anime.arules$source[(anime.arules$source == "Card game")] <- "Game"    # assign all game related sources to "Game"

anime.arules$source <- as.factor(as.character(anime.arules$source))    # re-assign levels

head(sort(table(anime.arules$studio), decreasing = TRUE), 30)    # look for the values in this attribute
anime.arules$studio <- as.factor(as.character(anime.arules$studio))    # re-assign levels

# divide genre by comma
anime.arules.atom <- data.frame()   # create new data frame to store results
for (i in 1:nrow(anime.arules)) {
  # divide genre by comma and remove white space
  genre.group <- gsub('\\s+', '', unlist(strsplit(as.character(anime.arules$genre[i]), "[,]")))
  for (j in 1:length(genre.group)) {
    anime.arules.atom <- rbind(anime.arules.atom, cbind(anime.arules[i,1:3], genre.group[j], stringsAsFactors = FALSE), stringsAsFactors = FALSE)
  }
}
colnames(anime.arules.atom) <- colnames(anime.arules)    # assign col names
sort(table(anime.arules.atom$genre), decreasing = TRUE)    # look for the values in this attribute
anime.arules.atom$genre <- as.factor(anime.arules.atom$genre)    # assign levels

# implement apriori function
library(arules)
anime.rules <- apriori(anime.arules.atom, parameter = list(supp=0.005, conf=0.8))
# anime.rules1.sub <- subset(anime.rules1, subset = rhs %pin% "genre=" )
anime.rules.sorted <- sort(anime.rules, by="lift")
inspect(head(anime.rules.sorted, 10))


