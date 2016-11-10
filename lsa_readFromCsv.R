# Code to use Latent Semantic Analysis on example documents
# Documents are stored in text.csv and is split into k topics

# Written by Cody Crawford, 2016
# Base code taken from
# http://meefen.github.io/blog/2013/03/11/analyze-text-similarity-in-r-latent-semantic-analysis-and-multidimentional-scaling/

# Import library files
library(tm)
library(ggplot2)
library(lsa)
library(xlsx)

# jphillips
cluster.sort <- function(data,f=median) {
  s <- seq(range(data)[1], range(data)[2])
  x <- rep(0,length(s))
  l <- list()
  for (i in 1:length(s)) {
    l[[i]] <- which(data == s[i])
    x[i] <- f(l[[i]])
  }
  ix <- sort(x, index.return=TRUE)$ix
  for (i in 1:length(s)) data[l[[ix[i]]]] <- i
  return (data)
}

# Example sentences
# !These are not used in the final analysis!
text <- c("transporting food by cars will cause global warming. so we should go local.",
          "we should try to convince our parents to stop using cars because it will cause global warming.",
          "some food, such as mongo, requires a warm weather to grow. so they have to be transported to canada.",
          "a typical electronic circuit can be built with a battery, a bulb, and a switch.",
          "electricity flows from batteries to the bulb, just like water flows through a tube.",
          "batteries have chemical energe in it. then electrons flow through a bulb to light it up.",
          "birds can fly because they have feathers and they are light.", 
          "why can some birds like pigeons fly while some others like chickens cannot?",
          #"dogs bark")
          "feathers are important for birds to fly. if the feathers on a bird's wings are removed, this bird cannot fly.")
text

# Read in documents from text.csv
setwd("/Users/cody/Documents/lda-lsa")
#testtext <- apply(read.table("text.csv", header=FALSE, sep=","), 2, as.character)
testtext <- apply(read.table("curatedafg_100_summary.csv", header=FALSE, sep=","), 2, as.character)
testtext
# Remove names from rows
testtext <- unname(testtext)
testtext


# for 4 topics, 12 rows
#view <- factor(rep(c("topic 1", "topic 2", "topic 3", "topic 4"), each = 3))
# for 3 topics, 9 rows
#view <- factor(rep(c("topic 1", "topic 2", "topic 3"), each = 3))
view <- factor(rep(c("topic 1", "topic 2", "topic 3", "topic 4", "topic 5"), each = 20))
#df <- read.table("text.csv", header=FALSE, sep=",") 
df <- data.frame(testtext, view, stringsAsFactors = FALSE)
df

# Stemming, removing stop words
#corpus <- Corpus(VectorSource(df$text)) #crc using text
corpus <- Corpus(VectorSource(df$testtext))
#corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
corpus <- tm_map(corpus, stemDocument, language = "english")
corpus

# Create a document term matrix
td.mat <- as.matrix(TermDocumentMatrix(corpus))
td.mat

# MDS with LSA
td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat) # weighting
lsaSpace <- lsa(td.mat.lsa) # create LSA space
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) # compute distance matrix
dist.mat.lsa # check distance matrix

# This creates a space graph - we do not need this right now
# MDS
fit <- cmdscale(dist.mat.lsa, eig = TRUE, k = 5)
#fit
#points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
#ggplot(points, aes(x = x, y = y)) + geom_point(data = points, aes(x = x, y = y, color = df$view)) + geom_text(data = points, aes(x = x, y = y - 0.2, label = row.names(df)))


# Get dimensions of td.mat
dim(td.mat)

# Store a textmatrix
tm <- as.textmatrix(lsaSpace)[,]
dim(tm)
as.matrix(dist(t(tm)))

# Set seed
set.seed(0)

k <- 5

# Split documents into topics and display in two rows
#data.kmeans <- kmeans(t(tm), k, nstart = 10)$cluster
data.kmeans <- kmeans(fit$points, k, nstart = 10)$cluster
data.kmeans

library(xlsx)
write.xlsx(data.kmeans, file = "/Users/cody/Documents/lda-lsa/lsaresults_curatedafg_100_5.xlsx")

preselected_topics <- read.xlsx("/Users/cody/Documents/lda-lsa/preselected_topics.xlsx", sheetIndex = 1, header = FALSE)
preselected_topics 

assn.sorted <- sort(preselected_topics$X1)
assn.sorted
write.xlsx(assn.sorted, file = "/Users/cody/Documents/lda-lsa/preselected_topics_sorted.xlsx")

assn.perm <- sort(preselected_topics$X1, index.return=TRUE)$ix
assn.perm

lsa.sorted <- cluster.sort(data.kmeans[assn.perm])
lsa.sorted

write.xlsx(lsa.sorted, file = "/Users/cody/Documents/lda-lsa/lsaresults_sorted_curatedafg_100_1.xlsx")
