# Written by Cody Newbold, 2016
# Base code taken from
# http://meefen.github.io/blog/2013/03/11/analyze-text-similarity-in-r-latent-semantic-analysis-and-multidimentional-scaling/

# Import library files
library(tm)
library(ggplot2)
library(lsa)
library(xlsx)
library(fields)

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

############## READ IN DOCUMENTS ##############
wd <- getwd()
testtext <- apply(read.table(paste(wd, "/lda-lsa/curatedafg_100_summary.csv", sep=""), header=FALSE, sep=","), 2, as.character)
# Remove names from rows
testtext <- unname(testtext)
#testtext

view <- factor(rep(c("topic 1", "topic 2", "topic 3", "topic 4", "topic 5"), each = 20))
df <- data.frame(testtext, view, stringsAsFactors = FALSE)
#df

# Stemming, removing stop words
corpus <- Corpus(VectorSource(df$testtext))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
corpus <- tm_map(corpus, stemDocument, language = "english")
#corpus

# Create a document term matrix
td.mat <- as.matrix(TermDocumentMatrix(corpus))
#td.mat

# MDS with LSA
td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat) # weighting
lsaSpace <- lsa(td.mat.lsa) # create LSA space
#do foldin() here
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) # compute distance matrix
#dist.mat.lsa # check distance matrix


fit <- cmdscale(dist.mat.lsa, eig = TRUE, k = 5)

# Get dimensions of td.mat
dim(td.mat)

# Store a textmatrix
tm <- as.textmatrix(lsaSpace)[,]
dim(tm)
as.matrix(dist(t(tm)))

# Set seed and number of topics
set.seed(0)
k <- 5

# Split documents into topics and display in two rows
data.kmeans <- kmeans(fit$points, k, nstart = 10)$cluster
data.kmeans

library(xlsx)
write.xlsx(data.kmeans, file = paste(wd, "/lda-lsa/lsaresults_curatedafg_100_5.xlsx", sep=""))

preselected_topics <- read.xlsx(paste(wd, "/lda-lsa/preselected_topics.xlsx", sep=""), sheetIndex = 1, header = FALSE)
#preselected_topics 

assn.sorted <- sort(preselected_topics$X1)
#assn.sorted
write.xlsx(assn.sorted, file = paste(wd, "/lda-lsa/preselected_topics_sorted.xlsx", sep=""))

assn.perm <- sort(preselected_topics$X1, index.return=TRUE)$ix
#assn.perm

lsa.sorted <- cluster.sort(data.kmeans[assn.perm])
lsa.sorted

write.xlsx(lsa.sorted, file = paste(wd, "/lda-lsa/lsaresults_sorted_curatedafg_100.xlsx", sep=""))

############## TRAINING SET JDM ##############

curated.data <- data.frame(read.xlsx(paste(wd, "/lda-lsa/preselected_topics.xlsx", sep=""), sheetIndex = 1, header = FALSE))
colnames(curated.data) <- c("curated")
curated.data

lsa.data <- data.frame(read.xlsx(paste(wd, "/lda-lsa/lsaresults_sorted_curatedafg_100.xlsx", sep=""), sheetIndex = 1, header = TRUE))
colnames(lsa.data) <- c("topic", "lsa")
lsa.data$topic <- as.numeric(lsa.data$topic)
lsa.data$lsa <- as.numeric(lsa.data$lsa)
lsa.data.sorted <- lsa.data[order(lsa.data$topic),]
lsa.data.sorted

jdm.lsa <- matrix(data=0.0, nrow=5, ncol=5)
rownames(jdm.lsa) <- c("cur1", "cur2", "cur3", "cur4", "cur5")
colnames(jdm.lsa) <- c("lsa1", "lsa2", "lsa3", "lsa4", "lsa5")

for (i in 1:100) {
  jdm.lsa[curated.data[i,],lsa.data.sorted[i,]$lsa] = jdm.lsa[curated.data[i,],lsa.data.sorted[i,]$lsa] + 1
}

for (i in 1:5) {
  for (j in 1:5) {
    jdm.lsa[i,j] = jdm.lsa[i, j] / 100
  }
}

image.plot(jdm.lsa, graphics.reset = TRUE)

# Calculate mutual information for LSA
marginals.lsa <- as.matrix(apply(jdm.lsa, 1, sum)) %*% apply(jdm.lsa, 2, sum) 
sum(jdm.lsa * log2(jdm.lsa/marginals.lsa), na.rm = TRUE)

