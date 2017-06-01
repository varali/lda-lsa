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

############## INIT VECTORS ##############
iterations <- 10000

mutual.info <- vector(mode = "numeric", length = iterations)
document.count <- vector(mode = "numeric", length = 100)
document.mutual.info <- vector(mode = "numeric", length = 100)
miv <- list()
for (i in 1:100) {
  miv[[i]] <- 0
}

#init s.indices for setting seed
s.indices <- sample(100,90)
randint <- 1

numTraining <- 90
numTesting <- 100 - numTraining

#get random sample of 10 documents - these will be our labeled documents
labeled.indices <- sample(100,10)
unlabeled.indices <- c(1:100)[-labeled.indices]


s <- 1

############## BEGIN LOOP ##############
while (s <= iterations) {
  
  # get random sample of 50 from the 100
  set.seed(s+s.indices[randint])
  s.indices <- sample(100,numTraining)
  s.indices <- sort(s.indices)
  #print(s.indices)
  
  labeledTraining <- which(s.indices %in% labeled.indices)
  labeledTesting <- which(c(1:100)[-s.indices] %in% labeled.indices)
  
  #cat(sprintf("labeledTraining %d labeledTesting %d\n", length(labeledTraining), length(labeledTesting)))
  if (length(labeledTesting) < 2) {
    randint <- randint + 1
    if (randint >= numTraining) {
      randint <- 1
    }
    next 
  }
  
  view <- factor(rep(c("topic 1", "topic 2", "topic 3", "topic 4", "topic 5"), each = 20))
  df <- data.frame(testtext, view, stringsAsFactors = FALSE)
  df
  
  # Stemming, removing stop words
  trainingCorpus <- Corpus(VectorSource(df$testtext))
  trainingCorpus <- tm_map(trainingCorpus, content_transformer(tolower))
  trainingCorpus <- tm_map(trainingCorpus, removePunctuation)
  trainingCorpus <- tm_map(trainingCorpus, function(x) removeWords(x, stopwords("english")))
  trainingCorpus <- tm_map(trainingCorpus, stemDocument, language = "english")
  
  # Create a document term matrix
  td.mat <- as.matrix(TermDocumentMatrix(trainingCorpus))
  #td.mat
  
  # MDS with LSA
  td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat) # weighting
  
  #print(s.indices)
  trainingSet <- td.mat.lsa[, s.indices]
  trainingLsa <- lsa(trainingSet)
  
  testingSpace <- fold_in(td.mat.lsa[, c(1:100)[-s.indices]], trainingLsa)
  trainingSpace <- fold_in(td.mat.lsa[, s.indices], trainingLsa)
  
  lsaSpace <- cbind(trainingSpace, testingSpace)
  lsaSpaceTest <- cbind(trainingSpace, testingSpace)
  
  testIdx <- 1
  trainIdx <- 1
  
  for (i in 1:100) {
    if (i %in% s.indices) {
      #cat(sprintf("adding to index %d from %d in s.indices\n", i, trainIdx))
      lsaSpace[,i] <- trainingSpace[,trainIdx]
      trainIdx <- trainIdx + 1
    }
    else if (i %in% c(1:100)[-s.indices]) {
      #cat(sprintf("adding to index %d from %d in -s.indices\n", i, testIdx))
      lsaSpace[,i] <- testingSpace[,testIdx]  
      testIdx <- testIdx + 1
    }
    
  }
  
  dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) # compute distance matrix
  
  fit <- cmdscale(dist.mat.lsa, eig = TRUE, k = 5)
  
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
  
  preselected_topics <- read.xlsx(paste(wd, "/lda-lsa/preselected_topics.xlsx", sep=""), sheetIndex = 1, header = FALSE)
  
  #CRN partial labeling changed from c(-s.indices)
  s.assn.perm <- sort(preselected_topics$X1[labeledTesting], index.return=TRUE)$ix
  s.lsa.sorted <- cluster.sort(data.kmeans[s.assn.perm])
  
  print(s.lsa.sorted)
  
  # make sure there are at least two unique in labeled test set
  if (length(unique(s.lsa.sorted)) == 1) {
    randint <- randint + 1
    if (randint >= numTraining) {
      randint <- 1
    }
    next
  }
  
  write.xlsx(s.lsa.sorted, file = paste(wd, "/lda-lsa/lsaresults_test_set_sorted_curatedafg_100.xlsx", sep=""))
  
  ############## TEST SET JDM ##############
  curated.data <- data.frame(read.xlsx(paste(wd, "/lda-lsa/preselected_topics.xlsx", sep=""), sheetIndex = 1, header = FALSE))
  colnames(curated.data) <- c("curated")
  s.curated.data <- curated.data[,1]
  #CRN partial labeling changed from c(-s.indices)
  s.curated.data <- s.curated.data[labeledTesting]
  
  testset.sorted <- data.frame(read.xlsx(paste(wd, "/lda-lsa/ldaresults_test_set_sorted_curatedafg_100.xlsx", sep=""), sheetIndex = 1, header = TRUE))
  colnames(testset.sorted) <- c("topic", "lsa")
  
  testset.sorted$topic <- as.numeric(testset.sorted$topic)
  testset.sorted$lsa <- as.numeric(testset.sorted$lsa)
  
  testset.resorted <- testset.sorted[order(testset.sorted$topic),]
  
  jdm.testset <- matrix(data=0.0, nrow=5, ncol=5)
  rownames(jdm.testset) <- c("cur1", "cur2", "cur3", "cur4", "cur5")
  colnames(jdm.testset) <- c("lsa1", "lsa2", "lsa3", "lsa4", "lsa5")
  
  for (i in 1:numTesting) {
    jdm.testset[s.curated.data[i],testset.resorted[i,]$lsa] = jdm.testset[s.curated.data[i],testset.resorted[i,]$lsa] + 1
  }
  
  for (i in 1:5) {
    for (j in 1:5) {
      jdm.testset[i,j] = jdm.testset[i, j] / numTesting
    }
  }
  
  print(jdm.testset)
  
  #image.plot(jdm.lsa, graphics.reset = TRUE)
  
  # Calculate mutual information for LSA
  marginals.lsa <- as.matrix(apply(jdm.testset, 1, sum)) %*% apply(jdm.testset, 2, sum) 
  sum(jdm.testset * log2(jdm.testset/marginals.lsa), na.rm = TRUE)
  
  mutual.info[s] <- sum(jdm.testset * log2(jdm.testset/marginals.lsa), na.rm = TRUE)
  
  documents.used <- setdiff(1:100, s.indices)
  for (i in 1:100) {
    if (i %in% documents.used) {
      document.mutual.info[i] <- document.mutual.info[i] + mutual.info[s]
      document.count[i] <- document.count[i] + 1
      if (document.count[i] == 1) {
        miv[[i]] <- mutual.info[s]
      }
      else {
        miv[[i]] <- c(mutual.info[s], miv[[i]])
      }
    }
  }
  
  cat(sprintf("%d: %f\n", s, mutual.info[s]))
  s <- s + 1
  
  
}

print("DONE")