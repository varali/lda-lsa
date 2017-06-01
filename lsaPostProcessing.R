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

stats <- boxplot.stats(sapply(miv, median))$stats

# Get files that were less than and greater than the median in miv
medians <- sapply(miv, median)
highest90 <- tail(sort(medians), 90)
gtMed90 <- which(medians %in% highest90)
lowest90 <- head(sort(medians), 90)
ltMed90 <- which(medians %in% lowest90)

highest10 <- tail(sort(medians), 10)
gtMed10 <- which(medians %in% highest10)
lowest10 <- head(sort(medians), 10)
ltMed10 <- which(medians %in% lowest10)

highest25 <- tail(sort(medians), 25)
gtMed25 <- which(medians %in% highest25)
lowest25 <- head(sort(medians), 25)
ltMed25 <- which(medians %in% lowest25)

highest75 <- tail(sort(medians), 75)
gtMed75 <- which(medians %in% highest75)
lowest75 <- head(sort(medians), 75)
ltMed75 <- which(medians %in% lowest75)

highest50 <- tail(sort(medians), 50)
gtMed50 <- which(medians %in% highest50)
lowest50 <- head(sort(medians), 50)
ltMed50 <- which(medians %in% lowest50)


filesToUse <- ltMed75
print(filesToUse)

numTraining <- 75
numTesting <- 100 - numTraining

############## READ IN DOCUMENTS ##############
wd <- getwd()
testtext <- apply(read.table(paste(wd, "/lda-lsa/curatedafg_100_summary.csv", sep=""), header=FALSE, sep=","), 2, as.character)
# Remove names from rows
testtext <- unname(testtext)
#testtext

iterations <- 1
pp.mutual.info <- vector(mode = "numeric", length = iterations)
pp.document.count <- vector(mode = "numeric", length = 100)
pp.document.mutual.info <- vector(mode = "numeric", length = 100)
pp.terms <- list()
pp.miv <- list()
for (i in 1:100) {
  pp.miv[[i]] <- 0
}

for (s in 1:iterations) {
  
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
  
  #CRN see below lsaSpace <- lsa(td.mat.lsa) # create LSA space
  #do foldin() here
  set.seed(s)
  trainingSet <- td.mat.lsa[, filesToUse]
  trainingLsa <- lsa(trainingSet)
  
  testingSpace <- fold_in(td.mat.lsa[, c(1:100)[-filesToUse]], trainingLsa)
  trainingSpace <- fold_in(td.mat.lsa[, filesToUse], trainingLsa)
  
  lsaSpace <- cbind(trainingSpace, testingSpace)
  
  testIdx <- 1
  trainIdx <- 1
  
  for (i in 1:100) {
    if (i %in% filesToUse) {
      #cat(sprintf("adding to index %d from %d in s.indices\n", i, trainIdx))
      lsaSpace[,i] <- trainingSpace[,trainIdx]
      trainIdx <- trainIdx + 1
    }
    else if (i %in% c(1:100)[-filesToUse]) {
      #cat(sprintf("adding to index %d from %d in -s.indices\n", i, testIdx))
      lsaSpace[,i] <- testingSpace[,testIdx]  
      testIdx <- testIdx + 1
    }
    
  }
  

  dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) # compute distance matrix
  #dist.mat.lsa # check distance matrix
  
  
  fit <- cmdscale(dist.mat.lsa, eig = TRUE, k = 5)
  
  # Get dimensions of td.mat
  #dim(td.mat)
  
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
  
  write.xlsx(data.kmeans, file = paste(wd, "/lda-lsa/lsaresults_curatedafg_100_5.xlsx", sep=""))
  
  preselected_topics <- read.xlsx(paste(wd, "/lda-lsa/preselected_topics.xlsx", sep=""), sheetIndex = 1, header = FALSE)
  
  s.assn.perm <- sort(preselected_topics$X1[c(-s.indices)], index.return=TRUE)$ix
  s.lsa.sorted <- cluster.sort(data.kmeans[s.assn.perm])
  print(s.lsa.sorted)
  
  write.xlsx(s.lsa.sorted, file = paste(wd, "/lda-lsa/lsaresults_test_set_sorted_curatedafg_100.xlsx", sep=""))
  
  
  ############## TEST SET JDM ##############
  
  curated.data <- data.frame(read.xlsx(paste(wd, "/lda-lsa/preselected_topics.xlsx", sep=""), sheetIndex = 1, header = FALSE))
  colnames(curated.data) <- c("curated")
  s.curated.data <- curated.data[,1]
  s.curated.data <- s.curated.data[c(-s.indices)]
  
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
  
  pp.mutual.info[s] <- sum(jdm.testset * log2(jdm.testset/marginals.lsa), na.rm = TRUE)
  
  documents.used <- setdiff(1:100, s.indices)
  for (i in 1:100) {
    if (i %in% documents.used) {
      pp.document.mutual.info[i] <- pp.document.mutual.info[i] + pp.mutual.info[s]
      pp.document.count[i] <- pp.document.count[i] + 1
      if (pp.document.count[i] == 1) {
        pp.miv[[i]] <- pp.mutual.info[s]
      }
      else {
        pp.miv[[i]] <- c(pp.mutual.info[s], pp.miv[[i]])
      }
    }
  }
  
  cat(sprintf("%d: %f\n", s, pp.mutual.info[s]))
  
}

print("DONE")
  
