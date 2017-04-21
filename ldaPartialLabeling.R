# Written by Cody Newbold, 2016
# Base code taken from
# http://www.rtexttools.com/blog/getting-started-with-latent-dirichlet-allocation-using-rtexttools-topicmodels

# Import library files
library(RTextTools)
library(topicmodels)  
library(ggplot2)
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
testdata <- apply(read.table(paste(wd, "/lda-lsa/curatedafg_100_summary.csv", sep=""), header=FALSE, sep=","), 2, as.character)
testdata <- unname(testdata)

testmatrix <- create_matrix(testdata, language="english", removeNumbers=TRUE, stemWords=TRUE)
testmatrix

############## INIT VECTORS ##############
iterations <- 10000
mutual.info <- vector(mode = "numeric", length = iterations)
document.count <- vector(mode = "numeric", length = 100)
labeled.count <- vector(mode = "numeric", length = 100)
document.mutual.info <- vector(mode = "numeric", length = 100)
terms <- list()
miv <- list()
for (i in 1:100) {
  miv[[i]] <- 0
}

#get random sample of 10 documents - these will be our labeled documents
labeled.indices <- sample(100,10)
unlabeled.indices <- c(1:100)[-labeled.indices]

s = 1

############## BEGIN LOOP ##############
while (s <= iterations) {
  
  # get random sample of 50 from the 100
  s.indices <- sample(100,50)
  s.indices
  
  labeledTraining <- which(s.indices %in% labeled.indices)
  labeledTesting <- which(c(1:100)[-s.indices] %in% labeled.indices)
  cat(sprintf("labeledTraining %d labeledTesting %d\n", length(labeledTraining), length(labeledTesting)))
  if (length(labeledTesting) < 2) {
    next 
  }
  
  #labeled.indices <- sample(c(1:100)[-s.indices], 10)
  #unlabeled.indices <- c(1:100)[-s.indices]
  #unlabeled.indices <- setdiff(unlabeled.indices, labeled.indices)
  
  
  s.data <- testdata[c(s.indices)]
  s.matrix <- create_matrix(s.data, language="english", removeNumbers=TRUE, stemWords=TRUE)
  
  
  ############## TRAIN LDA ##############
  k <- 5
  lda <- LDA(s.matrix, k, control = list(estimate.alpha = TRUE, alpha = 5, seed = seq(1, 10), nstart = 10)) 
  #str(lda)
  
  terms(lda)
  
  # Split documents into topics and list in two rows
  data.lda <- topics(lda)
  #data.lda
  
  write.xlsx(data.lda, file = paste(wd, "/lda-lsa/ldaresults_curatedafg_100_5.xlsx", sep=""))
  
  preselected_topics <- read.xlsx(paste(wd, "/lda-lsa/preselected_topics.xlsx", sep=""), sheetIndex = 1, header = FALSE)
  #preselected_topics 
  
  assn.perm <- sort(preselected_topics$X1[s.indices], index.return=TRUE)$ix
  #assn.perm
  
  lda.sorted <- cluster.sort(data.lda[assn.perm])
  #lda.sorted
  
  write.xlsx(lda.sorted, file = paste(wd, "/lda-lsa/ldaresults_sorted_curatedafg_100.xlsx", sep=""))
  
  
  ############## TRAINING SET JDM ############## 
  
  curated.data <- data.frame(read.xlsx(paste(wd, "/lda-lsa/preselected_topics.xlsx", sep=""), sheetIndex = 1, header = FALSE))
  colnames(curated.data) <- c("curated")
  #curated.data
  
  #lda.data <- data.frame(read.xlsx(paste(wd, "/lda-lsa/ldaresults_sorted_curatedafg_100.xlsx", sep=""), sheetIndex = 1, header = TRUE))
  #colnames(lda.data) <- c("topic", "lda")
  #lda.data
  #lda.data$topic <- as.numeric(as.character(lda.data$topic))
  #lda.data$lda <- as.numeric(as.character(lda.data$lda))
  #lda.data
  #lda.data.sorted <- lda.data[order(lda.data$topic),]
  #lda.data.sorted
  
  #jdm.lda <- matrix(data=0.0, nrow=5, ncol=5)
  #rownames(jdm.lda) <- c("cur1", "cur2", "cur3", "cur4", "cur5")
  #colnames(jdm.lda) <- c("lda1", "lda2", "lda3", "lda4", "lda5")
  
  #for (i in 1:100) {
  #jdm.lda[curated.data[i,],lda.data.sorted[i,]$lda] = jdm.lda[curated.data[i,],lda.data.sorted[i,]$lda] + 1
  #}
  
  #for (i in 1:5) {
  #for (j in 1:5) {
  #jdm.lda[i,j] = jdm.lda[i, j] / 100
  #}
  #}
  #jdm.lda
  
  #image.plot(t(jdm.lda), graphics.reset = TRUE)
  
  # Calculate mutual information for LDA
  #marginals.lda <- as.matrix(apply(jdm.lda, 1, sum)) %*% apply(jdm.lda, 2, sum) 
  #sum(jdm.lda * log2(jdm.lda/marginals.lda), na.rm = TRUE)
  
  #mutual.info[s] <- sum(jdm.lda * log2(jdm.lda/marginals.lda), na.rm = TRUE)
  #terms[[s]] <- (terms(lda))
  
  ############## CREATE TEST SET ##############
  
  #CRN before labeling s.test.data <- testdata[c(-s.indices)]
  s.test.data <- testdata[labeledTesting]
  #CRN ?? s.test.data <- testdata[-s.indices]
  s.test.matrix <- create_matrix(s.test.data, language="english", removeNumbers=TRUE, stemWords=TRUE)
  lda.testing <- posterior(lda, s.test.matrix)
  test.results <- apply(lda.testing$topics, 1, which.max)
  #print(test.results)
  
  #CRN before labeling s.assn.perm <- sort(preselected_topics$X1[c(-s.indices)], index.return=TRUE)$ix
  s.assn.perm <- sort(preselected_topics$X1[labeledTesting], index.return=TRUE)$ix
  s.lda.sorted <- cluster.sort(test.results[s.assn.perm])
  write.xlsx(s.lda.sorted, file = paste(wd, "/lda-lsa/ldaresults_test_set_sorted_curatedafg_100.xlsx", sep=""))
  
  ############## TEST SET JDM ##############
  s.curated.data <- curated.data[,1]
  #CRN before labeling s.curated.data <- s.curated.data[c(-s.indices)]
  s.curated.data <- s.curated.data[labeledTesting]
  # ?? s.curated.data <- s.curated.data[-s.indices]
  
  testset.sorted <- data.frame(read.xlsx(paste(wd, "/lda-lsa/ldaresults_test_set_sorted_curatedafg_100.xlsx", sep=""), sheetIndex = 1, header = TRUE))
  colnames(testset.sorted) <- c("topic", "lda")
  #print(testset.sorted)
  testset.sorted$topic <- as.numeric(as.character(testset.sorted$topic))
  testset.sorted$lda <- as.numeric(as.character(testset.sorted$lda))
  #testset.sorted
  testset.resorted <- testset.sorted[order(testset.sorted$topic),]
  #print(testset.resorted)
  #print(c(1:100)[-s.indices])
  
  jdm.testset <- matrix(data=0.0, nrow=5, ncol=5)
  rownames(jdm.testset) <- c("cur1", "cur2", "cur3", "cur4", "cur5")
  colnames(jdm.testset) <- c("lda1", "lda2", "lda3", "lda4", "lda5")
  
  for (i in 1:10) {
    jdm.testset[s.curated.data[i],testset.resorted[i,]$lda] = jdm.testset[s.curated.data[i],testset.resorted[i,]$lda] + 1
  }
  
  for (i in 1:5) {
    for (j in 1:5) {
      jdm.testset[i,j] = jdm.testset[i, j] / 10
    }
  }
  #jdm.testset
  
  #CRN for speed image.plot(t(jdm.testset), graphics.reset = TRUE)
  
  # Calculate mutual information for LDA
  marginals.testset <- as.matrix(apply(jdm.testset, 1, sum)) %*% apply(jdm.testset, 2, sum) 
  sum(jdm.testset * log2(jdm.testset/marginals.testset), na.rm = TRUE)
  
  mutual.info[s] <- sum(jdm.testset * log2(jdm.testset/marginals.testset), na.rm = TRUE)
  
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
  
} # end for loop

d.average <- document.mutual.info / document.count
d.average[is.na(d.average)] <- 0


print("DONE")