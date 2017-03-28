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


stats <- boxplot.stats(sapply(miv, median))$stats

# Get files that were less than and greater than the median in miv
ltMed <- which(sapply(miv, median) < stats[3])
gtMed <- which(sapply(miv, median) > stats[3])

filesToUse <- gtMed

############## READ IN DATA AGAIN ##############
testdata <- apply(read.table(paste(wd, "/lda-lsa/curatedafg_100_summary.csv", sep=""), header=FALSE, sep=","), 2, as.character)
testdata <- unname(testdata)

testmatrix <- create_matrix(testdata, language="english", removeNumbers=TRUE, stemWords=TRUE)
testmatrix

iterations <- 1
pp.mutual.info <- vector(mode = "numeric", length = iterations)
pp.document.count <- vector(mode = "numeric", length = 100)
pp.document.mutual.info <- vector(mode = "numeric", length = 100)
pp.terms <- list()
pp.miv <- list()
for (i in 1:100) {
  pp.miv[[i]] <- 0
}

############## START LOOP ##############
for (s in 1:iterations) {
  
  s.data <- testdata[c(filesToUse)]
  s.matrix <- create_matrix(s.data, language="english", removeNumbers=TRUE, stemWords=TRUE)
  
  ############## TRAIN LDA ##############
  k <- 5
  lda <- LDA(s.matrix, k, control = list(estimate.alpha = TRUE, alpha = 5, seed = seq(1, 10), nstart = 10)) 
  #str(lda)
  
  terms(lda)
  
  # Split documents into topics and list in two rows
  data.lda <- topics(lda)
  #data.lda
  
  preselected_topics <- read.xlsx(paste(wd, "/lda-lsa/preselected_topics.xlsx", sep=""), sheetIndex = 1, header = FALSE)
  #preselected_topics 
  
  assn.perm <- sort(preselected_topics$X1[filesToUse], index.return=TRUE)$ix
  #assn.perm
  
  lda.sorted <- cluster.sort(data.lda[assn.perm])
  #lda.sorted
  
  curated.data <- data.frame(read.xlsx(paste(wd, "/lda-lsa/preselected_topics.xlsx", sep=""), sheetIndex = 1, header = FALSE))
  colnames(curated.data) <- c("curated")
  
  ############## CREATE TEST SET ##############
  
  s.test.data <- testdata[c(-filesToUse)]
  s.test.matrix <- create_matrix(s.test.data, language="english", removeNumbers=TRUE, stemWords=TRUE)
  lda.testing <- posterior(lda, s.test.matrix)
  test.results <- apply(lda.testing$topics, 1, which.max)
  test.results
  
  s.assn.perm <- sort(preselected_topics$X1[c(-filesToUse)], index.return=TRUE)$ix
  s.lda.sorted <- cluster.sort(test.results[s.assn.perm])
  
  write.xlsx(s.lda.sorted, file = paste(wd, "/lda-lsa/ldaresults_test_set_sorted_curatedafg_100.xlsx", sep=""))
  
  ############## TEST SET JDM ##############
  s.curated.data <- curated.data[,1]
  s.curated.data <- s.curated.data[c(-filesToUse)]
  
  testset.sorted <- data.frame(read.xlsx(paste(wd, "/lda-lsa/ldaresults_test_set_sorted_curatedafg_100.xlsx", sep=""), sheetIndex = 1, header = TRUE))
  colnames(testset.sorted) <- c("topic", "lda")
  #testset.sorted
  testset.sorted$topic <- as.numeric(as.character(testset.sorted$topic))
  testset.sorted$lda <- as.numeric(as.character(testset.sorted$lda))
  #testset.sorted
  testset.resorted <- testset.sorted[order(testset.sorted$topic),]
  #testset.resorted
  
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
  
  marginals.testset <- as.matrix(apply(jdm.testset, 1, sum)) %*% apply(jdm.testset, 2, sum) 
  sum(jdm.testset * log2(jdm.testset/marginals.testset), na.rm = TRUE)
  
  pp.mutual.info[s] <- sum(jdm.testset * log2(jdm.testset/marginals.testset), na.rm = TRUE)
  
  pp.documents.used <- setdiff(1:100, filesToUse)
  for (i in 1:100) {
    if (i %in% pp.documents.used) {
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


