# Written by Cody Newbold, 2016
# Base code taken from
# http://www.rtexttools.com/blog/getting-started-with-latent-dirichlet-allocation-using-rtexttools-topicmodels

#random comment

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
iterations <- 1000
mutual.info <- vector(mode = "numeric", length = iterations)
document.count <- vector(mode = "numeric", length = 100)
document.mutual.info <- vector(mode = "numeric", length = 100)
terms <- list()
miv <- list()
for (i in 1:100) {
  miv[[i]] <- 0
}

numTraining <- 90
numTesting <- 100 - numTraining

############## BEGIN LOOP ##############
for (s in 1:iterations) {
  
  # get random sample of 50 from the 100
  s.indices <- sample(100,numTraining)
  s.indices

  s.data <- testdata[c(s.indices)]
  s.matrix <- create_matrix(s.data, language="english", removeNumbers=TRUE, stemWords=TRUE)


  ############## TRAIN LDA ##############
  k <- 5
  lda <- LDA(s.matrix, k, control = list(estimate.alpha = TRUE, alpha = 5, seed = seq(1, 10), nstart = 10)) 
  #str(lda)

  print(terms(lda))

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

  ############## CREATE TEST SET ##############
  
  #print(testdata)
  s.test.data <- testdata[c(-s.indices)]
  s.test.matrix <- create_matrix(s.test.data, language="english", removeNumbers=TRUE, stemWords=TRUE)
  lda.testing <- posterior(lda, s.test.matrix)
  test.results <- apply(lda.testing$topics, 1, which.max)
  #print(test.results)

  s.assn.perm <- sort(preselected_topics$X1[c(-s.indices)], index.return=TRUE)$ix
  s.lda.sorted <- cluster.sort(test.results[s.assn.perm])
  #print(s.lda.sorted)
  write.xlsx(s.lda.sorted, file = paste(wd, "/lda-lsa/ldaresults_test_set_sorted_curatedafg_100.xlsx", sep=""))

  ############## TEST SET JDM ##############
  s.curated.data <- curated.data[,1]
  s.curated.data <- s.curated.data[c(-s.indices)]

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

  for (i in 1:numTesting) {
    jdm.testset[s.curated.data[i],testset.resorted[i,]$lda] = jdm.testset[s.curated.data[i],testset.resorted[i,]$lda] + 1
  }

  print(jdm.testset)
  
  for (i in 1:5) {
    for (j in 1:5) {
      jdm.testset[i,j] = jdm.testset[i, j] / numTesting
    }
  }
  
  print(jdm.testset)

  

  # Calculate mutual information for LDA
  marginals.testset <- as.matrix(apply(jdm.testset, 1, sum)) %*% apply(jdm.testset, 2, sum) 
  sum(jdm.testset * log2(jdm.testset/marginals.testset), na.rm = TRUE)

  mutual.info[s] <- sum(jdm.testset * log2(jdm.testset/marginals.testset), na.rm = TRUE)
  
  plotTitle <- sprintf("LDA Image Plot at MIV %f", mutual.info[s])
  image.plot(t(jdm.testset), graphics.reset = TRUE, xlab="LDA Topic", ylab="Expected Topic", axes=F)
  title(main=plotTitle)
  
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
  

} # end for loop

d.average <- document.mutual.info / document.count
d.average[is.na(d.average)] <- 0

#stats <- boxplot.stats(sapply(miv, median))$stats
#ltMed <- which(sapply(miv, median) < stats[3])
#gtMed <- which(sapply(miv, median) > stats[3])



print("DONE")