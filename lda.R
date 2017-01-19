# Written by Cody Crawford, 2016
# Base code taken from
# http://www.rtexttools.com/blog/getting-started-with-latent-dirichlet-allocation-using-rtexttools-topicmodels

#random comment

# Import library files
library(RTextTools)
library(topicmodels)  
library(ggplot2)
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

# Read in documents 
wd <- getwd()
testdata <- apply(read.table(paste(wd, "/lda-lsa/curatedafg_100_summary.csv", sep=""), header=FALSE, sep=","), 2, as.character)
testdata <- unname(testdata)

testmatrix <- create_matrix(testdata, language="english", removeNumbers=TRUE, stemWords=TRUE)
testmatrix

iterations <- 10
mutual.info <- vector(mode = "numeric", length = iterations)
terms <- list()

for (s in 1:iterations) {
  
# get random sample of 90 from the 100
s.indices <- sample(100,90)
s.indices

s.data <- testdata[c(s.indices)]
s.matrix <- create_matrix(s.data, language="english", removeNumbers=TRUE, stemWords=TRUE)

# perform lda with 5 topics and 10 starts
k <- 5
lda <- LDA(s.matrix, k, control = list(estimate.alpha = TRUE, alpha = 10, seed = seq(1, 10), nstart = 10)) 
#str(lda)

terms(lda)

#str(topics(lda))

# Split documents into topics and list in two rows
data.lda <- topics(lda)
#data.lda

library(xlsx)
write.xlsx(data.lda, file = paste(wd, "/lda-lsa/ldaresults_curatedafg_100_5.xlsx", sep=""))

preselected_topics <- read.xlsx(paste(wd, "/lda-lsa/preselected_topics.xlsx", sep=""), sheetIndex = 1, header = FALSE)
#preselected_topics 

assn.perm <- sort(preselected_topics$X1[s.indices], index.return=TRUE)$ix
#assn.perm

lda.sorted <- cluster.sort(data.lda[assn.perm])
#lda.sorted

write.xlsx(lda.sorted, file = paste(wd, "/lda-lsa/ldaresults_sorted_curatedafg_100.xlsx", sep=""))


# Joint distribution matrix 

curated.data <- data.frame(read.xlsx(paste(wd, "/lda-lsa/preselected_topics.xlsx", sep=""), sheetIndex = 1, header = FALSE))
colnames(curated.data) <- c("curated")
#curated.data

lda.data <- data.frame(read.xlsx(paste(wd, "/lda-lsa/ldaresults_sorted_curatedafg_100.xlsx", sep=""), sheetIndex = 1, header = TRUE))
colnames(lda.data) <- c("topic", "lda")
#lda.data
lda.data$topic <- as.numeric(as.character(lda.data$topic))
lda.data$lda <- as.numeric(as.character(lda.data$lda))
#lda.data
lda.data.sorted <- lda.data[order(lda.data$topic),]
#lda.data.sorted

jdm.lda <- matrix(data=0.0, nrow=5, ncol=5)
rownames(jdm.lda) <- c("cur1", "cur2", "cur3", "cur4", "cur5")
colnames(jdm.lda) <- c("lda1", "lda2", "lda3", "lda4", "lda5")

for (i in 1:100) {
  jdm.lda[curated.data[i,],lda.data.sorted[i,]$lda] = jdm.lda[curated.data[i,],lda.data.sorted[i,]$lda] + 1
}

for (i in 1:5) {
  for (j in 1:5) {
    jdm.lda[i,j] = jdm.lda[i, j] / 100
  }
}
#jdm.lda

image.plot(t(jdm.lda), graphics.reset = TRUE)

# Calculate mutual information for LDA
marginals.lda <- as.matrix(apply(jdm.lda, 1, sum)) %*% apply(jdm.lda, 2, sum) 
sum(jdm.lda * log2(jdm.lda/marginals.lda), na.rm = TRUE)

mutual.info[s] <- sum(jdm.lda * log2(jdm.lda/marginals.lda), na.rm = TRUE)
terms[[s]] <- (terms(lda))

} # end for loop


print("DONE")