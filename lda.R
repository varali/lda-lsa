# Written by Cody Crawford, 2016
# Base code taken from
# http://www.rtexttools.com/blog/getting-started-with-latent-dirichlet-allocation-using-rtexttools-topicmodels

# Import library files
library(RTextTools)
library(topicmodels)  
library(ggplot2)
library(xlsx)

# Read in documents 
wd <- getwd()
testdata <- apply(read.table(paste(wd, "/lda-lsa/curatedafg_100_summary.csv", sep=""), header=FALSE, sep=","), 2, as.character)
testdata <- unname(testdata)

view <- factor(rep(c("topic 1", "topic 2", "topic 3", "topic 4", "topic 5"), each = 20))
df <- data.frame(testdata, view, stringsAsFactors = FALSE)
df

testmatrix <- create_matrix(testdata, language="english", removeNumbers=TRUE, stemWords=TRUE)
testmatrix

# perform lda with 5 topics and 10 starts
k <- 5
lda <- LDA(testmatrix, k, control = list(seed = seq(1, 10), nstart = 10)) 
str(lda)

terms(lda)

str(topics(lda))

# Split documents into topics and list in two rows
data.lda <- topics(lda)
data.lda

library(xlsx)
write.xlsx(data.lda, file = paste(wd, "/lda-lsa/ldaresults_curatedafg_100_5.xlsx", sep=""))

preselected_topics <- read.xlsx(paste(wd, "/lda-lsa/preselected_topics.xlsx", sep=""), sheetIndex = 1, header = FALSE)
preselected_topics 

assn.sorted <- sort(preselected_topics$X1)
assn.sorted

assn.perm <- sort(preselected_topics$X1, index.return=TRUE)$ix
assn.perm

lda.sorted <- cluster.sort(data.lda[assn.perm])
lda.sorted

write.xlsx(lda.sorted, file = paste(wd, "/lda-lsa/ldaresults_sorted_curatedafg_100.xlsx", sep=""))