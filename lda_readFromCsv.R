# Code to use Latent Dirichlet Allocation on example documents
# Documents are stored in text.csv and is split into k topics

# Written by Cody Crawford, 2016
# Base code taken from
# http://www.rtexttools.com/blog/getting-started-with-latent-dirichlet-allocation-using-rtexttools-topicmodels

# Import library files
library(RTextTools)
library(topicmodels)  
library(ggplot2)
library(xlsx)

# Read in documents from text.csv
#testdata <- apply(read.table("/Users/cody/Documents/text.csv", header=FALSE, sep=","), 2, as.character)
testdata <- apply(read.table("/Users/cody/Documents/lda-lsa/curatedafg_100_summary.csv", header=FALSE, sep=","), 2, as.character)
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

data.sorted <- sort(data.lda)
data.sorted

data.res <- cluster.sort(data.sorted)
data.res

library(xlsx)
write.xlsx(data.lda, file = "/Users/cody/Documents/lda-lsa/ldaresults_curatedafg_100_5.xlsx")

preselected_topics <- read.xlsx("/Users/cody/Documents/lda-lsa/preselected_topics.xlsx", sheetIndex = 1, header = FALSE)
preselected_topics 

assn.sorted <- sort(preselected_topics$X1)
assn.sorted

assn.perm <- sort(preselected_topics$X1, index.return=TRUE)$ix
assn.perm

lda.sorted <- cluster.sort(data.lda[assn.perm])
lda.sorted

write.xlsx(lda.sorted, file = "/Users/cody/Documents/lda-lsa/ldaresults_sorted_curatedafg_100_1.xlsx")

median(which(lda.sorted == 1))
median(which(lda.sorted == 2))
median(which(lda.sorted == 3))
median(which(lda.sorted == 4))
median(which(lda.sorted == 5))