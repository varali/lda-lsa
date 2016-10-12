# Code to use Latent Dirichlet Allocation on example documents
# Documents are stored in text.csv and is split into k topics

# Written by Cody Crawford, 2016
# Base code taken from
# http://www.rtexttools.com/blog/getting-started-with-latent-dirichlet-allocation-using-rtexttools-topicmodels

# Import library files
library(RTextTools)
library(topicmodels)
library(ggplot2)

# Read in documents from text.csv
#testdata <- apply(read.table("/Users/cody/Documents/text.csv", header=FALSE, sep=","), 2, as.character)
testdata <- apply(read.table("/Users/cody/Documents/rfiles/curatedafg_100_summary.csv", header=FALSE, sep=","), 2, as.character)
testdata <- unname(testdata)

view <- factor(rep(c("topic 1", "topic 2", "topic 3"), each = 3))
df <- data.frame(testdata, view, stringsAsFactors = FALSE)
df

testmatrix <- create_matrix(testdata, language="english", removeNumbers=TRUE, stemWords=TRUE)

testmatrix


# perform lda with 3 topics and 10 starts
k <- 3
lda <- LDA(testmatrix, k, control = list(seed = seq(1, 10), nstart = 10)) 
str(lda)

terms(lda)

str(topics(lda))

# Split documents into topics and list in two rows
topics(lda)

library(xlsx)
write.xlsx(topics(lda), "/Users/cody/Documents/Thesis/ldaresults_curatedafg_100_2.xlsx")


