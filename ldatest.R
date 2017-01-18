#LDA test from help("LDA")

#data("AssociatedPress", package = "topicmodels")
testdata <- apply(read.table(paste(wd, "/lda-lsa/dummy5_100_summary.csv", sep=""), header=FALSE, sep=","), 2, as.character)
testdata <- unname(testdata)
testmatrix <- create_matrix(testdata, language="english", removeNumbers=TRUE, stemWords=TRUE)
testmatrix

#lda <- LDA(testmatrix[1:90,], control = list(alpha = 0.1), k = 5)
lda <- LDA(testmatrix[1:90,], control = list(alpha = 50, seed = seq(1, 10), nstart = 10), k = 5)
lda_inf <- posterior(lda, testmatrix[91:100,])

tdata.lda.training <- topics(lda)
tdata.lda.training

tdata.lda.testing <- apply(lda_inf$topics, 1, which.max)
tdata.lda.testing

# Use the sample function
# sample(5,2)