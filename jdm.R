curated.data <- data.frame(read.xlsx("/Users/cody/Documents/lda-lsa/preselected_topics.xlsx", sheetIndex = 1, header = FALSE))
colnames(curated.data) <- c("curated")
curated.data

lda.data <- data.frame(read.xlsx("/Users/cody/Documents/lda-lsa/ldaresults_sorted_curatedafg_100_1.xlsx", sheetIndex = 1, header = TRUE))
colnames(lda.data) <- c("topic", "lda")
lda.data

lsa.data <- data.frame(read.xlsx("/Users/cody/Documents/lda-lsa/lsaresults_sorted_curatedafg_100_1.xlsx", sheetIndex = 1, header = TRUE))
colnames(lsa.data) <- c("topic", "lsa")
lsa.data

