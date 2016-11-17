library(xlsx)
library(fields)

curated.data <- data.frame(read.xlsx("/Users/feignedpoet/Documents/lda-lsa/preselected_topics.xlsx", sheetIndex = 1, header = FALSE))
colnames(curated.data) <- c("curated")
curated.data

lda.data <- data.frame(read.xlsx("/Users/feignedpoet/Documents/lda-lsa/ldaresults_sorted_curatedafg_100_1.xlsx", sheetIndex = 1, header = TRUE))
colnames(lda.data) <- c("topic", "lda")
lda.data
lda.data$topic <- as.numeric(as.character(lda.data$topic))
lda.data$lda <- as.numeric(as.character(lda.data$lda))
lda.data
lda.data.sorted <- lda.data[order(lda.data$topic),]
lda.data.sorted

lsa.data <- data.frame(read.xlsx("/Users/feignedpoet/Documents/lda-lsa/lsaresults_sorted_curatedafg_100_1.xlsx", sheetIndex = 1, header = TRUE))
colnames(lsa.data) <- c("topic", "lsa")
lsa.data$topic <- as.numeric(lsa.data$topic)
lsa.data$lsa <- as.numeric(lsa.data$lsa)
lsa.data.sorted <- lsa.data[order(lsa.data$topic),]
lsa.data.sorted

jdm.lda <- matrix(data=0.0, nrow=5, ncol=5)
colnames(jdm.lda) <- c("cur1", "cur2", "cur3", "cur4", "cur5")
rownames(jdm.lda) <- c("lda1", "lda2", "lda3", "lda4", "lda5")

for (i in 1:100) {
  jdm.lda[curated.data[i,],lda.data.sorted[i,]$lda] = jdm.lda[curated.data[i,],lda.data.sorted[i,]$lda] + 1
}

jdm.lsa <- matrix(data=0.0, nrow=5, ncol=5)
colnames(jdm.lsa) <- c("cur1", "cur2", "cur3", "cur4", "cur5")
rownames(jdm.lsa) <- c("lsa1", "lsa2", "lsa3", "lsa4", "lsa5")

for (i in 1:100) {
  jdm.lsa[curated.data[i,],lsa.data.sorted[i,]$lsa] = jdm.lsa[curated.data[i,],lsa.data.sorted[i,]$lsa] + 1
}

for (i in 1:5) {
  for (j in 1:5) {
    jdm.lda[i,j] = jdm.lda[i,j] / 100
    jdm.lsa[i,j] = jdm.lsa[i, j] / 100
  }
}

jdm.lda

jdm.lsa

#image.plot(t(jdm.lda), graphics.reset = TRUE)
image.plot(jdm.lda, graphics.reset = TRUE)

