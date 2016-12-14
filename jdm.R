# Written by Cody Crawford, 2016

# Import library files
library(xlsx)
library(fields)

wd <- getwd()

curated.data <- data.frame(read.xlsx(paste(wd, "/lda-lsa/preselected_topics.xlsx", sep=""), sheetIndex = 1, header = FALSE))
colnames(curated.data) <- c("curated")
curated.data

lda.data <- data.frame(read.xlsx(paste(wd, "/lda-lsa/ldaresults_sorted_curatedafg_100.xlsx", sep=""), sheetIndex = 1, header = TRUE))
colnames(lda.data) <- c("topic", "lda")
lda.data
lda.data$topic <- as.numeric(as.character(lda.data$topic))
lda.data$lda <- as.numeric(as.character(lda.data$lda))
lda.data
lda.data.sorted <- lda.data[order(lda.data$topic),]
lda.data.sorted

lsa.data <- data.frame(read.xlsx(paste(wd, "/lda-lsa/lsaresults_sorted_curatedafg_100.xlsx", sep=""), sheetIndex = 1, header = TRUE))
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
    jdm.lda[i,j] = jdm.lda[i, j] / 100
    jdm.lsa[i,j] = jdm.lsa[i, j] / 100
  }
}

jdm.lda

jdm.lsa

image.plot(t(jdm.lda), graphics.reset = TRUE)
#image.plot(jdm.lsa, graphics.reset = TRUE)

# Calculate mutual information for LDA
marginals.lda <- as.matrix(apply(jdm.lda, 1, sum)) %*% apply(jdm.lda, 2, sum) 
sum(jdm.lda * log2(jdm.lda/marginals.lda), na.rm = TRUE)

# Calculate mutual information for LSA
marginals.lsa <- as.matrix(apply(jdm.lsa, 1, sum)) %*% apply(jdm.lsa, 2, sum) 
sum(jdm.lsa * log2(jdm.lsa/marginals.lsa), na.rm = TRUE)



