# Grouped Bar Plot
LDA_Difference <- c(0.06, 0.22, 0.24, 0.16, 0.77)
LSA_Difference <- c(0.03, 0.08, 0.005, 0.02, 0.35)
counts <- rbind(LDA_Difference, LSA_Difference)
barplot(counts, main="LDA Versus LSA in Difference Between Phases",
        xlab="Difference in MI", col=c("dodgerblue1","chartreuse3"),
        beside=TRUE)
legend("topleft", cex=0.9, bty="n", legend=c("LDA Difference", "LSA Difference"), text.col=c("dodgerblue1", "chartreuse3"), pt.bg=c("dodgerblue1", "chartreuse3"), pch=c(21,21))