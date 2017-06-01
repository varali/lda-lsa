x_axis = c(10,25,50,75,90)
ldaPreproc = c(0.4048, 0.4082, 0.4115, 0.4728, 0.6644)
ldaPostproc = c(0.6996, 0.9569, 0.8034, 1.0180, 1.0955)

plot(x_axis, ldaPostproc, main="LDA Partial Labeling vs Post-processing", col="cyan3", xlab="Training Slice", ylab="Median MI", ylim=c(0.4, 1.10))
lines(x_axis, ldaPostproc, col="cyan3")
points(x_axis,ldaPreproc, col="blueviolet")
lines(x_axis, ldaPreproc, col="blueviolet")

legend("topleft", cex=0.9, bty="n", legend=c("Post-Processing Step", "Partial Labeling Step"), text.col=c("cyan3", "blueviolet"), pt.bg=c("cyan3", "blueviolet"), pch=c(21,21))