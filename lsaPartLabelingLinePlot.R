x_axis = c(10,25,50,75)
lsaPreproc = c(0.3464, 0.4113, 0.4293, 0.5573)
lsaPostproc = c(0.4131, 0.4309, 0.5679, 0.7660)

plot(x_axis, lsaPostproc, main="LSA Partial Labeling vs Post-processing", col="cyan3", xlab="Training Slice", ylab="Median MI", ylim=c(0.3, 0.80))
lines(x_axis, lsaPostproc, col="cyan3")
points(x_axis,lsaPreproc, col="blueviolet")
lines(x_axis, lsaPreproc, col="blueviolet")

legend("topleft", cex=0.9, bty="n", legend=c("Post-Processing Step", "Partial Labeling Step"), text.col=c("cyan3", "blueviolet"), pt.bg=c("cyan3", "blueviolet"), pch=c(21,21))