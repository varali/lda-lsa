x_axis = c(10,25,50,75,90)
ldaPreproc = c(0.4300, 0.4757, 0.5986, 0.7183, 0.8799)
ldaPostproc = c(0.4901, 0.7016, 0.8471, 0.8710, 1.6575)

plot(x_axis, ldaPostproc, main="LDA Analysis vs Post-processing", col="dodgerblue1", xlab="Training Slice", ylab="Median MI", ylim=c(0.4200,1.66))
lines(x_axis, ldaPostproc, col="dodgerblue1")
points(x_axis,ldaPreproc, col="chartreuse3")
lines(x_axis, ldaPreproc, col="chartreuse3")

legend("topleft", cex=0.9, bty="n", legend=c("Post-Processing Step", "Statistical Analysis Step"), text.col=c("dodgerblue1", "chartreuse3"), pt.bg=c("dodgerblue1", "chartreuse3"), pch=c(21,21))