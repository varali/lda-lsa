x_axis = c(10,25,50,75,90)
lsaPostproc = c(0.6483, 0.6273, 0.2478, 0.3815, 0.8855)
ldaPostproc = c(0.4901, 0.7016, 0.8471, 0.8710, 1.6575)

plot(x_axis, ldaPostproc, main="LDA Versus LSA Post-Processing", col="dodgerblue1", xlab="Training Slice", ylab="Median MI", ylim=c(0.2477,1.66))
lines(x_axis, ldaPostproc, col="dodgerblue1")
points(x_axis,lsaPostproc, col="chartreuse3")
lines(x_axis, lsaPostproc, col="chartreuse3")

legend("topleft", cex=0.9, bty="n", legend=c("LDA Post-Processing", "LSA Post_Processing"), text.col=c("dodgerblue1", "chartreuse3"), pt.bg=c("dodgerblue1", "chartreuse3"), pch=c(21,21))