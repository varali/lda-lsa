x_axis = c(10,25,50,75,90)
lsaPreproc = c(0.6121, 0.5489, 0.2417, 0.3639, 0.5420)
lsaPostproc = c(0.6483, 0.6273, 0.2479, 0.3816, 0.8855)

plot(x_axis, lsaPostproc, main="LSA Analysis vs Post-processing", col="blue", xlab="Training Slice", ylab="Median MI")
lines(x_axis, lsaPostproc, col="blue")
points(x_axis,lsaPreproc, col="chartreuse3")
lines(x_axis, lsaPreproc, col="chartreuse3")

legend("topleft", cex=0.9, bty="n", legend=c("Post-Processing Step", "Statistical Analysis Step"), text.col=c("blue", "chartreuse3"), pt.bg=c("blue", "chartreuse3"), pch=c(21,21))