plotTitle <- sprintf("LSA Image Plot at MIV %f", mutual.info[s])
image.plot(jdm.testset, graphics.reset = TRUE, xlab="LSA Topic", ylab="Expected Topic", axes=F)
title(main=plotTitle)