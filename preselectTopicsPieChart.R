# Pie Chart with Percentages
slices <- c(32, 54, 3, 5, 6) 
lbls <- c("Topic 1 (Direct/Indirect Fire) -", "Topic 2 (Cache/Mine/IED Found) -", "Topic 3 (Explosion/Hostile Action) -", "Topic 4 (Surveillance/Reconnaisance) -", "Topic 5 (Propaganda) -")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=c("blueviolet", "brown1", "cyan2", "dodgerblue1", "chartreuse3"),
    main="Preselected Topics")