# create a first textmatrix with some files
td = tempfile()
dir.create(td)
write( c("dog", "cat", "mouse"), file=paste(td, "D1", sep="/") )
write( c("hamster", "mouse", "sushi"), file=paste(td, "D2", sep="/") )
write( c("dog", "monster", "monster"), file=paste(td, "D3", sep="/") )
matrix1 = textmatrix(td, minWordLength=1)
unlink(td, recursive=TRUE)

# create a second textmatrix with some more files
td = tempfile()
dir.create(td)
write( c("cat", "mouse", "mouse"), file=paste(td, "A1", sep="/") )
write( c("nothing", "mouse", "monster"), file=paste(td, "A2", sep="/") )
write( c("cat", "monster", "monster"), file=paste(td, "A3", sep="/") )
matrix2 = textmatrix(td, vocabulary=rownames(matrix1), minWordLength=1)
unlink(td, recursive=TRUE)

# create an LSA space from matrix1
space1 = lsa(matrix1, dims=dimcalc_share())
as.textmatrix(space1)

# fold matrix2 into the space generated by matrix1
f <- fold_in( matrix2, space1)

rbind(as.textmatrix(space1), f)