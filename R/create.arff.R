create.arff <-
function(fvm, gold, name="politics", outfile="test.arff"){
  outString <- paste("@RELATION", name,"\n")
  
  for(c in rownames(fvm)){
    outString <- paste(outString, "@attribute", c, "NUMERIC\n")
  }
  outString <- paste(outString, "@attribute class {Economic, Social, Neither}\n")
  outString <- paste(outString, "@DATA\n")
  i<-1
  while(i<length(fc)){
    line <- paste(fc[, i], collapse=",")
    outString <- paste(outString, line)
    outString <- paste(outString, ",")
    outString <- paste(outString, gold[[i]])
    outString <- paste(outString, "\n")
    i <- i+1
    print(i)
  }
  
  writeChar(outString, file("/home/paul/testout.arff"))
}
