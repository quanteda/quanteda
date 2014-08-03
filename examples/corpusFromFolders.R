# work in progress for creating attributes for corpus from
# folder names


library(quanteda)


attName = "att1"
path = '/home/paul/Desktop/docs'
dirs <- list.files(path, include.dirs=TRUE, full.names=TRUE)


tempC <- NULL
for (d in dirs){
  print(d)
  v<-NULL
  vals<-NULL
  tmp <- getTextDir(d)
  vals <- rep(d, length(tmp))
  if (is.null(tempC)){
   tempC<- corpusCreate(tmp)
   tempC<- corpusAddAttributes(tempC, vals, name="newv")
  }else{
    #v<-data.frame(vals)
    names(vals)<- rep('newv', length(vals))
    tempC <- corpusAppend(tempC,tmp, vals)
  }
  
}
