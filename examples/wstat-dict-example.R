library(quanteda)
load(url("http://www.kenbenoit.net/files/ukmanifestos.Rdata"))

corpus <- subset(ukmanifestos, (year %in% c(1992, 2001, 2005) & (party %in% c("Lab", "LD", "Con", "BNP", "UKIP"))))
path <- '~/Dropbox/QUANTESS/corpora/LaverGarry.cat'

#lgDict <- readWStatDict(path)
d <- read.delim(path, header=FALSE)
d <- data.frame(lapply(d, as.character), stringsAsFactors=FALSE)
thismajorcat <- d[1,1]
# this loop fills in blank cells in the category|term dataframe
for (i in 1:nrow(d)) {
  if (d[i,1] == "") {
    d[i,1] <- thismajorcat
  } else {
    thismajorcat <- d[i,1]
  }
  for(j in 1:(ncol(d)-1)){
    if(d[i,j] == "" & length(d[i-1,j])!=0){
      d[i,j] <- d[i-1,j] 
    }
  }
  if (nchar(d[i,ncol(d)-1]) > 0){
    pat<- c("\\(")
  if( !length(grep(pat,  d[i,ncol(d)-1] ) )==0 ){
    d[i,ncol(d)] <- d[i,ncol(d)-1]
    d[i,ncol(d)-1] <- "_"
  }
  }
}
flatDict <- list()
categ <- list()
# this loop collapses the category cells together and
# makes the list of named lists compatible with dfm
for (i in 1:nrow(d)){
  if( d[i,ncol(d)]=='') next
  categ <- unlist(paste(d[i,(1:(ncol(d)-1))], collapse="."))
  w <- d[i,ncol(d)]
  w <- clean(unlist(strsplit(w, '\\('))[[1]])
  w <- gsub( " ", "", w)
  print(w)
  flatDict[[categ]] <- append(flatDict[[categ]],c(w))
}


popDict <- list(populism=c("elit*", "consensus*", "undemocratic*", "referend*",
                     "corrupt*", "propagand", "politici*", "*deceit*",
                     "*deceiv*", "*betray*", "shame*", "scandal*", "truth*",
                     "dishonest*", "establishm*", "ruling*"), test=c("no"))

popDfm <- dfm(corpus, dictionary=popDict)
lgDfm <- dfm(corpus, dictionary=lgDict)
simpleDfm <- dfm(corpus)
summary(corpus)

