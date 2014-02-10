library(quanteda)
neg_texts <- getTextDir("~/Dropbox/QUANTESS/corpora/movieReviews/smaller/neg/")
pos_texts <- getTextDir("~/Dropbox/QUANTESS/corpora/movieReviews/smaller/pos/")
texts <- c(neg_texts,pos_texts)
vals <-vector()
vals[1:1000] <- "neg"
vals[1001:2000] <- "pos"
atts <- data.frame(vals)
names(atts)<-c("label")
movies <- createCorpus(texts, attribs=atts)

contexts <- kwic(movies, "great")

dfm <- dfm(movies, group="label")