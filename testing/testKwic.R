library(quanteda)

#source("~/Dropbox/code/quanteda/R/languagetools.R")
#source("~/Dropbox/code/quanteda/R/corpustools.R")




neg_texts <- getTextDir("~/Dropbox/QUANTESS/corpora/movieReviews/smaller/neg/")
pos_texts <- getTextDir("~/Dropbox/QUANTESS/corpora/movieReviews/smaller/pos/")
texts <- c(neg_texts,pos_texts)

text <- "Keanu is really great in this movie, and Arnold is great too."

# called on a text
oneContext <- kwic(text, "great")

vals <-vector()
vals[1:1000] <- "neg"
vals[1001:2000] <- "pos"
atts <- data.frame(vals)
names(atts)<-c("label")
movies <- corpus.create(texts, attribs=atts)


# called on a corpus
allContext <- kwic(movies, "great")