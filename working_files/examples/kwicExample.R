library(quanteda)

neg_texts <- getTextDir("~/Dropbox/QUANTESS/corpora/movieReviews/smaller/neg/")
pos_texts <- getTextDir("~/Dropbox/QUANTESS/corpora/movieReviews/smaller/pos/")
texts <- c(neg_texts,pos_texts)
vals <-vector()
vals[1:1000] <- "neg"
vals[1001:2000] <- "pos"
atts <- data.frame(vals)
names(atts)<-c("label")
movies <- corpusCreate(texts, attribs=atts)

text <- "Keanu is really excellent in this movie, and Arnold is great too."

# called on a text
oneContext <- kwic(text, "great", window=2)

# called on a corpus
allContext <- kwic(movies, "great", window=8)