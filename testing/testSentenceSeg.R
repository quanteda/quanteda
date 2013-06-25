# temp testing code for sentence seg
library(quanteda)
library(RCurl)
library(RJSONIO)

<<<<<<< HEAD
texts <- getTextDir('~/Dropbox/QUANTESS/corpora/translationTests')

texts <- iconv(texts, from="latin1", to="ASCII", sub="byte")

#segmenting multiple texts
sents <- lapply(texts, sentenceSeg)



#translating the 1st text into Romanian
resps <- lapply(sents[[1]],translate, sourceLanguage='EN', targetLanguage='RO', key='AIzaSyDbQj08rNx4i2LAjXPYf5AD4TdfWr1ZEtM', verbose=TRUE)
=======
strings <- getTextDir('~/Dropbox/QUANTESS/corpora/UK Manifestos')
strings <- iconv(strings, from="latin1", to="ASCII", sub="byte")
corpus.ukmanifs <- corpus.create(strings, attribs=names(strings))
summary(corpus.ukmanifs)
>>>>>>> ecdda30c3b9a0007887e69ebde2f5cfa14603121

