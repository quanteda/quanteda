# temp testing code for sentence seg
library(quanteda)
library(RCurl)
library(RJSONIO)

texts <- getTextDir('~/Dropbox/QUANTESS/corpora/translationTests')

texts <- iconv(texts, from="latin1", to="ASCII", sub="byte")

#segmenting multiple texts
sents <- lapply(texts, sentenceSeg)



#translating the 1st text into Romanian
resps <- lapply(sents[[1]],translate, sourceLanguage='EN', targetLanguage='RO', key='AIzaSyDbQj08rNx4i2LAjXPYf5AD4TdfWr1ZEtM', verbose=TRUE)

