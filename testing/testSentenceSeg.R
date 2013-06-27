# temp testing code for sentence seg
library(quanteda)
library(RCurl)
library(RJSONIO)



strings <- getTextDir('~/Dropbox/QUANTESS/corpora/UK Manifestos')
strings <- iconv(strings[51:60], from="latin1", to="ASCII", sub="byte")
corpus <- corpus.create(strings)
result <- corpus.reshape(corpus)
df <- result$attribs
resps <- lapply(df$texts[1:3],translate, sourceLanguage='EN', targetLanguage='RO', key='AIzaSyDbQj08rNx4i2LAjXPYf5AD4TdfWr1ZEtM', verbose=TRUE)
