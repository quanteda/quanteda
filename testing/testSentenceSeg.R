# temp testing code for sentence seg
library(quanteda)
library(RCurl)
library(XML)
#string <- paste(readLines('/home/paul/Dropbox/QUANTESS/corpora/UK Manifestos/UK_natl_1997_en_Lab.txt'), collapse="\n")
strings <- getTextDir('/home/paul/Dropbox/QUANTESS/corpora/UK Manifestos')

strings <- getTextDir('~/Dropbox/QUANTESS/corpora/UK Manifestos')
strings <- iconv(strings, from="latin1", to="ASCII", sub="byte")
corpus.ukmanifs <- corpus.create(strings, attribs=names(strings))
summary(corpus.ukmanifs)

res <- sentenceSeg(strings[11:11])
ch <- translateChunk('this is a test',  'EN', 'RO', key='AIzaSyDbQj08rNx4i2LAjXPYf5AD4TdfWr1ZEtM', verbose=TRUE)
trans <- translate('this is a test',  'EN', 'RO', key='AIzaSyDbQj08rNx4i2LAjXPYf5AD4TdfWr1ZEtM', verbose=TRUE)
#print('line 5')
corpus.create(res)
