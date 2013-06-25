# temp testing code for sentence seg
library(quanteda)
#string <- paste(readLines('/home/paul/Dropbox/QUANTESS/corpora/UK Manifestos/UK_natl_1997_en_Lab.txt'), collapse="\n")
strings <- getTextDir('/home/paul/Dropbox/QUANTESS/corpora/UK Manifestos')
strings <- iconv(strings, from="latin1", to="ASCII", sub="byte")
res <- sentenceSeg(strings)
corpus.create(res)
