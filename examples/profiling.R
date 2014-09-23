
library(quanteda)

d <- getDirectory("~/Dropbox/QUANTESS/corpora/ukManRenamed" )
newCorp1 <- corpus(d, attribs="filenames", attNames=c('country','type','year','language', 'party'))
texts(newCorp1)<- iconv(texts(newCorp1), from="latin1", to="UTF-8")

#profiling tokenize and clean
tmp <- tempfile()
Rprof(tmp, interval = 0.001)

test <- tokenize(texts(newCorp1), clean=FALSE)
#test2 <- tokenizeSingle2(texts(newCorp1))

Rprof(NULL)
summaryRprof(tmp)



#profiling kwic
tmp <- tempfile()
Rprof(tmp, interval = 0.001)

res <- kwic(newCorp1, "immigration", 10)

res2 <- kwicNew(newCorp1, "immigration", 10)

Rprof(NULL)
summaryRprof(tmp)





#profiling typical use case and kwic
tmp <- tempfile()
Rprof(tmp, interval = 0.001)

d <- getDirectory("~/Dropbox/QUANTESS/corpora/ukManRenamed" )
newCorp1 <- corpus(d, attribs="filenames", attNames=c('country','type','year','language', 'party'))
texts(newCorp1)<- iconv(texts(newCorp1), from="latin1", to="UTF-8")
newCorp1 <- clean(newCorp1)
newCorp1 <- tokenize(newCorp1, clean=FALSE)
d <- dfm(newCorp1)
res <- kwic(newCorp1, "immigration", 10)
Rprof(NULL)
summaryRprof(tmp)
