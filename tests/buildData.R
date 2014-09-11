# a script to build the data objects that are included in quanteda

library(quanteda)
inaugTexts <- getTextDir('~/Dropbox/QUANTESS/corpora/inaugural')
save(inaugTexts, file=".//data//inaugTexts.RData")

inaugCorpus <- corpus(inaugTexts)
save(inaugCorpus, file=".//data//inaugCorpus.RData")
