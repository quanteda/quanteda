# a script to build the data objects that are included in quanteda

library(quanteda)
inaugTexts <- getTextDir('~/Dropbox/QUANTESS/corpora/inaugural')
save(inaugTexts, file=".//data//inaugTexts.RData")
inaugCorpus <- iconv(inaugTexts, from="latin1", to="ASCII")

d <- directory('~/Dropbox/QUANTESS/corpora/inaugural')

inaugCorpus <- corpus(d, docvarsfrom ="filenames", docvarnames = c("Year", "President"), sep = "-")

x <- dfm(inaugCorpus)

save(inaugCorpus, file = ".//data//inaugCorpus.RData")
