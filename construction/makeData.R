# a script to build the data objects that are included in quanteda

library(quanteda)
inaugTexts <- getTextDir('~/Dropbox/QUANTESS/corpora/inaugural')
names(inaugTexts) <- gsub("\\.txt", "", names(inaugTexts))
save(inaugTexts, file=".//data//inaugTexts.RData")
inaugCorpus <- iconv(inaugTexts, from="latin1", to="ASCII")
d <- directory('~/Dropbox/QUANTESS/corpora/inaugural')
note <-  "These texts have been cleaned so that they are pure ASCII, but the encoding flag is set to UTF-8."
inaugCorpus <- corpus(d, docvarsfrom ="filenames", docvarnames = c("year", "president"), sep = "-", notes=note)
language(inaugCorpus) <- "english"
encoding(inaugCorpus) <- "UTF-8"
save(inaugCorpus, file = ".//data//inaugCorpus.RData")
