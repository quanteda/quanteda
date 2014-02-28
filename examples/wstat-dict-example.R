library(quanteda)
load(url("http://www.kenbenoit.net/files/ukmanifestos.Rdata"))

corpus <- subset(ukmanifestos, (year %in% c(1992, 2001, 2005) & (party %in% c("Lab", "LD", "Con", "BNP", "UKIP"))))
path <- '~/Dropbox/QUANTESS/corpora/LaverGarry.cat'

lgDict <- readWStatDict(path)

popDict <- list(populism=c("elit*", "consensus*", "undemocratic*", "referend*",
                     "corrupt*", "propagand", "politici*", "*deceit*",
                     "*deceiv*", "*betray*", "shame*", "scandal*", "truth*",
                     "dishonest*", "establishm*", "ruling*"), test=c("no"))

popDfm <- dfm(corpus, dictionary=popDict)
lgDfm <- dfm(corpus, dictionary=lgDict)
simpleDfm <- dfm(corpus)
summary(corpus)

