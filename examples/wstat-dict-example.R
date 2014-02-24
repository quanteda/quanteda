library(quanteda)
path <- '~/Dropbox/QUANTESS/corpora/LoughranMcDonald.cat'
load(url("http://www.kenbenoit.net/files/ukmanifestos.Rdata"))

finSentDict <- readWStatDict(path)

corpus <- subset(ukmanifestos, (year %in% c(1992, 2001, 2005) & (party %in% c("Lab", "LD", "Con", "BNP", "UKIP"))))

financeDfm <- dfm(corpus, dictionary=finSentDict)
simpleDfm <- dfm(corpus)
# the total token counts are different...
summary(corpus)
(tokensDfm <- rowSums(simpleDfm))
# these token counts match those from rowSums(dfm(corpus))
data.frame(financeDfm, percentage=financeDfm$populism / rowSums(financeDfm) * 100)
