library(quanteda)
path <- '~/Dropbox/QUANTESS/corpora/LoughranMcDonald.cat'
load(url("http://www.kenbenoit.net/files/ukmanifestos.Rdata"))

finSentDict <- readWStatDict(path)

corpus <- subset(ukmanifestos, (year %in% c(1992, 2001, 2005) & (party %in% c("Lab", "LD", "Con", "BNP", "UKIP"))))

path <- '~/Dropbox/QUANTESS/corpora/LoughranMcDonald.cat'
d <- read.delim(path, header=FALSE)
thismajorcat <- d[1,1]
d <- d[-1,]
for (i in 1:nrow(d)) {
  if (d[i,1] == "") {
    d[i,1] <- thismajorcat
  } else {
    thismajorcat <- d[i,1]
    #    d <- d[-i,]
  }
}



financeDfm <- dfm(corpus, dictionary=finSentDict)
simpleDfm <- dfm(corpus)
# the total token counts are different...
summary(corpus)
(tokensDfm <- rowSums(simpleDfm))
# these token counts match those from rowSums(dfm(corpus))
data.frame(financeDfm, percentage=financeDfm$populism / rowSums(financeDfm) * 100)
