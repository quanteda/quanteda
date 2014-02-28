library(quanteda)

load(url("http://www.kenbenoit.net/files/ukmanifestos.Rdata"))

corpus <- subset(ukmanifestos, (year %in% c(1992, 2001, 2005) & (party %in% c("Lab", "LD", "Con", "BNP", "UKIP"))))
path <- '~/Dropbox/QUANTESS/corpora/LaverGarry.cat'
d <- read.delim(path, header=FALSE)

d <- data.frame(lapply(d, as.character), stringsAsFactors=FALSE)

newd <- data.frame()
thismajorcat <- d[1,1]
thisminorcat <- d[1,2]
for (i in 1:nrow(d)) {
  if (d[i,1] == "") {
    d[i,1] <- thismajorcat
  } else {
    thismajorcat <- d[i,1]
  }
  
}
View(d)

x <- list(populism=c("elit*", "consensus*", "undemocratic*", "referend*",
                     "corrupt*", "propagand", "politici*", "*deceit*",
                     "*deceiv*", "*betray*", "shame*", "scandal*", "truth*",
                     "dishonest*", "establishm*", "ruling*"), test=c("no"))

financeDfm <- dfm(corpus, dictionary=d)
simpleDfm <- dfm(corpus)
# the total token counts are different...
summary(corpus)
(tokensDfm <- rowSums(simpleDfm))
# these token counts match those from rowSums(dfm(corpus))
data.frame(financeDfm, percentage=financeDfm$populism / rowSums(financeDfm) * 100)
