## test the dictionary function added to dfm()
##
## KB

library(quanteda)
load(url("http://www.kenbenoit.net/files/ukmanifestos.Rdata"))
#ukmanifestos$attribs$text <- enc2utf8(ukmanifestos$attribs$text)

# use the populism dictionary from 
dictionaryPopulismEN <- 
    list(populism=c("elit*", "consensus*", "undemocratic*", "referend*",
                    "corrupt*", "propagand", "politici*", "*deceit*",
                    "*deceiv*", "*betray*", "shame*", "scandal*", "truth*",
                    "dishonest*", "establishm*", "ruling*"))
corpus <- subset(ukmanifestos, (year %in% c(1992, 2001, 2005) & (party %in% c("Lab", "LD", "Con", "BNP", "UKIP"))))
summary(corpus)
populismDfm <- dfm(corpus, dictionary=dictionaryPopulismEN)
data.frame(populismDfm, percentage=populismDfm$populism / rowSums(populismDfm) * 100)

## NOTE: The warnings are because of the character encodings in the ukmanifesto texts, not 
##       because of anything in the routine.  The grep facility is working great!
