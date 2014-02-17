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
# print the results including relative frequency
data.frame(populismDfm, percentage=populismDfm$populism / rowSums(populismDfm) * 100)

## NOTE: The warnings are because of the character encodings in the ukmanifesto texts, not 
##       because of anything in the routine.  The grep facility is working great!

hdict <- list(level1a = list(level1a1 = c("l1a11", "l1a12"),
                             level1a2 = c("l1a21", "l1a22")),
              level1b = list(level1b1 = c("l1b11", "l1b12"),
                             level1b2 = c("l1b21", "l1b22", "l1b23")),
              level1c = list(level1c1a = list(level1c1a1 = c("lowest1", "lowest2")),
                             level1c1b = list(level1c1b1 = c("lowestalone"))))
unlist(hdict)
unlist(hdict, recursive=FALSE)
flatten.dictionary(hdict)
flatten.dictionary(dictionaryPopulismEN)

