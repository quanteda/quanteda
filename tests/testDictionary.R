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
populismDfm <- dfm(corpus, dictionary=dictionaryPopulismEN)
populismDtm <- dfm(corpus)
# the total token counts are different...
summary(corpus)
(tokens.dfm <- rowSums(populismDtm))
# these token counts match those from rowSums(dfm(corpus))
data.frame(populismDfm, percentage=populismDfm$populism / rowSums(populismDfm) * 100)

# compare totals with Paul's results: total tokens
tokens.python <- c(30362, 11439, 17641, 
                   13350, 29284, 21477, 
                   25582, 7762, 24339, 16271, 8985)
par(mfrow=c(1,2))
plot(tokens.dfm, tokens.python,
     xlab="quanteda dfm()", ylab="Python", main="total tokens")
abline(a=0, b=1, col="red")

# compare totals with Paul's results: dictionary counts
populism.python <- c(10, 6, 10, 
                     20, 16, 17, 
                     67, 7, 17, 17, 16)
plot(populismDfm$populism, populism.python,
     xlab="quanteda dfm()", ylab="Python", main="dictionary counts")
abline(a=0, b=1, col="red")



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



## test the import of WordStat dictionaries
LGdict <- readWStatDict("~/Dropbox/QUANTESS/wordstatTest/dictionaries/LaverGarry.cat")


