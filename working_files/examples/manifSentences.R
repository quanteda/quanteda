library(quanteda)
library(quantedaData)


sents <- read.csv('~/Dropbox/QUANTESS/Manuscripts/Collocations/Corpora/sentence_level_dataCMP.csv')

# remove sentences that mention the party name or other party name
sents <- sents[sents$mentionThisPartyThisSentence==0,]
sents <- sents[sents$mentionOtherPartyThisSentence==0,]
txts <- as.character(sents$sentence_text)
atts <- sents[,4:44]

# make scalar measures from ordinal annotations
#theres probably a better way to do this
econCodes <- atts[,4:8]
atts$econMeans <- apply(econCodes,1, function(x) sum(c((x[1]*-2),(x[2]*-1),x[3],x[4],x[5]*2))/sum(x))

sentsCorp <- corpus(txts, docvars=atts, enc='UTF-8')
econCorp <- subset(sentsCorp, propEcon>0.66) # 3949 sentences remain
econDfm <- dfm(econCorp)

dim(econDfm)
class(econDfm)
econDfmTf <- weight(econDfm, type='tfidf') #error here


# can't reproduce the error with inaugTexts
inaugDfm <- dfm(inaugTexts)
dim(inaugDfm)
class(inaugDfm)
nfeature(inaugDfm)
inaugDfmTf <- weight(inaugDfm, type='tfidf')
