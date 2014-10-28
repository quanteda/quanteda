### Interesting results:
### - works poorly without stopword removal
### - works poorly for separation when nfeat much more than 200
###


# two dfms on the same feature set, same texts
inaugPost1980Corpus <- subset(inaugCorpus, Year>1980)
docvars(inaugPost1980Corpus, "Party") <- c(rep("Rep",3), rep("Dem",2), rep("Rep",2), rep("Dem",2))
presnames <- docvars(inaugPost1980Corpus, "President")
presnames[c(3, 6, 7)] <- c("BushSr", "BushJr", "BushJr")
inaugDfmParty <- (sort(dfm(inaugPost1980Corpus, groups="Party", stopwords=TRUE)))
inaugDfmPres  <- (sort(dfm(inaugPost1980Corpus, stopwords=TRUE)))
inaugDfmPartyWithSW <- (sort(dfm(inaugPost1980Corpus, groups="Party", stopwords=NULL)))
inaugDfmPresWithSW  <- (sort(dfm(inaugPost1980Corpus, stopwords=NULL)))

# fix number of top features to select
nfeat <- nfeature(inaugDfmParty)

# fit a wordscores model
inaugWordscores <- fitmodel(as.dfm((inaugDfmParty[,1:nfeat])), train=c(1,2), train.scores=c(-1,1), smooth=.5)
inaugTextScores <- predict(inaugWordscores, inaugDfmPres[,1:nfeat])
inaugWordscoresWithSW <- fitmodel(as.dfm(inaugDfmPartyWithSW[,1:nfeat]), train=c(1,2), train.scores=c(-1,1), smooth=.5)
inaugTextScoresWithSW <- predict(inaugWordscoresWithSW, inaugDfmPresWithSW[,1:nfeat])

cor(inaugTextScores, inaugTextScoresWithSW)

# fit a lasso model
library(glmnet)
inaugLassoParty <- glmnet(inaugDfmParty[,1:nfeat], c(-1, 1), alpha=1, standardize=FALSE)
inaugLassoPredict <- predict(inaugLassoParty, inaugDfmPres[,1:nfeat])
apply(inaugLassoPredict[,2:ncol(inaugLassoPredict)], 2, cor, inaugTextScores$textscore_raw)

# fit a ridge regression model
inaugRidgeParty <- glmnet(tfidf(as.dfm(inaugDfmParty[,1:nfeat])), c(-1, 1), alpha=0, standardize=FALSE)
inaugRidgePredict <- predict(inaugRidgeParty, tfidf(as.dfm(inaugDfmPres[,1:nfeat])))
inaugRidgePartyWithSW <- glmnet(tfidf(as.dfm(inaugDfmPartyWithSW[,1:nfeat])), c(-1, 1), alpha=0, standardize=FALSE)
inaugRidgePredictWithSW <- predict(inaugRidgePartyWithSW, tfidf(as.dfm(inaugDfmPresWithSW[,1:nfeat])))

apply(inaugRidgePredict, 2, cor, inaugTextScores$textscore_raw)

s <- 75
plot(inaugTextScores$textscore_raw, inaugRidgePredict[,s], pch=19,
     col = ifelse(docvars(inaugPost1980Corpus, "Party")=="Dem", "blue", "red"),
     xlab = "Wordscores \"text score\"", ylab="Ridge regression prediction",
     main = "Comparing Wordscores to Ridge Regression")
text(inaugTextScores$textscore_raw, inaugRidgePredict[,s], presnames, 
     cex=.8, pos=ifelse(docvars(inaugPost1980Corpus, "Party")=="Dem", 4, 2))
abline(lm(inaugRidgePredict[,s] ~ inaugTextScores$textscore_raw), lty="dashed", col="grey60")
text(-.11, 0, paste("r =", round(cor(inaugTextScores$textscore_raw, inaugRidgePredict[,s]), 2)))

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(data.frame(WS=inaugTextScores$textscore_raw, 
                 WSwithSW=inaugTextScoresWithSW$textscore_raw,
                 RR=inaugRidgePredict[,s],
                 RRwithSW=inaugRidgePredictWithSW[,s]),
      col = ifelse(docvars(inaugPost1980Corpus, "Party")=="Dem", "blue", "red"),
      pch=19, upper.panel = panel.cor)
