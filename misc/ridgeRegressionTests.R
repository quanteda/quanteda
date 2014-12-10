### Interesting results:
### - works poorly without stopword removal
### - works poorly for separation when nfeat much more than 200
###

library(quanteda)

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
inaugWordscores <- textmodel(inaugDfmParty, c(-1, 1), model="wordscores", smooth=.5)
inaugTextScores <- predict(inaugWordscores, inaugDfmPres)
inaugWordscoresWithSW <- textmodel(inaugDfmPartyWithSW, c(-1, 1), model="wordscores", smooth=.5)
inaugTextScoresWithSW <- predict(inaugWordscoresWithSW, inaugDfmPresWithSW)

cor(inaugTextScores, inaugTextScoresWithSW)

# fit a lasso model
library(glmnet)
inaugLassoParty <- glmnet(inaugDfmParty, c(-1, 1), alpha=1, standardize=FALSE)
inaugLassoPredict <- predict(inaugLassoParty, inaugDfmPres)
apply(inaugLassoPredict[,2:ncol(inaugLassoPredict)], 2, cor, inaugTextScores$textscore_raw)

# fit a ridge regression model
inaugRidgeParty <- glmnet(tfidf(as.dfm(inaugDfmParty[,1:nfeat])), c(-1, 1), alpha=0)
inaugRidgePredict <- predict(inaugRidgeParty, tfidf(as.dfm(inaugDfmPres[,1:nfeat])))
inaugRidgePartyWithSW <- glmnet(tfidf(as.dfm(inaugDfmPartyWithSW[,1:nfeat])), c(-1, 1), alpha=0)
inaugRidgePredictWithSW <- predict(inaugRidgePartyWithSW, tfidf(as.dfm(inaugDfmPresWithSW[,1:nfeat])))

apply(inaugRidgePredict, 2, cor, inaugTextScores$textscore_raw)

quartz()
s <- 75
plot(inaugTextScores$textscore_raw, inaugRidgePredictWithSW[,s], pch=19,
     col = ifelse(docvars(inaugPost1980Corpus, "Party")=="Dem", "blue", "red"),
     xlab = "Wordscores \"text score\"", ylab="Ridge regression prediction",
     main = "Comparing Wordscores to Ridge Regression")
text(inaugTextScores$textscore_raw, inaugRidgePredictWithSW[,s], presnames, 
     cex=.8, pos=ifelse(docvars(inaugPost1980Corpus, "Party")=="Dem", 4, 2))
abline(lm(inaugRidgePredictWithSW[,s] ~ inaugTextScores$textscore_raw), lty="dashed", col="grey60")
text(-.11, 0, paste("r =", round(cor(inaugTextScores$textscore_raw, inaugRidgePredictWithSW[,s]), 2)))

s <- 75
plot(inaugTextScores$textscore_raw, inaugLassoPredict[,s], pch=19,
     col = ifelse(docvars(inaugPost1980Corpus, "Party")=="Dem", "blue", "red"),
     xlab = "Wordscores \"text score\"", ylab="Lasso regression prediction",
     main = "Comparing Wordscores to Lasso Regression")
text(inaugTextScores$textscore_raw, inaugLassoPredict[,s], presnames, 
     cex=.8, pos=ifelse(docvars(inaugPost1980Corpus, "Party")=="Dem", 4, 2))
abline(lm(inaugLassoPredict[,s] ~ inaugTextScores$textscore_raw), lty="dashed", col="grey60")
text(-.11, 0, paste("r =", round(cor(inaugTextScores$textscore_raw, inaugLassoPredict[,s]), 2)))



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



### some tests with Lasso logisitic
require(quantedaData)
data(ie2010Corpus)
ieDfm <- dfm(ie2010Corpus)
refscores <- c(rep(NA, 4), -1, 1, rep(NA, 8))
ieWordscores  <- textmodel(ieDfm, refscores, model="wordscores", smooth=1)
ieBayesscores <- textmodel(ieDfm[5:6,], refscores[5:6], model="wordscores", scale="logit", smooth=1)
ieBayesPredict <- predict(ieBayesscores, ieDfm)
require(glmnet)
ieLassoLogistic <- glmnet(tf(ieDfm[5:6,]+1), factor(refscores[5:6]), family="binomial",
                          standardize = FALSE, alpha = 0)
ieLassoLogisticPred <- predict(ieLassoLogistic, ieDfm+1)

ieLassoGaussian <- glmnet(tf(ieDfm[5:6,]+1), refscores[5:6], family="gaussian",
                          standardize = FALSE, alpha = 0)
ieLassoGaussianPred <- predict(ieLassoGaussian, ieDfm+1)
scale(ieLassoGaussianPred[,2])[,1]


lambda <- 2
plot(ieBayesscores$pi, exp(ieLassoLogistic$beta[, lambda]),# type="n",
     xlab = "Bayes Score", ylab="Standardized Logistic Ridge betas")
subtext <- which(scale(ieLassoLogistic$beta[, lambda])[,1] < -10)
points(ieBayesscores$pi[-subtext], scale(ieLassoLogistic$beta[, lambda])[-subtext,1], cex=.75)
text(ieBayesscores$pi[subtext], 
     scale(ieLassoLogistic$beta[, lambda])[subtext,1],
     features(ieDfm)[subtext])

plot(ieWordscores$pi, exp(ieLassoGaussian$beta[, lambda]), 
     xlab = "Word Score", ylab="exp Gaussian Ridge betas")
subtext <- which(exp(ieLassoGaussian$beta[, lambda]) < .97)
points(ieWordscores$pi[-subtext], exp(ieLassoGaussian$beta[-subtext, lambda]), cex=.75)
text(ieWordscores$pi[subtext], 
     exp(ieLassoGaussian$beta[subtext, lambda]),
     features(ieDfm)[subtext])



plot(ieBayesPredict$textscore_raw, exp(ieLassoLogisticPred[,2]),
     xlab="Bayes Text Score", ylab="Logisitic Ridge Prediction")

### some tests with Lasso logisitic
require(quantedaData)
data(LBGexample)
refscores <- c(seq(-1.5, 1.5, .75), NA)
wsLBG <- textmodel(LBGexample, refscores, model="wordscores", smooth=0)
(tsLBG <- predict(wsLBG))
#bsLBG <- textmodel(LBGexample[-6, ], refscores[-6], model="wordscores", scale="logit", smooth=0)


require(glmnet)
ieLassoLBG <- glmnet(tf(LBGexample[-6, ]), refscores[-6], family="gaussian",
                          standardize = FALSE, alpha = 0)
ieLassoLBGPred <- predict(ieLassoLBG, LBGexample)
scale(ieLassoLBGPred[, lambda])[,1]

# the word scores
lambda <- 2
plot(wsLBG$pi, ieLassoLBG$beta[,lambda], type="n",      
     ylab="Ridge Regression Coefficient Estimates",
     xlab="LBG Wordscores",
     main="LBG (2003) Table 1 Example")
text(wsLBG$pi, ieLassoLBG$beta[,lambda], features(LBGexample))


plot(tsLBG$textscore_raw, scale(ieLassoLBGPred[, lambda])[,1],
     xlab="LBG Text Score", ylab="Standardized Gaussian Ridge Prediction", type="n")
text(tsLBG$textscore_raw, scale(ieLassoLBGPred[, lambda])[,1], docnames(LBGexample))
abline(a=0, b=1, col="red")
abline(lm(scale(ieLassoLBGPred[, lambda])[-c(1,6),1] ~ tsLBG$textscore_raw[-c(1,6)]), col="red")



### From thinking in NB's office
toydfm <- matrix(c(0, 2, 4, 0, 2, 
                   1, 0, 0, 5, 3,
                   2, 4, 6, 0, 1), nrow=3, byrow=TRUE)
colnames(toydfm) <- LETTERS[1:5]
rownames(toydfm) <- c("d1", "d2", "d3")
refvals <- c(-10, -5, 5)

for (l in colnames(toydfm)) {
    dfmTemp <- tf(toydfm)
    dfmTemp[which(toydfm[, l] == 0), l] <- NA
    cat(l, "regression coeff (0=NA) =", as.numeric(coef(lm(refvals ~ dfmTemp[, l]))), "\n")
    cat(l, "regression coeff (0=0) =", as.numeric(coef(lm(refvals ~ tf(toydfm)[, l]))), "\n\n")
}

expDfm <- data.frame(Ydoc = rep(rep(rownames(toydfm), 5), as.vector(toydfm)),
                     Y = rep(rep(refvals, 5), as.vector(toydfm)),
                     X = rep(colnames(toydfm), colSums(toydfm)))
expDfm
summary(expDfmFit <- lm(Y ~ X - 1, expDfm))
aggregate(predict(expDfmFit), list(expDfm$Ydoc), mean)


dfm2long <- function(x, refvals) {
    stopifnot(is.dfm(x))
    stopifnot(length(refvals) == ndoc(x))
    result <- data.frame(docname = rep(rep(docnames(x), nfeature(x)), as.vector(x)),
                         Y = rep(rep(refvals, nfeature(x)), as.vector(x)),
                         X = factor(rep(features(x), colSums(x))))
    result
}

dfm2long(as.dfm(toydfm), refvals)


require(quantedaData)
data(LBGexample)
refscores <- c(seq(-1.5, 1.5, .75), NA)
wsLBG <- textmodel(LBGexample, refscores, model="wordscores", smooth=1)
(tsLBG <- predict(wsLBG))
LBGlong <-  dfm2long(LBGexample, refscores)
wsLBGlm <- lm(Y ~ X - 1, data=LBGlong)
summary(wsLBGlm)
(sum(wsLBG$pi - coef(wsLBGlm)) < 10e-14)
cor(wsLBG$pi, coef(wsLBGlm))

wsLBGlmPred <- predict(wsLBGlm, newdata=LBGlong)
round(aggregate(wsLBGlmPred, by=list(LBGlong$docname), mean)[,2], 3)
scale(aggregate(wsLBGlmPred, by=list(LBGlong$docname), mean)[,2])[,1]

## binary version w/R2, R4
wsLBGbin <- textmodel(LBGexample[c(1,5),], c(-1, 1), model="wordscores", smooth=1, scale="logit")
wsLBG <- textmodel(LBGexample[c(1,5),], c(-1, 1), model="wordscores", smooth=1, scale="linear")

(tsLBGbin <- predict(wsLBGbin, LBGexample))
LBGlong <- dfm2long(LBGexample + 1, c(0, NA, NA, NA, 1, NA))
LBGlong$binRef <- ifelse(LBGlong$docname=="R5", 1, ifelse(LBGlong$docname=="R5", -1, NA))
wsLBGlogit <- glm(factor(Y) ~ X - 1, data=LBGlong, family="binomial")

plot(wsLBGbin$pi, coef(wsLBGlogit), type="n")
text(jitter(wsLBGbin$pi, 10), jitter(coef(wsLBGlogit), 10), features(LBGexample))


