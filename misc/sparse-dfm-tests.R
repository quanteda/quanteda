
## build the dfms
## Washington2 + Lincoln2 - both short
dfmDense  <- dfm(inaugTexts[c(2, 21)], matrixType="dense") 
dfmSparse <- dfm(inaugTexts[c(2, 21)], matrixType="sparse")

## topfeatures.dfm* -- PASSED
topfeatures(dfmDense)
topfeatures(dfmSparse)

## [.dfm* -- PASSED
dfmDense[, 1:5]
dfmSparse[, 1:5]
rowSums(dfmDense[, 1:5])
rowSums(dfmSparse[, 1:5])  # but lacks labels
apply(dfmSparse[, 1:5], 1, sum) # include labels
apply(dfmSparse[, 1:5], 1, mean)

## docnames.dfm*
docnames(dfmDense)
docnames(dfmSparse)  ### FAILS

## features.dfm*
features(dfmDense)[1:20]
features(dfmSparse)[1:20]  ### FAILS

## ndoc.dfm*
ndoc(dfmDense)
ndoc(dfmSparse)  ### FAILS

## nfeature.dfm*
nfeature(dfmDense)
nfeature(dfmSparse)  ### FAILS

## plot.dfm*
plot(dfmDense)
plot(dfmSparse)  ### NOT A WORDCLOUD

## print.dfm          
print(dfmDense)
print(dfmSparse)  ### NOT A WORDCLOUD

## sort.dfm*
sort(dfmDense)[,1:10]
sort(dfmSparse)[,1:10]  ### FAILS

## statLexdiv.dfm*
statLexdiv(dfmDense)   ### works but warning message
statLexdiv(dfmSparse)  ### FAILS

## stopwordsRemove.dfm*
topfeatures(stopwordsRemove(dfmDense))
stopwordsRemove(dfmSparse)  ### FAILS

## tfidf.dfm*  AND  weight - AND weight needs fixing in general!!
(dfmDense)[,1:10]
identical(weight(dfmDense, "tfidf"), tfidf(dfmDense))
weight(dfmDense)[,1:10]
weight(dfmSparse)[,1:10]

## textmodel.dfm
## wordscores
data(ie2010Corpus, package="quantedaData")
ieDfmDense <- dfm(ie2010Corpus)
ieDfmSparse <- dfm(ie2010Corpus, matrixType="sparse")
refscores <- c(rep(NA, 4), -1, 1, rep(NA, 8))
ws <- textmodel(ieDfmDense, refscores, model="wordscores", smooth=1)
predict(ws, ieDfmDense, rescaling="mv")
ws <- textmodel(ieDfmSparse, refscores, model="wordscores", smooth=1) ## FAILS
predict(ws, ieDfmSparse, rescaling="mv")

## wordfish

## topicmodels: LDA
## topicmodels: stm





##  is.resampled.dfm*  ### NOT IMPLEMENTED YET
##  nresample.dfm*     ### NOT IMPLEMENTED YET
##  settings.dfm*      ### NOT IMPLEMENTED YET
