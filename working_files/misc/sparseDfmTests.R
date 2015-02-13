rm(list=ls())
library(quanteda)

## build the dfms
## Washington2 + Lincoln2 - both short
dfmDense  <- dfm(inaugTexts[c(2, 21)], matrixType="dense", verbose=FALSE)
dfmSparse <- dfm(inaugTexts[c(2, 21)], matrixType="sparse", verbose=FALSE)
dfmWeighted <- weight(dfmSparse, smooth=1)

# t() -- PASS
## returns matrix or dg[eC]Matrix but that's ok since no longer a proper dfm
t(dfmDense)[1:15,]
t(dfmSparse)[1:15,]
t(dfmWeighted)[1:15,]  

## docnames.dfm* -- PASSED
docnames(dfmDense)
docnames(dfmSparse)  
docnames(dfmWeighted)  

## features.dfm* -- PASSED
features(dfmDense)[1:20]
features(dfmSparse)[1:20] 
features(dfmWeighted)[1:20] 

## ndoc.dfm* -- PASSED
ndoc(dfmDense)
ndoc(dfmSparse)
ndoc(dfmWeighted)

## nfeature.dfm* -- PASSED
nfeature(dfmDense)
nfeature(dfmSparse)
nfeature(dfmWeighted)

## [ for dense -- PASSED
dfmDense[1:2, 1:5] 
dfmDense[, 1:5]
dfmDense[1:2, ]
dfmDense[1, 1:5]
dfmDense[1, 1:5, drop=TRUE]
dfmDense[, "the"]
## [ for sparse -- PASSED
dfmSparse[1:2, 1:5] 
dfmSparse[, 1:5]  
dfmSparse[1:2, ]  
dfmSparse[1, 1:5]
dfmSparse[1, 1:5, drop=TRUE]
dfmSparse[1:2, "the"]
## [ for weighted -- PASSED
dfmWeighted[1:2, 1:5] 
dfmWeighted[, 1:5]  
dfmWeighted[1:2, ]  
dfmWeighted[1, 1:5]
dfmWeighted[1, 1:5, drop=TRUE]
dfmWeighted[1:2, "the"]

# rowSums, colSums -- PASSED
rowSums(dfmDense)
rowSums(dfmSparse) 
rowSums(dfmWeighted)   
colSums(dfmDense)[1:10]
colSums(dfmSparse)[1:10]
colSums(dfmWeighted)[1:10] 
colSums(dfmDense[, 1:10])
colSums(dfmSparse[, 1:10])
colSums(dfmWeighted[, 1:10]) 
apply(dfmSparse[, 1:5], 1, sum)
apply(dfmSparse[, 1:5], 1, mean)
apply(dfmWeighted[, 1:5], 1, mean)  

## topfeatures.dfm -- PASSED
topfeatures(dfmDense)
topfeatures(dfmSparse)
topfeatures(dfmWeighted)

## sort.dfm* -- PASSED
sort(dfmDense)[,1:10]
sort(dfmSparse)[,1:10] 
sort(dfmWeighted)[,1:10] 

## statLexdiv.dfm* -- PASSED
lexdiv(dfmDense)   ### works but warning message
lexdiv(dfmDense, "C")   ### works but warning message
lexdiv(dfmSparse)  
lexdiv(dfmSparse, "C")  
# statLexdiv(dfmWeighted)  
# statLexdiv(dfmWeighted, "CTTR")  

## print.dfm -- PASSES
print(dfmDense) 
print(dfmDense[,1:5])  
print(dfmSparse)        
print(dfmSparse[, 1:5]) 
print(dfmWeighted)        
print(dfmWeighted[, 1:5]) 
## show  
dfmDense[, 1:10]
dfmSparse[, 1:10]
dfmWeighted[, 1:10]

## stopwordsRemove.dfm*  -- PASSED
topfeatures(stopwordsRemove(dfmDense, stopwordsGet("english")))
topfeatures(stopwordsRemove(dfmSparse, stopwordsGet("english"))) 
topfeatures(stopwordsRemove(dfmWeighted, stopwordsGet("english"))) 

## plot.dfm  -- PASSED
plot(stopwordsRemove(dfmDense, stopwordsGet("SMART")), random.order=FALSE)
plot(stopwordsRemove(dfmSparse, stopwordsGet("SMART")), random.order=FALSE)
identical(rowSums(stopwordsRemove(dfmDense, stopwordsGet("SMART"))),
          rowSums(stopwordsRemove(dfmSparse, stopwordsGet("SMART"))))
plot(stopwordsRemove(dfmWeighted, stopwordsGet("SMART")), random.order=FALSE)


## weight
dfmDense[, 1:10]
identical(weight(dfmDense, "tfidf"), tfidf(dfmDense))  ## DIFFERENT
# run through options
objects <- c("dfmDense", "dfmSparse", "dfmWeighted")
weightTypes <- c("relFreq", "relMaxFreq", "logFreq", "tfidf")
for (w in weightTypes) {
    cat("\n ==============", w, "\n")
    for (o in objects) {
        cat("\n ------->", o, "\n")
        print(weight(eval(parse(text=o)), w)[, 1:10])
    }
}


## textmodel.dfm  -- FAILS ON PRINT/PREDICT
## wordscores
data(ie2010Corpus, package="quantedaData")
ieDfmDense <- dfm(ie2010Corpus)
ieDfmSparse <- dfm(ie2010Corpus, matrixType="sparse")
refscores <- c(rep(NA, 4), -1, 1, rep(NA, 8))
ws <- textmodel_wordscores(ieDfmDense, refscores, smooth=1)  
wsp <- predict(ws, rescaling="mv")
wsp  # FAILS
wsS <- textmodel_wordscores(ieDfmSparse, refscores, smooth=1)
wspS <- predict(ws, rescaling="mv")  # FAILS
wspS  # FAILS





## wordfish

## topicmodels: LDA
## topicmodels: stm

## trim()



##  is.resampled.dfm*  ### NOT IMPLEMENTED YET
##  nresample.dfm*     ### NOT IMPLEMENTED YET
##  settings.dfm*      ### NOT IMPLEMENTED YET



## bug in Matrix package??
testMat <- as(matrix(1:20, nrow=4), "dgeMatrix")
str(testMat)
testMat[,]
testMat[, 1:3]
testMat[1:2, ]
testMat[1:2, 1:3]  # FAILS
testMat[1:2, 1:3, drop=TRUE]  # PASSES
colnames(testMat) <- paste0("col", 1:5)
testMat
testMat[1:2, "col3", drop=FALSE]
testMat[1:2, "col3", drop=TRUE]
testMat[1:2, "col3"]  # FAILS
testMat[1:2, c("col1", "col3"), drop=FALSE]
testMat[1:2, c("col1", "col3"), drop=TRUE]
