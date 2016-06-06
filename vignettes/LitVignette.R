## ----eval=FALSE----------------------------------------------------------
#  install.packages("quanteda", dependencies = TRUE)

## ----eval=TRUE-----------------------------------------------------------
require(quanteda)

# read the text as a single file
# alternative:
# mobydickText <- texts(textfile("http://www.gutenberg.org/cache/epub/2701/pg2701.txt"))
summary(mobydickText)

## ------------------------------------------------------------------------
substring(mobydickText, 1, 75)

## ------------------------------------------------------------------------
# extract the header information
endMetadataIndex <- regexec("CHAPTER 1. Loomings.", mobydickText)[[1]]
metadata.v <- substring(mobydickText, 1, endMetadataIndex - 1)

## ------------------------------------------------------------------------
# verify that "orphan" is the end of the novel
kwic(mobydickText, "orphan")

# extract the novel -- a better way
novel.v <- substring(mobydickText, endMetadataIndex, 
                     regexec("End of Project Gutenberg's Moby Dick.", mobydickText)[[1]]-1)

## ------------------------------------------------------------------------
# lowercase
novel.lower.v <- toLower(novel.v)

## ------------------------------------------------------------------------
# tokenize
moby.word.v <- tokenize(novel.lower.v, removePunct = TRUE, simplify = TRUE)
length(moby.word.v)
total.length <- length(moby.word.v)
str(moby.word.v)
moby.word.v[1:10]
moby.word.v[99986] 

moby.word.v[c(4,5,6)]

head(which(moby.word.v=="whale"))

## ------------------------------------------------------------------------
moby.word.v <- tokenize(novel.lower.v, simplify = TRUE)
# count of the word 'whale'
length(moby.word.v[which(moby.word.v == "whale")])

# total occurrences of 'whale' including possessive
length(moby.word.v[which(moby.word.v == "whale")]) + length(moby.word.v[which(moby.word.v == "whale's")])
# same thing using kwic()
nrow(kwic(novel.lower.v, "whale"))
nrow(kwic(novel.lower.v, "whale*")) # includes words like 'whalemen'
(total.whale.hits <- nrow(kwic(novel.lower.v, "^whale('s){0,1}$", valuetype = 'regex')))

## ------------------------------------------------------------------------
total.whale.hits / ntoken(novel.lower.v, removePunct=TRUE)  

## ------------------------------------------------------------------------
# total unique words
length(unique(moby.word.v))
ntype(toLower(novel.v), removePunct = TRUE)

## ----eval=TRUE-----------------------------------------------------------
# ten most frequent words
mobyDfm <- dfm(novel.lower.v)
mobyDfm[, "whale"]

topfeatures(mobyDfm)
plot(topfeatures(mobyDfm, 100), log = "y", cex = .6, ylab = "Term frequency")

## ----eval=TRUE-----------------------------------------------------------
# frequencies of 'he' and 'she' - these are matrixes, not numerics
mobyDfm[, c("he", "she", "him", "her")]
mobyDfm[, "her"]
mobyDfm[, "him"]/mobyDfm[, "her"]
mobyDfm[, "he"]/mobyDfm[, "she"]

## ------------------------------------------------------------------------
mobyDfmPct <- weight(mobyDfm, "relFreq") * 100
mobyDfmPct[, "the"]

plot(topfeatures(mobyDfmPct), type="b",
     xlab="Top Ten Words", ylab="Percentage of Full Text", xaxt ="n")
axis(1, 1:10, labels = names(topfeatures(mobyDfmPct)))

## ----eval=TRUE, fig.width=8, fig.height=1.5------------------------------
# using words from tokenized corpus for dispersion
plot(kwic(novel.v, "whale"))

## ----eval=TRUE, fig.width=8, fig.height=2.5------------------------------
plot(
     kwic(novel.v, "whale"),
     kwic(novel.v, "Ahab"),
     kwic(novel.v, "Pequod")
)

## ----eval = FALSE--------------------------------------------------------
#  # identify the chapter break locations
#  (chap.positions.v <- kwic(novel.v, "CHAPTER \\d", valuetype = "regex")$position)

## ------------------------------------------------------------------------
head(kwic(novel.v, 'chapter'))
chaptersVec <-unlist(segment(novel.v, what='other', delimiter="CHAPTER\\s\\d", perl=TRUE))
chaptersLowerVec <- toLower(chaptersVec)
chaptersCorp <- corpus(chaptersVec)

## ----eval=TRUE-----------------------------------------------------------
chapDfm <- dfm(chaptersCorp)
barplot(as.numeric(chapDfm[, 'whale']))
barplot(as.numeric(chapDfm[, 'ahab']))

## ----eval=TRUE-----------------------------------------------------------
relDfm <- weight(chapDfm, type='relFreq') * 100
head(relDfm)
barplot(as.numeric(relDfm[, 'whale']))
barplot(as.numeric(relDfm[, 'ahab']))

## ------------------------------------------------------------------------
wf <- as.numeric(relDfm[,'whale'])
af <- as.numeric(relDfm[,'ahab'])
cor(wf, af)

waDfm <- cbind(relDfm[,'whale'], relDfm[,'ahab'])
cor(as.matrix(waDfm))

## ------------------------------------------------------------------------
samples <- replicate(1000, cor(sample(af), sample(wf)))

h <- hist(samples, breaks=100, col="grey",
xlab="Correlation Coefficient",
main="Histogram of Random Correlation Coefficients\n
with Normal Curve",
plot=T)
xfit <- seq(min(samples),max(samples),length=1000)
yfit <- dnorm(xfit,mean=mean(samples),sd=sd(samples))
yfit <- yfit*diff(h$mids[1:2])*length(samples)
lines(xfit, yfit, col="black", lwd=2)

cor.test(wf, af)


## ------------------------------------------------------------------------
firstChap <- as.matrix(chapDfm[1,])
numWords <- length(firstChap[firstChap > 0])
sum(chapDfm[1,])/numWords
sum(chapDfm[1,])/ntype(chaptersCorp[1], removePunct=TRUE)

## ------------------------------------------------------------------------
chapMeans <- Matrix::rowMeans(chapDfm)
plot(chapMeans, type="h")

