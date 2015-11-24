## ----eval=FALSE----------------------------------------------------------
#  install.packages("quanteda", dependencies = TRUE)

## ----eval=TRUE-----------------------------------------------------------
require(quanteda)

# read the text as a single file
# mobydicktf <- textfile("http://www.gutenberg.org/cache/epub/2701/pg2701.txt")
mobydicktf <- textfile("~/Dropbox/QUANTESS/corpora/project_gutenberg/pg2701.txt")
mobydicktf

## ------------------------------------------------------------------------
substring(texts(mobydicktf), 1, 75)

## ------------------------------------------------------------------------
# extract the header information
mobydickText <- texts(mobydicktf)
endMetadataIndex <- regexec("CHAPTER 1. Loomings.", mobydickText)[[1]]
metadata.v <- substring(texts(mobydicktf), 1, endMetadataIndex - 1)

# verify that "orphan" is the end of the novel
kwic(mobydickText, "orphan")

# extract the novel -- a better way
novel.v <- substring(mobydickText, endMetadataIndex, 
                     regexec("End of Project Gutenberg's Moby Dick.", mobydickText)[[1]]-1)

## ------------------------------------------------------------------------
# lowercase
novel.lower.v <- toLower(novel.v)
# tokenize
moby.word.v <- tokenize(novel.lower.v, removePunct = TRUE, simplify = TRUE)
str(moby.word.v)
moby.word.v[1:10]
moby.word.v[99986]  # different because we removed punctiation

moby.word.v[c(4,5,6)]

head(which(moby.word.v=="whale"))

## ------------------------------------------------------------------------
length(moby.word.v[which(moby.word.v=="whale")])
length(moby.word.v)
ntoken(novel.v, removePunct = TRUE)

whale.hits.v <- length(moby.word.v[which(moby.word.v=="whale")])

# Put a count of total words into total.words.v 
total.words.v <- length(moby.word.v)
# now divide
whale.hits.v/total.words.v

# total unique words
length(unique(moby.word.v))
ntype(toLower(novel.v), removePunct = TRUE)

## ----eval=TRUE-----------------------------------------------------------
# ten most frequent words
mobyDfm <- dfm(novel.lower.v)
mobyDfm[, "whale"]

topfeatures(mobyDfm)
plot(topfeatures(mobyDfm, 100), log = "y", cex = .6)

# whale:token ratio
length(which(moby.word.v == "whale")) / ntoken(mobyDfm)

## ----eval=TRUE-----------------------------------------------------------
# frequencies of 'he' and 'she' - these are matrixes, not numerics
mobyDfm[, c("he", "she", "him", "her")]

mobyDfm[, "him"]/mobyDfm[, "her"]
mobyDfm[, "he"]/mobyDfm[, "she"]

## ------------------------------------------------------------------------
mobyDfmPct <- weight(mobyDfm, "relFreq") * 100
mobyDfmPct[, "the"]

plot(topfeatures(mobyDfmPct), type="b",
     xlab="Top Ten Words", ylab="Percentage of Full Text", xaxt ="n")
axis(1, 1:10, labels = names(topfeatures(mobyDfmPct)))

## ----eval=TRUE, fig.width=8, fig.height=2--------------------------------
# using words from tokenized corpus for dispersion
kwic_whale <- kwic(novel.v, "whale")
plot(kwic_whale)

## ----eval = FALSE--------------------------------------------------------
#  require('ggplot2')
#  qplot(whales, geom="histogram", binwidth=1000)
#  qplot(whales, geom="density", adjust=0.1)

## ----eval=FALSE----------------------------------------------------------
#  kwic(mobyCorp, 'chapter')
#  chapters <- segment(mobyCorp, what='other', delimiter="CHAPTER\\s\\d", perl=TRUE)
#  chapDfm <- dfm(chapters)
#  barplot(as.numeric(chapDfm[, 'whale']))
#  barplot(as.numeric(chapDfm[, 'ahab']))
#  

## ----eval=TRUE-----------------------------------------------------------
#ttr <- statLexdiv(chapDfm)
#lens <- length(rowSums(chapDfm))


