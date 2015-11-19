## ----eval=TRUE-----------------------------------------------------------
require(quanteda)
text <- scan("http://www.gutenberg.org/cache/epub/2701/pg2701.txt",
what="character", sep="\n")
start  <- which(text == "CHAPTER 1. Loomings.")
end  <- which(text == "orphan.")
body <- text[start:end]
body[1]
mobyWords <- unlist(strsplit(text, "\\W"))
which(mobyWords=="whale")

## ----eval=TRUE-----------------------------------------------------------
require(stringi)

flatText <- stri_flatten(body)


# ten most frequent words
mobyCorp <- corpus(flatText)
mobyDfm <- dfm(mobyCorp)

# type and token frequencies
ntype(mobyCorp) / ntoken(mobyCorp)






mobyWords <- unlist(tokenize(mobyCorp))

# whale:token ratio
length(which(mobyWords == "whale")) / ntoken(mobyCorp)


## ----eval=TRUE-----------------------------------------------------------

# frequencies of 'he' and 'she' - these are matrixes, not numerics
mobyDfm[,'he']
mobyDfm[,'she']

# relative frequencies:
mobyDfm <- weight(mobyDfm, type='relFreq')
mobyDfm[,'he']
mobyDfm[,'she']

topfeatures(mobyDfm, n=10)
plot(topfeatures(mobyDfm, n=10))


## ----eval=TRUE-----------------------------------------------------------
# using words from tokenized corpus for dispersion
mobyWords <- unlist(tokenize(mobyCorp))
nTime <- seq(1:length(mobyWords))
whales <- which(mobyWords == "whale")
wcount <- rep(NA,length(nTime))
wcount[whales] <- 1

# dispersion plot as in book.
plot(wcount, main="Dispersion Plot of `whale' in Moby Dick",
xlab="Novel Time", ylab="whale", type="h", ylim=c(0,1), yaxt='n')

