source("/home/paul/Dropbox/code/quanteda/R/quanteda.R")
dictPath <- "~/Dropbox/QUANTESS/corpora/cmudict.0.7a.txt"

print(countSyllables("This is a test sentence for a very pythonic R algorithm", dictPath))