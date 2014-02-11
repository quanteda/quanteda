library(quanteda)
s <- "This is an example of a sentence with some syllables. And acronyms like NASA and non-dictionary words like wojech szczney"
counts <- countSyllables(s, verbose=TRUE)