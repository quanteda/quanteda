# attempt to improve sentence syllable count efficiency

library(quanteda)

#if i don't declare the encoding like this nchar fails("invalid multibyte string)
Encoding(sentences$sentence_text) <- "UTF-8"

countSyllables <- function(sourceText) {
  # load the RData file but only if not already loaded!
  # note that data() defaults to .Globalenv
  if (!exists(as.character(substitute(counts)))) {
    data(syllableCounts)
    print("loaded: syllableCounts")
  }
  # clean the string, change to uppercase for syllable dictionary match
  string <- gsub("[[:punct:][:digit:]]", "", sourceText)
  string <- gsub("\n", "", string)
  string <- toupper(string)
  words <- unlist(strsplit(string, " "))
  # lookup the syllables in the words found in the dictionary
  # uses vectorization and named vector indexing - not looping!
  n.syllables <- counts[words]
  # name the syllable count vector with the words
  names(n.syllables) <- words
  # count the syllables in each word?
  vowel.count.lookup <- sapply(words, function(l) sum((attr(gregexpr("[AEIOUY]*", l)[[1]], "match.length"))!=0))
  # replace the un-looked-up words with vowel formula words
  n.syllables[is.na(n.syllables)] <- 
    vowel.count.lookup[is.na(n.syllables)]
  return(sum(n.syllables))
}

rm(counts)
system.time(syllable_counts <- sapply(sentences$sentence_text[1:1000], countSyllables, USE.NAMES=FALSE))
system.time(syllable_counts <- sapply(sentences$sentence_text, countSyllables, USE.NAMES=FALSE))

