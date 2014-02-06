#' Returns a count of the number of syllables in the input

#' This function takes a text and returns a count of the number of syllables it contains.
#' For British English words, the syllable count is exact and looked up from the CMU
#' pronunciation dictionary. For any word not in the dictionary the syllable count
#' is estimated by counting vowel clusters.
#' 
#' @param sourceText Text to be counted
#' @return numeric A count (estimate) of the number of syllables in sourceText
#' @export
#' @examples
#' countSyllables("This is an example sentence.")
countSyllables <- function(sourceText){
  #load the RData file
  data(sylCounts)
  #clean the string
  string <- clean(sourceText)
  words <- tokenize(string)
  print(words)
  # lookup the syllables in the words found in the dictionary
  # uses vectorization and named vector indexing - not looping!
  n.syllables <- counts[words]
  print(n.syllables)
  # name the syllable count vector with the words
  names(n.syllables) <- words
  # count the syllables in each word?
  vowel.count.lookup <- sapply(words, function(l) sum((attr(gregexpr("[AEIOUY]*", l)[[1]], "match.length"))!=0))
  # replace the un-looked-up words with vowel formula words
  n.syllables[is.na(n.syllables)] <- 
    vowel.count.lookup[is.na(n.syllables)]
  return(sum(n.syllables))
}
