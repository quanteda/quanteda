
sentences <- list("This is the first sentence.",
                  "R is a fantastic general-purpose stastitical package, and costs nothing.",
                  "Sally sells seashells by the sea shore.")

# dictionary is a vector of syllable counts
# this can be generated once from the phonetic dictionary and saved
# as long as the dictionary is in this format, then it can be for any language 
# in the R package it can be saved as an Rdata object, and then loaded using
# data(dictionary.english) within a function when and only when needed.
dictionary <- c(1, 1, 1, 1, 3, 3, 2, 2, 2, 4, 2)
# the words name the elements of each vector
names(dictionary) <- c("This", "is", "the", "first", "fantastic", "general", "purpose",
                       "Sally", "seashells", "statistical", "sentence")
# sort the dictionary vector alphabetically
dictionary <- dictionary[order(names(dictionary))]


count.syllables <- function(sentence, dictionary) {
  words.vector <- scan(what="char", text=sentence, quiet=TRUE)
  # the R way to do it!
  words.syllables.vector <- dictionary[words.vector]
  # but we need a way to flag <NA> values which represent words in the dicitonary
  # maybe export a list to be added to the dictionary? and/or make a guess?
  syllables.total <- sum(words.syllables.vector, na.rm=TRUE)
  return(syllables.total)
}

# demonstrate for one sentence
count.syllables("R is a fantastic general-purpose stastitical package, and costs nothing.",
                dictionary)

# apply to a list and return a vector of syllable counts
sapply(sentences, count.syllables, dictionary)


library(openNLP)
determine.pos <- function(sentence) {
  # clean sentence of punctuation and numbers
  sentence <- gsub("[[:punct:][:digit:]]", "", sentence)
  print(sentence)
  # tage sentence parts of speech
  tagged.sentence <- tagPOS(sentence)
  # tokenize
  tagged.sentence.pos.char.vector <- scan(what="char", text=tagged.sentence, quiet=TRUE)
  # create a list of splits on the / character that precedes POS tags
  strsplit(tagged.sentence.char.vector, "/")
  tagged.sentence.pos.parsedlist <- strsplit(tagged.sentence.pos.char.vector, "/")
  # put the second element of the list into a (factor) vector 
  tagged.sentence.pos.factor.vector <- 
    factor(sapply(tagged.sentence.pos.parsedlist, function(x) x[2]))
  # name the vector with the word
  names(tagged.sentence.pos.factor.vector) <- 
    sapply(tagged.sentence.pos.parsedlist, function(x) x[1])
  # convert to table of POS and return as list
  return(as.list(table(tagged.sentence.pos.factor.vector)))
} 

determine.pos("This is a test sentence for a part of speech tagger")

test <- determine.pos("Quantitative methods for scaling latent political traits have much in
common with algorithms commonly used for applied machine learning
tasks such as email spam detection and product recommender systems.
While the statistical methods used in both domains are similar, the
research goals and philosophical underpinnings are quite different:
machine learning is usually concerned with predicting a class or value
for a variable which is known, or can be verified by human judges,
most often with a practical application in mind.  Political traits
associated with texts are inherently unobservable through direct
means, and solicited expert judgments may be too costly or too
subjective to apply to many measurement tasks.  In this paper we show
that the Naive Bayes classifier, one of the most widely used machine
learning classification algorithms, underlies Laver, Benoit and
Garry's (2003) ``wordscores'' algorithm for measuring policy
positions.  Using the text of party manifestos in addition to
legislative speeches, we apply classification and regression methods
from the machine learning domain to political text scaling problems,
and evaluate the results using both class accuracy and distance from
human judgements as measures of error.  We also discuss in more
general terms how methods for classification and error analysis from
the machine learning domain can be applied to common political
research problems, including the measurement of latent traits and the
prediction of political categories.")
cat(paste("Nouns:", test$NN, "\nVerbs:", test$VBZ))
as.data.frame(test)







