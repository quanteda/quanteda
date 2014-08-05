# work in progress for new version of stopwords remover
library(quanteda)

testString <- "In computing, stop words are words which are filtered out prior to,
or after, processing of natural language data (text).[1] There is not one definite 
list of stop words which all tools use and such a filter is not always used. Some 
tools specifically avoid removing them to support phrase search. Any group of words
can be chosen as the stop words for a given purpose. For some search machines, these
are some of the most common, short function words, such as the, is, at, which, and
on. In this case, stop words can cause problems when searching for phrases that
include them, particularly in names such as 'The Who', 'The The', or 'Take That'. 
Other search engines remove some of the most common words—including lexical words, 
such as want—from a query in order to improve performance."


someText <- "Here is an example of text containing some stopwords we want to remove. "
itText <- "Ecco un esempio di testo contenente alcune parole non significative che vogliamo rimuovere."
removeStopwords(someText)
removeStopwords(someText, kind="italian")
removeStopwords(someText, stopwords = c("containing", "example"))


print(removeStopwords(testString))