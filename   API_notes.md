# Notes on function renaming

## Function descriptions

 function | notes | in | out
:---------|:------|:---|:---
`collocations` | finds sequences in text that are statistically unlikely | tokens | collocations
`kwic` | words and context | tokens, text, corpus | kwic (df)
`lexdiv` | lexical diversity scores | dfm | data.frame
`readability` | reading level stats | sentences (dfm, text) | data.frame
`similarity`  | compute distance or similarity | dfm |  (similarity) matrix
`scrabble` | lookup scrabble values | character (vector), tokens | integer
`syllables` | lookup syllables | character (vector), tokens | integer
`topfeatures` | show top occuring features | dfm | (named) integer
`docfreq` | document frequency of features | dfm | (named) integer

Perhaps collocations should first form ngrams and then score them against other ngrams?


## For plots:

*  `textplot_{wordcloud,xray,positions}`,   
*  `textmodel_{NB,wordfish,worscores,ca}`



# To Do List:

*  implement new tokens object
*  `fcm()`
*  redo `collocations`
*  
