# Suggested API changes

## Motivations for the change

The package started to get a bit haphazard in terms of names and functionality, defeating some of the purposes for which it was designed (to be simple to use and intuitive).  In addition, renaming and reorganizing makes it easier:

1.  To comply more with [standards recommended by ROpenSci](https://github.com/ropensci/onboarding/blob/master/packaging_guide.md#funvar), the funcitons and their operation have been renamed.  
2.  To interface more easily with other packages, such as **tokenizers**, to do some of the lower-level work without reinventing it.


## Function name changes

The following table maps the new functions to the old, and identifies the input object classes for which methods are defined, and identifies the object type produced:

### Package-level

new name | original name | methods | output object | keyword
:--------|:------------- |:------- |:------------- |:-------
`quanteda-package` | - | - | - | -

This will be a help page describing the package, its development, its contributors, and future plans.  It will also detail the classes and provide links to the class definitions, which will be otherwise removed from the manual index by attaching the `internal`
keywords to them.

### Data Objects

new name | original name | notes | keyword
:--------|:------------- |:----- |:-------
`data_char_encodedtexts` | `encodedTexts` | | data
`data_char_inaug` | `inaugTexts` |  | data
`data_char_sampletext` | `exampleString` |   | data
`data_char_ukimmigration` | `ukimmigTexts` |   | data
`data_corpus_ie2010` | `ie2010Corpus` |   | data
`data_corpus_inaug` | `inaugCorpus` |   | data
`data_dfm_LBGexample` | `LBGexample` |  | data
`data_int_syllables` | `englishSyllables` |  (used by `syllables`) | internal
`data_list_char_wordlists` | `wordlists` |  (used by `readability()`) | internal
`data_list_char_stopwords` | `.stopwords` | (used by `stopwords()`) | internal


### R functions

These are fucntions in the core R package for which methods have been written 

new name | original name | methods | output object | keyword
:--------|:------------- |:------- |:------------- |:-------
`as.data.frame` | - | dfm | data.frame | ?
`cbind` | - | dfm | dfm | ?
`rbind` | - | dfm | dfm | ?
`plot`  | - | dfm | (plot) | ?
`plot`  | - | kwic | (plot) | ?
`print` | - | dfm | (printed output) | internal
`print` | - | tokenizedTexts | (printed output) | internal
`print` | - | tokenSequences | (printed output) | internal
`print` | - | kwic | (printed output) | internal
`print` | - | settings | (printed output) | internal
`print` | - | similMatrix | (printed output) | internal
`print` | - | tokenSequences | (printed output) | internal
`show` | - | dictioary | (printed output) | internal
`sample` | - | corpus | corpus | ?
`sample` | - | dfm | dfm | ?
`sort` | - | dfm | dfm | ?
`subset` | - | corpus | corpus | ?
`summary` | - | corpus | (invisible) data.frame | ?
`head` | - | dfm | dfm | ?
`head` | - | tokenSequences | tokenSequences | ?
`tail` | - | dfm | dfm | ?
`tail` | - | tokenSequences | tokenSequences | ?
`c` | - | corpus | corpus | ?
`colMeans` | - | dfm |  numeric | quanteda
`colSums` | - | dfm |  numeric | quanteda
`rowMeans` | - | dfm |  numeric | quanteda
`rowSums` | - | dfm |  numeric | quanteda
`sort` | - | dfm | dfm | quanteda
`t` | - | dfm | dfm | quanteda
*R-like functions* | | |
`ndoc` | - | corpus, dfm | int | quanteda
`nfeature` | - | corpus, dfm | int | quanteda
`nsentence` | - | character, corpus | int | quanteda
`ntoken` | - | character, corpus | int | quanteda
`ntype` | - | character, corpus, tokenizedTexts, dfm | int | quanteda
`is.corpus` | - | any | logical | quanteda
`is.dfm` | - | any | logical | quanteda
`as.dfm` | - | matrix, data.frame | dfm | quanteda
`as.matrix` | - | dfm | matrix | quanteda
`as.matrix.similMatrix`	| - | similMatrix | matrix | quanteda
`is.tokenizedTexts`	| - | any | logical | quanteda
`as.tokenizedTexts`	| - | list: char | tokenizedTexts | quanteda
*inter-package converter functions* | | | |
`as.wfm`| - | dfm | austin::wfm | conversion
`as.DocumentTermMatrix`	| - | dfm |  tm::DocumentTermMatrix | quanteda
`convert` | - | dfm | *(multiple)* | conversion
`dfm2ldaformat` | - | dfm | **lda** input object | conversion
`quantedaformat2dtm` | - | dfm | tm::DocumentTermMatrix | conversion
*quanteda core* | | | |
dictionary_create | dictionary | named list | dictionary | dictionary
dictionary_apply | applyDictionary | dfm, dictionary | dfm | dictionary
dictionary_apply | applyDictionary | dfm, dictionary | dfm | dictionary
*classes* | | |
quanteda_class_corpus | corpus | | | internal
quanteda_class_dfm | dfm | | | internal
quanteda_class_dictionary | dictionary | | | internal
quanteda_class_tokenizedTexts | tokenizedTexts | | |  internal
quanteda_class_corpusSource | corpusSource | | |  internal
quanteda_class_kwic | kwic | | | internal
quanteda_class_collocations | collocations | | |  internal
quanteda_class_similmstrix | similMatrix | | | internal
quanteda_class_corpussource | corpusSource | | |  internal
quanteda_class_dfm_sparse | dfmDense | | | internal
quanteda_class_dfm_dense | dfm | | | internal
* need to classify* | | | |
`changeunits` | | | |
`collocations` | | | |
`compress` | | | |
`corpus` | | | |
`dfm` | | | |
`docfreq` | | | |  
`docnames` | | | |
`docvars` | | | |
`encoding` | | | |
`features` | | | |
`findSequences` | | | |
`joinTokens` | | | |
`kwic` | | | |
`lexdiv` | | | |
`metacorpus` | | | |
`metadoc` | | | |
`ngrams` | | | |
`phrasetotoken` | | | |
`readability` | | | |
`removeFeatures` | | | |
`readability` | | | |
`scrabble` | | | |
`segement` | | | |
`selectFeatures` | | | |
`settings` | | | |
`scrabble` | | | |
`similarity` | | | |
`skipgrams` | | | |
`smoother` | | | |
`stopwords` | | | |
`syllables` | | | |
`textfile` | | | |
`texts` | | | |
`tfidf` | | | |
`tokenize` | | | |
`topfeatures` | | | |
`trim` | | | |
`weight` | | | |
`wordlists` | | | |
`wordstem` | | | |
**textmodel** |
predict.textmodel_NB_fitted	 | | | |
predict.textmodel_wordscores_fitted | | | |
print.textmodel_wordfish_fitted	| | | |
print.textmodel_wordscores_fitted	| | | |
print.textmodel_wordscores_predicted	| | | |
`show-method` | | internal  |
textmodel	| | |  |
textmodel-method | | | |
textmodel_ca	| | |  |
textmodel_fitted-class	| | | |
textmodel_NB	| | | |
textmodel_wordfish	| | | |
textmodel_wordfish_fitted-class	| | | |
textmodel_wordfish_predicted-class	 | | | |
textmodel_wordscores	 | | | |
textmodel_wordscores_fitted-class	 | | | |
textmodel_wordscores_predicted-class  | | | |
**REMOVE** |
`describeTexts` | REMOVE | character | | REMOVE
`clean` | REMOVE | | |
