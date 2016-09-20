# Suggested API changes

## Motivations for the change

The package started to get a bit haphazard in terms of names and functionality, defeating some of the purposes for which it was designed (to be simple to use and intuitive).  In addition, renaming and reorganizing makes it easier:

1.  To comply more with [standards recommended by ROpenSci](https://github.com/ropensci/onboarding/blob/master/packaging_guide.md#funvar), the funcitons and their operation have been renamed.  
2.  To interface more easily with other packages, such as **tokenizers**, to do some of the lower-level work without reinventing it.


## Function name changes

The following table maps the new functions to the old, and identifies the input object classes for which methods are defined, and identifies the object type produced:

new name | original name | methods | output object | keyword
--------------| -------- | ------- | ---- | ---
*Data* | | | |
`data_char_encodedtexts` | `encodedTexts` | - | *data object* | data
`data_char_inaug` | `inaugTexts` | - | *data object* | data
`data_char_sampletext` | `exampleString` | - | *data object* | data
`data_char_ukimmigration` | `ukimmigTexts` | - | *data object* | data
`data_corpus_ie2010` | `ie2010Corpus` | - | *data object* | data
`data_corpus_inaug` | `inaugCorpus` | - | *data object* | data
`data_dfm_LBGexample` | `LBGexample` | - | *data object* | data
`data_int_syllables` | `englishSyllables` | - | (used by `syllables`) | internal
`data_list_char_wordlists` | `wordlists` | - | (used by `readability()`) | internal
`data_list_char_stopwords` | `.stopwords` | - | (used by `stopwords()`) | internal
*package-level* | | |
`quanteda-package` | unchanged | - | - | -
*R functions* | | |
`as.data.frame` | unchanged | dfm | data.frame | ?
`cbind` | unchanged | dfm | dfm | ?
`rbind` | unchanged | dfm | dfm | ?
`plot`  | unchanged | dfm | (plot) | ?
`plot`  | unchanged | kwic | (plot) | ?
`print` | unchanged | dfm | (printed output) | internal
`print` | unchanged | tokenizedTexts | (printed output) | internal
`print` | unchanged | tokenSequences | (printed output) | internal
`print` | unchanged | kwic | (printed output) | internal
`print` | unchanged | settings | (printed output) | internal
`print` | unchanged | similMatrix | (printed output) | internal
`print` | unchanged | tokenSequences | (printed output) | internal
`show` | unchanged | dictioary | (printed output) | internal
`sample` | unchanged | corpus | corpus | ?
`sample` | unchanged | dfm | dfm | ?
`sort` | unchanged | dfm | dfm | ?
`subset` | unchanged | corpus | corpus | ?
`summary` | unchanged | corpus | (invisible) data.frame | ?
`head` | unchanged | dfm | dfm | ?
`head` | unchanged | tokenSequences | tokenSequences | ?
`tail` | unchanged | dfm | dfm | ?
`tail` | unchanged | tokenSequences | tokenSequences | ?
`c` | unchanged | corpus | corpus | ?
*R-like functions* | | |
`ndoc` | unchanged | corpus, dfm | int | quanteda
`nfeature` | unchanged | corpus, dfm | int | quanteda
`nsentence` | unchanged | character, corpus | int | quanteda
`ntoken` | unchanged | character, corpus | int | quanteda
`ntype` | unchanged | character, corpus, tokenizedTexts, dfm | int | quanteda
`is.corpus` | unchanged | any | logical | quanteda
`is.dfm` | unchanged | any | logical | quanteda
`as.dfm` | unchanged | matrix, data.frame | dfm | quanteda
`as.DocumentTermMatrix`	| unchanged | dfm | tm::DocumentTermMatrix | quanteda
`as.matrix` | unchanged | dfm | matrix | quanteda
`as.matrix.similMatrix`	| unchanged | similMatrix | matrix | quanteda
`is.tokenizedTexts`	| unchanged | any | logical | quanteda `as.tokenizedTexts`	| unchanged | list: char | tokenizedTexts | quanteda
`as.wfm`| unchanged | dfm | austin::wfm | quanteda
*quanteda core* | | |
dictionary_create | dictionary | named list | dictionary | dictionary
dictionary_apply | applyDictionary | dfm, dictionary | dfm | dictionary
dictionary_apply | applyDictionary | dfm, dictionary | dfm | dictionary
*classes* | | |
quanteda_class_corpus | corpus | | internal
quanteda_class_dfm | dfm | | internal
quanteda_class_dictionary | dictionary | | internal
quanteda_class_tokenizedTexts | tokenizedTexts | | internal
quanteda_class_corpusSource | corpusSource | | internal
quanteda_class_kwic | kwic | | internal
quanteda_class_collocations | collocations | | internal
quanteda_class_similmstrix | similMatrix | | internal
quanteda_class_corpussource | corpusSource | | internal
quanteda_class_dfm_sparse | dfmDense | | internal
quanteda_class_dfm_dense | dfm | | internal
* need to classify* | | |
`changeunits` | | |
`clean` | | |
`collocations` | | |
`colMeans` | unchanged | dfm |  numeric | quanteda
`colSums` | unchanged | dfm |  numeric | quanteda
`rowMeans` | unchanged | dfm |  numeric | quanteda
`rowSums` | unchanged | dfm |  numeric | quanteda
`compress` |
`convert` |
`corpus` |
`describeTexts` | | |
`dfm` | | |
`dfm2ldaformat` | | |
`docfreq` | | |
`docnames` | | |
`docvars` | | |
`encoding` | | |
`features` | | |
`findSequences` | | |
`joinTokens` | | |
`kwic` | | |
`lexdiv` | | |
`metacorpus` | | |
`metadoc` | | |
`ngrams` | | |
`phrasetotoken` | | |
`quantedaformat2dtm` | | |
`readability` | | |
`removeFeatures` | | |
`readability` | | |
`scrabble` | | |
`segement` | | |
`selectFeatures` | | |
`settings` | | |
`scrabble` | | |
`similarity` | | |
`skipgrams` | | |
`smoother` | | |
`sort` | | |
`syllables` | | |
`t` | unchanged | dfm




**textmodel** | | |
predict.textmodel_NB_fitted	 | | |
predict.textmodel_wordscores_fitted | | |
print.textmodel_wordfish_fitted	| | |
print.textmodel_wordscores_fitted	| | |
print.textmodel_wordscores_predicted	| | |
`show-method` | | internal
textmodel	| | |
textmodel-method | | |
textmodel_ca	| | |
textmodel_fitted-class	| | |
textmodel_NB	| | |
textmodel_wordfish	| | |
textmodel_wordfish_fitted-class	| | | 
textmodel_wordfish_predicted-class	the fitted textmodel classes
textmodel_wordscores	Wordscores text model
textmodel_wordscores_fitted-class	the fitted textmodel classes
textmodel_wordscores_predicted-class
