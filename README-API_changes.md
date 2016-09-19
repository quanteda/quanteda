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
`plot`  | unchanged | dfm | (plot) | ?
`plot`  | unchanged | kwic | (plot) | ?
`print` | unchanged | dfm | (printed output) | internal
`print` | unchanged | tokenizedTexts | (printed output) | internal
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
*R-like functions* | | |
`ndoc` | unchanged | corpus, dfm | int | quanteda
`nfeature` | unchanged | corpus, dfm | int | quanteda
`nsentence` | unchanged | character, corpus | int | quanteda
`ntoken` | unchanged | character, corpus | int | quanteda
*quanteda core* | | |
dictionary_create | dictionary | named list | dictionary | dictionary
dictionary_apply | applyDictionary | dfm, dictionary | dfm | dictionary
*classes* | | |
quanteda_class_corpus | corpus | | internal
quanteda_class_dfm | dfm | | internal
quanteda_class_dictionary | dictionary | | internal
quanteda_class_tokenizedTexts | tokenizedTexts | | internal
quanteda_class_corpusSource | corpusSource | | internal
quanteda_class_kwic | kwic | | internal
quanteda_class_collocations | collocations | | internal
