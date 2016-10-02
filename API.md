# **quanteda** Major API Changes in *quanteda* 0.99

<!-- TOC depthFrom:2 depthTo:6 withLinks:1 updateOnSave:1 orderedList:0 -->

- [Motivations for the change](#motivations-for-the-change)
- [Package-level documentation](#package-level-documentation)
- [Data Objects](#data-objects)
- [**quanteda** classes](#quanteda-classes)
- [Function Inventory](#function-inventory)
	- [Constructor functions](#constructor-functions)
	- [Functions for working with `corpus` objects](#functions-for-working-with-corpus-objects)
	- [Functions for working with `tokens` objects](#functions-for-working-with-tokens-objects)
	- [Functions for working with `dfm` objects](#functions-for-working-with-dfm-objects)
	- [Package-level functions](#package-level-functions)
	- [to be classified](#to-be-classified)
	- [R functions extended for **quanteda** objects](#r-functions-extended-for-quanteda-objects)
	- [R-like functions](#r-like-functions)
	- [Converter functions for working with other R packages](#converter-functions-for-working-with-other-r-packages)
	- [Text modeling functions](#text-modeling-functions)
	- [Functions to kill off](#functions-to-kill-off)

<!-- /TOC -->

In anticipation of the announcment of "**quanteda** 1.0", we have made some major changes to the API to **quanteda**, to make the function names more consistent and easier to learn.  This follows long discussions with the core contributors, reflections on user experiences following many training sessions, classes taught using the package, and user feedback through various on-line forums.  This document explains the changes and their logic, and maps the new function names to the old.

## Motivations for the change

The package started to get a bit haphazard in terms of names and functionality, defeating some of the purposes for which it was designed (to be simple to use and intuitive).  In addition, renaming and reorganizing makes it easier:

1.  To comply more with [standards recommended by ROpenSci](https://github.com/ropensci/onboarding/blob/master/packaging_guide.md#funvar), the funcitons and their operation have been renamed.  
2.  To interface more easily with other packages, such as **tokenizers**, to do some of the lower-level work without reinventing it.


## Package-level documentation

new name | original name | methods | output object | keyword
:--------|:------------- |:------- |:------------- |:-------
`quanteda-package` | - | - | - | -

This will be a help page describing the package, its development, its contributors, and future plans.  It will also detail the classes and provide links to the class definitions, which will be otherwise removed from the manual index by attaching the `internal`
keywords to them.  Many of the functions previously indexed separately will now be listed only in this section, such as references to base R functions (e.g. `cbind()` for dfm objects, `c()` for corpus concatenation, etc.) that have been extended to **quanteda** objects.

## Data Objects

Used for demonstration purposes, these have been renamed to reflect that they are data, a tag for the object class, and a description.  Internal objects will be hidden from the index through keywords.

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
`data_char_wordlists` | `wordlists` |  (used by `readability()`) | internal
`data_char_stopwords` | `.stopwords` | (used by `stopwords()`) -- but see below| internal

**Proposed function change:**  replace stopwords() with a built-in data object `data_char_stopwords`, which requires the user to specify the list element.  Instead of `stopwords("english")`, the user would now simply access the data directly as `data_char_stopwords$english`.   


## **quanteda** classes

These are the core classes defined by **quanteda** and used for method dispatch.  The logic here is that a constructor function will create a quanteda object class of the same name.  These classes would be internal, however, and (mostly) hidden from users.  They would not be documented except in the source code.

new name | original name | constructor function
:--------|:------------- |:-------
collocations | collocations | `collocations()`
corpus | corpus | `corpus()`
dictionary | dictionary | `dictionary()`
dfm | dfm, dfmDense, dfmSparse | `dfm()`
kwic | kwic | `kwic()`
similarity | similarity | `similarity()`
tokens | tokenizedTexts | `tokenize()`


## Function Inventory

Using the new logic, we would define a constructor function of a simple name (mostly) corresponding to the existing constructor function names: `corpus()`, `dfm()`, etc.  The idea is to make these consistent, with the function name describing the noun of the object it creates.  This means that `tokenize()` will become `tokens()`, creating a class of tokenized texts called `tokens` (replacing the existing class name of `tokenizedTexts`.  Each function that works with these objects will be named using an *object*`_`*verb* scheme, such as `corpus_subset()`.  To be included in this scheme, a function will operate on the object class, and return a modified version of that object class.  Only functions that behave in this "functional programming" fashion, and return the object class of input, will be named using this scheme.

The following table maps the new functions to the old, and identifies the input
object classes for which methods are defined, and identifies the object type
produced:

### Constructor functions

These functions process one object, either a base or a quanteda type, and create a new, quanteda-class object.

new name | original name | methods | output object | keyword
:--------|:------------- |:------- |:------------- |:-------
`corpus` | | corpusSource, character, data.frame, VCorpus | corpus | corpus
`dfm` | | corpus, character | dfm | dfm
`dictionary` |  | named list | dictionary | dictionary
`tokens` | `tokenize` | corpus, character | tokenizedTexts | tokens

### Functions for working with `corpus` objects

new name | original name | methods | output object | keyword
:--------|:------------- |:------- |:------------- |:-------
`corpus` |      | character, data.frame, VCorpus, *files* | corpus | corpus
`corpus_segment` | `segment`      | corpus | corpus | corpus
`corpus_reshape` | `changeunits`      | corpus | corpus | corpus
`corpus_subset` | `subset`      | corpus | corpus | corpus
`corpus_sample` | `sample` | corpus | corpus | corpus
`corpus_metadata` | `metadoc`, `metacorpus` | corpus | corpus | corpus
`corpus_docvars<-` | `docvars<-` | corpus + data.frame | corpus | corpus
`corpus_docvars` | `docvars` | corpus | data.frame | corpus


### Functions for working with `tokens` objects

new name | original name | methods | output object | keyword
:--------|:------------- |:------- |:------------- |:-------
`tokens` | `tokenize`    | character, corpus | tokens | tokens
`tokens_join` | `phrasetotoken`, `joinTokens` | tokens + collocations/dictionary/character | tokens | tokens
`tokens_select` | `selectTokens` | tokens + collocations/dictionary/character | tokens | tokens
`tokens_remove` | `removeTokens` | tokens + collocations/dictionary/character | tokens | tokens
`tokens_wordstem` | `wordstem` | tokens | tokens | tokens
`tokens_ngrams` | `ngrams` | tokens | tokens | tokens
`tokens_skipgrams` | `skipgrams` | tokens | tokens | tokens
`tokens_sample` | `sample` | tokens | tokens | tokens
`tokens_tolower` | `toLower` | tokens | tokens | tokens
`tokens_toupper` | `toUpper` | tokens | tokens | tokens
`tokens_hash`    | *in progress* | tokens | tokens_hashed | tokens
`tokens_tag`    | *in progress* | tokens | tokens_tagged | tokens

### Functions for working with `dfm` (and `fcm`<sup>1</sup>) objects

new name | original name | methods | output object | keyword
:--------|:------------- |:------- |:------------- |:-------
`dfm` |     | character, corpus, tokens | dfm | dfm
`dfm_compress` | `compress` | dfm  | dfm | dfm
`dfm_select` | `selectFeatures` | dfm + collocations/dictionary/character | dfm | dfm
`dfm_remove` | `removeFeatures` | dfm + collocations/dictionary/character | dfm | dfm
`dfm_trim`   | `trim` | dfm | dfm | dfm
`dfm_wordstem` | `wordstem` | dfm | dfm | dfm
`dfm_weight` | `weight` (also: `tfidf`) | dfm | dfm | dfm
`dfm_smooth` | `smoother` | dfm | dfm | dfm
`dfm_sample` | (`trim` option) | dfm | dfm | dfm
`dfm_tolower` | `toLower` | dfm | dfm | dfm
`dfm_toupper` | `toUpper` | dfm | dfm | dfm
`dfm_dictionary` | `applyDictionary` | dfm | dfm | dfm
`dfm_sort`    | `sort` | dfm | dfm | dfm
`dfm_convert`<sup>2</sup>    | `convert` | dfm | *foreign* | dfm

Notes:
1.  Application to a "context-feature matrix": We would define the same set of functions for `cfm()`, the "context-feature matrix".  (Proposal: rename this "feature co-occurrence matrix", or `fcm()`).
2.  `dfm_convert`:  Strictly speaking, this may not fit into the scheme, since it starts with `dfm_` but does not return a dfm object.  Should this be renamed?

### Package-level functions

These are functions designed to work at the package level (e.g. `settings()`) or designed to work with any **quanteda** object, such as `history()`, which provides a list of the commands applied to any object and its predecessor objects.

new name | original name | notes
:--------|:------------- |:-------
`settings` | `settings`  | Could use system `options()` and define quanteda options, e.g. `quanteda_remove_punct`
`history` |  | works for any object


### Analytic functions

new name | original name | methods | output object | keyword
:--------|:------------- |:------- |:------------- |:-------
`explore_kwic`       | `kwic` | corpus, character, tokens | data.frame |
`score_collocations` | `collocations`, `findSequences` | tokens | data.frame |  
`stats_lexdiv` | `lexdiv` | dfm, tokens | data.frame |
`stats_readability` | `readability` | character, tokens | data.frame |
`stats_similarity`  | `similarity`   | dfm |  (similarity) matrix |
`counts_scrabble` | `scrabble` | character (vector), tokens | integer
`counts_syllables` | `syllables`   | character (vector), tokens | integer |
`counts_features`  | `topfeatures` | dfm | integer |
`counts_docfreq`   | `docfreq` | dfm | integer
`textmodel_NB`        | | `dfm` | textmodel_NB |
`textmodel_wordscores`        | | `dfm` | textmodel_wordscores |
`textmodel_wordfish`        | | `dfm` | textmodel_wordfish |
`textmodel_ca` | | `dfm` | textmodel_ca |


### R functions extended for **quanteda** objects

These are functions in the core R package for which methods have been defined in quanteda, in order to extend their functionality to quanteda objects.  The motivation is that for those familiar with how these work in R, the extension of these methods will provide an intuitive and natural application to quanteda objects.

new name | original name | methods | output object | keyword
:--------|:------------- |:------- |:------------- |:-------
`as.data.frame` | - | dfm | data.frame | extensions
`c` | - | corpus | corpus | (with `corpus`)
`cbind` | - | dfm | dfm | extensions
`rbind` | - | dfm | dfm | extensions
`plot`  | - | dfm | (plot) | plotting
`plot`  | - | kwic | (plot) | plotting
`head` | - | dfm | dfm | extensions
`head` | - | tokenSequences | tokenSequences | extensions
`colMeans` | - | dfm |  numeric | extensions
`colSums` | - | dfm |  numeric | extensions
`rowMeans` | - | dfm |  numeric | extensions
`rowSums` | - | dfm |  numeric | extensions
`sample` | - | corpus | corpus | extensions
`sample` | - | dfm | dfm | extensions
`sort` | - | dfm | dfm | extensions
`subset` | - | corpus | corpus | extensions
`summary` | - | corpus | (invisible) data.frame | extensions
`t` | - | dfm | dfm | extensions
`tail` | - | dfm | dfm | extensions
`tail` | - | tokenSequences | tokenSequences | extensions
`print` | - | dfm | (printed output) | internal
`print` | - | tokenizedTexts | (printed output) | internal
`print` | - | tokenSequences | (printed output) | internal
`print` | - | kwic | (printed output) | internal
`print` | - | settings | (printed output) | internal
`print` | - | similMatrix | (printed output) | internal
`print` | - | tokenSequences | (printed output) | internal
`show` | - | dictionary | (printed output) | internal

### R-like functions

These are functions that are not extensions of base R methods, but rather additions to base R-*like* methods, defined for **quanteda** objects.  These follow the R conventions for naming and syntax as closely as possible.  For instance, `ntype()` and `ntoken()` return the number of types and tokens in an object respectively, similar to `nrow()` and `ncol()` in base R.  (Note the use of singular for the **quanteda** functions, just as in base R.)  Each method is defined for numerous **quanteda** object classes as appropriate.  When a method does not work for a specific object class, for instnace `nfeature()` does not work for a `character` class object, this is by design -- in this case, because features have to be defined through user choice, as "features" do not exist inherently in a character string without the user having first defined and selected features.  

There are some fudges to this, for instance the definition of an `ntoken()` and `ntype()` methods for `character` and `corpus` objects, since tokens (and types) are only countable after the user has tokenized the text.  For this reason, however, `ntoken()` and `ntype()` methods pass through options in `...` to `tokenize()`, which is called as a result of dispatching these methods on `character` and `corpus` objects.  Calls to these functions are performed by the `summary.character()` method, for instance, which returns counts of tokens, types, and sentences.

new name | original name | methods | output object | keyword
:--------|:------------- |:------- |:------------- |:-------
`ndoc` | - | corpus, dfm | int | quanteda
`nfeature` | - | corpus, dfm | int | quanteda
`nsentence` | - | character, corpus | int | quanteda
`ntoken` | - | character, corpus, tokenizedTexts, dfm | int | quanteda
`ntype` | - | character, corpus, tokenizedTexts, dfm | int | quanteda
`is.corpus` | - | any | logical | quanteda
`is.dfm` | - | any | logical | quanteda
`as.dfm` | - | matrix, data.frame | dfm | quanteda
`as.matrix` | - | dfm | matrix | quanteda
`as.matrix.similMatrix`	| - | similMatrix | matrix | quanteda
`is.tokenizedTexts`	| - | any | logical | quanteda
`as.tokenizedTexts`	| - | list: char | tokenizedTexts | quanteda
`as.character` | `texts` | corpus | character | corpus
`as.data.frame` | `docvars` | corpus | data.frame | corpus
`docnames` | `docnames` | corpus, tokens, dfm | character |
`docnames<-` | `docnames<-` | corpus | character |
`featnames` | `features` | dfm | character |
`as.list` | | `tokens` | list of character |
`encoding`<sup>1</sup> |  | character, corpus | data.frame (of character encoding labels) |

Notes:
1. `encoding`: This is similar to, but quite different in terms of how it works, the `base::Encoding()` which queries or sets simple encoding bits for character vectors.  Since that is not a generic, it cannot be overloaded without redefining it as a generic (and therefore issuing a "masked function" warning when the package is loaded).  For consistency with the scheme described here, it cannot be `corpus_encoding` since it does not return a corpus.

### Functions to kill off

new name | original name | methods | output object | keyword
:--------|:------------- |:------- |:------------- |:-------
`describeTexts` | REMOVE | character | |
`clean` | REMOVE | | |
`corpus(file = "")` | `textfile` | *files* | corpus | corpus
`convert` | `as.wfm`|  dfm | austin::wfm | conversion
`convert` | `as.DocumentTermMatrix`	|  dfm |  tm::DocumentTermMatrix | dfm
`convert` | `dfm2ldaformat` |  dfm | **lda** input object | conversion
`convert` | `quantedaformat2dtm` | dfm | tm::DocumentTermMatrix | conversion
`stopwords` | | | character |
`wordlists` | | | character | internal
