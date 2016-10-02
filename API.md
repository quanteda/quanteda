# **quanteda** Major API Changes in *quanteda* 0.99

<!-- TOC depthFrom:2 depthTo:6 withLinks:1 updateOnSave:1 orderedList:0 -->

- [Introduction](#introduction)
	- [Motivations for the change](#motivations-for-the-change)
	- [The New Logic of **quanteda**'s design](#the-new-logic-of-quantedas-design)
- [Package-level documentation](#package-level-documentation)
- [Data Objects](#data-objects)
- [**quanteda** classes](#quanteda-classes)
- [Function Inventory](#function-inventory)
	- [Constructor functions](#constructor-functions)
	- [Functions for working with `corpus` objects](#functions-for-working-with-corpus-objects)
	- [Functions for working with `tokens` objects](#functions-for-working-with-tokens-objects)
	- [Functions for working with `dfm` (and `fcm`<sup>1</sup>) objects](#functions-for-working-with-dfm-and-fcmsup1sup-objects)
	- [Package-level functions](#package-level-functions)
	- [Analytic functions](#analytic-functions)
	- [R functions extended for **quanteda** objects](#r-functions-extended-for-quanteda-objects)
	- [R-like functions](#r-like-functions)
	- [Functions retained for backwards compatibility](#functions-retained-for-backwards-compatibility)
	- [Functions to kill off](#functions-to-kill-off)

<!-- /TOC -->

## Introduction

In anticipation of the announcement of "**quanteda** 1.0", we have made some major changes to the API to **quanteda**, to make the function names more consistent and easier to learn.  This follows long discussions with the core contributors, reflections on user experiences following many training sessions, classes taught using the package, and user feedback through various on-line forums.  This document explains the changes and their logic, and maps the new function names to the old.

### Motivations for the change

The package started to get a bit haphazard in terms of names and functionality, defeating some of the purposes for which it was designed (to be simple to use and intuitive).  In addition, renaming and reorganizing makes it easier:

1.  To comply more with [standards recommended by ROpenSci](https://github.com/ropensci/onboarding/blob/master/packaging_guide.md#funvar), the functions and their operation have been renamed.  
2.  To interface more easily with other packages, such as **tokenizers**, to do some of the lower-level work without reinventing it.

### The New Logic of **quanteda**'s design

The **quanteda** package consists of a few core data types, [created by calling constructors with similar names](#constructor-functions).  These are all "nouns" in the sense of declaring what they construct.  This follows very similar R behaviour in many of the core R objects, such as `data.frame()`, `list()`, etc.  Analytical functions also "construct" analytic objects, but have a two-part constructor name indicating their specific analytic function: `textmodel_NB` to fit a Naive Bayes model to a dfm, for instance, or `plot_wordcloud`.  This is slightly inconsistent, but not if you think of the second part of the constructor name as a form of verb (such as constructing a wordcloud or fitting a Naive Bayes model).

Methods previously defined as generics and then dispatching on these classes of objects are [now largely replaced by versions named by the object class, followed by an underscore, followed by a verb (or verb-like statement)](#functions-for-working-with-corpus-objects) describing what action is taken on that object.   The functions `corpus_sample`, `tokens_sample`, and `dfm_sample`, for instance, replace a previously overloaded version of `sample()` that could be dispatched on a corpus, tokenized text, or dfm object.  This is designed to make it more clear to the user which functions are available for which object classes, and avoid unexpected behaviours arising from a user's lack of understanding how the base functions are overloaded.

Simpler base R functions, however, are still [extended to quanteda objects through overloading](#r-functions-extended-for-quanteda-objects).  The logic of allowing is that these functions, e.g. `cbind()` for a dfm, are very simple and very common, and therefore are well-known to users.  Furthermore, they can operate in only one fashion on the object for which they are defined, such as `cbind()` combining two dfm objects by joining columns.  Similar functions extended in this way include `print`, `head`, `tail`, and `t()`.

Additional functions have been defined for **quanteda** objects that are [very similar to simple base R functions](#r-like-functions), but are not named using the `class_action` format because they do not return a modified object of the same class.  These follow as closely as possible the naming conventions found in the base R functions that are similar.  For instance, `docnames()` and `featnames()` return the document names of various **quanteda** objects, in the same way that `rownames()` does for matrix-like objects (a matrix, data.frame, data.table, etc.).  Likewise, `ndoc()` returns the number of documents, using the singular form similar to `nrow()` and `ncol()`.  


## Package-level documentation

new name | original name | notes
:--------|:------------- |:-------
`quanteda-package` | - |

This will be a help page describing the package, its development, its contributors, and future plans.  It will also detail the classes and provide links to the class definitions, which will be otherwise removed from the manual index by attaching the `internal`
keywords to them.  Many of the functions previously indexed separately will now be listed only in this section, such as references to base R functions (e.g. `cbind()` for dfm objects, `c()` for corpus concatenation, etc.) that have been extended to **quanteda** objects.  These will be documented on the main help page, rather than in their own index entries, mainly because their functions are obvious, and because so doing reduces the length (and hence complexity) of the index in the package documentation.

## Data Objects

Used for demonstration purposes, these have been renamed to reflect that they are data, a tag for the object class, and a description.  Internal objects will be hidden from the index through keywords.

new name | original name | notes | keyword
:--------|:------------- |:----- |:-------
`data_char_encodedtexts` | `encodedTexts` | | data
`data_files_encodedtexts` | `encodedTextFiles` |  | data
`data_char_sampletext` | `exampleString` |   | data
`data_char_mobydick`   | `mobydickText` | | data
`data_char_ukimmigration` | `ukimmigTexts` |   | data
`data_char_inaugural` | `inaugTexts` |  | data
`data_corpus_inaugual` | `inaugCorpus` |   | data
`data_corpus_irishbudget2010` | `ie2010Corpus` |   | data
`data_dfm_LBGexample` | `LBGexample` |  | data
`data_int_syllables` | `englishSyllables` |  (used by `textcount_syllables()`) | internal
`data_char_wordlists` | `wordlists` |  (used by `readability()`) | internal
`data_char_stopwords` | `.stopwords` | (used by `stopwords()` -- see [below](#functions-retained-for-backwards-compatibility)) | internal


## **quanteda** classes

These are the core classes defined by **quanteda** and used for method dispatch.  The logic here is that a constructor function will create a quanteda object class of the same name.  These classes would be internal, however, and (mostly) hidden from users.  They would not be documented except in the source code.

new name | original name | constructor function
:--------|:------------- |:-------
collocations | collocations | `collocations()`
corpus | corpus | `corpus()`
dictionary | dictionary | `dictionary()`
dfm | dfm, dfmDense, dfmSparse | `dfm()`
tokens | tokenizedTexts | `tokenize()`
explore_kwic | kwic | `kwic()`
textstat_similarity_matrix | similarity | `similarity()`
similarity_similarity_list | similarity | `similarity()`


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
`corpus_docvars` | `docvars` | corpus | *data.frame* | corpus


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
`dfm_weight_tf` | `tf` | dfm | dfm | dfm
`dfm_weight_tfidf` | `tfidf` | dfm | dfm | dfm
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
`metadata` | `metadoc`, `metacorpus`, `docvars` | Meta-data is designed primarily for a corpus, but some level of meta-data can be propagated through to downstream objects


### Analytic functions

*(naming still a work in progress...can anyone think of a better name for the `collocations` function?)*

new name | original name | methods | output object | keyword
:--------|:------------- |:------- |:------------- |:-------
`textscore_collocations` | `collocations`, `findSequences` | tokens | data.frame | textscore
`textstat_lexdiv` | `lexdiv` | dfm, tokens | data.frame | textstat
`textstat_readability` | `readability` | character, tokens | data.frame | textstat
`textstat_similarity`  | `similarity`   | dfm |  (similarity) matrix `textstat_similarity_matrix`, which can be coerced using `as.list()` into a `textstat_similarity_list` similar to thr object returned by `tm::findAssocs` |  textstat
`textcount_kwic`       | `kwic` | corpus, character, tokens | data.frame |  textcount
`textcount_scrabble` | `scrabble` | character (vector), tokens | integer | textcount
`textcount_syllables` | `syllables`   | character (vector), tokens | integer | textcount
`textcount_features`  | `topfeatures` | dfm | integer | textcount
`textcount_docfreq`   | `docfreq` | dfm | integer | textcount
`textmodel_NB`        | | dfm | textmodel_NB | textmodel
`textmodel_wordscores`        | | dfm | textmodel_wordscores | textmodel
`textmodel_wordfish`        | | dfm | textmodel_wordfish | textmodel
`textmodel_ca` | | dfm | textmodel_ca | textmodel
`textplot_wordcloud` | `plot.dfm` | dfm | (wordcloud plot) | textplot
`textplot_xray`  | `plot_kwic` |  kwic | (x-ray plot) | textplot


### R functions extended for **quanteda** objects

These are functions in the core R package for which methods have been defined in quanteda, in order to extend their functionality to quanteda objects.  The motivation is that for those familiar with how these work in R, the extension of these methods will provide an intuitive and natural application to quanteda objects.

new name | original name | methods | output object | keyword
:--------|:------------- |:------- |:------------- |:-------
`as.data.frame` | - | dfm | data.frame | (with `dfm`)
`as.matrix` | - | dfm | matrix | (with `dfm`)
`as.matrix.textstat_similarity_list`	| - | similMatrix | matrix | (with `textstat_similarity`)
`as.matrix.textstat_similarity_matrix`	| - | similMatrix | list | (with `textstat_similarity`)
`c` | - | corpus | corpus | (with `corpus`)
`cbind` | - | dfm | dfm | internal
`rbind` | - | dfm | dfm | internal
`head` | - | dfm | dfm | internal
`head` | - | tokenSequences | tokenSequences | internal
`colMeans` | - | dfm |  numeric | internal
`colSums` | - | dfm |  numeric | internal
`rowMeans` | - | dfm |  numeric | internal
`rowSums` | - | dfm |  numeric | internal
`summary` | - | corpus, character, tokens | (invisible) data.frame | quanteda
`t` | - | dfm | dfm | internal
`tail` | - | dfm | dfm | internal
`tail` | - | tokenSequences | tokenSequences | internal
`print` | - | dfm | (printed output) | internal
`print` | - | tokenizedTexts | (printed output) | internal
`print` | - | tokenSequences | (printed output) | internal
`print` | - | kwic | (printed output) | internal
`print` | - | settings | (printed output) | internal
`print` | - | similMatrix | (printed output) | internal
`print` | - | tokenSequences | (printed output) | internal
`show` | - | dictionary | (printed output) | internal
`as.list`   | -  | tokens | list of character | internal
`as.list`   | -  | dictionary | list of character | internal
`as.character` | `texts` | corpus | character | (with `corpus`)
`as.data.frame` | `docvars` | corpus | data.frame | (with `corpus`)



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
`is.tokens`	| - | any | logical | quanteda
`as.tokens`	| - | list: char | tokenizedTexts | quanteda
`docnames` | `docnames` | corpus, tokens, dfm | character |
`docnames<-` | `docnames<-` | corpus | character |
`featnames` | `features` | dfm | character |
`encoding`<sup>1</sup> |  | character, corpus | data.frame (of character encoding labels) |

Notes:
1. `encoding`: This is similar to, but quite different in terms of how it works, the `base::Encoding()` which queries or sets simple encoding bits for character vectors.  Since that is not a generic, it cannot be overloaded without redefining it as a generic (and therefore issuing a "masked function" warning when the package is loaded).  For consistency with the scheme described here, it cannot be `corpus_encoding` since it does not return a corpus.

### Functions retained for backwards compatibility

These functions are inconsistent with the naming scheme, but are retained for convenience and because they are so widely used from earlier versions of the package.

function | notes | output object | keyword
:------------- |:------- |:------------- |:-------
`stopwords` | *input: character indicating language* | character |
`as.DocumentTermMatrix.dfm`	|  coerces/converts to a **tm** matrix, redundant with `convert()` |  tm::DocumentTermMatrix | dfm
`texts` | extracts character vector of texts from a corpus | character | corpus


### Functions to kill off

The biggest change here is the phasing out of `textfile()`, the intermediate function to load files into R for use in constructing a corpus.  The new version of `corpus()` will allow this to take place directly.

new name | original name | methods | output object | keyword
:--------|:------------- |:------- |:------------- |:-------
`describeTexts` | REMOVE | character | |
`clean` | REMOVE | | |
`corpus(file = "")` | `textfile` | *files* | corpus | corpus
`convert` | `as.wfm`|  dfm | austin::wfm | conversion
`convert` | `dfm2ldaformat` |  dfm | **lda** input object | conversion
`convert` | `quantedaformat2dtm` | dfm | tm::DocumentTermMatrix | conversion
`wordlists` | | | character | internal
