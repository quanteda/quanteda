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
	- [Functions for working with dfm and fcm objects](#functions-for-working-with-dfm-and-fcm-objects)
	- [Package-level functions](#package-level-functions)
	- [Analytic functions](#analytic-functions)
		- ["Discovery" functions](#discovery-functions)
		- [Functions that compute stuff](#functions-that-compute-stuff)
		- [Functions that fit models to dfm objects](#functions-that-fit-models-to-dfm-objects)
		- [Plotting functions](#plotting-functions)
	- [R functions extended for **quanteda** objects](#r-functions-extended-for-quanteda-objects)
	- [R-like functions](#r-like-functions)
	- [Functions retained for backwards compatibility](#functions-retained-for-backwards-compatibility)
	- [Functions that will be deprecated and removed](#functions-that-will-be-deprecated-and-removed)

<!-- /TOC -->

## Introduction

In anticipation of the announcement of "**quanteda** 1.0", we have made some major changes to the **quanteda**'s API.  While this "breaks" many of the commands used previously, it has the advantage going forward of making the function names much more consistent.  As a consequence, we hope that they will be easier to learn, and to extend going forward.  The changes follow long discussions with the core contributors, reflections on user experiences following many training sessions, classes taught using the package, and  feedback received through various on-line forums.  This document explains the changes and their logic, and maps the new function names to the old.

### Motivations for the change

The package started to get a bit haphazard in terms of names and functionality, defeating some of the purposes for which it was designed (to be simple to use and intuitive).  In addition, renaming and reorganizing makes it easier:

1.  To comply more with [standards recommended by ROpenSci](https://github.com/ropensci/onboarding/blob/master/packaging_guide.md#funvar), the functions and their operation have been renamed.  
2.  To interface more easily with other packages, such as **tokenizers**, to do some of the lower-level work without reinventing it.  
3.  To make the package easier to extend, for instance by writing companion packages that depend on **quanteda**.

### The New Logic of **quanteda**'s design

The **quanteda** package consists of a few core data types, [created by calling constructors with similar names](#constructor-functions).  These are all "nouns" in the sense of declaring what they construct.  This follows very similar R behaviour in many of the core R objects, such as `data.frame()`, `list()`, etc.  Analytical functions also "construct" analytic objects, but have a two-part constructor name indicating their specific analytic function: `textmodel_NB` to fit a Naive Bayes model to a dfm, for instance, or `plot_wordcloud`.  This is slightly inconsistent, but not if you think of the second part of the constructor name as a form of verb (such as constructing a wordcloud or fitting a Naive Bayes model).

Methods previously defined as generics and then dispatching on these classes of objects are [now largely replaced by versions named by the object class, followed by an underscore, followed by a verb (or verb-like statement)](#functions-for-working-with-corpus-objects) describing what action is taken on that object.   The functions `corpus_sample`, `tokens_sample`, and `dfm_sample`, for instance, replace a previously overloaded version of `sample()` that could be dispatched on a corpus, tokenized text, or dfm object.  This is designed to make it more clear to the user which functions are available for which object classes, and avoid unexpected behaviours arising from a user's lack of understanding how the base functions are overloaded.  Additionally, in the case of `sample()`, we avoid the namespace "conflict" caused by redefining the function as a generic, so that it could be overloaded.  Our new, more specific naming conventions therefore reduce the likelihood of namespace conflicts with other packages.

Simpler base R functions, however, are still [extended to quanteda objects through overloading](#r-functions-extended-for-quanteda-objects).  The logic of allowing is that these functions, e.g. `cbind()` for a dfm, are very simple and very common, and therefore are well-known to users.  Furthermore, they can operate in only one fashion on the object for which they are defined, such as `cbind()` combining two dfm objects by joining columns.  Similar functions extended in this way include `print`, `head`, `tail`, and `t()`.

Additional functions have been defined for **quanteda** objects that are [very similar to simple base R functions](#r-like-functions), but are not named using the `class_action` format because they do not return a modified object of the same class.  These follow as closely as possible the naming conventions found in the base R functions that are similar.  For instance, `docnames()` and `featnames()` return the document names of various **quanteda** objects, in the same way that `rownames()` does for matrix-like objects (a matrix, data.frame, data.table, etc.).  The abbreviation of `featnames()` is intentionally modeled on `colnames()`.  Likewise, `ndoc()` returns the number of documents, using the singular form similar to `nrow()` and `ncol()`.  


## Package-level documentation

new name | original name | notes
:--------|:------------- |:-------
`quanteda-package` | - |

This will be a help page describing the package, its development, its contributors, and future plans.  It will also detail the classes and provide links to the class definitions, which will be otherwise removed from the manual index by attaching the `internal`
familys to them.  Many of the functions previously indexed separately will now be listed only in this section, such as references to base R functions (e.g. `cbind()` for dfm objects, `c()` for corpus concatenation, etc.) that have been extended to **quanteda** objects.  These will be documented on the main help page, rather than in their own index entries, mainly because their functions are obvious, and because so doing reduces the length (and hence complexity) of the index in the package documentation.

## Data Objects

Used for demonstration purposes, these have been renamed to reflect that they are data, a tag for the object class, and a description.  Internal objects will be hidden from the index through familys.

new name | original name | notes | family
:--------|:------------- |:----- |:-------
`data_char_encodedtexts` | `encodedTexts` | | data
`data_files_encodedtexts` | `encodedTextFiles` |  | data
`data_char_sampletext` | `exampleString` |   | data
`data_char_mobydick`   | `mobydickText` | | data
`data_char_ukimmigration` | `ukimmigTexts` |   | data
`data_char_inaugural` | `inaugTexts` |  | data
`data_corpus_inaugural` | `inaugCorpus` |   | data
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
tokens | tokenizedTexts | `tokens()`
explore_kwic | kwic | `explore_kwic()`
textstat_similarity_matrix | similarity | `textstat_similarity()`
textstat_similarity_list | similarity | `textstat_similarity()`


## Function Inventory

Using the new logic, we would define a constructor function of a simple name (mostly) corresponding to the existing constructor function names: `corpus()`, `dfm()`, etc.  The idea is to make these consistent, with the function name describing the noun of the object it creates.  This means that `tokenize()` will become `tokens()`, creating a class of tokenized texts called `tokens` (replacing the existing class name of `tokenizedTexts`.  Each function that works with these objects will be named using an *object*`_`*verb* scheme, such as `corpus_subset()`.  To be included in this scheme, a function will operate on the object class, and return a modified version of that object class.  Only functions that behave in this "functional programming" fashion, and return the object class of input, will be named using this scheme.  (See [Function Inventory](#function-inventory) below.)

The following table maps the new functions to the old, and identifies the input
object classes for which methods are defined, and identifies the object type
produced:

### Constructor functions

These functions process one object, either a base or a **quanteda** type, and create a new, quanteda-class object.

new name | original name | methods | output object | family
:--------|:------------- |:------- |:------------- |:-------
`corpus` | | readtext, character, data.frame, VCorpus | corpus | corpus
`dfm` | | corpus, character | dfm | dfm
`dictionary` |  | named list | dictionary | dictionary
`tokens` | `tokenize` | corpus, character | tokenizedTexts | tokens

### Functions for working with `corpus` objects

new name | original name | methods | output object | family
:--------|:------------- |:------- |:------------- |:-------
`corpus` |      | character, data.frame, VCorpus, *files* | corpus | corpus
`corpus_segment` | `segment`      | corpus | corpus | corpus
`corpus_reshape` | `changeunits`      | corpus | corpus | corpus
`corpus_subset` | `subset`      | corpus | corpus | corpus
`corpus_sample` | `sample` | corpus | corpus | corpus


### Functions for working with `tokens` objects

new name | original name | methods | output object | family
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

### Functions for working with dfm and fcm objects

new name | original name | methods | output object | family
:--------|:------------- |:------- |:------------- |:-------
`dfm` |     | character, corpus, tokens | dfm | dfm
`dfm_compress` | `compress` | dfm  | dfm | dfm
`dfm_select` | `selectFeatures` | dfm + collocations/dictionary/character | dfm | dfm
`dfm_remove` | `removeFeatures` | dfm + collocations/dictionary/character | dfm | dfm
`dfm_trim`   | `trim` | dfm | dfm | dfm
`dfm_wordstem` | `wordstem` | dfm | dfm | dfm
`dfm_weight` | `weight` (also: `tfidf`) | dfm | dfm | dfm
`dfm_tf` | `tf` | dfm | dfm | dfm
`dfm_tfidf` | `tfidf` | dfm | dfm | dfm
`dfm_smooth` | `smoother` | dfm | dfm | dfm
`dfm_sample` | (`trim` option) | dfm | dfm | dfm
`dfm_tolower` | `toLower` | dfm | dfm | dfm
`dfm_toupper` | `toUpper` | dfm | dfm | dfm
`dfm_lookup` | `applyDictionary` | dfm | dfm | dfm
`dfm_sort`    | `sort` | dfm | dfm | dfm


**For `fcm`**:  

Construction of a "feature co-occurrence matrix" would be slightly different from constructing a dfm.   Unlike the "Swiss-army" knife approach of `dfm()`, `fcm()` would work only on tokens, since the definition of how the context of co-occurrence is defined is dependent on token sequences and therefore highly dependent on tokenization options.  In addition, `fcm()` is likely to be used a lot less frequently, and primarily by more expert users.

Some of these will be internal only (e.g. `fcm_compress()`), as users are unlikely to need them, although they are useful when available to other functions.  Others are not defined, such as `fcm_wordstem` and `fcm_tolower`, because collapsing these and treating them as equivalent (as for a dfm object) is incorrect for the context in which co-occurrence is defined, such as a +/- 5 token window.

new name | original name | methods | output object | family
:--------|:------------- |:------- |:------------- |:-------
`fcm` |     | tokens | fcm | fcm
`fcm_select` | `selectFeatures` | fcm + collocations/dictionary/character | fcm | fcm
`fcm_remove` | `removeFeatures` | fcm + collocations/dictionary/character | fcm | fcm
`fcm_sort`    | `sort` | fcm | fcm | fcm
`fcm_compress` | `compress` | fcm  | fcm | internal


### Package-level functions

These are functions designed to work at the package level (e.g. `settings()`) or designed to work with any **quanteda** object, such as `history()`, which provides a list of the commands applied to any object and its predecessor objects.

new name | original name | notes
:--------|:------------- |:-------
`settings` | `settings`  | Could use system `options()` and define quanteda options, e.g. `quanteda_remove_punct`
`history` | -- | works for any object
`metadata` | `metadoc`, `metacorpus`, `docvars` | Meta-data is designed primarily for a corpus, but some level of meta-data can be propagated through to downstream objects
`convert`    | `convert` | Designed to convert a dfm to a "foreign" dfm-type format, this function retains the general name as it could, conceivably, operate on other object types such as a corpus.  But it is not named `dfm_convert` and `corpus_convert` because the output object is not the same as its **quanteda** input object, hence breaking the naming convention.


### Analytic functions

These are still very much a **work in progress**, so input much appreciated!

The challenge is to figure out an accurate, descriptive, consistent, and not overly cumbersome naming convention.  This should group similar functions, and also describe what they do.  This is very important because extensions (and extension packages) built on quanteda will take cues from this naming convention for analytic objects.  

The proposed naming convention here breaks from the previous `x_y` scheme in that the "x" does not identify a previously constructed object to which a verb "x" is applied, but rather provides a hierachy of analytic functions where "x" is the general category and "y" is the more specific analysis applied.

This makes sense as a scheme, but is inconsistent with the broader scheme.  Is this something we should therefore avoid?  And if so, what is the alternative that remains consistent?

#### "Discovery" functions

new name | original name | methods | output object | family
:--------|:------------- |:------- |:------------- |:-------
`collocations` | `collocations`, `findSequences` | tokens | data.frame | textscore
`kwic`         | `kwic` | corpus, character, tokens | data.frame |  textcount

These are the worst fitting, in my opinion.  Surely there are better alternatives!

#### Functions that compute stuff

new name | original name | methods | output object | family
:--------|:------------- |:------- |:------------- |:-------
`textstat_lexdiv` | `lexdiv` | dfm, tokens | data.frame | textstat
`textstat_readability` | `readability` | character, tokens | data.frame | textstat
`textstat_simil`, `simil`  | `similarity`   | dfm |  dist | textstat
`textstat_dist`, `dist`   | `similarity`   | dfm |  dist | textstat



#### Functions that fit models to dfm objects

(These are already implemeted this way, so no change needed.)

new name | original name | methods | output object | family
:--------|:------------- |:------- |:------------- |:-------
`textmodel_NB`        | `textmodel_NB` | dfm | textmodel_NB | textmodel
`textmodel_wordscores`        | `textmodel_wordscores`  | dfm | textmodel_wordscores | textmodel
`textmodel_wordfish`        | `textmodel_wordfish` | dfm | textmodel_wordfish | textmodel
`textmodel_ca` | `textmodel_ca` | dfm | textmodel_ca | textmodel

#### Plotting functions

new name | original name | methods | output object | family
:--------|:------------- |:------- |:------------- |:-------
`textplot_wordcloud` | `plot.dfm` | dfm | (wordcloud plot) | textplot
`textplot_xray`  | `plot_kwic` |  kwic | (x-ray plot) | textplot
`textplot_positions`  | `plot.textmodel_wordfish_fitted` |  textmodel_wordfish_fitted | scaled positions with intervals | textplot

We can think of (many) additional functions to produce other plot types (from `fcm` objects).  



### R functions extended for **quanteda** objects

These are functions in the core R package for which methods have been defined in quanteda, in order to extend their functionality to quanteda objects.  The motivation is that for those familiar with how these work in R, the extension of these methods will provide an intuitive and natural application to quanteda objects.

new name | original name | methods | output object | family
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
`as.list.simil` | `similarity` | dist | list | (with `textstat_simil`)



### R-like functions

These are functions that are not extensions of base R methods, but rather additions to base R-*like* methods, defined for **quanteda** objects.  These follow the R conventions for naming and syntax as closely as possible.  For instance, `ntype()` and `ntoken()` return the number of types and tokens in an object respectively, similar to `nrow()` and `ncol()` in base R.  (Note the use of singular for the **quanteda** functions, just as in base R.)  Each method is defined for numerous **quanteda** object classes as appropriate.  When a method does not work for a specific object class, for instnace `nfeature()` does not work for a `character` class object, this is by design -- in this case, because features have to be defined through user choice, as "features" do not exist inherently in a character string without the user having first defined and selected features.  

There are some fudges to this, for instance the definition of an `ntoken()` and `ntype()` methods for `character` and `corpus` objects, since tokens (and types) are only countable after the user has tokenized the text.  For this reason, however, `ntoken()` and `ntype()` methods pass through options in `...` to `tokenize()`, which is called as a result of dispatching these methods on `character` and `corpus` objects.  Calls to these functions are performed by the `summary.character()` method, for instance, which returns counts of tokens, types, and sentences.

new name | original name | methods | output object | family
:--------|:------------- |:------- |:------------- |:-------
`ndoc` | - | corpus, dfm | int | quanteda
`nfeature` | - | corpus, dfm | integer | quanteda
`nsentence` | - | character, corpus | integer | quanteda
`ntoken` | - | character, corpus, tokenizedTexts, dfm | integer | quanteda
`ntype` | - | character, corpus, tokenizedTexts, dfm | integer | quanteda
`nscrabble` | `scrabble` | character (vector), tokens | integer | textcount
`nsyllable` | `syllables`   | character (vector), tokens | integer | textcount
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
`docvars` | extracts document variables from a corpus | data.frame
`docvars<-` | sets document variables in a corpus | corpus
`kwic` | performs keyword in context search | data.frame
`plot.dfm` | same as `textplot_wordcloud` for a dfm, and will be undocumented but equivalent to that command | (plot) | textplot
`plot.kwic` | same as `textplot_xray` for a kwic class object, and will be undocumented but equivalent to that command | (plot) | textplot
`topfeatures`  | sorted list of n top occurring features from a dfm | integer | ?
`docfreq`   | document frequency of features | integer | weight


### Functions that will be deprecated and removed

The biggest change here is the phasing out of `textfile()`, the intermediate function to load files into R for use in constructing a corpus.  Instead, this will be put into a new package, called **readtext**.  This package will also handle the encoding (detecting and conversion) issues. This would create a special class of object, from which `quanteda::corpus()` would construct a corpus (with metadata and docvars) directly. *readtext* would also have coercion methods to make a data.frame, extract texts as a character object, etc.  This not only moves this tricky functions outside of **quanteda**, but also makes it possible for others to use the functions for reading texts, even if they are not using quanteda.


function name | replacement name | input object | output object | notes
:--------|:------------- |:------- |:------------- |:-------
`describeTexts` | `describeTexts` | character | |
`clean` |  | | |
`textfile` | (new **readtext** package) | *(files)* | corpus | incorporated into `corpus()`
`as.wfm`|`convert` |   dfm | austin::wfm |
`dfm2ldaformat` | `convert` |  dfm | **lda** input object |
`quantedaformat2dtm` | `convert` |  dfm | tm::DocumentTermMatrix |
`wordlists` | `data_char_wordlists` | *(data)* | character | replaced by
