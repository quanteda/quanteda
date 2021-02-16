# quanteda 2.1.2

## Changes

* `textstat_keyness()` performance is now improved through implementation in (multi-threaded) C++.

## Bug fixes and stability enhancements

* Fixes breaking tests and examples on Solaris platform as well as other changes introduced by changes to the stringi package.

# quanteda 2.1.1

## Changes

## Bug fixes and stability enhancements

* `corpus_reshape()` now allows reshaping back to documents even when segmented texts were of zero length. (#1978)
* Special handling applied for Solaris to some issues breaking on that build, relating to the cacheing in `summary.corpus()`/`textstat_summary()`.


# quanteda 2.1.0

## Changes

* Added `block_size` to `quanteda_options()` to control the number of documents in blocked tokenization.
* Fixed `print.dictionary2()` to control the printing of nested levels with `max_nkey` (#1967)
* Added `textstat_summary()` to provide detailed information about dfm, tokens and corpus objects. It will replace `summary()` in future versions.
* Fixed a performance issue causing slowdowns in tokenizing (using the default `what = "word"`) corpora with large numbers of documents that contain social media tags and URLs that needed to be preserved (such a large corpus of Tweets).
* Updated the (default) "word" tokenizer to preserve hashtags and usernames better with non-ASCII text, and made these patterns user-configurable in `quanteda_options()`.  The following are now preserved: "#政治" as well as Weibo-style hashtags such as "#英国首相#".
* `convert(x, to = "data.frame")` now outputs the first column as "doc_id" rather than "document" since "document" is a commonly occurring term in many texts. (#1918)
* Added new methods `char_select()`, `char_keep()`, and `char_remove()` for easy manipulation of character vectors.
* Added `dictionary_edit()` for easy, interactive editing of dictionaries, plus the functions `char_edit()` and `list_edit()` for editing character and list of character objects.
* Added a method to `textplot_wordcloud()` that plots objects from `textstat_keyness()`, to visualize keywords either by comparison or for the target category only.
* Improved the performance of `kwic()` (#1840).
* Added new `logsmooth` scheme to `dfm_weight()`.
* Added new `textstat_summary()` method, which returns summary information about the tokens/types/features etc in an object.  It also caches summary information so that this can be retrieved on subsequent calls, rather than re-computed.

## Bug fixes and stability enhancements

* Stopped returning `NA` for non-existent features when `n` > `nfeat(x)` in `textstat_frequency(x, n)`.  (#1929)
* Fixed a problem in `dfm_lookup()` and `tokens_lookup()` in which an error was caused when no dictionary key returned a single match (#1946).
* Fixed a bug that caused a `textstat_simil/dist` object converted to a data.frame to drop its `document2` labels (#1939).
* Fixed a bug causing `dfm_match()` to fail on a dfm that included "pads" (`""`). (#1960)
* Updated the `data_dfm_lbgexample` object using more modern dfm internals.
* Updates `textstat_readability()`, `textstat_lexdiv()`, and `nscrabble()` so that empty texts are not dropped in the result. (#1976)


# quanteda 2.0.1

## Changes

* Moved `data_corpus_irishbudget2010` and `data_corpus_dailnoconf1991` to the **quanteda.textmodels** package.
* Em dashes and double dashes between words, whether surrounded by a space or not, are now converted to " - " to distinguish them from infix hyphens.  (#1889)
* Verbose output for dfm and tokens creation is now corrected and more consistent.  (#1894)

## Bug fixes and stability enhancements

* Number removal is now both improved and fixed (#1909).
* Fixed an issue causing CRAN errors in pre-v4, related to the new default of `stringsAsFactors = FALSE` for data.frame objects.
* An error in the print method for dfm objects is now fixed (#1897)
* Fixed a bug in `tokens_replace()` when the pattern was not matched (#1895)
* Fixed the names of dimensions not exchanging when a dfm was transposed (#1903)


# quanteda 2.0

## Changes

**quanteda** 2.0 introduces some major changes, detailed here.

1.  New corpus object structure.  

    The internals of the corpus object have been redesigned, and now are based around a character vector with meta- and system-data in attributes.  These are all updated to work with the existing extractor and replacement functions.  If you were using these before, then you should not even notice the change.  Docvars are now handled separately from the texts, in the same way that  docvars are handled for tokens objects.
    
2.  New metadata handling.

    Corpus-level metadata is now inserted in a user metadata list via `meta()` and `meta<-()`.  `metacorpus()` is kept as a synonym for `meta()`, for backwards compatibility.  Additional system-level corpus information is also recorded, but automatically when an object is created.  
    
    Document-level metadata is deprecated, and now all document-level information is simply a "docvar".  For backward compatibility, `metadoc()` is kept and will insert document variables (docvars) with the name prefixed by an underscore.
    
3.  Corpus objects now store default summary statistics for efficiency.  When these are present, `summary.corpus()` retrieves them rather than computing them on the fly.

4.  New index operators for core objects.  The main change here is to redefine the `$` operator for corpus, tokens, and dfm objects (all objects that retain docvars) to allow this operator to access single docvars by name.  Some other index operators have been redefined as well, such as `[.corpus` returning a slice of a corpus, and `[[.corpus` returning the texts from a corpus.

    See the full details at https://github.com/quanteda/quanteda/wiki/indexing_core_objects.
    
5.  `*_subset()` functions.  

     The `subset` argument now must be logical, and the `select` argument has been removed.  (This is part of `base::subset()` but has never made sense, either in **quanteda** or **base**.)

6.  Return format from `textstat_simil()` and `textstat_dist()`.

    Now defaults to a sparse matrix from the **Matrix** package, but coercion methods are provided for `as.data.frame()`, to make these functions return a data.frame just like the other textstat functions.  Additional coercion methods are provided for `as.dist()`, `as.simil()`, and `as.matrix()`. 
    
7.  settings functions (and related slots and object attributes) are gone.  These are now replaced by a new `meta(x, type = "object")` that records object-specific meta-data, including settings such as the `n` for tokens (to record the `ngrams`).

8.  All included data objects are upgraded to the new formats.  This includes the three corpus objects, the single dfm data object, and the LSD 2015 dictionary object.

9.  New print methods for core objects (corpus, tokens, dfm, dictionary) now exist, each with new global options to control the number of documents shown, as well as the length of a text snippet (corpus), the tokens (tokens), dfm cells (dfm), or keys and values (dictionary).  Similar to the extended printing options for dfm objects, printing of corpus objects now allows for brief summaries of the texts to be printed, and for the number of documents and the length of the previews to be controlled by new global options.

10. All textmodels and related functions have been moved to a new package **quanteda.textmodels**.  This makes them easier to maintain and update, and keeps the size of the core package down.
    
11. **quanteda** v2 implements major changes to the `tokens()` constructor.  These are designed to simplify the code and its maintenance in **quanteda**, to allow users to work with other (external) tokenizers, and to improve consistency across the tokens processing options.  Changes include:

    -  A new method `tokens.list(x, ...)` constructs a `tokens` object from named list of characters, allowing users to tokenize texts using some other function (or package) such as `tokenize_words()`, `tokenize_sentences()`, or `tokenize_tweets()` from the **tokenizers** package, or the list returned by `spacyr::spacy_tokenize()`.  This allows users to use their choice of tokenizer, as long as it returns a named list of characters.  With `tokens.list()`, all tokens processing (`remove_*`) options can be applied, or the list can be converted directly to a `tokens` object without processing using `as.tokens.list()`.
    
    - All tokens options are now _intervention_ options, to split or remove things that by default are not split or removed.  All `remove_*` options to `tokens()` now remove them from tokens objects by calling `tokens.tokens()`, after constructing the object.  "Pre-processing" is now  actually post-processing using `tokens_*()` methods internally, after a conservative tokenization on token boundaries. This both improves performance and improves consistency in handling special characters (e.g. Twitter characters) across different tokenizer engines. (#1503, #1446, #1801)    
    
    Note that `tokens.tokens()` will remove what is found, but cannot "undo" a removal -- for instance it cannot replace missing punctuation characters if these have already been removed.

    - The option `remove_hyphens` is removed and deprecated, but replaced by `split_hyphens`.  This preserves infix (internal) hyphens rather than splitting them.  This behaviour is implemented in both the `what = "word"` and `what = "word2"` tokenizer options.  This option is `FALSE` by default.
    
    -  The option `remove_twitter` has been removed.  The new `what = "word"` is a smarter tokenizer that preserves social media tags, URLs, and email-addresses.  "Tags" are defined as valid social media hashtags and usernames (using Twitter rules for validity) rather than removing the `#` and `@` punctuation characters, even if `remove_punct = TRUE`.
    
## New features

* Changed the default value of the `size` argument in `dfm_sample()` to the number of features, not the number of documents.  (#1643)
* Fixes a few CRAN-related issues (compiler warnings on Solaris and encoding warnings on r-devel-linux-x86_64-debian-clang.)
* Added `startpos` and `endpos` arguments to `tokens_select()`, for selecting on token positions relative to the start or end of the tokens in each document. (#1475)
* Added a `convert()` method for corpus objects, to convert them into data.frame or json formats.
* Added a `spacy_tokenize()` method for corpus objects, to provide direct access via the **spacyr** package.

## Behaviour changes

* Added a `force = TRUE` option and error checking for the situations of applying `dfm_weight()` or `dfm_group()` to a dfm that has already been weighted.  (#1545)  The function `textstat_frequency()` now allows passing this argument to `dfm_group()` via `...`.  (#1646)
* `textstat_frequency()` now has a new argument for resolving ties when ranking term frequencies, defaulting to the "min" method.  (#1634)
* New docvars accessor and replacement functions are available for corpus, tokens, and dfm objects via `$`.  (See Index Operators for Core Objects above.)
* `textstat_entropy()` now produces a data.frame that is more consistent with other `textstat` methods.  (#1690)

## Bug fixes and stability enhancements

*  docnames now enforced to be character (formerly, could be numeric for some objects).
*  docnames are now enforced to be strictly unique for all object classes.
*  Grouping operations in `tokens_group()` and `dfm_group()` are more robust to using multiple grouping variables, and preserve these correctly as docvars in the new dfm.  (#1809)
*  Some fixes to documented ... objects in two functions that were previously causing CRAN check failures on the release of 1.5.2.

## Other improvements

* All of the (three) included corpus objects have been cleaned up and augmented with improved meta-data and docvars.  The inaugural speech corpus, for instance, now includes the President's political party affiliation.



# quanteda 1.5.2

## New features

* Added Yule's I to `textstat_lexdiv()`.
* Added forward compatibility for newer (v2) corpus class objects.
* Added a new function `featfreq()` to compute the overall feature frequencies from a dfm.  

## Bug fixes

* Fixed a bug in `tokens_lookup()` when `exclusive = FALSE` and the tokens object has paddings. (#1743)
* Fixed a bug in `tokens_replace()` (#1765).


# quanteda 1.5.1

## New features

* Added `omit_empty` as an argument to `convert()`, to allow the user to control whether empty documents are excluded from converted dfm objects for certain formats. (#1660)

## Bug fixes and stability enhancements

* Fixed a bug that affects the new `textstat_dist()` and `textstat_simil()`. (#1730)
* Fixed a bug in how `textstat_dist()` and `textstat_simil()` class symmetric matrices.

# quanteda 1.5.0

## New features

* Add `flatten` and `levels` arguments to `as.list.dictionary2()` to enable more flexible conversion of dictionary objects. (#1661)
* In `corpus_sample()`, the `size` now works with the `by` argument, to control the size of units sampled from each group.
* Improvements to `textstat_dist()` and `textstat_simil()`, see below.
* Long tokens are not discarded automatically in the call to `tokens()`. (#1713)

## Behaviour changes

* `textstat_dist()` and `textstat_simil()` now return sparse symmetric matrix objects using classes from the **Matrix** package.  This replaces the former structure based on the `dist` class.  Computation of these classes is now also based on the fast implementation in the **proxyC** package.  When computing similarities, the new `min_simil` argument allows a user to ignore certain values below a specified similarity threshold.  A new coercion method `as.data.frame.textstat_simildist()` now exists for converting these returns into a data.frame of pairwise comparisons.  Existing methods such as `as.matrix()`, `as.dist()`, and `as.list()` work as they did before.
* We have removed the "faith", "chi-squared", and "kullback" methods from `textstat_dist()` and `textstat_simil()` because these were either not symmetric or not invariant to document or feature ordering. Finally, the `selection` argument has been deprecated in favour of a new `y` argument.  
* `textstat_readability()` now defaults to `measure = "Flesch"` if no measure is supplied.  This makes it consistent with `textstat_lexdiv()` that also takes a default measure ("TTR") if none is supplied.  (#1715)
* The default values for `max_nchar` and `min_nchar` in `tokens_select()` are now NULL, meaning they are not applied if the user does not supply values.  Fixes #1713.

## Bug fixes and stability enhancements

* `kwic.corpus()` and `kwic.tokens()` behaviour now aligned, meaning that dictionaries are correctly faceted by key instead of by value. (#1684)
* Improved formatting of `tokens()` verbose output. (#1683)
* Subsetting and printing of subsetted kwic objects is more robust. (#1665)
* The "Bormuth" and "DRP" measures are now fixed for `textstat_readability()`. (#1701)

# quanteda 1.4.1

## Bug fixes and stability enhancements

* Fixed an issue with special handling of whitespace variants that caused a test to fail when running Ubuntu 18.10 system with libicu-dev version 63.1 (#1604).
* Fixed the operation of `docvars<-.corpus()` in a way that solves #1603 (reassignment of docvar names).

# quanteda 1.4.0

## Bug fixes and stability enhancements

* Fixed bug in `dfm_compress()` and `dfm_group()` that changed or deleted docvars attributes of dfm objects (#1506).
* Fixed a bug in `textplot_xray()` that caused incorrect facet labels when a pattern contained multiple list elements or values (#1514).
* `kwic()` now correctly returns the pattern associated with each match as the `"keywords"` attribute, for all `pattern` types (#1515)
* Implemented some improvements in efficiency and computation of unusual edge cases for `textstat_simil()` and `textstat_dist()`.

## New features

* `textstat_lexdiv()` now works on tokens objects, not just dfm objects.  New methods of lexical diversity now include MATTR (the Moving-Average Type-Token Ratio, Covington & McFall 2010) and MSTTR (Mean Segmental Type-Token Ratio).
* New function `tokens_split()` allows splitting single into multiple tokens based on a pattern match. (#1500)
* New function `tokens_chunk()` allows splitting tokens into new documents of equally-sized "chunks". (#1520)
* New function `textstat_entropy()` now computes entropy for a dfm across feature or document margins.
* The documentation for `textstat_readability()` is vastly improved, now providing detailing all formulas and providing full references.
* New function `dfm_match()` allows a user to specify the features in a dfm according to a fixed vector of feature names, including those of another dfm.  Replaces `dfm_select(x, pattern)` where `pattern` was a dfm.
* A new argument `vertex_labelsize` added to `textplot_network()` to allow more precise control of label sizes, either globally or individually.

## Behaviour changes

* `tokens.tokens(x, remove_hyphens = TRUE)` where `x` was generated with `remove_hyphens = FALSE` now behaves similarly to how the same tokens would be handled had this option been called on character input as `tokens.character(x, remove_hyphens = TRUE)`. (#1498)


# quanteda 1.3.14

## Bug fixes and stability enhancements

* Improved the robustness of `textstat_keyness()` (#1482).
* Improved the accuracy of sparsity reporting for the print method of a dfm (#1473).
* Diagonals on a `textstat_simil()` return object coerced to matrix now default to 1.0, rather than 0.0 (#1494).

## New Features

* Added the following measures to `textstat_lexdiv()`: Yule's _K_, Simpson's _D_, and Herdan's _Vm_.

# quanteda 1.3.13

## Bug fixes and stability enhancements

* Fixed a bug causing incorrect counting in `fcm(x, ordered = TRUE)`. (#1413)  Also set the condition that `window` can be of size 1 (formerly the limit was 2 or greater).
* Fixed deprecation warnings from adding a dfm as docvars, and this now imports the feature names as docvar names automatically. (related to #1417)
* Fixed behaviour from `tokens(x, what = "fasterword", remove_separators = TRUE)` so that it correctly splits words separated by `\n` and `\t` characters.  (#1420)
* Add error checking for functions taking dfm inputs in case a dfm has empty features (#1419).
* For `textstat_readability()`, fixed a bug in Dale-Chall-based measures and in the Spache word list measure.  These were caused by an incorrect lookup mechanism but also by limited implementation of the wordlists.  The new wordlists include all of the variations called for in the original measures, but using fast fixed matching. (#1410)
* Fixed problems with basic dfm operations (`rowMeans()`, `rowSums()`, `colMeans()`, `colSums()`) caused by not having access to the **Matrix** package methods.  (#1428)
* Fixed problem in `textplot_scale1d()` when input a predicted wordscores object with `se.fit = TRUE` (#1440).
* Improved the stability of `textplot_network()`. (#1460)

## New Features

* Added new argument `intermediate` to `textstat_readability(x, measure, intermediate = FALSE)`, which if `TRUE` returns intermediate quantities used in the computation of readability statistics.  Useful for verification or direct use of the intermediate quantities.
* Added a new `separator` argument to `kwic()` to allow a user to define which characters will be added between tokens returned from a keywords in context search.  (#1449)
* Reimplemented `textstat_dist()` and `textstat_simil()` in C++ for enhanced performance.  (#1210)
* Added a `tokens_sample()` function (#1478).

## Behaviour changes

* Removed the Hamming distance method from `textstat_dist()` (#1443), based on the reasoning in #1442.
* Removed the "chisquared" and "chisquared2" distance measures from `textstat_simil()`. (#1442)

# quanteda 1.3.4

## Bug fixes and stability enhancements

* Keep encodings of types when a tokens object is recompiled. (#1387)
* More robust handling in `predict.textmodel_worscores()` when training and test feature sets are difference (#1380).  
* `char_segment()` and `corpus_segment()` are more robust to whitespace characters preceding a pattern (#1394).  
* `tokens_ngrams()` is more robust to handling large numbers of documents (#1395).  
* `corpus.data.frame()` is now robust to handling data.frame inputs with improper or missing variable names (#1388).  


## New Features

* Added `as.igraph.fcm()` method for converting an fcm object into an **igraph** graph object.
* Added a `case_insensitive` argument to `char_segment()` and `corpus_segment()`.  


# quanteda 1.3.0

## New Features

* Added `to = "tripletlist"` output type for `convert()`, to convert a dfm into a simple triplet list. (#1321) 
* Added `tokens_tortl()` and `char_tortl()` to add markers for right-to-left language tokens and character objects. (#1322)

## Behaviour changes

* Improved `corpus.kwic()` by adding new arguments `split_context` and `extract_keyword`.
* `dfm_remove(x, selection = anydfm)` is now equivalent to `dfm_remove(x, selection = featnames(anydfm))`.  (#1320)
* Improved consistency of `predict.textmodel_nb()` returns, and added `type = ` argument. (#1329)

## Bug fixes

* Fixed a bug in `textmodel_affinity()` that caused failure when the input dfm had been compiled with `tolower = FALSE`.  (#1338)
* Fixed a bug affecting `tokens_lookup()` and `dfm_lookup()` when `nomatch` is used. (#1347)
* Fixed a problem whereby NA texts created a "document" (or tokens) containing `"NA"` (#1372)

# quanteda 1.2.0

## New Features

* Added an `nsentence()` method for **spacyr** parsed objects.  (#1289)

## Bug fixes and stability enhancements

* Fix bug in `nsyllable()` that incorrectly handled cased words, and returned wrong names with `use.names = TRUE`. (#1282)
* Fix the overwriting of `summary.character()` caused by previous import of the **network** package namespace. (#1285)
* `dfm_smooth()` now correctly sets the smooth value in the dfm (#1274).  Arithmetic operations on dfm objects are now much more consistent and do not drop attributes of the dfm, as sometimes happened with earlier versions.

## Behaviour changes

* `tokens_toupper()` and `tokens_tolower()` no longer remove unused token types.  Solves #1278.
* `dfm_trim()` now takes more options, and these are implemented more consistently.  `min_termfreq` and `max_termfreq` have replaced `min_count` and `max_count`, and these can be modified using a `termfreq_type` argument.  (Similar options are implemented for `docfreq_type`.)  Solves #1253, #1254.
* `textstat_simil()` and `textstat_dist()` now take valid dfm indexes for the relevant margin for the `selection` argument.  Previously, this could also be a direct vector or matrix for comparison, but this is no longer allowed.  Solves #1266.
* Improved performance for `dfm_group()` (#1295).

# quanteda 1.1.1

## New Features

* Added `as.dfm()` methods for **tm** `DocumentTermMatrix` and `TermDocumentMatrix` objects. (#1222)
* `predict.textmodel_wordscores()` now includes an `include_reftexts` argument to exclude training texts from the predicted model object (#1229).  The default behaviour is `include_reftexts = TRUE`, producing the same behaviour as existed before the introduction of this argument.  This allows rescaling based on the reference documents (since rescaling requires prediction on the reference documents) but provides an easy way to exclude the reference documents from the predicted quantities.
* `textplot_wordcloud()` now uses code entirely internal to **quanteda**, instead of using the **wordcloud** package.

## Bug fixes and stability enhancements

* Fixed a problem in the examples for `textplot_scale1d()` by adjusting the refscores for `data_corpus_irishbudget2010`.
* Eliminated unnecessary dependency on the **digest** package.
* Updated the vignette title to be less generic.
* Improved the robustness of `dfm_trim()` and `dfm_weight()` for previously weighted dfm objects and when supplied thresholds are proportions instead of counts.  (#1237)
* Fixed a problem in `summary.corpus(x, n = 101)` when `ndoc(x) > 100` (#1242). 
* Fixed a problem in `predict.textmodel_wordscores(x, rescaling = "mv")` that always reset the reference values for rescaling to the first and second documents (#1251).
* Issues in the color generation and labels for `textplot_keyness()` are now resolved (#1233, #1233).


## Performance improvements

* textmodel methods are now exported, to facilitate extension packages for other textmodel methods (e.g. wordshoal).

## Behaviour changes

* Changed the default in `textmodel_wordfish()` to `sparse = FALSE`, in response to #1216.
* `dfm_group()` now preserves docvars that are constant for the group aggregation (#1228).
* The default threads is now 2, to comply with CRAN policies.  (The user can increase this via `quanteda_options(threads = ...)`.


# quanteda 1.0.0

## New Features

* Added `vertex_labelfont` to `textplot_network()`.
* Added `textmodel_lsa()` for Latent Semantic Analysis models.  
* Added `textmodel_affinity()` for the Perry and Benoit (2017) class affinity scaling model.
* Added Chinese stopwords.
* Added a pkgdown vignette for applications in the Chinese language.
* Added `textplot_network()` function.
* The `stopwords()` function and the associated internal data object `data_char_stopwords` have been removed from **quanteda**, and replaced by equivalent functionality in the **stopwords** package.
* Added `tokens_subset()`, now consistent with other `*_subset()` functions (#1149).

## Bug fixes and stability enhancements

* Performance has been improved for `fcm()` and for `textmodel_wordfish()`.
* `dfm()` now correctly passes through all `...` arguments to `tokens()`.  (#1121)
* All `dfm_*()` functions now work correctly with empty dfm objects.  (#1133)
* Fixed a bug in `dfm_weight()` for named weight vectors (#1150)
* Fixed a bug preventing `textplot_influence()` from working (#1116).

## Behaviour Changes

* The convenience wrappers to `convert()` are simplified and no longer exported.  To convert a dfm, `convert()` is now the only official function.
* `nfeat()` replaces `nfeature()`, which is now deprecated. (#1134)
* `textmodel_wordshoal()` has been removed, and relocated to a new package (**wordshoal**).
* The generic wrapper function `textmodel()`, which used to be a gateway to specific `textmodel_*()` functions, has been removed.
* (Most of) the `textmodel_*()` have been reimplemented to make their behaviour consistent with the `lm/glm()` families of models, including especially how the `predict`, `summary`, and `coef` methods work (#1007, #108). 
* The GitHub home for the repository has been moved to https://github.com/quanteda/quanteda.


# quanteda 0.99.12

## New Features

* `tokens_segment()` has a new `window` argument, permitting selection within an asymmetric window around the `pattern` of selection. (#521)
* `tokens_replace()` now allows token types to be substituted directly and quickly. 
* `textmodel_affinity()` now adds functionality to fit the Perry and Benoit (2017) class affinity model.
* Added a `spacy_parse` method for corpus objects.  Also restored quanteda methods for **spacyr** `spacy_parsed` objects.

## Bug fixes and stability enhancements

* Improved documentation for `textmodel_nb()` (#1010), and made output quantities from the fitted NB model regular matrix objects instead of **Matrix** classes.

## Behaviour Changes

* All of the deprecated functions are now removed. (#991)
* `tokens_group()` is now significantly faster.
* The deprecated "list of characters" `tokenize()` function and all methods associated with the `tokenizedTexts` object types have been removed.
* Added convenience functions for keeping tokens or features: `tokens_keep()`, `dfm_keep()`, and `fcm_keep()`. (#1037)
* `textmodel_NB()` has been replaced by `textmodel_nb()`.

# quanteda 0.99.9

## New Features

* Added methods for changing the docnames of tokens and dfm objects (#987).
* Added new function `textmodel_lsa()` for Latent Semantic Analysis.

## Bug fixes and stability enhancements

* The computation of tfidf has been more thoroughly described in the documentation for this function (#997).
* Fixed a bug discovered in #1011 for unused keys in `tokens_lookup(..., exclusive = FALSE)`.


# quanteda 0.99

## New Features

* Added `tokens_segment()`, which works on tokens objects in the same way as `corpus_segment()` does on corpus objects (#902).
* Added **magrittr** pipe support (#927).  `%>%` can now be used with **quanteda** without needing to attach **magrittr** (or, as many users apparently believe, the entire tidyverse.)  
* `corpus_segment()` now behaves more logically and flexibly, and is clearly differentiated from `corpus_reshape()` in terms of its functionality.  Its documentation is also vastly improved.  (#908)
* Added `data_dictionary_LSD2015`, the Lexicoder Sentiment 2015 dictionary (#963).
* Significant improvements to the performance of `tokens_lookup()` and `dfm_lookup()` (#960).
* New functions `head.corpus()`, `tail.corpus()` provide fast subsetting of the first or last documents in a corpus. (#952)

## Bug fixes and stability enhancements

* Fixed a problem when applying `purrr::map()` to `dfm()` (#928).
* Added documentation for `regex2fixed()` and associated functions.
* Fixed a bug in `textstat_collocations.tokens()` caused by "documents" containing only `""` as tokens. (#940)
* Fixed a bug caused by `cbind.dfm()` when features shared a name starting with `quanteda_options("base_featname")` (#946)
* Improved dictionary handling and creation now correctly handles nested LIWC 2015 categories. (#941)
* Number of threads now set correctly by `quanteda_options()`. (#966)

## Behaviour changes

* `summary.corpus()` now generates a special data.frame, which has its own print method, rather than requiring `verbose = FALSE` to suppress output (#926).
* `textstat_collocations()` is now multi-threaded.
* `head.dfm()`, `tail.dfm()` now behave consistently with base R methods for matrix, with the added argument `nfeature`.  Previously, these methods printed the subset and invisibly returned it.  Now, they simply return the subset. (#952)
*  Dictionary keys are now unique, and if multiple, identical keys are defined for a dictionary when constructed, the values will be merged into the consolidated key. (#959)


# quanteda 0.9.9-65

## New features

*  Improvements and consolidation of methods for detecting multi-word expressions, now active only through `textstat_collocations()`, which computes only the `lambda` method for now, but does so accurately and efficiently.  (#753, #803).  This function is still under development and likely to change further.
*  Added new `quanteda_options` that affect the maximum documents and features displayed by the dfm print method (#756).
*  `ngram` formation is now significantly faster, including with skips (skipgrams).
*  Improvements to `topfeatures()`:
    - now accepts a `groups` argument that can be used to generate lists of top (or bottom) features in a group of texts, including by document (#336).
    - new argument `scheme` that takes the default of (frequency) `"count"` but also a new `"docfreq"` value (#408).
*  New wrapper `phrase()` converts whitespace-separated multi-word patterns into a list of patterns.  This affects the feature/pattern matching in `tokens/dfm_select/remove`, `tokens_compound`, `tokens/dfm_lookup`, and `kwic`.  `phrase()` and the associated changes also make the behaviour of using character vectors, lists of characters, dictionaries, and collocation objects for pattern matches far more consistent.  (See #820, #787, #740, #837, #836, #838)
*  `corpus.Corpus()` for creating a corpus from a **tm** Corpus now works with more complex objects that include document-level variables, such as data from the **manifestoR** package (#849).
*  New plot function `textplot_keyness()` plots term "keyness", the association of words with contrasting classes as measured by `textstat_keyness()`.
*  Added corpus constructor for corpus objects (#690).
*  Added dictionary constructor for dictionary objects (#690).
*  Added a tokens constructor for tokens objects (#690), including updates to `tokens()` that improve the consistency and efficiency of the tokenization.
*  Added new `quanteda_options()`: `language_stemmer` and `language_stopwords`, now used for default in `*_wordstem` functions and `stopwords()` for defaults, respectively.  Also uses this option in `dfm()` when `stem = TRUE`, rather than hard-wiring in the "english" stemmer (#386).
*  Added a new function `textstat_frequency()` to compile feature frequencies, possibly by groups. (#825)
*  Added `nomatch` option to `tokens_lookup()` and `dfm_lookup()`, to provide tokens or feature counts for categories not matched to any dictionary key. (#496)

## Behaviour changes

*  The functions `sequences()` and `collocations()` have been removed and replaced by `textstat_collocations()`.
*  (Finally) we added "will" to the list of English stopwords (#818).
*  `dfm` objects with one or both dimensions having zero length, and empty `kwic` objects now display more appropriately in their print methods (per #811).
*  Pattern matches are now implemented more consistently across functions.  In functions such as `*_select`, `*_remove`, `tokens_compound`, `features` has been replaced by `pattern`, and in `kwic`, `keywords` has been replaced by `pattern`.  These all behave consistently with respect to `pattern`, which now has a unified single help page and parameter description.(#839)  See also above new features related to `phrase()`.
*  We have improved the performance of the C++ routines that handle many of the `tokens_*` functions using hashed tokens, making some of them 10x faster (#853).
*  Upgrades to the `dfm_group()` function now allow "empty" documents to be created using the `fill = TRUE` option, for making documents conform to a selection (similar to how `dfm_select()` works for features, when supplied a dfm as the pattern argument).  The `groups` argument now behaves consistently across the functions where it is used. (#854)
*  `dictionary()` now requires its main argument to be a list, not a series of elements that can be used to build a list.
*  Some changes to the behaviour of `tokens()` have improved the behaviour of  `remove_hyphens = FALSE`, which now behaves more correctly regardless of the setting of `remove_punct` (#887).
*  Improved `cbind.dfm()` function allows cbinding vectors, matrixes, and (recyclable) scalars to dfm objects.

## Bug fixes and stability enhancements

*  For the underlying methods behind `textstat_collocations()`, we corrected the word matching, and lambda and z calculation methods, which were slightly incorrect before.  We also removed the chi2, G2, and pmi statistics, because these were incorrectly calculated for size > 2.  
*  LIWC-formatted dictionary import now robust to assignment to term assignment to missing categories.
*  `textmodel_NB(x, y, distribution = "Bernoulli")` was previously inactive even when this option was set.  It has now been fully implemented and tested (#776, #780).
*  Separators including rare spacing characters are now handled more robustly by the `remove_separators` argument in `tokens()`.  See #796.
*  Improved memory usage when computing `ntoken()` and `ntype()`. (#795)
*  Improvements to `quanteda_options()` now does not throw an error when **quanteda** functions are called directly without attaching the package.  In addition, **quanteda** options can be set now in .Rprofile and will not be overwritten when the options initialization takes place when attaching the package.
*  Fixed a bug in `textstat_readability()` that wrongly computed the number of words with fewer than 3 syllables in a text; this affected the `FOG.NRI` and the `Linsear.Write` measures only.
*  Fixed mistakes in the computation of two docfreq schemes: `"logave"` and `"inverseprob"`.
*  Fixed a bug in the handling of multi-thread options where the settings using `quanteda_options()` did not actually set the number of threads.  In addition, we fixed a bug causing threading to be turned off on macOS (due to a check for a gcc version that is not used for compiling the macOS binaries) prevented multi-threading from being used at all on that platform.
*  Fixed a bug causing failure when functions that use `quanteda_options()` are called without the namespace or package being attached or loaded (#864).
*  Fixed a bug in overloading the View method that caused all named objects in the RStudio/Source pane to be named "x". (#893) 

#  quanteda 0.9.9-50

## New features

* Corpus construction using `corpus()` now works for a `tm::SimpleCorpus` object. (#680)
* Added `corpus_trim()` and `char_trim()` functions for selecting documents or subsets of documents based on sentence, paragraph, or document lengths.
* Conversion of a dfm to an stm object now passes docvars through in the `$meta` of the return object.
* New `dfm_group(x, groups = )` command, a convenience wrapper around `dfm.dfm(x, groups = )` (#725).
* Methods for extending quanteda functions to readtext objects updated to match CRAN release of readtext package.
* Corpus constructor methods for data.frame objects now conform to the "text interchange format" for corpus data.frames, automatically recognizing `doc_id` and `text` fields, which also provides interoperability with the **readtext** package.  corpus construction methods are now more explicitly tailored to input object classes.

## Bug fixes and stability enhancements

* `dfm_lookup()` behaves more robustly on different platforms, especially for keys whose values match no features (#704).
* `textstat_simil()` and `textstat_dist()` no longer take the `n` argument, as this was not sorting features in correct order.
* Fixed failure of `tokens(x, what = "character")` when `x` included Twitter characters `@` and `#` (#637).
* Fixed bug #707 where `ntype.dfm()` produced an incorrect result.
* Fixed bug #706 where `textstat_readability()` and `textstat_lexdiv()` for single-document returns when `drop = TRUE`.
* Improved the robustness of `corpus_reshape()`.
* `print`, and `head`, and `tail` methods for `dfm` are more robust (#684).
* Fixed bug in `convert(x, to = "stm")` caused by zero-count documents and zero-count features in a dfm (#699, #700, #701).  This also removes docvar rows from `$meta` when this is passed through the dfm, for zero-count documents.
* Corrected broken handling of nested Yoshikoder dictionaries in `dictionary()`. (#722)
* `dfm_compress` now preserves a dfm's docvars if collapsing only on the features margin, which means that `dfm_tolower()` and `dfm_toupper()` no longer remove the docvars.
* `fcm_compress()` now retains the fcm class, and generates and error when an asymmetric compression is attempted (#728).
* `textstat_collocations()` now returns the collocations as character, not as a factor (#736)
* Fixed a bug in `dfm_lookup(x, exclusive = FALSE)` wherein an empty dfm ws returned with there was no no match (#116).
* Argument passing through `dfm()` to `tokens()` is now robust, and preserves variables defined in the calling environment (#721).
* Fixed issues related to dictionaries failing when applying `str()`, `names()`, or other indexing operations, which started happening on Linux and Windows platforms following the CRAN move to 3.4.0. (#744)
* Dictionary import using the LIWC format is more robust to improperly formatted input files (#685).
* Weights applied using `dfm_weight()` now print friendlier error messages when the weight vector contains features not found in the dfm.  See [this Stack Overflow question](https://stackoverflow.com/questions/44132313/can-the-anew-dictionary-be-used-for-sentiment-analysis-in-quanteda/) for the use case that sparked this improvement.

# quanteda 0.9.9-24

## New features

* `corpus_reshape()` can now go from sentences and paragraph units back to documents.
* Added a `by = ` argument to `corpus_sample()`, for use in bootstrap resampling of sub-document units.
* Added an experimental method `bootstrap_dfm()` to generate a list of dimensionally-equivalent dfm objects based on sentence-level resampling of the original documents.
* Added option to `tokens()` and `dfm()` for passing docvars through to to tokens and dfm objects, and added `docvars()` and `metadoc()` methods for tokens and dfm class objects.  Overall, the code for docvars and metadoc is now more robust and consistent.  
* `docvars()` on eligible objects that contain no docvars now returns an empty 0 x 0 data.frame (in the spirit of #242).
* Redesigned `textmodel_scale1d` now produces sorted and grouped document positions for fitted wordfish models, and produces a ggplot2 plot object.
* `textmodel_wordfish()` now preserves sparsity while processing the dfm, and uses a fast approximation to an SVD to get starting values.  This also dramatically improves performance in computing this model.  (#482, #124)
* The speed of `kwic()` is now dramatically improved, and also returns an indexed set of tokens that makes subsequent commands on a kwic class object much faster. (#603)
* Package options (for verbose, threads) can now be set or queried using `quanteda_options()`.
* Improved performance and better documentation for `corpus_segment()`. (#634)
* Added functions `corpus_trimsentences()` and `char_trimsentences()` to remove sentences from a corpus or character object, based on token length or pattern matching.
* Added options to `textstat_readability()`: `min_sentence_length` and `max_sentence_length`. (#632)
* Indexing now works for dictionaries, for slicing out keys and values (`[`), or accessing values directly (`[[`).  (#651)
* Began the consolidation of collocation detection and scoring into a new function `textstat_collocations()`, which combines the existing `collocations()` and `sequences()` functions.  (#434)  Collocations now behave as sequences for other functions (such as `tokens_compound()`) and have a greatly improved performance for such uses.

## Behaviour changes 

* `docvars()` now permits direct access to "metadoc" fields (starting with `_`, e.g. `_document`)
* `metadoc()` now returns a vector instead of a data.frame for a single variable, similar to `docvars()`
* Most `verbose` options now take the default from `getOption("verbose")` rather than fixing the value in the function signatures. (#577)
* `textstat_dist()` and `textstat_simil()` now return a matrix if a `selection` argument is supplied, and coercion to a list produces a list of distances or similarities only for that selection.
* All remaining camelCase arguments are gone.  For commonly used ones, such as those in `tokens()`, the old arguments (e.g. `removePunct`) still produce the same behaviour but with a deprecation warning.
* Added `n_target` and `n_reference` columns to `textstat_keyness()` to return counts for each category being compared for keyness.

## Bug fixes

* Fixed an problem in tokens generation for some irregular characters (#554).
* Fixed a problem in setting the parallel thread size on single-core machines (#556).
* Fixed problems for `str()` on a corpus with no docvars (#571).
* `removeURL` in `tokens()` now removes URLs where the first part of the URL is a single letter (#587).
* `dfm_select` now works correctly for ngram features (#589).
* Fixed a bug crashing corpus constructors for character vectors with duplicated names (the cause of #580).
* Fixed a bug in the behaviour for `dfm_select(x, features)` when `features` was a dfm, that failed to produce the intended featnames matches for the output dfm.
* Fixed a bug in `corpus_segment(x, what = "tags")` when a document contained a whitespace just before a tag, at the beginning of the file, or ended with a tag followed by no text (#618, #634).
* Fixed some problems with dictionary construction and reading some dictionary formats (#454, #455, #459).


# quanteda 0.9.9-17

## New features

* `textstat_keyness()` now returns a data.frame with p-values as well as the test statistic, and rownames containing the feature.  This is more consistent with the other textstat functions.
* `tokens_lookup()` implements new rules for nested and linked sequences in dictionary values.  See #502.
* `tokens_compound()` has a new `join` argument for better handling of nested and linked sequences.  See #517.
* Internal operations on `tokens` are now significantly faster due to a reimplementation of the hash table functions in C++. (#510)
* `dfm()` now works with multi-word dictionaries and thesauruses, which previously worked only with `tokens_lookup()`.
* `fcm()` is now parallelized for improved performance on multi-core systems.

## Bug fixes

* Fixed C++ incompatibilities on older platforms due to compiler incompatibilities with the required TBB libraries (for multi-threading) (#531, #532, #535), in addition to safeguarding against other compiler warnings across a variety of new tested undefined behaviours.  
* Fixed a bug in `convert(x, to = "lsa")` that transposed row and column names (#526)
* Added missing `fcm()` method for corpus objects (#538)
* Fixed some minor issues with reading in Lexicoder format dictionaries (Improvements to Lexicoder dictionary handling 


## quanteda 0.9.9-3

## Bug fixes

* Fixed a bug causing `dfm` and `tokens` to break on > 10,000 documents. (#438)
* Fixed a bug in `tokens(x, what = "character", removeSeparators = TRUE)` that returned an empty string.  
* Fixed a bug in `corpus.VCorpus` if the VCorpus contains a single document. (#445)
* Fixed a bug in `dfm_compress` in which the function failed on documents that contained zero feature counts. (#467)
* Fixed a bug in `textmodel_NB` that caused the class priors `Pc` to be refactored alphabetically instead of in the order of assignment (#471), also affecting predicted classes (#476).

## New features

* New textstat function `textstat_keyness()` discovers words that occur at differential rates between partitions of a dfm (using chi-squared, Fisher's exact test, and the G^2 likelihood ratio test to measure the strength of associations).  
* Added 2017-Trump to the inaugural corpus datasets (`data_corpus_inaugual` and `data_char_inaugural`).  
* Improved the `groups` argument in `texts()` (and in `dfm()` that uses this function), which will now coerce to a factor rather than requiring one.
* Added a dfm constructor from dfm objects, with the option of collapsing by groups.
* Added new arguments to `sequences()`: `ordered` and `max_length`, the latter to prevent memory leaks from extremely long sequences.  
* `dictionary()` now accepts YAML as an input file format.
* `dfm_lookup` and `tokens_lookup` now accept a `levels` argument to determine which level of a hierarchical dictionary should be applied. 
* Added `min_nchar` and `max_nchar` arguments to `dfm_select`.  
* `dictionary()` can now be called on the argument of a `list()` without explicitly wrapping it in `list()`.  
* `fcm` now works directly on a dfm object when `context = "documents"`.  


This release has some **major changes to the API**, described below.

## Data objects

### Renamed data objects

new name | original name | notes 
:--------|:------------- |:----- 
`data_char_sampletext` | `exampleString` | 
`data_char_mobydick` | `mobydickText`|
`data_dfm_LBGexample` | `LBGexample` |
`data_char_sampletext` | `exampleString` | 


### Renamed internal data objects

The following objects have been renamed, but will not affect user-level functionality because they are primarily internal.  Their man pages have been moved to a common ?`data-internal` man page, hidden from the index, but linked from some of the functions that use them.

new name | original name | notes 
:--------|:------------- |:----- 
`data_int_syllables` | `englishSyllables` |  (used by `textcount_syllables()`) 
`data_char_wordlists` | `wordlists` |  (used by `readability()`) 
`data_char_stopwords` | `.stopwords` | (used by `stopwords()`

### Deprecated data objects

In v.0.9.9 the old names remain available, but are deprecated.

new name | original name | notes 
:--------|:------------- |:----- 
`data_char_ukimmig2010` | `ukimmigTexts` | 
`data_corpus_irishbudget2010` | `ie2010Corpus` |
`data_char_inaugural` | `inaugTexts` |
`data_corpus_inaugural` | `inaugCorpus` |



## Deprecated functions

The following functions will still work, but issue a deprecation warning:

new function | deprecated function | constructs:
:--------|:------------- |:-------
`tokens` | `tokenize()` | `tokens` class object
`corpus_subset` | `subset.corpus` | `corpus` class object
`corpus_reshape` | `changeunits` | `corpus` class object
`corpus_sample` | `sample`| `corpus` class object
`corpus_segment` | `segment`| `corpus` class object
`dfm_compress` | `compress` | `dfm` class object
`dfm_lookup` | `applyDictionary` | `dfm` class object
`dfm_remove` | `removeFeatures.dfm` | `dfm` class object
`dfm_sample`   | `sample.dfm` | `dfm` class object
`dfm_select` | `selectFeatures.dfm` | `dfm` class object
`dfm_smooth` | `smoother` | `dfm` class object
`dfm_sort`   | `sort.dfm` | `dfm` class object
`dfm_trim`   | `trim.dfm` | `dfm` class object
`dfm_weight` | `weight` | `dfm` class object
`textplot_wordcloud` | `plot.dfm` | (plot)
`textplot_xray` | `plot.kwic`  | (plot)
`textstat_readability` | `readability` | `data.frame` 
`textstat_lexdiv` | `lexdiv` | `data.frame`
`textstat_simil` | `similarity` | `dist`
`textstat_dist` | `similarity` | `dist`
`featnames` | `features` | `character`
`nsyllable` | `syllables` | (named) `integer`
`nscrabble` | `scrabble` | (named) `integer`
`tokens_ngrams` | `ngrams` | `tokens` class object
`tokens_skipgrams` | `skipgrams` | `tokens` class object
`tokens_toupper` | `toUpper.tokens`, `toUpper.tokenizedTexts` | `tokens`, `tokenizedTexts`
`tokens_tolower` | `toLower.tokens`, `toLower.tokenizedTexts` | `tokens`, `tokenizedTexts`
`char_toupper` | `toUpper.character`, `toUpper.character` | `character`
`char_tolower` | `toLower.character`, `toLower.character` | `character`
`tokens_compound` | `joinTokens`, `phrasetotoken` | `tokens` class object


## New functions

The following are new to v0.9.9 (and not associated with deprecated functions):

new function | description | output class
:--------|:------------- |:-------
`fcm()` | constructor for a feature co-occurrence matrix | `fcm` 
`fcm_select` | selects features from an `fcm` | `fcm`
`fcm_remove` | removes features from an `fcm` | `fcm`
`fcm_sort`    | sorts an `fcm` in alphabetical order of its features| `fcm`
`fcm_compress` | compacts an `fcm` | `fcm`
`fcm_tolower` | lowercases the features of an `fcm` and compacts | `fcm`
`fcm_toupper` | uppercases the features of an `fcm` and compacts | `fcm`
`dfm_tolower` | lowercases the features of a `dfm` and compacts | `dfm`
`dfm_toupper` | uppercases the features of a `dfm` and compacts | `dfm`
`sequences`   | experimental collocation detection | `sequences`

## Deleted functions and data objects

new name | reason
:--------|:-------------
`encodedTextFiles.zip` | moved to the [**readtext**](https://github.com/quanteda/readtext) package
`describeTexts` | deprecated several versions ago for `summary.character`
`textfile` | moved to package [**readtext**](https://github.com/quanteda/readtext)
`encodedTexts` | moved to package [**readtext**](https://github.com/quanteda/readtext), as `data_char_encodedtexts`
`findSequences` | replaced by `sequences`


## Other new features

*  `to = "lsa"` functionality added to `convert()` (#414)  
*  Much faster pattern matching in general, through an overhaul of how `valuetype` matches work for many functions.  
*  Added experimental `View` methods for `kwic` objects, based on Javascript Datatables.  
*  `kwic` is completely rewritten, now uses fast hashed index matching in C++ and fully implements vectorized matches (#306) and all `valuetype`s (#307).
*  `tokens_lookup`, `tokens_select`, and `tokens_remove` are faster and use parallelization (based on the TBB library).
*  `textstat_dist` and `textstat_simil` add fast, sparse, and parallel computation of many new distance and similarity matrices.  
*  Added `textmodel_wordshoal` fitting function.
*  Add `max_docfreq` and `min_docfreq` arguments, and better verbose output, to `dfm_trim` (#383).
*  Added support for batch hashing of tokens through `tokens()`, for more memory-efficient token hashing when dealing with very large numbers of documents.  
*  Added support for in-memory compressed corpus objects.
*  Consolidated corpus-level metadata arguments in `corpus()` through the `metacorpus` list argument.  
*  Added Greek stopwords.  (See #282).  
*  Added index handling `[`, `[[`, and `$` for (hashed) `tokens` objects.  
*  Now using ggplot2.
*  Added tokens methods for `collocations()` and `kwic()`.
*  Much improved performance for `tokens_select()` (formerly `selectFeatures.tokens()`).
*  Improved `ngrams()` and `joinTokens()` performance for hashed `tokens` class objects.
*  Improved `dfm.character()` by using new `tokens()` constructor to create hashed tokenized texts by default when creating a dfm, resulting in performance gains when constructing a dfm.  Creating a dfm from a hashed `tokens` object is now 4-5 times faster than the older `tokenizedTexts` object.
*  Added new (hashed) `tokens` class object.
*  Added plot method for fitted `textmodel_wordscores objects`.  
*  Added fast `tokens_lookup()` method (formerly `applyDictionary()`), that also works with 
   dictionaries that have multi-word keys.  Addresses but does not entirely yet solve #188.
*  Added `sparsity()` function to compute the sparsity of a dfm.
*  Added feature co-occurrence matrix functions (`fcm`).



# quanteda 0.9.8

## New Features

*  Improved the performance of `selectFeatures.tokenizedTexts()`.  
*  Improved the performance of `rbind.dfm()`.  
*  Added support for different docvars when importing multiple files using `textfile()`.  (#147)  
*  Added support for comparison dispersion plots in `plot.kwic()`. (#146)  
*  Added a corpus constructor method for kwic objects.  
*  Substantially improved the performance of `convert(x, to = "stm")` for dfm export, including adding an argument for meta-data (docvars, in quanteda parlance). (#209)  
*  Internal rewrite of `textfile()`, now supports more file types, more wildcard patterns, and is far more robust generally.  
*  Add support for loading external dictionary formats: 
  - Yoshikoder, 
  - Lexicoder v2 and v3 (#228)
* Autodetect dictionary file format from file extension, so no longer require `format` keyword for loading dictionaries (#227)
* Improved compatibility with rOpenSci guidelines (#218):
  - Use httr to get remote files
  - Use `messages()` to display messages rather than `print` or `cat`
  - Reorganise sections in README file  
* Added new `punctuation` argument to `collocations()` to provide new options for handling collocations separated by punctuation characters (#220).


## Bug fixes

*  (0.9.8.7) Solved #267 in which `fcm(x, tri = TRUE)` temporarily created a dense logical matrix.
*  (0.9.8.7) Added feature co-occurrence matrix functions (`fcm`).
*  (0.9.8.5) Fixed an incompatibility in sequences.cpp with Solaris x86 (#257)
*  (0.9.8.4) Fix bug in verbose output of dfm that causes misreporting of number of features (#250)
*  (0.9.8.4) Fix a bug in `selectFeatures.dfm()` that ignored `case_insensitive = TRUE` settings (#251) 
   correct the documentation for this function.
*  (0.9.8.3) Fix a bug in `tf(x, scheme = "propmax")` that returned a wrong computation; 
   correct the documentation for this function.
*  (0.9.8.2) Fixed a bug in textfile() causing all texts to have the same name, for types using the "textField" argument (a single file containing multiple documents).  
*  Fixed bug in `phrasetotoken()` where if pattern included a `+` for `valuetype = c("glob", "fixed")` it threw a regex error.  #239  
*  Fixed bug in `textfile()` where source is a remote .zip set.  (#172)  
*  Fixed bug in `wordstem.dfm()` that caused an error if supplied a dfm with a feature whose total frequency
   count was zero, or with a feature whose total docfreq was zero.  Fixes #181.  
*  Fix #214 "mysterious stemmed token" bug in `wordstem.dfm()`, introduced in fixing #181.  
*  Fixed previously non-functional `toLower = ` argument in `dfm.tokenizedTexts()`.
*  Fixed some errors in the computation of a few readability formulas (#215).
* Added filenames names to text vectors returned by `textfile` (#221).
* `dictionary()` now works correctly when reading LIWC dictionaries where all terms belong to one key (#229).
* `convert(x, to = "stm") now indexes the dfm components from 1, not 0 (#222).
*  Remove temporary stemmed token (#214).
*  Fixed bug in textmodel_NB() for non-"uniform" priors (#241)

## Changes

*  Added `warn = FALSE` to the `readLines()` calls in `textfile()`, so that no warnings are issued when files are read that are missing a final EOL or that contain embedded nuls.
* `trim()` now prints an output message even when no features are removed (#223)
* We now skip some platform-dependent tests on CRAN, travis-ci and Windows.



quanteda 0.9.6 
==============

*  Improved Naive Bayes model and prediction, `textmodel(x, y, method = "NB")`, now works correctly on k > 2.

*  Improved tag handling for segment(x, what = "tags")

*  Added valuetype argument to segment() methods, which allows faster and more robust segmentation on large texts.

*  corpus() now converts all hyphen-like characters to simple hyphen

*  segment.corpus() now preserves all existing docvars.

*  corpus documentation now removes the description of the corpus object's structure since too many users were accessing these internal elements directly, which is strongly discouraged, as we are likely to change the corpus internals (soon and often).  Repeat after me: "encapsulation".

*  Improve robustness of `corpus.VCorpus()` for constructing a corpus from a tm Corpus object.

*  Add UTF-8 preservation to ngrams.cpp.

*  Fix encoding issues for textfile(), improve functionality.

*  Added two data objects: Moby Dick is now available as `mobydickText`, without needing to access a zipped text file; `encodedTextFiles.zip` is now a zipped archive of different encodings of (mainly) the UN Declaration of Human Rights, for testing conversions from 8-bit encodings in different (non-Roman) languages.

*  phrasetotoken() now has a method correctly defined for corpus class objects.

*  lexdiv() now works just like readability(), and is faster (based on data.table) and the code is simpler.

*  removed quanteda::df() as a synonym for docfreq(), as this conflicted with stats::df().

*  added version information when package is attached.

*  improved rbind() and cbind() methods for dfm.  Both now take any length sequence of dfms and perform better type checking.  
   rbind.dfm() also knits together dfms with different features, which can be useful for information and retrieval purposes 
   or machine learning.
   
*  `selectFeatures(x, anyDfm)` (where the second argument is a dfm) now works with a selection = "remove" option.

*  tokenize.character adds a removeURL option.

*  added a corpus method for data.frame objects, so that a corpus can be constructed directly from a data.frame.  Requires the addition of a `textField` argument (similar to textfile).

*  added `compress.dfm()` to combine identically named columns or rows.  #123

*  Much better `phrasetotoken()`, with additional methods for all combinations of corpus/character v. dictionary/character/collocations.

*  Added a` weight(x, type, ...`) signature where the second argument can be a named numeric vector of weights, not just a label for a type of weight.  Thanks https://stackoverflow.com/questions/36815926/assigning-weights-to-different-features-in-r/36823475#36823475.

*  `as.data.frame` for dfms now passes `...` to `as.data.frame.matrix`.

*  Fixed bug in `predict.fitted_textmodel_NB()` that caused a failure with k > 2 classes (#129)

*  Improved `dfm.tokenizedTexts()` performance by taking care of zero-token documents more efficiently.

*  `dictionary(file = "liwc_formatted_dict.dic", format = "LIWC")` now handles poorly formatted dictionary files better, such as the Moral Foundations Dictionary in the examples for `?dictionary`.

*  added `as.tokenizedTexts` to coerce any list of characters to a tokenizedTexts object.


Bug fixes >= 0.9.6-3
--------------------    
*  Fix bug in phrasetotoken, signature 'corpus,ANY' that was causing an infinite loop.

*  Fixed bug introduced in commit b88287f (0.9.5-26) that caused a failure in dfm() with empty (zero-token) documents.  Also fixes Issue #168.

*  Fixed bug that caused dfm() to break if no features or only one feature was found.

*  Fixed bug in predict.fitted_textmodel_NB() that caused a failure with k > 2 classes (#129)

Bug fixes
---------
*  Fixed a false-alarm warning message in textmodel_wordfish()

*  Argument defaults for readability.corpus() now same as readability.character().  Fixes #107.

*  Fixed a bug causing LIWC format dictionary imports to fail if extra characters followed the closing % in the file header.

*  Fixed a bug in applyDictionary(x, dictionary, exclusive = FALSE) when the dictionary produced no matches at all, caused by an attempt to negative index a NULL.  #115

*  Fixed #117, a bug where wordstem.tokenizedTexts() removed attributes from the object, causing a failure of dfm.tokenizedTexts().

*  Fixed #119, a bug in selectFeatures.tokenizedTexts(x, features, selection = "remove") that returned a NULL for a document's tokens when no matching pattern for removal was found.

*  Improved the behaviour of the `removeHyphens` option to `tokenize()` when `what = "fasterword"` or `what "fastestword"`. 

*  readability() now returns measures in order called, not function definition order.

*  textmodel(x, model = "wordfish") now removes zero-frequency documents and words prior to calling Rcpp.

*  Fixed a bug in sample.corpus() that caused an error when no docvars existed. #128

quanteda 0.9.4
==============

*  Added presidents' first names to inaugCorpus

*  Added textmodel implementation of multinomial and Bernoulli Naive Bayes.

*  Improved documentation.

*  Added `c.corpus()` method for concatenating arbitrarily large sets of corpus objects.

*  Default for `similarity()` is now `margin = "documents"` -- prevents overly massive results if
   `selection = NULL`.
   
*  Defined `rowMeans()` and `colMeans()` methods for dfm objects.

*  Enhancements to summary.character() and summary.corpus(): Added n = to summary.character(); added pass-through options to tokenize() in summary.corpus() and summary.character() methods; added toLower as an argument to both.

*  Enhancements to corpus object indexing, including [[ and [[<-.


Bug fixes
---------
*  Fixed a bug preventing `smoother()` from working.

*  Fixed a bug in segment.corpus(x, what = "tag") that was failing to recover the tag values after the first text.

*  Fix bug in `plot.dfm(x, comparison = TRUE)` method causing warning about rowMeans() failing.

*  Fixed an issue for `mfdict <- dictionary(file = "http://ow.ly/VMRkL", format = "LIWC")` causing it to fail 
   because of the irregular combination of tabs and spaces in the dictionary file.

*  Fixed an exception thrown by wordstem.character(x) if one element of x was NA.  

*  dfm() on a text or tokenized text containing an NA element now returns a row with 0 feature counts.  Previously 
   it returned a count of 1 for an NA feature.

*  Fix issue #91 removeHyphens = FALSE not working in tokenise for some multiple intra-word hyphens, such as
   "one-of-a-kind"

*  Fixed a bug in `as.matrix.similMatrix()` that caused scrambled conversion when feature sets compared were unequal, which 
   normally occurs when setting `similarity(x, n = <something>)` when n < nfeature(x)

*  Fixed a bug in which a corpusSource object (from `textfile()`) with empty docvars prevented this argument
   from being supplied to `corpus(corpusSourceObject, docvars = something)`.

*  Fixed inaccurate documentation for `weight()`, which previously listed unavailable options.

*  More accurate and complete documentation for `tokenize()`.

*  traps an exception when calling wordstem.tokenizedTexts(x) where x was not word tokenized.

*  Fixed a bug in `textfile()` that prevented passthrough arguments in ..., such as `fileEncoding =` or `encoding =`

*  Fixed a bug in `textfile()` that caused exceptions with input documents containing docvars when there was only a single column of docvars (such as .csv files)


quanteda 0.9.2
==============

*  added new methods for similarity(), including sparse matrix computation for method = "correlation" and "cosine".
   (More planned soon.)  Also allows easy conversion to a matrix using as.matrix() on similarity lists.

*  more robust implementation of LIWC-formatted dictionary file imports

*  better implementation of tf-idf, and relative frequency weighting, especially for very large sparse
   matrix objects.  tf(), idf(), and tfidf() now provide relative term frequency, inverse document frequency,
   and tf-idf directly.

*  textmodel_wordfish() now accepts an integer `dispersionFloor` argument to constrain the phi parameter
   to a minimum value (of underdispersion).

*  textfile() now takes a vector of filenames, if you wish to construct these yourself.  See ?textfile
   examples.
   
*  removeFeatures() and selectFeatures.collocations() now all use a consistent interface and same underlying
   code, with removeFeatures() acting as a wrapper to selectFeatures().
   
*  convert(x, to = "stm") now about 3-4x faster because it uses index positions from the dgCMatrix to convert to the 
   sparse matrix format expected by stm.
   
Bug fixes
---------
*  Fixed a bug in textfile() preventing encodingFrom and encodingTo from working properly.

*  Fixed a nasty bug problem in `convert(x, to = "stm")` that mixed up the word indexes.  Thanks Felix Haass for 
   spotting this!

*  Fixed a problem where wordstem was not working on ngram=1 tokenized objects

*  Fixed toLower(x, keepAcronyms = TRUE) that caused an error when x contained no acronyms.

*  Creating a corpus from a tm VCorpus now works if a "document" is a vector of texts rather than a single text

*  Fixed a bug in texts(x, groups = MORE THAN ONE DOCVAR) that now groups correctly on combinations of multiple groups


quanteda 0.9.0
==============

*  trim() now accepts proportions in addition to integer thresholds.  Also accepts a new sparsity argument, which works
   like tm's removeSparseTerms(x, sparse = ) (for those who really want to think of sparsity this way).

*  [i] and [i, j] indexing of corpus objects is now possible, for extracting texts or docvars using
   convenient notation.  See ?corpus Details.

*  ngrams() and skipgrams() now use the same underlying function, with `skip` replacing the previous
   `window` argument (where a skip = window - 1).  For efficiency, both are now implemented in C++.

*  tokenize() has a new argument, removeHyphens, that controls the treatment of intra-word hyphens.

*  Added new measures from readability for mean syllables per word and mean words per sentence directly.

*  wordstem now works on ngrams (tokenizedTexts and dfm objects).

*  Enhanced operation of kwic(), including the definition of a kwic class object, and a plot method for
   this object (produces a dispersion plot).
   
*  Lots more error checking of arguments passed to ... (and potentially misspecified or misspelled).
   Addresses Issue #62.
   
*  Almost all methods are now methods defined for objects, from a generic.

*  texts(x, groups = ) now allows groups to be factors, not just document variable labels.  There is a new
   method for texts.character(x, groups = ) which is useful for supplying a factor to concatenate character
   objects by group.

Bug Fixes
---------
*  corrected inaccurate printing of valuetype in verbose note of selectFeatures.dfm().  (Did not affect functionality.)

*  fixed broken quanteda.R demo, expanded demonstration code.


quanteda 0.8.6
==============

*  removeFeatures.dfm(x, stopwords), selectFeatures.dfm(x, features), and dfm(x, ignoredFeatures) now work on objects created with ngrams.  (Any ngram containing a stopword is removed.)  Performance on these functions is already good but will be improved further soon.

*  selectFeatures(x, features = <anotherdfm>) is now possible, to produce a selection of features from x identical to those in <dfm>.  Not only are only features kept in x that are in <anotherdfm>, but also features in <anotherdfm> not in x are added to x as padded zero counts.  This functionality can also be accessed via dfm(x, keptFeatures = <anotherdfm>).  This is useful when new data used in a test set needs to have identical features as a training set dfm constructed at an earlier stage.

*  head.dfm() and tail.dfm() methods added.

*  kwic() has new formals and new functionality, including a completely flexible set of matching for phrases, as well as control over how the texts and matching keyword(s) are tokenized.

*  segment(x, what = "sentence"), and changeunits(x, to = "sentences") now uses tokenize(x, what = "sentence").  Annoying warning messages now gone.

*  smoother() and weight() formal "smooth" now changed to "smoothing" to avoid clashes with stats::smooth().

*  Updated `corpus.VCorpus()` to work with recent updates to the **tm** package.

*  added print method for tokenizedTexts

Bug fixes
---------

*  fixed signature error message caused by `weight(x, "relFreq")` and `weight(x, "tfidf")`.  Both now correctly produce objects of class dfmSparse.

*  fixed bug in dfm(, keptFeatures = "whatever") that passed it through as a glob rather than a regex to selectFeatures().  Now takes a regex, as per the manual description.

*  fixed textfeatures() for type json, where now it can call jsonlite::fromJSON() on a file directly.

*  dictionary(x, format = "LIWC") now expanded to 25 categories by default, and handles entries that are listed on multiple lines in .dic files, such as those distributed with the LIWC.

quanteda 0.8.4
==============

*  ngrams() rewritten to accept fully vectorized arguments for `n` and for `window`, thus implementing "skip-grams".
   Separate function skipgrams() behaves in the standard "skipgram" fashion.
   bigrams(), deprecated since 0.7, has been removed from the namespace.

*  corpus() no longer checks all documents for text encoding; rather, this is now based on a random sample of max()

*  wordstem.dfm() both faster and more robust when working with large objects.

*  toLower.NULL() now allows toLower() to work on texts with no words (returns NULL for NULL input)

*  textfile() now works on zip archives of *.txt files, although this may not be entirely portable.


Bug fixes
---------

*  fixed bug in selectFeatures() / removeFeatures() that returned zero features if no features were found matching removal pattern

*  corpus() previously removed document names, now fixed

*  non-portable \donttest{} examples now removed completely from all documentation


quanteda 0.8.2
==============

*  0.8.2-1: Changed R version dependency to 3.2.0 so that Mac binary would build on CRAN.

*  0.8.2-1: `sample.corpus()` now samples documents from a corpus, and `sample.dfm()` samples documents 
   or features from a dfm.  `trim()` method for with `nsample` argument now calls `sample.dfm()`.

* `sample.corpus()` now samples documents from a corpus, and `sample.dfm()` samples documents 
   or features from a dfm.  `trim()` method for with `nsample` argument now calls `sample.dfm()`.

* tokenize improvements for what = "sentence": more robust to specifying options, and does not split
  sentences after common abbreviations such as "Dr.", "Prof.", etc.
  
* corpus() no longer automatically converts encodings detected as non-UTF-8, as this detection is too imprecise.

* new function `scrabble()` computes English Scrabble word values for any text, applying any summary numerical function.

* dfm() now 2x faster, replacing previous data.table matching with direct construction of sparse matrix from match().  
  Code is also much simpler, based on using three new functions that are also available directly:

    * new "dfm" method for removeFeatures()  
    * new "dfm" method: selectFeatures() that is now how features can be added or removed from a dfm, based on vectors of regular expressions, globs, or fixed matching  
    * new "dfm" method: applyDictionary() that can replace features through matching with values in key-value lists from a dictionary class objects, based on vectors of regular expressions, globs, or fixed matching for dictionary values.  All functionality for applying dictionaries now takes place through applyDictionary().


Bug Fixes
---------
* fixed the problem that document names were getting erased in corpus() because stringi functions
  were removing them
* fixed problem in tokenize(x, "character", removePunct = TRUE) that deleted texts that had no punctuation to begin with
* fixed problem in dictionary(, format = "LIWC") causing import to fail for some LIWC dictionaries.
* fixed problem in tokenize(x, ngrams = N) where N > length(x).  Now returns NULL instead of an erroneously tokenized set of ngrams.
* Fixed a bug in `subset.corpus()` related to environments that sometimes caused the method to break if
  nested in function environments.

Deletions
---------
* `clean()` is no more.

API changes
-----------
* `addto` option removed from `dfm()`

Imminent Changes
----------------
* change behaviour of `ignoredFeatures` and `removeFeatures()` applied to ngrams; change behaviour of stem = TRUE applied to ngrams (in `dfm()`)
* create `ngrams.tokenizedTexts()` method, replacing current `ngrams()`, `bigrams()`


quanteda 0.8.0
==============

Syntax changes and workflow streamlining
----------------------------------------
The workflow is now more logical and more streamlined, with a new workflow vignette
as well as a design vignette explaining the principles behind the workflow and the
commands that encourage this workflow.  The document also details the development
plans and things remaining to be done on the project.

Encoding detection and conversion
---------------------------------
Newly rewritten command encoding() detects encoding for character, corpus, and corpusSource objects
(created by textfile).  When creating a corpus using corpus(), detection is automatic to UTF-8 if
an encoding other than UTF-8, ASCII, or ISO-8859-1 is detected.

Major infrastructural changes
-----------------------------
The tokenization, cleaning, lower-casing, and dfm construction functions now use the `stringi` package, based on the ICU library.  This results not only in substantial speed improvements,
but also more correctly handles Unicode characters and strings.

* tokenize() and clean() now using stringi, resulting in much faster 
  performance and more consistent behaviour across platforms.

* tokenize() now works on sentences

* summary.corpus() and summary.character() now use the new tokenization functions
  for counting tokens

* dfm(x, dictionary = mydict) now uses stringi and is both more reliable and
  many many times faster.
  
* phrasetotoken() now using stringi.

* removeFeatures() now using stringi and fixed binary matches on tokenized texts

Other changes
-------------
* textfile has a new option, cache = FALSE, for not writing the data to a temporary file, but rather
  storing the object in memory if that is preferred.
  
* language() is removed.  (See Encoding... section  above for changes to encoding().)

* new object encodedTexts contains some encoded character objects for testing.

* ie2010Corpus now has UTF-8 encoded texts (previously was Unicode escaped for non-ASCII characters)

* texts() and docvars() methods added for corpusSource objects.

* new methods for `tokenizedTexts` objects: `dfm()`, `removeFeatures()`, and `syllables()`

* `syllables()` is now much faster, using matching through `stringi` and merging using `data.table`.

* added `readability()` to compute (fast!) readability indexes on a text or corpus

* tokenize() now creates ngrams of any length, with two new arguments: `ngrams =` <integer vector> and 
  `concatenator = "_"`.  The new arguments to `tokenize()` can be passed through from `dfm()`.

Bug fixes
---------
* fixed a problem in `textfile()` causing it to fail on Windows machines when loading `*.txt`

* nsentence() was not counting sentences correctly if the text was lower-cased - now issues an 
  error if no upper-case characters are detected.  This was also causing readability() to fail.

quanteda 0.7.3
==============
* added an ntoken() method for dfm objects.

* fixed a bug wherein `convert(anydfm, to = "tm")` created a DocumentTermMatrix, not a 
  TermDocumentMatrix.  Now correctly creates a TermDocumentMatrix.  (Both worked
  previously in topicmodels::LDA() so many users may not notice the change.)

quanteda 0.7.2
==============
* phrasetotokens works with dictionaries and collocations, to transform
  multi-word expressions into single tokens in texts or corpora

* dictionaries now redefined as S4 classes

* improvements to collocations(), now does not include tokens that are separated 
  by punctuation

* created tokenizeOnly*() functions, for testing tokenizing separately from cleaning,
  and a cleanC(), where both new separate functions are implemented in C

* tokenize() now has a new option, cpp=TRUE, to use a C++ tokenizer and cleaner, resulting
  in much faster text tokenization and cleaning, including that used in dfm()

* textmodel_wordfish now implemented entirely in C for speed.  No std errors yet but 
  coming soon.  No predict method currently working either.

* ie2010Corpus, and exampleString now moved into quanteda (formerly were only in 
  quantedaData because of non-ASCII characters in each - solved with native2ascii
  and \uXXXX encodings).

* All dependencies, even conditional, to the quantedaData and austin packages
  have been removed.

quanteda 0.7.1
==============
Many major changes to the syntax in this version.

* trimdfm, flatten.dictionary, the textfile functions, dictionary converters
  are all gone from the NAMESPACE

* formals changed a bit in clean(), kwic().  

* compoundWords() -> phrasetotoken()

* Cleaned up minor issues in documentation.

* countSyllables data object renamed to englishSyllables.Rdata, and function
  renamed to syllables().

* stopwordsGet() changed to stopwords().  stopwordsRemove() changed to 
  removeFeatures().

* new dictionary() constructor function that also does import and conversion,
  replacing old readWStatdict and readLIWCdict functions.

* one function to read in text files, called `textsource`, that does the 
  work for different file types based on the filename extension, and works
  also for wildcard expressions (that can link to directories for example)


quanteda 0.7.0
==============
* dfm now sparse by default, implemented as subclasses of the 
  Matrix package.  Option dfm(..., matrixType="sparse") is now
  the default, although matrixType="dense" will still produce the
  old S3-class dfm based on a regular matrix, and all dfm methods
  will still work with this object.

* Improvements to: weight(), print() for dfms.

* New methods for dfms: docfreq(), weight(), summary(), as.matrix(), 
  as.data.frame.


quanteda 0.6.6
==============
* No more depends, all done through imports.  Passes clean check.
  The start of our reliance more on the master branch rather than
  having merges from dev to master happen only once in a blue moon.

* bigrams in dfm() when bigrams=TRUE and ignoredFeatures=<something>
  now removed if any bigram contains an ignoredFeature

* stopwordsRemove() now defined for sparse dfms and for collocations.

* stopwordsRemove() now requires an explicit stopwords=<char> argument,
  to emphasize the user's responsibility for applying stopwords.


quanteda 0.6.5
==============
* New engine for dfm now implemented as standard, using data.table and
  Matrix for fast, efficient (sparse) matrixes.

* Added trigram collocations (n=3) to collocations().

* Improvements to clean(): Minor fixes to clean() so that removeDigits=TRUE 
  removes "€10bn" entirely and not just the "€10". clean() now removes http and 
  https URLs by default, although does not 
  preserve them (yet).  clean also handles numbers better, to remove
  1,000,000 and 3.14159 if removeDigits=TRUE but not crazy8 or 4sure.

* dfm works for documents that contain no features, including for
  dictionary counts. Thanks to Kevin Munger for catching this.

quanteda 0.6.4
==============
* first cut at REST APIs for Twitter and Facebook

* some minor improvements to sentence segmentation

* improvements to package dependencies and imports - but this is ongoing!

* Added more functions to dfms, getting there...

* Added the ability to segment a corpus on tags (e.g. ##TAG1 text text, ##TAG2) 
  and have the document split using the tags as a delimiter and the tag then
  added to the corpus as a docvar.


quanteda 0.6.3
==============

* added textmodel_lda support, including LDA, CTM, and STM.  Added a 
  converter dfm2stmformat() between dfm and stm's input format.

* as.dfm works now for data.frame objects

* added Arabic to list of stopwords.  (Still working on a stemmer for Arabic.)

quanteda 0.6.2
==============

* The first appearance of dfms(), to create a sparse Matrix using the 
  Matrix package.  Eventually this will become the default format for 
  all but small dfms.  Not only is this far more efficient, it is also
  much faster.

* Minor speed gains for clean() -- but still much more work to be done
  with clean().

quanteda 0.6.1
==============

* started textmodel_wordfish, textmodel_ca.  textmodel_wordfish takes an mcmc argument
  that calls JAGS wordfish.

* now depends on ca, austin rather than importing them

* dfm subsetting with [,] now works

* docnames()[], []<-, docvars()[] and []<- now work correctly


quanteda 0.6.0 
==============

* Added textmodel for scaling and prediction methods, including for starters,
  wordscores and naivebayes class models.  LIKELY TO BE BUGGY AND QUIRKY FOR A WHILE.

* Added smoothdfm() and weight() methods for dfms.

* Fixed a bug in segmentSentence().


quanteda 0.5.8 
==============

Classification and scaling methods
----------------------------------

* New dfm methods for fitmodel(), predict(), and specific model fitting
  and prediction methods called by these, for classification and scaling 
  of different "textmodel" types, such as wordscores and Naive Bayes
  (for starters).

quanteda 0.5.7
==============

* added compoundWords() to turn space-delimited phrases into single "tokens".
  Works with dfm(, dictionary=) if the text has been pre-processed with 
  compoundWords() and the dictionary joins phrases with the connector ("_").
  May add this functionality to be more automatic in future versions.

* new keep argument for trimdfm() now takes a regular expression for which 
  feature labels to retain.  New defaults for minDoc and minCount (1 each).

* added nfeature() method for dfm objects.

New arguments for dfm()
-----------------------

* thesaurus: works to record equivalency classes
  as lists of words or regular expressions for a given key/label.

* keep: regular expression pattern match for features to keep


quanteda 0.5.6
==============

* added readLIWCdict() to read LIWC-formatted dictionaries

* fixed a "bug"/feature in readWStatDict() that eliminated wildcards (and all other
  punctuation marks) - now only converts to lower.

* improved clean() functions to better handle Twitter, punctuation, and 
  removing extra whitespace

quanteda 0.5.5
==============

* fixed broken dictionary option in dfm()

* fixed a bug in dfm() that was preventing clean() options from being passed through

* added Dice and point-wise mutual information as association measures for
  collocations()

* added: similarity() to implement similarity measures for documents or features
  as vector representations

* begun: implementing dfm resample methods, but this will need more time to work.  
  (Solution: a three way table where the third dim is the resampled text.)

* added is.resample() for dfm and corpus objects

* added Twitter functions: getTweets() performs a REST search through twitteR,
  corpus.twitter creates a corpus object with test and docvars form each tweet
  (operational but needs work)

* added various resample functions, including making dfm a multi-dimensional object
  when created from a resampled corpus and dfm(, bootstrap=TRUE).

* modified the print.dfm() method.


quanteda 0.5.4 
==============

* updated corpus.directory to allow specification of the file extension mask

* updated docvars<- and metadoc<- to take the docvar names from the assigned data.frame if
  field is omitted. 

* added field to docvars()

* enc argument in corpus() methods now actually converts from enc to "UTF-8"

* started working on clean to give it exceptions for @ # _ for twitter text and
  to allow preservation of underscores used in bigrams/collocations.

* Added: a `+` method for corpus objects, to combine a corpus using this operator.

* Changed and fixed: collocations(), which was not only fatally slow and inefficient,
  but also wrong.  Now is much faster and O(n) because it uses data.table and vector 
  operations only.

* Added: resample() for corpus texts.

quanteda 0.5.3
==============

* added statLexdiv() to compute the lexical diversity of texts from a dfm.

* minor bug fixes; update to print.corpus() output messages.

* added a wrapper function for SnowballC::wordStem, called wordstem(), so that
  this can be imported without loading the whole package.


quanteda 0.5.2
==============

* Added a corpus constructor method for the VCorpus class object from the tm package.

* added zipfiles() to unzip a directory of text files from disk or a URL, for easy
  import into a corpus using corpus.directory(zipfiles())


quanteda 0.5.1
==============

* Fixed all the remaining issues causing warnings in R CMD CHECK, now all are fixed.  
  Mostly these related to documentation.

* Fixed corpus.directory to better implementing naming of docvars, if found.

* Moved twitter.R to the R_NEEDFIXING until it can be made to pass tests.  Apparently
  setup_twitter_oauth() is deprecated in the latest version of the twitteR package.

quanteda 0.5.0
==============

Lots of new functions
---------------------

* plot.dfm method for producing word clouds from dfm objects

* print.dfm, print.corpus, and summary.corpus methods now defined

* new accessor functions defined, such as docnames(), settings(), docvars(),
  metadoc(), metacorpus(), encoding(), and language()

* replacement functions defined that correspond to most of the above
  accessor functions, e.g. encoding(mycorpus) <- "UTF-8"

* segment(x, to=c("tokens", "sentences", "paragraphs", "other", ...) now
  provides an easy and powerful method for segmenting a corpus by units
  other than just tokens

* a settings() function has been added to manage settings that would commonly govern
  how texts are converted for processing, so that these can be preserved in a corpus
  and applied to operations that are relevant.  These settings also propagate to a
  dfm for both replication purposes and to govern operations for which they would be
  relevant, when applied to a dfm.

Old functions vastly improved
-----------------------------

* better ways now exist to manage corpus internals, such as through the 
  accessor functions, rather than trying to access the internal structure of 
  the corpus directly.

* basic functions such as tokenize(), clean(), etc are now faster, neater, and
  operate generally on vectors and return consistent object types

Better object and class design
------------------------------

* the corpus object has been redesigned with more flexible components, including
  a settings list, better corpus-level metadata, and smarter implementation of 
  document-level attributes including user-defined variables (docvars) and document-
  level meta-data (metadoc)

* the dfm now has a proper class definition, including additional attributes that 
  hold the settings used to produce the dfm.

* all important functions are now defined as methods for classes of built-in (e.g.
  character) objects, or quanteda objects such as a corpus or dfm.  Lots of functions
  operate on both, for instance dfm.corpus(x) and dfm.character(x).

more complete documentation
---------------------------

* all functions are now documented and have working examples

* quanteda.pdf provides a pdf version of the function documentation in one easy-to-access
  document

