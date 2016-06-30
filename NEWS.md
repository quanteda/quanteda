quanteda 0.9.7
==============

## New Features

*  Improved the performance of `selectFeatures.tokenizedTexts()`.  
*  Improved the performance of `rbind.dfm()`.  
*  Added support for different docvars when importing multiple files using `textfile()`.  (#147)  
*  Added support for comparison dispersion plots in `plot.kwic()`. (#146)  
*  Added a corpus constructor method for kwic objects.  
*  Substantially improved the performance of `convert(x, to = "stm")` for dfm export, including adding an argument for meta-data (docvars, in quanteda parlance). (#209)  
*  Internal rewrite of `textfile()`, now supports more file types, more wildcard patterns, and is far more robust generally.  
*  Add support for loading external dictionary formats: 
  - yoshikoder, 
  - lexicoder v2 and v3 (#228)
* Autodetect dictionary file format from file extension, so no longer require `format` keyword for loading dictionaries (#227)
* Improved compatibility with rOpenSci guidelines (#218):
  - Use httr to get remote files
  - Use `messages()` to display messages rather than `print` or `cat`
  - Reorganise sections in README file  
* Added new `punctuation` argument to `collocations()` to provide new options for handling collocations separated by punctuation characters (#220).


## Bug fixes

*  Fixed bug in textfile() where source is a remote .zip set.  (#172)  
*  Fixed bug in wordstem.dfm() that caused an error if supplied a dfm with a feature whose total frequency
   count was zero, or with a feature whose total docfreq was zero.  Fixes #181.  
*  Fix #214 "mysterious stemmed token" bug in `wordstem.dfm()`, introduced in fixing #181.  
*  Fixed previously non-functional `toLower = ` argument in `dfm.tokenizedTexts()`.
*  Fixed some errors in the computation of a few readability formulas (#215).
* Added filenames names to text vectors returned by `textfile` (#221).
* `dictionary()` now works correctly when reading LIWC dictionaries where all terms belong to one key (#229).
* `convert(x, to = "stm") now indexes the dfm components from 1, not 0 (#222).
*  Remove temporary stemmed token (#214).

## Changes

*  Added `warn = FALSE` to the `readLines()` calls in `textfile()`, so that no warnings are issued when files are read that are missing a final EOL or that contain  embedded nuls.
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
   
*  selectFeatures(x, anyDfm) (where the second argument is a dfm) now works with a selection = "remove" option.

*  tokenize.character adds a removeURL option.

*  added a corpus method for data.frame objects, so that a corpus can be constructed directly from a data.frame.  Requires the addition of a `textField` argument (similar to textfile).

*  added `compress.dfm()` to combine identically named columns or rows.  #123

*  Much better `phrasetotoken()`, with additional methods for all combinations of corpus/character v. dictionary/character/collocations.

*  Added a` weight(x, type, ...`) signature where the second argument can be a named numeric vector of weights, not just a label for a type of weight.  Thanks http://stackoverflow.com/questions/36815926/assigning-weights-to-different-features-in-r/36823475#36823475.

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

*  Added `c.corpus()` method for concatenating arbitarily large sets of corpus objects.

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
   to a minimium value (of underdispersion).

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

*  Fixed a problem where wordstem was not working on ngram=1 tokenied objects

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

*  selectFeatures(x, features = <anotherdfm>) is now possible, to produce a selection of features from x identical to those in <dfm>.  Not only are only features kept in x that are in <anotherdfm>, but also fatures in <anotherdfm> not in x are added to x as padded zero counts.  This functionality can also be accessed via dfm(x, keptFeatures = <anotherdfm>).  This is useful when new data used in a test set needs to have identical features as a training set dfm constructed at an earlier stage.

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
* clean() is no more.

API changes
-----------
* addto option removed from dfm()

Imminent Changes
----------------
* change behaviour of `ignoredFeatures` and `removeFeatures()` applied to ngrams; change behaviour of stem = TRUE applied to ngrams (in `dfm()`)
* create ngrams.tokenizedTexts() method, replacing current ngrams(), bigrams()


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

* ie2010Corpus now has UTF-8 encoded texts (previously was unicode escaped for non-ASCII characters)

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

* fixed a bug wherein convert(anydfm, to="tm") created a DocumentTermMatrix, not a 
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
  removes €10bn entirely and not just the €10. clean() now removes http and 
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

