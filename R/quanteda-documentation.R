#' An R package for the quantitative analysis of textual data
#'
#' @description Functions for creating and managing textual corpora, extracting
#'   features from textual data, and analyzing those features using quantitative
#'   methods.
#'
#' @details \pkg{quanteda} makes it easy to manage texts in the form of a
#'   corpus, defined as a collection of texts that includes document-level
#'   variables specific to each text, as well as meta-data. \pkg{quanteda}
#'   includes tools to make it easy and fast to manipulate the texts in a
#'   corpus, by performing the most common natural language processing tasks
#'   simply and quickly, such as tokenizing, stemming, or forming ngrams.
#'   \pkg{quanteda}'s functions for tokenizing texts and forming multiple
#'   tokenized documents into a document-feature matrix are both extremely fast
#'   and very simple to use. \pkg{quanteda} can segment texts easily by words,
#'   paragraphs, sentences, or even user-supplied delimiters and tags.
#'
#' @details Built on the text processing functions in the \pkg{stringi} package,
#'   which is in turn built on C++ implementation of the ICU libraries for
#'   Unicode text handling, \pkg{quanteda} pays special attention to fast and
#'   correct implementation of Unicode and the handling of text in any character
#'   set.
#'
#' @details \pkg{quanteda} is built for efficiency and speed, through its design
#'   around three infrastructures: the \pkg{stringi} package for text
#'   processing, the \pkg{Matrix} package for sparse matrix objects, and
#'   computationally intensive processing (e.g. for tokens) handled in
#'   parallelized C++. If you can fit it into memory, \pkg{quanteda} will handle
#'   it quickly. (And eventually, we will make it possible to process objects
#'   even larger than available memory.)
#'
#' @details \pkg{quanteda} is principally designed to allow users a fast and
#'   convenient method to go from a corpus of texts to a selected matrix of
#'   documents by features, after defining what the documents and features. The
#'   package makes it easy to redefine documents, for instance by splitting them
#'   into sentences or paragraphs, or by tags, as well as to group them into
#'   larger documents by document variables, or to subset them based on logical
#'   conditions or combinations of document variables. The package also
#'   implements common NLP feature selection functions, such as removing
#'   stopwords and stemming in numerous languages, selecting words found in
#'   dictionaries, treating words as equivalent based on a user-defined
#'   "thesaurus", and trimming and weighting features based on document
#'   frequency, feature frequency, and related measures such as tf-idf.
#'   
#' @details Tools for working with dictionaries are one of \pkg{quanteda}'s
#'   principal strengths, and the package includes several core functions for
#'   preparing and applying dictionaries to texts, for example for lexicon-based
#'   sentiment analysis.
#'
#' @details Once constructed, a \pkg{quanteda} document-feature matrix ("[dfm]")
#'   can be easily analyzed using either \pkg{quanteda}'s built-in tools for
#'   scaling document positions, or used with a number of other text analytic
#'   tools, such as: topic models (including converters for direct use with the
#'   topicmodels, LDA, and stm packages) document scaling (using the
#'   \pkg{quanteda.textmodels} package's functions for the "wordfish" and
#'   "Wordscores" models, or direct use with the **ca** package for
#'   correspondence analysis), or machine learning through a variety of other
#'   packages that take matrix or matrix-like inputs. \pkg{quanteda} includes
#'   functions for converting its core objects, but especially a dfm, into other
#'   formats so that these are easy to use with other analytic packages.
#'
#' @details
#'  Additional features of \pkg{quanteda} include:
#' * powerful, flexible tools for working with [dictionaries][dictionary];
#' * the ability to identify [keywords][quanteda.textstats::textstat_keyness]
#'   associated with documents or groups of documents;
#' * the ability to explore texts using [key-words-in-context][kwic];
#' * quick computation of word or document
#'   [similarities][quanteda.textstats::textstat_simil], for clustering or to
#'   compute distances for other purposes;
#' * a comprehensive suite of [descriptive statistics on text][summary.corpus]
#'   such as the number of sentences, words, characters, or syllables per
#'   document; and
#' * flexible, easy to use graphical tools to portray many of the analyses
#'   available in the package.
#'
#' @section Source code and additional information:
#'
#' <https://github.com/quanteda/quanteda>
#' @import methods Matrix
#' @importFrom Rcpp evalCpp
#' @useDynLib quanteda, .registration = TRUE
"_PACKAGE"

#' Pattern matching using valuetype
#'
#' Pattern matching in \pkg{quanteda} using the `valuetype` argument.
#' @param valuetype the type of pattern matching: `"glob"` for "glob"-style
#'   wildcard expressions; `"regex"` for regular expressions; or `"fixed"` for
#'   exact matching. See [valuetype] for details.
#' @param case_insensitive logical; if `TRUE`, ignore case when matching a
#'   `pattern` or [dictionary] values
#' @details Pattern matching in in \pkg{quanteda} uses "glob"-style pattern
#'   matching as the default, because this is simpler than regular expression
#'   matching while addressing most users' needs.  It is also has the advantage
#'   of being identical to fixed pattern matching when the wildcard characters
#'   (`*` and `?`) are not used. Finally, most [dictionary] formats use glob
#'   matching.
#'
#'   \describe{ \item{`"glob"`}{"glob"-style wildcard expressions, the quanteda
#'   default. The implementation used in \pkg{quanteda} uses `*` to match any
#'   number of any characters including none, and `?` to match any single
#'   character.  See also [utils::glob2rx()] and References below.}
#'   \item{`"regex"`}{Regular expression matching.} \item{`"fixed"`}{Fixed
#'   (literal) pattern matching.} }
#' @note If "fixed" is used with `case_insensitive = TRUE`, features will
#'   typically be lowercased internally prior to matching.  Also, glob matches
#'   are converted to regular expressions (using [glob2rx][utils::glob2rx]) when
#'   they contain wild card characters, and to fixed pattern matches when they
#'   do not.
#' @name valuetype
#' @aliases case_insensitive
#' @seealso [utils::glob2rx()], [glob pattern matching
#'   (Wikipedia)](https://en.wikipedia.org/wiki/Glob_(programming)),
#'   [stringi::stringi-search-regex()], [stringi::stringi-search-fixed()]
#' @keywords internal
NULL

#' Pattern for feature, token and keyword matching
#'
#' Pattern(s) for use in matching features, tokens, and keywords through a
#' [valuetype] pattern.
#' @param pattern a character vector, list of character vectors, [dictionary],
#'   or collocations object.  See [pattern] for details.
#' @details The `pattern` argument is a vector of patterns, including
#'   sequences, to match in a target object, whose match type is specified by
#'   [valuetype()]. Note that an empty pattern (`""`) will match
#'   "padding" in a [tokens] object.
#'   \describe{
#'   \item{`character`}{A character vector of token patterns to be selected
#'   or removed. Whitespace is not privileged, so that in a character vector,
#'   white space is interpreted literally. If you wish to consider
#'   whitespace-separated elements as sequences of tokens, wrap the argument in
#'   [phrase()]. }
#'   \item{`list of character objects`}{If the list elements are character
#'   vectors of length 1, then this is equivalent to a vector of characters.  If
#'   a list element contains a vector of characters longer than length 1, then
#'   for matching will consider these as sequences of matches, equivalent to
#'   wrapping the argument in [phrase()], except for matching to
#'   [dfm] features where this does not apply. }
#'   \item{`dictionary`}{Values in [dictionary] are used as patterns,
#'   for literal matches. Multi-word values are automatically converted into
#'   phrases, so performing selection or compounding using a dictionary is the
#'   same as wrapping the dictionary in [phrase()]. }
#'   \item{`collocations`}{Collocations objects created from
#'   `quanteda.textstats::textstat_collocations()`, which are treated as phrases
#'   automatically.
#'     }
#'   }
#' @name pattern
#' @examples
#' # these are interpreted literally
#' (patt1 <- c("president", "white house", "house of representatives"))
#' # as multi-word sequences
#' phrase(patt1)
#'
#' # three single-word patterns
#' (patt2 <- c("president", "white_house", "house_of_representatives"))
#' phrase(patt2)
#'
#' # this is equivalent to phrase(patt1)
#' (patt3 <- list(c("president"), c("white", "house"),
#'                c("house", "of", "representatives")))
#'
#' # glob expression can be used
#' phrase(patt4 <- c("president?", "white house", "house * representatives"))
#'
#' # this is equivalent to phrase(patt4)
#' (patt5 <- list(c("president?"), c("white", "house"), c("house", "*", "representatives")))
#'
#' # dictionary with multi-word matches
#' (dict1 <- dictionary(list(us = c("president", "white house", "house of representatives"))))
#' phrase(dict1)
#' @keywords internal
NULL

#' Grouping variable(s) for various functions
#'
#' Groups for aggregation by various functions that take grouping options.
#' @param groups grouping variable for sampling, equal in length to the number
#'   of documents. This will be evaluated in the docvars data.frame, so that
#'   docvars may be referred to by name without quoting. This also changes
#'   previous behaviours for `groups`. See [quanteda-deprecations] for details.
#' @param fill logical; if `TRUE` and `groups` is a factor, then use all levels
#'   of the factor when forming the new "documents" of the grouped dfm.  This
#'   will result in a new "document" with empty content, for levels not
#'   observed.  Has no effect if the `groups` variable(s) are not factors.
#' @name groups
#' @keywords internal
NULL

#' Print methods for quanteda core objects
#'
#' Print method for \pkg{quanteda} objects.  In each `max_n*` option, 0 shows none, and
#' -1 shows all.
#' @name print-method
#' @rdname print-method
#' @param x the object to be printed
#' @param max_ndoc max number of documents to print; default is from the
#'   `print_*_max_ndoc` setting of [quanteda_options()]
#' @param show_summary print a brief summary indicating the number of documents
#'   and other characteristics of the object, such as docvars or sparsity.
#' @seealso [quanteda_options()]
#' @examples
#' corp <- corpus(data_char_ukimmig2010)
#' print(corp, max_ndoc = 3, max_nchar = 40)
#'
#' toks <- tokens(corp)
#' print(toks, max_ndoc = 3, max_ntoken = 6)
#'
#' dfmat <- dfm(toks)
#' print(dfmat, max_ndoc = 3, max_nfeat = 10)
NULL

#' quanteda deprecations
#' 
#' Details on major changes in \pkg{quanteda} version 3, including breaking
#' changes.
#' 
#' @section Behaviour changes:
#' * Non-standard evaluation for `by`, in [corpus_sample()], [tokens_sample()],
#'   and [dfm_sample()]. The `by` argument is now evaluated within the docvars
#'   data.frame of the object, so that docvar columns may be specified unquoted.
#'   Prior to v3, `by` took quoted docvar names, including "document" as a
#'   shortcut to `[docid()]`.  These are now evaluated literally and hence cannot
#'   refer to docvar columns or the docid of the object.
#' 
#' @section Deprecations and removals in quanteda v3:
#' The main user-facing changes in version 3 relate to the
#' deprecation or elimination of shortcut steps that allowed functions that
#' required tokens inputs to skip the tokens creation step.  We did this to
#' require users to take more direct control of tokenization options, or to
#' substitute the alternative tokeniser of their choice (and then coercing it to
#' tokens via [as.tokens()]).  This also allows our function behaviour to be
#' more consistent, with each function performing a single task, rather than
#' combining functions (such as tokenisation _and_ constructing a matrix).
#' 
#' The most common example involves constructing a [dfm] directly from a
#' character or corpus object.  Formerly, this would construct a [tokens] object
#' internally before creating the dfm, and allowed passing arguments to
#' [tokens()] via `...`.  This is now deprecated, although still functional with
#' a warning.
#' 
#' We strongly encourage either creating a [tokens] object first, or piping the
#' tokens return to [dfm()] using `%>`.  (See examples below.)
#' 
#' We have also deprecated direct character or corpus inputs to [kwic()], since
#' this also requires a tokenised input.
#' 
#' The full list of deprecations:
#' * `dfm_sample(x, margins = "features")` is deprecated; future versions will
#'   not support sampling on features using `dfm_sample()`.
#' * `dfm.character()` and `dfm.corpus()` are deprecated.  Users should create a
#'   tokens object first, and input that to `dfm()`.
#' * `dfm()`: As of version 3, only tokens objects are supported as inputs to
#'   `dfm()`.  Calling `dfm()` for character or corpus objects is still
#'   functional, but issues a warning.  Convenience passing of arguments to
#'   `tokens()` via `...` for `dfm()` is also deprecated, but undocumented, and
#'   functions only with a warning.  Users should now create a tokens object
#'   (using `tokens()` from character or corpus inputs before calling `dfm()`.
#' * `kwic()`: As of version 3, only tokens objects are supported as inputs to
#'   `kwic()`.  Calling `kwic()` for character or corpus objects is still
#'   functional, but issues a warning.  Passing arguments to `tokens()` via `...`
#'   in `kwic()` is now disabled.  Users should now create a tokens object (using
#'   `tokens()` from character or corpus inputs before calling `kwic()`.
#' * Shortcut arguments to `dfm()` are now deprecated.  These are still active,
#'   with a warning, although they are no longer documented.  These are:
#'     - `stem` -- use [tokens_wordstem()] or [dfm_wordstem()] instead.
#'     - `select`/`remove` -- use [tokens_select()] / [tokens_remove()], or
#'       [dfm_select()] / [dfm_remove()] instead
#'     - `dictionary`, `thesaurus` -- use [tokens_lookup()] or [dfm_lookup()] instead.
#'     - `valuetype`, `case_insensitive` -- these are disabled; for the deprecated arguments that take these qualifiers, they are fixed to the defaults `"glob"` and `TRUE`.
#'     - `groups` -- use [tokens_group()] or [dfm_group()] instead.
#' 
#' The following functionality is removed in version 3:
#' * The `textplot_*()` and `textstat_*()` functions.
#' 
#' * The following functions have been removed:
#'     - all methods for defunct `corpuszip` objects.
#'     - `View()` functions
#'     - `as.wfm()` and `as.DocumentTermMatrix()` (the same functionality is available via `convert()`)
#'     - `metadoc()` and `metacorpus()`
#'     - `corpus_trimsentences()` (replaced by `corpus_trim()`)
#'     - all of the `tortl` functions
#'     - all legacy functions related to the ancient "corpuszip" corpus variant.
#' * `dfm` objects can no longer be used as a `pattern` in `dfm_select()` (formerly deprecated).
#' 
#' @section Moved to other packages:
#' In version 3, we also completed a modularisation of the "quantedaverse"
#' packages begun with version 2. All `textstat_*()` functions are now moved to
#' the \pkg{quanteda.textstats} package, and all `textplot_*()` functions now
#' reside in the \pkg{quanteda.textplots} package.  (The `textmodel_*()`
#' functions were moved to the \pkg{quanteda.textmodels} package with the
#' version 2 release.)
#' 
#' @name quanteda-deprecations
#' @examples
#' # creating a dfm
#' txt <- c("This is a text example.", "With @usernames and #hashtag tokens.")
#' tokens(txt, remove_punct = TRUE) %>%
#'     dfm() %>%
#'     dfm_remove(pattern = c("@*", "#*"))
#'     
#' # keywords-in-context
#' data_corpus_inaugural[1:6] %>%
#'     tokens() %>%
#'     kwic(pattern = "secur*", window = 3)
NULL
