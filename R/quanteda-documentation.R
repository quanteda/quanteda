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
#' * the ability to identify [keywords][kwic]
#'   associated with documents or groups of documents;
#' * the ability to explore texts using [keywords-in-context][kwic];
#' * quick computation of word or document statistics, using the 
#'   \pkg{quanteda.textstats} package, for clustering or to compute distances
#'   for other purposes;
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
#'   are converted to regular expressions (using [utils::glob2rx()]) when
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
#'   [valuetype]. Note that an empty pattern (`""`) will match
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
#' @seealso [valuetype], [case_insensitive]
NULL

#' Grouping variable(s) for various functions
#'
#' Groups for aggregation by various functions that take grouping options.
#' @param groups grouping variable for sampling, equal in length to the number
#'   of documents. This will be evaluated in the docvars data.frame, so that
#'   docvars may be referred to by name without quoting. This also changes
#'   previous behaviours for `groups`. See `news(Version >= "3.0", package =
#'   "quanteda")` for details.
#' @param fill logical; if `TRUE` and `groups` is a factor, then use all levels
#'   of the factor when forming the new documents of the grouped object.  This
#'   will result in a new "document" with empty content for levels not observed,
#'   but for which an empty document may be needed.  If `groups` is a factor of
#'   dates, for instance, then `fill = TRUE` ensures that the new object will
#'   consist of one new "document" by date, regardless of whether any documents
#'   previously existed with that date.  Has no effect if the `groups`
#'   variable(s) are not factors.
#' @name groups
#' @seealso [corpus_group()], [tokens_group()], [dfm_group()]
#' @keywords internal
NULL

#' Print methods for quanteda core objects
#'
#' Print method for \pkg{quanteda} objects.  In each `max_n*` option, 0 shows none, and
#' -1 shows all.
#' @name print-methods
#' @rdname print-methods
#' @param x the object to be printed
#' @param max_ndoc max number of documents to print; default is from the
#'   `print_*_max_ndoc` setting of [quanteda_options()]
#' @param show_summary print a brief summary indicating the number of documents
#'   and other characteristics of the object, such as docvars or sparsity.
#' @param ... passed to [base::print()] for [tokens]; unused for all other
#'   objects.
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

#' Modify only documents matching a logical condition
#'
#' Applies the modification only to documents matching a condition.
#' 
#' @param apply_if logical vector of length `ndoc(x)`; documents are modified
#'   only when corresponding values are `TRUE`, others are left unchanged.
#' @name apply_if
#' @keywords internal
NULL
