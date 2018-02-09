#' An R package for the quantitative analysis of textual data
#'
#' A set of functions for creating and managing text corpora, extracting
#' features from text corpora, and analyzing those features using quantitative
#' methods.
#'   
#'   \pkg{quanteda} makes it easy to manage texts in the form of a corpus,
#'   defined as a collection of texts that includes document-level variables
#'   specific to each text, as well as meta-data for documents and for the
#'   collection as a whole. \pkg{quanteda} includes tools to make it easy and
#'   fast to manipulate the texts in a corpus, by performing the most common
#'   natural language processing tasks simply and quickly, such as tokenizing,
#'   stemming, or forming ngrams. \pkg{quanteda}'s functions for tokenizing
#'   texts and forming multiple tokenized documents into a document-feature
#'   matrix are both extremely fast and extremely simple to use.
#'   \pkg{quanteda} can segment texts easily by words, paragraphs, sentences,
#'   or even user-supplied delimiters and tags.
#'   
#'   Built on the text processing functions in the \pkg{stringi} package, which
#'   is in turn built on C++ implementation of the ICU libraries for Unicode
#'   text handling, \pkg{quanteda} pays special attention to fast and correct
#'   implementation of Unicode and the handling of text in any character set.
#'   
#'   \pkg{quanteda} is built for efficiency and speed, through its design
#'   around three infrastructures: the \pkg{stringi} package for text
#'   processing, the \pkg{data.table} package for indexing large documents
#'   efficiently, and the \pkg{Matrix} package for sparse matrix objects. If you
#'   can fit it into memory, \pkg{quanteda} will handle it quickly. (And
#'   eventually, we will make it possible to process objects even larger than
#'   available memory.)
#'   
#'   \pkg{quanteda} is principally designed to allow users a fast and
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
#'   Once constructed, a \pkg{quanteda} document-feature matrix ("\link{dfm}") 
#'   can be easily analyzed using
#'   either \pkg{quanteda}'s built-in tools for scaling document positions,
#'   or used with a number of other text analytic tools, such as: topic models
#'   (including converters for direct use with the topicmodels, LDA, and stm
#'   packages) document scaling (using \pkg{quanteda}'s own functions for the
#'   "wordfish" and "Wordscores" models, direct use with the \strong{ca} 
#'   package for
#'   correspondence analysis, or scaling with the austin package) machine
#'   learning through a variety of other packages that take matrix or
#'   matrix-like inputs.
#'   
#'   Additional features of \pkg{quanteda} include: \itemize{ 
#'   \item{powerful, flexible tools for working with \link[=dictionary]{dictionaries};}
#'   \item{the ability to identify \link[=textstat_keyness]{keywords} associated with documents or groups of documents;}
#'   \item{the ability to explore texts using \link[=kwic]{key-words-in-context};}
#'   \item{fast computation of a variety of \link[=textstat_readability]{readability indexes};}
#'   \item{fast computation of a variety of \link[=textstat_lexdiv]{lexical diversity measures};}
#'   \item{quick computation of word or document \link[=textstat_simil]{similarities}, for clustering or to compute distances for other purposes;}
#'   \item{a comprehensive suite of \link[=summary.corpus]{descriptive statistics on text} such as the number of sentences, words, characters, or
#'   syllables per document; and}
#'   \item{flexible, easy to use graphical tools to portray many of the analyses available in the package.}
#'   }
#'   
#' @section Source code and additional information:
#' 
#' \url{http://github.com/quanteda/quanteda}
#' @useDynLib quanteda, .registration = TRUE  
"_PACKAGE"

#' Pattern matching using valuetype
#' 
#' Pattern matching in \pkg{quanteda} using the \code{valuetype} argument.
#' @param valuetype the type of pattern matching: \code{"glob"} for 
#'   "glob"-style wildcard expressions; \code{"regex"} for regular expressions;
#'   or \code{"fixed"} for exact matching. See \link{valuetype} for details.
#' @details Pattern matching in in \pkg{quanteda} uses "glob"-style pattern
#'   matching as the default, because this is simpler than regular expression
#'   matching while addressing most users' needs.  It is also has the advantage
#'   of being identical to fixed pattern matching when the wildcard characters
#'   (`*` and `?`) are not used. Finally, most \link{dictionary} formats use
#'   glob matching.
#'   
#'   \describe{
#'   \item{\code{"glob"}}{"glob"-style wildcard expressions, the quanteda default.  
#'     The implementation used in \pkg{quanteda} uses `*` to match any number of any 
#'     characters including none, and `?` to match any single character.  See also 
#'   \code{\link[utils]{glob2rx}} and References below.}
#'   \item{\code{"regex"}}{Regular expression matching.}
#'   \item{\code{"fixed"}}{Fixed (literal) pattern matching.}
#'   } 
#' @note If "fixed" is used with \code{case_insensitive = TRUE}, features will 
#'   typically be lowercased internally prior to matching.  Also, glob matches
#'   are converted to regular expressions (using \link[utils]{glob2rx}) when
#'   they contain wild card characters, and to fixed pattern matches when they
#'   do not.
#' @name valuetype
#' @seealso \code{\link[utils]{glob2rx}}, 
#' \href{https://en.wikipedia.org/wiki/Glob_(programming)}{glob pattern matching (Wikipedia)}, 
#' \code{\link[stringi]{stringi-search-regex}}, \code{\link[stringi]{stringi-search-fixed}}
#' @keywords internal
NULL

#' Pattern for feature, token and keyword matching
#' 
#' Pattern(s) for use in matching Feature, tokens, and keywords through a
#' \link{valuetype} pattern.
#' @param pattern a character vector, list of character vectors, \link{dictionary},
#' \link{collocations}, or \link{dfm}. See \link{pattern} for details.
#' @details The \code{pattern} argument is a vector of patterns, including sequences, 
#'   to match in a target object, whose match type is specified by \code{\link{valuetype}}.
#'   Note that an empty pattern (\code{""}) will match "padding" in a \link{tokens} object.
#'   \describe{
#'   \item{\code{character}}{A character vector of token patterns to be selected or removed. 
#'     Whitespace is not privileged, so that in a character vector, white space is interpreted 
#'     literally. If you wish to consider whitespace-separated elements as sequences of tokens,
#'     wrap the argument in \code{\link{phrase}}.  
#'     }
#'   \item{\code{list of character objects}}{If the list elements are character vectors of 
#'     length 1, then this is equivalent to a vector of characters.  If a list element contains
#'     a vector of characters longer than length 1, then for matching will consider these
#'     as sequences of matches, equivalent to wrapping the argument in \code{\link{phrase}}, 
#'     except for matching to \link{dfm} features where this does not apply.
#'     }
#'   \item{\code{dictionary}}{Values in \link{dictionary} are used as patterns, for literal matches.  
#'     Multi-word values are automatically converted into phrases, so performing selection or
#'     compounding using a dictionary is the same as wrapping the dictionary in \code{\link{phrase}}.
#'     } 
#'   \item{\code{collocations}}{Collocations objects created from \code{\link{textstat_collocations}},
#'     which are treated as phrases automatically.
#'     }
#'   \item{\code{dfm}}{Only \code{\link{dfm_select}} accepts \code{dfm} as features to create a new \code{dfm}
#'     identical in its feature set, using a fixed match.
#'     }
#'   }
#' @name pattern
#' @examples 
#' # these are interpreted literally
#' (patt1 <- c('president', 'white house', 'house of representatives'))
#' # as multi-word sequences
#' phrase(patt1)
#' 
#' # three single-word patterns
#' (patt2 <- c('president', 'white_house', 'house_of_representatives'))
#' phrase(patt2)
#' 
#' # this is equivalent to phrase(patt1)
#' (patt3 <- list(c('president'), c('white', 'house'), c('house', 'of', 'representatives')))
#'
#' # glob expression can be used 
#' phrase(patt4 <- c('president?', 'white house', 'house * representatives'))
#' 
#' # this is equivalent to phrase(patt4)
#' (patt5 <- list(c('president?'), c('white', 'house'), c('house', '*', 'representatives')))
#' 
#' # dictionary with multi-word matches
#' (dict1 <- dictionary(list(us = c('president', 'white house', 'house of representatives'))))
#' phrase(dict1)
#' @keywords internal
NULL

#' Grouping variable(s) for various functions
#' 
#' Groups for aggregation by various functions that take grouping options. 
#' Groups can be the name(s) of document variables (as a character vector), or
#' variables whose length or number of rows (if a data.frame) equal the number
#' of documents.
#' @param groups either: a character vector containing the names of document 
#'   variables to be used for grouping; or a factor or object that can be 
#'   coerced into a factor equal in length or rows to the number of documents. 
#'   See \link{groups} for details.
#' @name groups
#' @keywords internal
NULL


