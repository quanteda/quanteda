#' @name quanteda-package
#' @aliases quanteda
#' @docType package
#' @title An R package for the quantitative analysis of textual data
#' @author Ken Benoit and Paul Nulty
#' @description A set of functions for creating and managing text corpora,
#'   extracting features from text corpora, and analyzing those features using
#'   quantitative methods.
#'   
#'   \pkg{quanteda} makes it easy to manage texts in the form of a corpus,
#'   defined as a collection of texts that includes document-level variables
#'   specific to each text, as well as meta-data for documents and for the
#'   collection as a whole. \pkg{quanteda} includes tools to make it easy and
#'   fast to manuipulate the texts in a corpus, by performing the most common
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
#'   Once constructed, a \pkg{quanteda} "\link{dfm}"" can be easily analyzed using
#'   either \pkg{quanteda}'s built-in tools for scaling document positions,
#'   or used with a number of other text analytic tools, such as: topic models
#'   (including converters for direct use with the topicmodels, LDA, and stm
#'   packages) document scaling (using \pkg{quanteda}'s own functions for the
#'   "wordfish" and "Wordscores" models, direct use with the ca package for
#'   correspondence analysis, or scaling with the austin package) machine
#'   learning through a variety of other packages that take matrix or
#'   matrix-like inputs.
#'   
#'   Additional features of \pkg{quanteda} include: \itemize{ 
#'   \item{the ability to explore texts using \link[=kwic]{key-words-in-context};}
#'   \item{fast computation of a variety of \link[=readability]{readability indexes};}
#'   \item{fast computation of a variety of \link[=lexdiv]{lexical diversity measures};}
#'   \item{quick computation of word or document \link[=similarity]{similarities}, for clustering or to compute distances for other purposes; and}
#'   \item{a comprehensive suite of \link[=summary.corpus]{descriptive statistics on text} such as the number of sentences, words, characters, or
#'   syllables per document.}
#'   }
NULL
