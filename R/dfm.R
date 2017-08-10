#' create a document-feature matrix
#' 
#' Construct a sparse document-feature matrix, from a character, \link{corpus}, 
#' \link{tokens}, or even other \link{dfm} object.
#' @param x character, \link{corpus}, \link{tokens}, or \link{dfm} object
#' @param tolower convert all features to lowercase
#' @param stem if \code{TRUE}, stem words
#' @param remove a \link{pattern} of user-supplied features to ignore, such as 
#'   "stop words".  To access one possible list (from any list you wish), use 
#'   \code{\link{stopwords}()}.  The pattern matching type will be set by 
#'   \code{valuetype}.  See also \code{\link{tokens_select}}.  For behaviour of
#'   \code{remove} with \code{ngrams > 1}, see Details.
#' @param select a  \link{pattern}  of user-supplied features to keep, while 
#'   excluding all others.  This can be used in lieu of a dictionary if there 
#'   are only specific features that a user wishes to keep. To extract only 
#'   Twitter usernames, for example, set \code{select = "@@*"} and make sure 
#'   that \code{remove_twitter = FALSE} as an additional argument passed to 
#'   \link{tokenize}.  Note: \code{select = "^@@\\\w+\\\b"} would be the regular
#'   expression version of this matching pattern.  The pattern matching type 
#'   will be set by \code{valuetype}.  See also \code{\link{tokens_remove}}.
#' @param dictionary a \link{dictionary} object to apply to the tokens when 
#'   creating the dfm
#' @param thesaurus a \link{dictionary} object that will be applied as if 
#'   \code{exclusive = FALSE}. See also \code{\link{tokens_lookup}}.  For more 
#'   fine-grained control over this and other aspects of converting features 
#'   into dictionary/thesaurus keys from pattern matches to values, consider 
#'   creating the dfm first, and then applying \code{\link{dfm_lookup}} 
#'   separately, or using \code{\link{tokens_lookup}} on the tokenized text
#'   before calling \code{dfm}.
#' @inheritParams valuetype
#' @inheritParams groups
#' @note When \code{x} is a \link{dfm}, \code{groups} provides a convenient and
#'   fast method of combining and refactoring the documents of the dfm according
#'   to the groups.
#' @param verbose display messages if \code{TRUE}
#' @param ... additional arguments passed to \link{tokens}; not used when \code{x}
#'   is a \link{dfm}
#' @details The default behavior for \code{remove}/\code{select} when 
#'   constructing ngrams using \code{dfm(x, } \emph{ngrams > 1}\code{)} is to 
#'   remove/select any ngram constructed from a matching feature.  If you wish 
#'   to remove these before constructing ngrams, you will need to first tokenize
#'   the texts with ngrams, then remove the features to be ignored, and then 
#'   construct the dfm using this modified tokenization object.  See the code 
#'   examples for an illustration.
#' @return a \link{dfm-class} object
#' @import Matrix
#' @export
#' @rdname dfm
#' @keywords dfm
#' @seealso  \code{\link{dfm_select}}, \link{dfm-class}
#' @examples
#' ## for a corpus
#' corpus_post80inaug <- corpus_subset(data_corpus_inaugural, Year > 1980)
#' dfm(corpus_post80inaug)
#' dfm(corpus_post80inaug, tolower = FALSE)
#' 
#' # grouping documents by docvars in a corpus
#' dfm(corpus_post80inaug, groups = "President", verbose = TRUE)
#' 
#' # with English stopwords and stemming
#' dfm(corpus_post80inaug, remove = stopwords("english"), stem = TRUE, verbose = TRUE)
#' # works for both words in ngrams too
#' dfm("Banking industry", stem = TRUE, ngrams = 2, verbose = FALSE)
#' 
#' # with dictionaries
#' corpus_post1900inaug <- corpus_subset(data_corpus_inaugural, Year > 1900)
#' mydict <- dictionary(list(christmas = c("Christmas", "Santa", "holiday"),
#'                opposition = c("Opposition", "reject", "notincorpus"),
#'                taxing = "taxing",
#'                taxation = "taxation",
#'                taxregex = "tax*",
#'                country = "states"))
#' dfm(corpus_post1900inaug, dictionary = mydict)
#' 
#' 
#' # removing stopwords
#' testText <- "The quick brown fox named Seamus jumps over the lazy dog also named Seamus, with
#'              the newspaper from a boy named Seamus, in his mouth."
#' testCorpus <- corpus(testText)
#' # note: "also" is not in the default stopwords("english")
#' featnames(dfm(testCorpus, select = stopwords("english")))
#' # for ngrams
#' featnames(dfm(testCorpus, ngrams = 2, select = stopwords("english"), remove_punct = TRUE))
#' featnames(dfm(testCorpus, ngrams = 1:2, select = stopwords("english"), remove_punct = TRUE))
#' 
#' # removing stopwords before constructing ngrams
#' tokensAll <- tokens(char_tolower(testText), remove_punct = TRUE)
#' tokensNoStopwords <- removeFeatures(tokensAll, stopwords("english"))
#' tokensNgramsNoStopwords <- tokens_ngrams(tokensNoStopwords, 2)
#' featnames(dfm(tokensNgramsNoStopwords, verbose = FALSE))
#' 
#' # keep only certain words
#' dfm(testCorpus, select = "*s", verbose = FALSE)  # keep only words ending in "s"
#' dfm(testCorpus, select = "s$", valuetype = "regex", verbose = FALSE)
#' 
#' # testing Twitter functions
#' testTweets <- c("My homie @@justinbieber #justinbieber shopping in #LA yesterday #beliebers",
#'                 "2all the ha8ers including my bro #justinbieber #emabiggestfansjustinbieber",
#'                 "Justin Bieber #justinbieber #belieber #fetusjustin #EMABiggestFansJustinBieber")
#' dfm(testTweets, select = "#*", remove_twitter = FALSE)  # keep only hashtags
#' dfm(testTweets, select = "^#.*$", valuetype = "regex", remove_twitter = FALSE)
#' 
#' # for a dfm
#' dfm1 <- dfm(data_corpus_irishbudget2010)
#' dfm2 <- dfm(dfm1, 
#'             groups = ifelse(docvars(data_corpus_irishbudget2010, "party") %in% c("FF", "Green"),
#'                             "Govt", "Opposition"), 
#'             tolower = FALSE, verbose = TRUE)
#' 
dfm <- function(x, 
                tolower = TRUE,
                stem = FALSE,
                select = NULL,
                remove = NULL,
                dictionary = NULL,
                thesaurus = NULL,
                valuetype = c("glob", "regex", "fixed"), 
                groups = NULL, 
                verbose = quanteda_options("verbose"), 
                ...) {
    UseMethod("dfm")
}

# GLOBAL FOR dfm THAT FUNCTIONS CAN RESET AS NEEDED TO RECORD TIME ELAPSED
dfm_env <- new.env()
dfm_env$START_TIME <- NULL  

# dfm function to check that any ellipsis arguments belong only to tokens formals
check_dfm_dots <-  function(dots) {
    if (length(dots) && any(!(names(dots)) %in% names(formals(tokens))))
        warning("Argument", ifelse(length(dots)>1, "s ", " "), names(dots), " not used.", sep = "", noBreaks. = TRUE)
}


#' @rdname dfm
#' @noRd
#' @export
dfm.character <- function(x, 
                          tolower = TRUE,
                          stem = FALSE,
                          select = NULL,
                          remove = NULL,
                          dictionary = NULL,
                          thesaurus = NULL,
                          valuetype = c("glob", "regex", "fixed"),
                          groups = NULL,
                          verbose = quanteda_options("verbose"),
                          ...) {
    
    if (who_called_me_first(sys.calls(), "dfm") == "character") {
        dfm_env$START_TIME <- proc.time()
        if (verbose) catm("Creating a dfm from a character vector ...\n")
    }

    dfm(corpus(x),
        tolower = tolower, 
        stem = stem, 
        select = select, remove = remove, 
        dictionary = dictionary, thesaurus = thesaurus, valuetype = valuetype, 
        groups = groups, 
        verbose = verbose,
        ...)
}


#' @rdname dfm
#' @noRd
#' @export
dfm.corpus <- function(x, 
                       tolower = TRUE,
                       stem = FALSE,
                       select = NULL,
                       remove = NULL,
                       dictionary = NULL,
                       thesaurus = NULL,
                       valuetype = c("glob", "regex", "fixed"),
                       groups = NULL, 
                       verbose = quanteda_options("verbose"),
                       ...) {
    if (who_called_me_first(sys.calls(), "dfm") == "corpus") {
        dfm_env$START_TIME <- proc.time()
        if (verbose) catm("Creating a dfm from a corpus ...\n")
    }
    dfm(tokens(x, ...),  
        tolower = tolower, 
        stem = stem, 
        select = select, remove = remove, 
        dictionary = dictionary, thesaurus = thesaurus, valuetype = valuetype, 
        groups = groups, 
        verbose = verbose)
}    

    
#' @noRd
#' @importFrom utils glob2rx
#' @export
dfm.tokenizedTexts <- function(x, 
                               tolower = TRUE,
                               stem = FALSE, 
                               select = NULL,
                               remove = NULL,
                               dictionary = NULL,
                               thesaurus = NULL,
                               valuetype = c("glob", "regex", "fixed"), 
                               groups = NULL, 
                               verbose = quanteda_options("verbose"), 
                               ...) {

    valuetype <- match.arg(valuetype)
    # set document names if none
    if (is.null(names(x))) {
        names(x) <- paste0(quanteda_options("base_docname"), seq_along(x))
    } 

    if (who_called_me_first(sys.calls(), "dfm") %in% c("tokens", "tokenizedTexts")) {
        dfm_env$START_TIME <- proc.time()
        if (verbose) catm("Creating a dfm from a", class(x)[1], "object ...\n")
        check_dfm_dots(list(...))
        x <- tokens(x, ...)
    }
    
    if (tolower) {
        if (verbose) catm("   ... lowercasing\n", sep="")
        x <- tokens_tolower(x)
        tolower <- FALSE
    }

    if (verbose) {
        catm("   ... found ", 
             format(length(x), big.mark = ","), " document",
             ifelse(length(x) > 1, "s", ""), ### replace with: ntoken()
             ", ",
             format(length(types(x)), big.mark = ","),  ### replace with: ntype()
             " feature",
             ifelse(length(types(x)) > 1, "s", ""),
             "\n", sep="")
    }
    
    if (!is.null(groups)) {
        if (verbose) catm("   ... grouping texts\n") 
        group <- generate_groups(x, groups)
        x <- tokens_group(x, groups)
    }
    
    # use tokens_lookup for tokens objects
    if (!is.null(dictionary) || !is.null(thesaurus)) {
        if (!is.null(thesaurus)) dictionary <- dictionary(thesaurus)
        if (verbose) catm("   ... ")
        x <- tokens_lookup(x, dictionary,
                           exclusive = ifelse(!is.null(thesaurus), FALSE, TRUE),
                           valuetype = valuetype,
                           verbose = verbose)
    }
    
    # use tokens_select for tokens objects
    if (!is.null(c(remove, select))) {
        if (!is.null(remove) & !is.null(select)) stop("only one of select and remove may be supplied at once")
        if (verbose) catm("   ... ")
        x <- tokens_select(x, 
                           pattern = if (!is.null(remove)) remove else select,
                           selection = if (!is.null(remove)) "remove" else "keep",
                           valuetype = valuetype, 
                           verbose = verbose)
    }
    
    # compile the dfm
    result <- compile_dfm(x, verbose = verbose)
    
    # copy, set attributes
    result@ngrams <- as.integer(attr(x, "ngrams"))
    result@skip <- as.integer(attr(x, "skip"))
    result@concatenator <- attr(x, "concatenator")
    if (attr(x, 'what') == "dictionary") {
        attr(result, 'what') <- "dictionary"
        attr(result, 'dictionary') <- attr(x, 'dictionary')
    }
    if (!is.null(attr(x, "docvars"))) {
        result@docvars <- attr(x, "docvars")
    } else {
        result@docvars <- data.frame(row.names = docnames(x))
    }
    
    dfm(result, tolower = FALSE, stem = stem, verbose = verbose)
}

#' @noRd
#' @author Kenneth Benoit
#' @import Matrix
#' @export
dfm.dfm <- function(x, 
                    tolower = TRUE,
                    stem = FALSE,
                    select = NULL,
                    remove = NULL,
                    dictionary = NULL,
                    thesaurus = NULL,
                    valuetype = c("glob", "regex", "fixed"), 
                    groups = NULL, 
                    verbose = quanteda_options("verbose"), 
                    ...) {

    valuetype <- match.arg(valuetype)
    if (length(args <- list(...))) warning("arguments ", names(args), "unused")

    if ((who_called_me_first(sys.calls(), "dfm") == "dfm")) {
        dfm_env$START_TIME <- proc.time()
        if (verbose) catm("Creating a dfm from a", class(x)[1], "object ...\n")
    }

    if (tolower) {
        if (verbose) catm("   ... lowercasing\n", sep="")
        x <- dfm_tolower(x)
    }
    
    if (!is.null(groups)) {
        if (verbose) catm("   ... grouping texts\n") 
        x <- dfm_group(x, groups)
    }
    
    if (!is.null(dictionary) | !is.null(thesaurus)) {
        if (!is.null(thesaurus)) dictionary <- dictionary(thesaurus)
        if (verbose) catm("   ... ")
        x <- dfm_lookup(x, dictionary,
                        exclusive = ifelse(!is.null(thesaurus), FALSE, TRUE),
                        valuetype = valuetype,
                        verbose = verbose)
    }
    
    if (!is.null(c(remove, select))) {
        if (!is.null(remove) & !is.null(select)) stop("only one of select and remove may be supplied at once")
        if (verbose) catm("   ... ")
        # if ngrams > 1 and remove or selct is specified, then convert these into a
        # regex that will remove any ngram containing one of the words
        if (!identical(x@ngrams, 1L)) {
            remove <- make_ngram_pattern(remove, valuetype, x@concatenator)
            valuetype <- "regex"
        }
        x <- dfm_select(x, 
                        pattern = if (!is.null(remove)) remove else select,
                        selection = if (!is.null(remove)) "remove" else "keep",
                        valuetype = valuetype, 
                        verbose = verbose)
    }
    
    language <- quanteda_options("language_stemmer")
    if (stem) {
        if (verbose) catm("   ... stemming features (", stri_trans_totitle(language), ")", sep="")
        oldNfeature <- nfeature(x)
        x <- dfm_wordstem(x, language)
        if (verbose) 
            if (oldNfeature - nfeature(x) > 0) 
                catm(", trimmed ", oldNfeature - nfeature(x), " feature variant",
                     ifelse(oldNfeature - nfeature(x) != 1, "s", ""), "\n", sep = "")
    }
    
    # remove any NA named columns
    if (any(naFeatures <- is.na(featnames(x))))
        x <- x[, -which(naFeatures), drop = FALSE]

    if (verbose) 
        catm("   ... created a", paste(format(dim(x), big.mark=",", trim = TRUE), 
                                       collapse=" x "), 
             "sparse dfm\n   ... complete. \nElapsed time:", 
             format((proc.time() - dfm_env$START_TIME)[3], digits = 3),
             "seconds.\n")
    return(x)
}


####
#### core constructors for dfm 
####

## internal function to compile the dfm
compile_dfm <- function(x, verbose = TRUE) {
    UseMethod("compile_dfm")
}

## internal function to compile the dfm
compile_dfm.tokenizedTexts <- function(x, verbose = TRUE) {

    # index documents
    if (verbose) catm("   ... indexing documents: ", 
                      format(length(x), big.mark=","), " document",
                      ifelse(length(x) > 1, "s", ""), "\n", sep="")
    nTokens <- lengths(x)
    
    # index features
    if (verbose) catm("   ... indexing features: ")
    if (sum(nTokens) == 0) {
        catm("\n   ... Error in dfm.tokenizedTexts(): no features found.\n")
        return(NULL)
    }
    allFeatures <- unlist(x, use.names=FALSE)
    uniqueFeatures <- unique(allFeatures)
    totalfeatures <- length(uniqueFeatures)
    if (verbose) catm(format(totalfeatures, big.mark=","), " feature type",
                      ifelse(totalfeatures > 1, "s", ""), "\n", sep="")
    
    docIndex <- c(rep(seq_along(nTokens), nTokens))
    featureIndex <- match(allFeatures, uniqueFeatures)

    # make the dfm
    temp <- sparseMatrix(i = docIndex, 
                         j = featureIndex, 
                         x = 1L, 
                         dims = c(length(x), length(uniqueFeatures)),
                         dimnames = list(docs = names(x), 
                                      features = uniqueFeatures))
    new("dfmSparse", temp)
}

compile_dfm.tokens <- function(x, verbose = TRUE) {
    
    types <- types(x)
    x <- unclass(x)
    
    # shift index for padding, if any
    index <- unlist(x, use.names = FALSE)
    if (attr(x, 'padding')) {
        types <- c("", types)
        index <- index + 1
    }
    
    temp <- sparseMatrix(j = index, 
                         p = cumsum(c(1, lengths(x))) - 1, 
                         x = 1L, 
                         dims = c(length(names(x)), length(types)),
                         dimnames = list(docs = names(x),
                                         features = as.character(types)))
    new("dfmSparse", temp)
}


####
#### utility functions
####

## convert patterns (remove and select) to ngram regular expressions
make_ngram_pattern <- function(features, valuetype, concatenator) {
    if (valuetype == "glob") {
        features <- stri_replace_all_regex(features, "\\*", ".*")
        features <- stri_replace_all_regex(features, "\\?", ".{1}")
    }
    features <- paste0("(\\b|(\\w+", concatenator, ")+)", 
                       features, "(\\b|(", concatenator, "\\w+)+)")
    features
}

