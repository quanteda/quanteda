#' Create a document-feature matrix
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
#'   \link{tokens}.  Note: \code{select = "^@@\\\w+\\\b"} would be the regular
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
#' @details The default behaviour for \code{remove}/\code{select} when
#'   constructing ngrams using \code{dfm(x, } \emph{ngrams > 1}\code{)} is to
#'   remove/select any ngram constructed from a matching feature.  If you wish
#'   to remove these before constructing ngrams, you will need to first tokenize
#'   the texts with ngrams, then remove the features to be ignored, and then
#'   construct the dfm using this modified tokenization object.  See the code
#'   examples for an illustration.
#'
#'   To select on and match the features of a another \link{dfm}, \code{x} must
#'   also be a \link{dfm}.
#' @return a \link{dfm-class} object
#' @import Matrix
#' @export
#' @rdname dfm
#' @keywords dfm
#' @seealso  \code{\link{dfm_select}}, \link{dfm-class}
#' @examples
#' ## for a corpus
#' dfmat1 <- corpus_subset(data_corpus_inaugural, Year > 1980)
#' dfm(dfmat1)
#' dfm(dfmat1, tolower = FALSE)
#'
#' # grouping documents by docvars in a corpus
#' dfm(dfmat1, groups = "President", verbose = TRUE)
#'
#' # with English stopwords and stemming
#' dfm(dfmat1, remove = stopwords("english"), stem = TRUE, verbose = TRUE)
#' # works for both words in ngrams too
#' dfm("Banking industry", stem = TRUE, ngrams = 2)
#'
#' # with dictionaries
#' dfmat2 <- corpus_subset(data_corpus_inaugural, Year > 1900)
#' dict <- dictionary(list(christmas = c("Christmas", "Santa", "holiday"),
#'                opposition = c("Opposition", "reject", "notincorpus"),
#'                taxing = "taxing",
#'                taxation = "taxation",
#'                taxregex = "tax*",
#'                country = "states"))
#' dfm(dfmat2, dictionary = dict)
#'
#'
#' # removing stopwords
#' txt <- "The quick brown fox named Seamus jumps over the lazy dog also named Seamus, with
#'              the newspaper from a boy named Seamus, in his mouth."
#' corp <- corpus(txt)
#' # note: "also" is not in the default stopwords("english")
#' featnames(dfm(corp, select = stopwords("english")))
#' # for ngrams
#' featnames(dfm(corp, ngrams = 2, select = stopwords("english"), remove_punct = TRUE))
#' featnames(dfm(corp, ngrams = 1:2, select = stopwords("english"), remove_punct = TRUE))
#'
#' # removing stopwords before constructing ngrams
#' toks1 <- tokens(char_tolower(txt), remove_punct = TRUE)
#' toks2 <- tokens_remove(toks1, stopwords("english"))
#' toks3 <- tokens_ngrams(toks2, 2)
#' featnames(dfm(toks3))
#'
#' # keep only certain words
#' dfm(corp, select = "*s")  # keep only words ending in "s"
#' dfm(corp, select = "s$", valuetype = "regex")
#'
#' # testing Twitter functions
#' txttweets <- c("My homie @@justinbieber #justinbieber shopping in #LA yesterday #beliebers",
#'                 "2all the ha8ers including my bro #justinbieber #emabiggestfansjustinbieber",
#'                 "Justin Bieber #justinbieber #belieber #fetusjustin #EMABiggestFansJustinBieber")
#' dfm(txttweets, select = "#*", remove_twitter = FALSE)  # keep only hashtags
#' dfm(txttweets, select = "^#.*$", valuetype = "regex", remove_twitter = FALSE)
#'
#' # for a dfm
#' dfmat3 <- dfm(data_corpus_irishbudget2010)
#' dfmat4 <- dfm(dfmat3,
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

    if (!is.dfm(x) && is.dfm(select)) {
        stop("selection on a dfm is only available when x is a dfm")
    }

    dfm_env$START_TIME <- proc.time()
    object_class <- class(x)[1]
    if (object_class == "dfmSparse") object_class <- "dfm"
    if (verbose) message("Creating a dfm from a ", object_class, " input...")
    UseMethod("dfm")
}

#' @rdname dfm
#' @noRd
#' @export
dfm.default <- function(x, ...) {
    stop(friendly_class_undefined_message(class(x), "dfm"))
}

# GLOBAL FOR dfm THAT FUNCTIONS CAN RESET AS NEEDED TO RECORD TIME ELAPSED
dfm_env <- new.env()
dfm_env$START_TIME <- NULL


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
    dfm.tokens(tokens(corpus(x), ...),
        tolower = tolower,
        stem = stem,
        select = select, remove = remove,
        dictionary = dictionary, thesaurus = thesaurus, valuetype = valuetype,
        groups = groups,
        verbose = verbose)
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
    dfm.tokens(tokens(x, ...),
               tolower = tolower,
               stem = stem,
               select = select, remove = remove,
               dictionary = dictionary, thesaurus = thesaurus,
               valuetype = valuetype,
               groups = groups,
               verbose = verbose)
}

#' @noRd
#' @importFrom utils glob2rx
#' @export
dfm.tokens <- function(x,
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

    # call tokens only if options given
    if (length(intersect(names(list(...)), names(formals("tokens"))))) {
        x <- tokens(x, ...)
    } else {
        check_dots(list(...), names(formals("dfm")))
    }

    if (tolower) {
        if (verbose) catm("   ... lowercasing\n", sep = "")
        x <- tokens_tolower(x)
        tolower <- FALSE
    }

    if (verbose) {
        catm("   ... found ",
             format(length(x), big.mark = ","), " document",
             # TODO: replace with: ntoken()
             ifelse(length(x) > 1, "s", ""),
             ", ",
             # TODO: replace with: ntype()
             format(length(types(x)), big.mark = ","),
             " feature",
             ifelse(length(types(x)) > 1, "s", ""),
             "\n", sep = "")
    }

    if (!is.null(groups)) {
        if (verbose) catm("   ... grouping texts\n")
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
        if (!is.null(remove) & !is.null(select))
            stop("only one of select and remove may be supplied at once")
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
    if (attr(x, "what") == "dictionary") {
        attr(result, "what") <- "dictionary"
        attr(result, "dictionary") <- attr(x, "dictionary")
    }
    if (!is.null(attr(x, "docvars"))) {
        result@docvars <- attr(x, "docvars")
    } else {
        result@docvars <- data.frame(matrix(ncol = 0, nrow = length(x)))
    }

    dfm.dfm(result, tolower = FALSE, stem = stem, verbose = verbose)
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

    x <- as.dfm(x)
    valuetype <- match.arg(valuetype)
    check_dots(list(...))

    if (tolower) {
        if (verbose) catm("   ... lowercasing\n", sep = "")
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
        if (!is.null(remove) & !is.null(select))
            stop("only one of select and remove may be supplied at once")
        if (verbose) catm("   ... ")
        # if ngrams > 1 and remove or selct is specified, then convert these
        # into a regex that will remove any ngram containing one of the words
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
        if (verbose)
            catm("   ... stemming features (", stri_trans_totitle(language),
                 ")\n", sep = "")
        nfeat_org <- nfeat(x)
        x <- dfm_wordstem(x, language)
        if (verbose)
            if (nfeat_org - nfeat(x) > 0)
                catm(", trimmed ", nfeat_org - nfeat(x), " feature variant",
                     ifelse(nfeat_org - nfeat(x) != 1, "s", ""),
                     "\n", sep = "")
    }

    # remove any NA named columns
    is_na <- is.na(featnames(x))
    if (any(is_na))
        x <- x[, !is_na, drop = FALSE]

    if (verbose)
        catm("   ... created a",
             paste(format(dim(x), big.mark = ",", trim = TRUE), collapse = " x "),
             "sparse dfm\n   ... complete. \nElapsed time:",
             format( (proc.time() - dfm_env$START_TIME)[3], digits = 3),
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

compile_dfm.tokens <- function(x, verbose = TRUE) {

    types <- types(x)
    x <- unclass(x)

    # shift index for padding, if any
    index <- unlist(x, use.names = FALSE)
    if (attr(x, "padding")) {
        types <- c("", types)
        index <- index + 1
    }

    result <- new("dfm", 
                  sparseMatrix(j = index,
                               p = cumsum(c(1, lengths(x))) - 1,
                               x = 1L,
                               dims = c(length(names(x)), 
                                        length(types))))
    set_dfm_dimnames(result) <- list(names(x), types)
    return(result)
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

# create an empty dfm for given features and documents
make_null_dfm <- function(feature = NULL, document = NULL) {
    if (is.null(feature)) feature <- character()
    if (is.null(document)) document <- character()
    result <- new("dfm",
        as(sparseMatrix(
        i = NULL,
        j = NULL,
        dims = c(length(document), length(feature))
    ), "dgCMatrix"))
    set_dfm_dimnames(result) <- list(document, feature)
    return(result)
}

# pad dfm with zero-count features
pad_dfm <- function(x, feature) {
    feat_pad <- setdiff(feature, featnames(x))
    if (length(feat_pad)) {
        x <- cbind(x, make_null_dfm(feat_pad, docnames(x)))
    }
    x <- x[, feature]
    return(x)
}

# foce dfm conformat for prediction with new data
force_conformance <- function(x, feature, force) {
    if (force) {
        n <- length(featnames(x)) - length(intersect(featnames(x), feature))
        if (n)
            warning(n, " feature", if (n == 1) "" else "s",
                    " in newdata not used in prediction.",
                    call. = FALSE, noBreaks. = TRUE)
        return(dfm_select(x, make_null_dfm(feature)))
    } else {
        if (!identical(featnames(x), feature))
            stop("newdata's feature set is not conformant to model terms.")
        return(x)
    }
}
