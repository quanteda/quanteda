#' Create a document-feature matrix
#'
#' Construct a sparse document-feature matrix, from a character, [corpus],
#' [tokens], or even other [dfm] object.
#' @param x character, [corpus], [tokens], or [dfm] object
#' @param tolower convert all features to lowercase
#' @param stem if `TRUE`, stem words
#' @param remove a [pattern] of user-supplied features to ignore, such as "stop
#'   words".  To access one possible list (from any list you wish), use
#'   [stopwords()].  The pattern matching type will be set by `valuetype`.  See
#'   also [tokens_select()].  For behaviour of `remove` with `ngrams > 1`, see
#'   Details.
#' @param select a  [pattern]  of user-supplied features to keep, while
#'   excluding all others.  This can be used in lieu of a dictionary if there
#'   are only specific features that a user wishes to keep. To extract only
#'   Twitter usernames, for example, set `select = "@@*"` and make sure that
#'   `split_tags = FALSE` as an additional argument passed to [tokens].
#'   Note: `select = "^@@\\\w+\\\b"` would be the regular expression version of
#'   this matching pattern.  The pattern matching type will be set by
#'   `valuetype`.  See also [tokens_remove()].
#' @param dictionary a [dictionary] object to apply to the tokens when creating
#'   the dfm
#' @param thesaurus a [dictionary] object that will be applied as if `exclusive
#'   = FALSE`. See also [tokens_lookup()].  For more fine-grained control over
#'   this and other aspects of converting features into dictionary/thesaurus
#'   keys from pattern matches to values, consider creating the dfm first, and
#'   then applying [dfm_lookup()] separately, or using [tokens_lookup()] on the
#'   tokenized text before calling `dfm`.
#' @inheritParams valuetype
#' @inheritParams groups
#' @note When `x` is a [dfm], `groups` provides a convenient and fast method of
#'   combining and refactoring the documents of the dfm according to the groups.
#' @param verbose display messages if `TRUE`
#' @param ... additional arguments passed to [tokens]; not used when `x` is a
#'   [dfm]
#' @details The default behaviour for `remove`/`select` when constructing ngrams
#'   using `dfm(x, ` *ngrams > 1*`)` is to remove/select any ngram constructed
#'   from a matching feature.  If you wish to remove these before constructing
#'   ngrams, you will need to first tokenize the texts with ngrams, then remove
#'   the features to be ignored, and then construct the dfm using this modified
#'   tokenization object.  See the code examples for an illustration.
#'
#'   To select on and match the features of a another [dfm], `x` must also be a
#'   [dfm].
#' @return a [dfm-class] object
#' @import Matrix
#' @export
#' @rdname dfm
#' @keywords dfm
#' @seealso  [dfm_select()], [dfm-class]
#' @examples
#' ## for a corpus
#' corp <- corpus_subset(data_corpus_inaugural, Year > 1980)
#' dfm(corp)
#' dfm(corp, tolower = FALSE)
#'
#' # grouping documents by docvars in a corpus
#' dfm(corp, groups = "President", verbose = TRUE)
#'
#' # with English stopwords and stemming
#' dfm(corp, remove = stopwords("english"), stem = TRUE, verbose = TRUE)
#' # works for both words in ngrams too
#' tokens("Banking industry") %>%
#'     tokens_ngrams(n = 2) %>%
#'     dfm(stem = TRUE)
#'
#' # with dictionaries
#' dict <- dictionary(list(christmas = c("Christmas", "Santa", "holiday"),
#'                opposition = c("Opposition", "reject", "notincorpus"),
#'                taxing = "taxing",
#'                taxation = "taxation",
#'                taxregex = "tax*",
#'                country = "states"))
#' dfm(corpus_subset(data_corpus_inaugural, Year > 1900), dictionary = dict)
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
#' dfm(txttweets, select = "#*", split_tags = FALSE)  # keep only hashtags
#' dfm(txttweets, select = "^#.*$", valuetype = "regex", split_tags = FALSE)
#'
#' # for a dfm
#' dfm(corpus_subset(data_corpus_inaugural, Year > 1980), groups = "Party")
#'
dfm <- function(x,
                tolower = TRUE,
                stem = FALSE,
                select = NULL,
                remove = NULL,
                dictionary = NULL,
                thesaurus = NULL,
                valuetype = c("glob", "regex", "fixed"),
                case_insensitive = TRUE,
                groups = NULL,
                verbose = quanteda_options("verbose"),
                ...) {

    if (!is.dfm(x) && is.dfm(select)) {
        stop("selection on a dfm is only available when x is a dfm")
    }

    dfm_env$START_TIME <- proc.time()
    object_class <- class(x)[1]
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
                          case_insensitive = TRUE,
                          groups = NULL,
                          verbose = quanteda_options("verbose"),
                          ...) {
    dfm.tokens(tokens(corpus(x), ...),
        tolower = tolower,
        stem = stem,
        select = select, remove = remove,
        dictionary = dictionary,
        thesaurus = thesaurus,
        valuetype = valuetype,
        case_insensitive = case_insensitive,
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
                       case_insensitive = TRUE,
                       groups = NULL,
                       verbose = quanteda_options("verbose"),
                       ...) {
    dfm.tokens(tokens(x, ...),
               tolower = tolower,
               stem = stem,
               select = select, remove = remove,
               dictionary = dictionary, thesaurus = thesaurus,
               valuetype = valuetype,
               case_insensitive = case_insensitive,
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
                       case_insensitive = TRUE,
                       groups = NULL,
                       verbose = quanteda_options("verbose"),
                       ...) {
    valuetype <- match.arg(valuetype)

    # call tokens only if options given
    if (length(intersect(names(list(...)), names(formals("tokens"))))) {
        x <- tokens(x, ...)
    } else {
        unused_dots(...)
    }

    if (tolower) {
        if (verbose) catm(" ...lowercasing\n", sep = "")
        x <- tokens_tolower(x)
        tolower <- FALSE
    }

    if (verbose) {
        catm(" ...found ",
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
        if (verbose) catm(" ...grouping texts\n")
        x <- tokens_group(x, groups, fill = FALSE)
    }

    # use tokens_lookup for tokens objects
    if (!is.null(dictionary) || !is.null(thesaurus)) {
        if (!is.null(thesaurus)) dictionary <- dictionary(thesaurus)
        if (verbose) catm(" ...")
        x <- tokens_lookup(x, dictionary,
                           exclusive = ifelse(!is.null(thesaurus), FALSE, TRUE),
                           valuetype = valuetype,
                           case_insensitive = case_insensitive,
                           verbose = verbose)
    }

    # use tokens_select for tokens objects
    if (!is.null(c(remove, select))) {
        if (!is.null(remove) & !is.null(select))
            stop("only one of select and remove may be supplied at once")
        if (verbose) catm(" ...")
        x <- tokens_select(x,
                           pattern = if (!is.null(remove)) remove else select,
                           selection = if (!is.null(remove)) "remove" else "keep",
                           valuetype = valuetype,
                           case_insensitive = case_insensitive,
                           verbose = verbose)
    }

    language <- quanteda_options("language_stemmer")
    if (stem) {
        if (verbose) catm(" ...stemming types (", stri_trans_totitle(language), ")\n", sep = "")
        x <- tokens_wordstem(x, language = language)
    }

    # compile the dfm
    type <- types(x)
    attrs <- attributes(x)
    temp <- unclass(x)

    # shift index for padding, if any
    index <- unlist(temp, use.names = FALSE)
    if (attr(temp, "padding")) {
        type <- c("", type)
        index <- index + 1L
    }

    temp <-  sparseMatrix(j = index,
                          p = cumsum(c(1L, lengths(x))) - 1L,
                          x = 1L,
                          dims = c(length(x),
                                   length(type)))
    dfm.dfm(
        build_dfm(
            temp,
            features = type,
            docvars = get_docvars(x, user = TRUE, system = TRUE),
            meta = attrs[["meta"]]),
        tolower = FALSE, stem = FALSE, verbose = verbose
    )
}


#' @noRd
#' @author Kenneth Benoit
#' @import Matrix
#' @importFrom stringi stri_trans_totitle
#' @export
dfm.dfm <- function(x,
                    tolower = TRUE,
                    stem = FALSE,
                    select = NULL,
                    remove = NULL,
                    dictionary = NULL,
                    thesaurus = NULL,
                    valuetype = c("glob", "regex", "fixed"),
                    case_insensitive = TRUE,
                    groups = NULL,
                    verbose = quanteda_options("verbose"),
                    ...) {

    unused_dots(...)

    x <- as.dfm(x)
    valuetype <- match.arg(valuetype)

    if (!is.null(groups)) {
        if (verbose) catm(" ...grouping texts\n")
        x <- dfm_group(x, groups, fill = FALSE)
    }

    if (!is.null(dictionary) || !is.null(thesaurus)) {
        if (!is.null(thesaurus)) dictionary <- dictionary(thesaurus)
        if (verbose) catm(" ...")
        x <- dfm_lookup(x, dictionary,
                        exclusive = ifelse(!is.null(thesaurus), FALSE, TRUE),
                        valuetype = valuetype,
                        case_insensitive = case_insensitive,
                        verbose = verbose)
    }

    if (!is.null(c(remove, select))) {
        if (!is.null(remove) & !is.null(select))
            stop("only one of select and remove may be supplied at once")
        if (verbose) catm(" ...")
        # if ngrams > 1 and remove or select is specified, then convert these
        # into a regex that will remove any ngram containing one of the words
        # if (!identical(field_object(attrs, "ngram"), 1L)) {
        #     remove <- make_ngram_pattern(remove, valuetype,
        #                                  field_object(attrs, "concatenator"))
        #     valuetype <- "regex"
        # }
        x <- dfm_select(x,
                        pattern = if (!is.null(remove)) remove else select,
                        selection = if (!is.null(remove)) "remove" else "keep",
                        valuetype = valuetype,
                        case_insensitive = case_insensitive,
                        verbose = verbose)
    }

    if (tolower) {
        if (verbose) catm(" ...lowercasing\n", sep = "")
        x <- dfm_tolower(x)
    }

    language <- quanteda_options("language_stemmer")
    if (stem) {
        if (verbose)
            catm(" ...stemming features (", stri_trans_totitle(language),
                 ")", sep = "")
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

    if (verbose) {
        catm(" ...complete, elapsed time:",
             format((proc.time() - dfm_env$START_TIME)[3], digits = 3), "seconds.\n")
        catm("Finished constructing a", paste(format(dim(x), big.mark = ",", trim = TRUE), collapse = " x "),
             "sparse dfm.\n")
    }
    return(x)
}


####
#### utility functions
####

## convert patterns (remove and select) to ngram regular expressions
# make_ngram_pattern <- function(features, valuetype, concatenator) {
#     if (valuetype == "glob") {
#         features <- stri_replace_all_regex(features, "\\*", ".*")
#         features <- stri_replace_all_regex(features, "\\?", ".{1}")
#     }
#     features <- paste0("(\\b|(\\w+", concatenator, ")+)",
#                        features, "(\\b|(", concatenator, "\\w+)+)")
#     features
# }

# create an empty dfm for given features and documents
make_null_dfm <- function(feature = NULL, document = NULL) {
    if (is.null(feature)) feature <- character()
    if (is.null(document)) document <- character()
    temp <- as(sparseMatrix(
        i = NULL,
        j = NULL,
        dims = c(length(document), length(feature))
    ), "dgCMatrix")

    build_dfm(temp, feature,
              docvars = make_docvars(length(document), document))
}

# pad dfm with zero-count features
pad_dfm <- function(x, feature) {
    feat_pad <- setdiff(feature, featnames(x))
    if (length(feat_pad)) {
        suppressWarnings(
            x <- cbind(x, make_null_dfm(feat_pad, docnames(x)))
        )
    }
    x <- x[, match(feature, featnames(x))]
    return(x)
}
