#' Create a document-feature matrix
#'
#' Construct a sparse document-feature matrix, from a character, [corpus],
#' [tokens], or even other [dfm] object.
#' @param x a [tokens] or [dfm] object
#' @param tolower convert all features to lowercase
#' @param stem if `TRUE`, stem words
#' @param remove a [pattern] of user-supplied features to ignore, such as "stop
#'   words".  To access one possible list (from any list you wish), use
#'   [stopwords()].  The pattern matching type will be set by `valuetype`.  See
#'   also [tokens_select()].  
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
#' @return a [dfm-class] object
#' @import Matrix
#' @export
#' @rdname dfm
#' @keywords dfm
#' @seealso  [dfm_select()], [dfm-class]
#' @examples
#' ## for a corpus
#' toks <- data_corpus_inaugural %>%
#'   corpus_subset(Year > 1980) %>%
#'   tokens()
#' dfm(toks)
#'
#' # removal options
#' toks <- tokens(c("a b c", "A B C D")) %>%
#'     tokens_remove("b", padding = TRUE)
#' toks
#' dfm(toks)                
#' dfm(toks, remove = "") # remove "pads"
#' 
#' # preserving case
#' dfm(toks, tolower = FALSE)
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
    check_class(class(x), "dfm")
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
    .Deprecated(msg = "'dfm.character()' is deprecated. Use 'tokens()' first.")
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
    .Deprecated(msg = "'dfm.corpus()' is deprecated. Use 'tokens()' first.")
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
    stem <- check_logical(stem)
    check_dots(..., method = "tokens")
    
    # call tokens only if options given
    if (length(list(...)))
        x <- tokens(x, ...)
    
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
        .Deprecated(msg = 'groups is deprecated; use dfm_group() instead')
        if (verbose) catm(" ...grouping texts\n")
        x <- tokens_group(x, groups, fill = FALSE)
    }
    
    # use tokens_lookup for tokens objects
    if (!is.null(dictionary) || !is.null(thesaurus)) {
        .Deprecated(msg = 'dictionary and thesaurus are deprecated; use dfm_lookup() instead')
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
    
    if (stem) {
        .Deprecated(msg = 'stem is deprecated; use dfm_wordstem() instead')
        language <- quanteda_options("language_stemmer")
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
            temp, type,
            docvars = get_docvars(x, user = TRUE, system = TRUE),
            meta = attrs[["meta"]]),
        tolower = FALSE, stem = FALSE, verbose = verbose
    )
}


#' @noRd
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
    
    x <- as.dfm(x)
    valuetype <- match.arg(valuetype)
    check_dots(...)
    
    if (!is.null(groups)) {
        .Deprecated(msg = 'groups is deprecated; use dfm_group() instead')
        if (verbose) catm(" ...grouping texts\n")
        x <- dfm_group(x, groups, fill = FALSE)
    }
    
    if (!is.null(dictionary) || !is.null(thesaurus)) {
        .Deprecated(msg = 'dictionary and thesaurus are deprecated; use dfm_lookup() instead')
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
    
    if (stem) {
        .Deprecated(msg = 'stem is deprecated; use dfm_wordstem() instead')
        language <- quanteda_options("language_stemmer")
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
