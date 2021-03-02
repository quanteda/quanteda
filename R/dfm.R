#' Create a document-feature matrix
#'
#' Construct a sparse document-feature matrix, from a character, [corpus],
#' [tokens], or even other [dfm] object.
#' @param x a [tokens] or [dfm] object
#' @param tolower convert all features to lowercase
#' @param remove a [pattern] of user-supplied features to ignore, such as "stop
#'   words".  To access one possible list (from any list you wish), use
#'   [stopwords()].  The pattern matching type is fixed to "glob"; if you want
#'   greater control, use [tokens_remove()] or [dfm_remove()].  `remove` is
#'   primarily useful for removing artifacts created during the [tokens]
#'   creation process, such as pads.  See examples.
#' @param verbose display messages if `TRUE`
#' @param ... not used directly
#' @section Changes to dfm() in version 3:
#' In \pkg{quanteda} v3, many convenience functions formerly available in
#' `dfm()` were deprecated. Formerly, `dfm()` could be called directly on a
#' `character` or `corpus` object, but we now steer users to tokenise their
#' inputs first using [tokens()].  Other convenience arguments to `dfm()` were
#' also removed, such as `select`, `dictionary`, `thesaurus`, and `groups`.  All
#' of these functions are available elsewhere, e.g. through [dfm_group()].
#' See [quanteda-deprecations].
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
                remove = NULL,
                verbose = quanteda_options("verbose"),
                ...) {
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
                          remove = NULL,
                          verbose = quanteda_options("verbose"),
                          ...) {
    .Deprecated(msg = "'dfm.character()' is deprecated. Use 'tokens()' first.")
    
    # deprecation for passing tokens arguments via ...
    otherargs <- list(...)
    otherargs_tokens <- otherargs[names(otherargs) %in% names(as.list(args("tokens")))]
    if (length(otherargs_tokens))
        .Deprecated(msg = "'...' should not be used for tokens() arguments; use 'tokens()' first.")

    x <- do.call(tokens, c(list(x = x), otherargs_tokens))    
    do.call(dfm, c(list(x = x), otherargs[!names(otherargs) %in% names(otherargs_tokens)]))
}


#' @rdname dfm
#' @noRd
#' @export
dfm.corpus <- function(x,
                       tolower = TRUE,
                       remove = NULL,
                       verbose = quanteda_options("verbose"),
                       ...) {
    .Deprecated(msg = "'dfm.corpus()' is deprecated. Use 'tokens()' first.")
    
    # deprecation for passing tokens arguments via ...
    otherargs <- list(...)
    otherargs_tokens <- otherargs[names(otherargs) %in% names(as.list(args("tokens")))]
    if (length(otherargs_tokens))
        .Deprecated(msg = "'...' should not be used for tokens() arguments; use 'tokens()' first.")

    x <- do.call(tokens, c(list(x = x), otherargs_tokens))    
    do.call(dfm, c(list(x = x), otherargs[!names(otherargs) %in% names(otherargs_tokens)]))
}

#' @noRd
#' @importFrom utils glob2rx
#' @export
dfm.tokens <- function(x,
                       tolower = TRUE,
                       remove = NULL,
                       verbose = quanteda_options("verbose"),
                       ...) {
    # check for arguments passed to tokens via ...
    otherargs <- list(...)
    otherargs_tokens <- otherargs[names(otherargs) %in% names(as.list(args("tokens")))]
    if (length(otherargs_tokens)) {
        .Deprecated(msg = "'...' should not be used for tokens() arguments; use 'tokens()' first.")
        x <- do.call(tokens, c(list(x = x), otherargs_tokens))
        otherargs <- otherargs[!names(otherargs) %in% names(otherargs_tokens)]
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
    
    # deprecation for groups
    if ("groups" %in% names(otherargs)) {
        .Deprecated(msg = "'groups' is deprecated; use dfm_group() instead")
        if (verbose) catm(" ...grouping texts\n")
        x <- do.call(tokens_group, list(x = x, groups = otherargs[["groups"]], fill = FALSE))
        otherargs <- otherargs[-which(names(otherargs) == "groups")]
    }
    
    # deprecations for dictionary, thesaurus
    if (any(c("dictionary", "thesaurus") %in% names(otherargs))) {
        .Deprecated(msg = "'dictionary' and 'thesaurus' are deprecated; use dfm_lookup() instead")
        dictionary <- otherargs[["dictionary"]]
        thesaurus <- otherargs[["thesaurus"]]
        if (!is.null(thesaurus)) dictionary <- dictionary(thesaurus)
        if (verbose) catm(" ...")
        x <- do.call(tokens_lookup, list(x = x, dictionary = dictionary,
                           exclusive = ifelse(!is.null(thesaurus), FALSE, TRUE),
                           # valuetype = valuetype,
                           # case_insensitive = case_insensitive,
                           verbose = verbose))
        otherargs <- otherargs[-which(names(otherargs) %in% c("dictionary", "thesaurus"))]
    }
    
    # deprecation for select
    if ("select" %in% names(otherargs)) {
        .Deprecated(msg = "'select' is deprecated; use dfm_select() instead")
        if (!is.null(remove))
            stop("only one of select and remove may be supplied at once")
        if (verbose) catm(" ...")
        x <- do.call(tokens_select, list(x = x,
                           pattern = otherargs[["select"]],
                           selection = "keep",
                           # valuetype = valuetype,
                           # case_insensitive = case_insensitive,
                           verbose = verbose))
        otherargs <- otherargs[-which(names(otherargs) %in% "select")]
    }
    
    if (!is.null(remove)) {
        x <- tokens_remove(x = x, pattern = remove)
    }
        
    if ("stem" %in% names(otherargs)) {
        .Deprecated(msg = "'stem' is deprecated; use dfm_wordstem() instead")
        if (otherargs[["stem"]]) {
            language <- quanteda_options("language_stemmer")
            if (verbose) catm(" ...stemming types (", stri_trans_totitle(language), ")\n", sep = "")
            x <- do.call(tokens_wordstem, list(x = x, language = language))
        }
        otherargs <- otherargs[-which(names(otherargs) %in% "stem")]
    }
    
    check_dots(otherargs, method = "dfm")
    
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
        tolower = FALSE, verbose = verbose
    )
}


#' @noRd
#' @import Matrix
#' @importFrom stringi stri_trans_totitle
#' @export
dfm.dfm <- function(x,
                    tolower = TRUE,
                    remove = NULL,
                    verbose = quanteda_options("verbose"),
                    ...) {
    x <- as.dfm(x)
    otherargs <- list(...)

    # deprecation for groups
    if ("groups" %in% names(otherargs)) {
        .Deprecated(msg = "'groups' is deprecated; use dfm_group() instead")
        if (verbose) catm(" ...grouping texts\n")
        x <- do.call(dfm_group, list(x = x, groups = otherargs[["groups"]], fill = FALSE))
        otherargs <- otherargs[-which(names(otherargs) == "groups")]
    }

    # deprecations for dictionary, thesaurus
    if (any(c("dictionary", "thesaurus") %in% names(otherargs))) {
        .Deprecated(msg = "'dictionary' and 'thesaurus' are deprecated; use dfm_lookup() instead")
        dictionary <- otherargs[["dictionary"]]
        thesaurus <- otherargs[["thesaurus"]]
        if (!is.null(thesaurus)) dictionary <- dictionary(thesaurus)
        if (verbose) catm(" ...")
        x <- do.call(dfm_lookup, list(x = x, dictionary = dictionary,
                           exclusive = ifelse(!is.null(thesaurus), FALSE, TRUE),
                           # valuetype = valuetype,
                           # case_insensitive = case_insensitive,
                           verbose = verbose))
        otherargs <- otherargs[-which(names(otherargs) %in% c("dictionary", "thesaurus"))]
    }
    
    # deprecation for select
    if ("select" %in% names(otherargs)) {
        .Deprecated(msg = "'select' is deprecated; use dfm_select() instead")
        if (!is.null(remove))
            stop("only one of select and remove may be supplied at once")
        if (verbose) catm(" ...")
        x <- do.call(dfm_select, list(x = x,
                           pattern = otherargs[["select"]],
                           selection = "keep",
                           # valuetype = valuetype,
                           # case_insensitive = case_insensitive,
                           verbose = verbose))
        otherargs <- otherargs[-which(names(otherargs) %in% "select")]
    }
    
    if (!is.null(remove)) {
        x <- dfm_remove(x = x, pattern = remove)
    }
        
    if (tolower) {
        if (verbose) catm(" ...lowercasing\n", sep = "")
        x <- dfm_tolower(x)
    }
    
    if ("stem" %in% names(otherargs)) {
        .Deprecated(msg = "'stem' is deprecated; use dfm_wordstem() instead")
        if (otherargs[["stem"]]) {
            language = quanteda_options("language_stemmer")
            if (verbose)
                if (verbose) catm(" ...stemming types (", stri_trans_totitle(language), ")\n", sep = "")
            x <- do.call(dfm_wordstem, list(x = x, language = language))
        }
        otherargs <- otherargs[-which(names(otherargs) %in% "stem")]
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
