#' Create a document-feature matrix
#'
#' Construct a sparse document-feature matrix, from a character, [corpus],
#' [tokens], or even other [dfm] object.
#' @param x a [tokens] or [dfm] object
#' @param tolower convert all features to lowercase
#' @param remove_padding logical; if `TRUE`, remove the "pads" left as empty tokens after
#' calling [tokens()] or [tokens_remove()] with `padding = TRUE`
#' @param verbose display messages if `TRUE`
#' @param ... not used directly
#' @section Changes in version 3:
#' In \pkg{quanteda} v3, many convenience functions formerly available in
#' `dfm()` were deprecated. Formerly, `dfm()` could be called directly on a
#' `character` or `corpus` object, but we now steer users to tokenise their
#' inputs first using [tokens()].  Other convenience arguments to `dfm()` were
#' also removed, such as `select`, `dictionary`, `thesaurus`, and `groups`.  All
#' of these functions are available elsewhere, e.g. through [dfm_group()].
#' See `news(Version >= "2.9", package = "quanteda")` for details.
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
                remove_padding = FALSE,
                verbose = quanteda_options("verbose"),
                ...) {
    dfm_env$START_TIME <- proc.time()
    object_class <- class(x)[1]
    if (verbose) message("Creating a dfm from a ", object_class, " input...")
    UseMethod("dfm")
}

#' @export
dfm.default <- function(x, ...) {
    check_class(class(x), "dfm")
}

# GLOBAL FOR dfm THAT FUNCTIONS CAN RESET AS NEEDED TO RECORD TIME ELAPSED
dfm_env <- new.env()
dfm_env$START_TIME <- NULL

#' @export
dfm.character <- function(x, ...) {
    .Deprecated(msg = "'dfm.character()' is deprecated. Use 'tokens()' first.")
    (function(x, tolower = TRUE, remove_padding = FALSE, 
              stem = NULL, select = NULL, #remove = NULL, 
              dictionary = NULL,  thesaurus = NULL, groups = NULL, ...) {
        dfm.tokens(tokens(corpus(x), ...), 
                   tolower = tolower, remove_padding = remove_padding,
                   stem = stem, select = select, #remove = remove, 
                   dictionary = dictionary, thesaurus = thesaurus, 
                   groups = groups, ...)
    })(x, ...)
}

#' @export
dfm.corpus <- function(x, ...) {
    .Deprecated(msg = "'dfm.corpus()' is deprecated. Use 'tokens()' first.")
    (function(x, tolower = TRUE, remove_padding = FALSE, 
              stem = NULL, select = NULL, #remove = NULL, 
              dictionary = NULL, thesaurus = NULL, groups = NULL, ...) {
        dfm.tokens(tokens(x, ...), 
                   tolower = tolower, remove_padding = remove_padding,
                   stem = stem, select = select, #remove = remove, 
                   dictionary = dictionary, thesaurus = thesaurus,
                   groups = groups, ...)
    })(x, ...)

}

#' @export
dfm.tokens <- function(x,
                       tolower = TRUE,
                       remove_padding = FALSE,
                       verbose = quanteda_options("verbose"),
                       ...) {
    x <- as.tokens(x)
    
    if (length(intersect(names(list(...)), names(formals("tokens"))))) {
        warning("'...' should not be used for tokens() arguments; use 'tokens()' first.", call. = FALSE)
        x <- (function(x, tolower = TRUE, remove_padding = FALSE, 
                       stem = NULL, select = NULL, #remove = NULL, 
                       dictionary = NULL, thesaurus = NULL, groups = NULL, ...) {
                 tokens.tokens(x, ...)
        })(x, ...)
    }
    
    if (tolower) {
        if (verbose) catm(" ...lowercasing\n", sep = "")
        x <- tokens_tolower(x)
        tolower <- FALSE
    }
    
    # trap deprecated arguments
    x <- (function(stem = NULL, select = NULL, remove = NULL, dictionary = NULL,
                   thesaurus = NULL, valuetype = NULL, case_insensitive = NULL,
                   groups = NULL, remove_padding = NULL, ...) {
        
        check_dots(..., method = c("dfm", "tokens"))
        
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
        
        # if "remove" was matched to "remove_padding"
        if (!is.logical(remove_padding) && is.null(remove))
            remove <- remove_padding
    
        # deprecation for groups
        if (!is.null(groups)) {
            warning("'groups' is deprecated; use dfm_group() instead", call. = FALSE)
            if (verbose) catm(" ...grouping texts\n")
            x <- tokens_group(x, groups = groups, fill = FALSE)
        }
    
        # fix to set valuetype and case_insensitive for dictionary/thesaurus, select/remove
        if (!is.null(valuetype)) {
            warning("valuetype is deprecated in dfm()", call. = FALSE)
            valuetype <- match.arg(valuetype, c("glob", "regex", "fixed"))
        }
        
        if (!is.null(case_insensitive)) {
            warning("case_insensitive is deprecated in dfm()", call. = FALSE)
            case_insensitive <- check_logical(case_insensitive)
        } else {
            case_insensitive <- TRUE
        }
        
        # deprecations for dictionary, thesaurus
        if (!is.null(dictionary) || !is.null(thesaurus)) {
            warning("'dictionary' and 'thesaurus' are deprecated; use dfm_lookup() instead",
                    call. = FALSE)
            if (!is.null(thesaurus)) dictionary <- dictionary(thesaurus)
            if (verbose) catm(" ...")
            x <- tokens_lookup(x, dictionary = dictionary,
                               exclusive = ifelse(!is.null(thesaurus), FALSE, TRUE),
                               valuetype = valuetype,
                               case_insensitive = case_insensitive,
                               verbose = verbose)
        }
    
        # deprecation for select/remove
        if (!is.null(select) || !is.null(remove)) {
            if (!is.null(select) && !is.null(remove))
                stop("only one of select and remove may be supplied at once", call. = FALSE)
            if (!is.null(select)) {
                warning("'select' is deprecated; use dfm_select() instead", call. = FALSE)
                pattern <- select
            }
            if (!is.null(remove)) {
                warning("'remove' is deprecated; use dfm_remove() instead", call. = FALSE)
                pattern <- remove   
            }
            if (verbose) catm(" ...")
            x <- tokens_select(x,
                               pattern = pattern,
                               selection = if (!is.null(select)) "keep" else "remove",
                               valuetype = valuetype,
                               case_insensitive = case_insensitive,
                               verbose = verbose)
        }
    
        if (!is.null(stem)) {
            warning("'stem' is deprecated; use dfm_wordstem() instead", call. = FALSE)
            stem <- check_logical(stem)
            language <- quanteda_options("language_stemmer")
            if (verbose)
                if (verbose) catm(" ...stemming types (", stri_trans_totitle(language), ")\n", sep = "")
            x <- tokens_wordstem(x, language = language)
        }
        
        return(x)
        
    })(remove_padding = remove_padding, ...) # deprecation trap ends
    
    # if "remove" was matched to "remove_padding"
    if (!is.logical(remove_padding))
        remove_padding <- FALSE
    
    remove_padding <- check_logical(remove_padding)
    if (remove_padding)
        x <- tokens_remove(x, "", valuetype = "fixed")

    # compile the dfm
    type <- types(x)
    attrs <- attributes(x)
    temp <- unclass(x)

    # shift index for padding, if any
    index <- unlist_integer(temp, use.names = FALSE)
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


#' @importFrom stringi stri_trans_totitle
#' @export
dfm.dfm <- function(x,
                    tolower = TRUE,
                    remove_padding = FALSE,
                    verbose = quanteda_options("verbose"),
                    ...) {
    x <- as.dfm(x)
    
    if (tolower) {
        if (verbose) catm(" ...lowercasing\n", sep = "")
        x <- dfm_tolower(x)
    }
    
    # trap deprecated arguments
    x <- (function(stem = NULL, select = NULL, remove = NULL, dictionary = NULL, 
                   thesaurus = NULL, valuetype = NULL, case_insensitive = NULL, 
                   groups = NULL,  remove_padding = FALSE, ...) {
        
        check_dots(..., method = "dfm")
        
        # if "remove" was matched to "remove_padding"
        if (!is.logical(remove_padding) && is.null(remove))
            remove <- remove_padding
        
        # deprecation for groups
        if (!is.null(groups)) {
            warning("'groups' is deprecated; use dfm_group() instead", call. = FALSE)
            if (verbose) catm(" ...grouping texts\n")
            x <- dfm_group(x, groups = groups, fill = FALSE)
        }
    
        # fix to set valuetype and case_insensitive for dictionary/thesaurus, select/remove
        if (!is.null(valuetype)) {
            warning("valuetype is deprecated in dfm()", call. = FALSE)
            valuetype <- match.arg(valuetype, c("glob", "regex", "fixed"))
        }
        
        if (!is.null(case_insensitive)) {
            warning("case_insensitive is deprecated in dfm()", call. = FALSE)
            case_insensitive <- check_logical(case_insensitive)
        } else {
            case_insensitive <- TRUE
        }
        
        # deprecations for dictionary, thesaurus
        if (!is.null(dictionary) || !is.null(thesaurus)) {
            warning("'dictionary' and 'thesaurus' are deprecated; use dfm_lookup() instead",
                    call. = FALSE)
            if (!is.null(thesaurus)) dictionary <- dictionary(thesaurus)
            if (verbose) catm(" ...")
            x <- dfm_lookup(x, dictionary = dictionary,
                            exclusive = ifelse(!is.null(thesaurus), FALSE, TRUE),
                            valuetype = valuetype,
                            case_insensitive = case_insensitive,
                            verbose = verbose)
        }
    
        # deprecation for select/remove
        if (!is.null(select) || !is.null(remove)) {
            if (!is.null(select) && !is.null(remove))
                stop("only one of select and remove may be supplied at once", call. = FALSE)
            if (!is.null(select)) {
                warning("'select' is deprecated; use dfm_select() instead", call. = FALSE)
                pattern <- select
            }
            if (!is.null(remove)) {
                warning("'remove' is deprecated; use dfm_remove() instead", call. = FALSE)
                pattern <- remove   
            }
            if (verbose) catm(" ...")
            x <- dfm_select(x,
                            pattern = pattern,
                            selection = if (!is.null(select)) "keep" else "remove",
                            valuetype = valuetype,
                            case_insensitive = case_insensitive,
                            verbose = verbose)
        }
        
        if (!is.null(stem)) {
            warning("'stem' is deprecated; use dfm_wordstem() instead", call. = FALSE)
            stem <- check_logical(stem)
            language <- quanteda_options("language_stemmer")
            if (verbose)
                if (verbose) catm(" ...stemming features (", stri_trans_totitle(language), ")\n", sep = "")
            x <- dfm_wordstem(x, language = language)
        }
        
        return(x)
        
    })(remove_padding = remove_padding, ...) # deprecation trap ends
    
    # if "remove" was matched to "remove_padding"
    if (!is.logical(remove_padding))
        remove_padding <- FALSE
    
    remove_padding <- check_logical(remove_padding)
    if (remove_padding)
        x <- dfm_remove(x, "", valuetype = "fixed")

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
