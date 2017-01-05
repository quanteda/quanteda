
#' apply a dictionary or thesaurus to an object
#' 
#' Convert features into equivalence classes defined by values of a dictionary 
#' object.
#' @note Selecting only features defined in a "dictionary" is traditionally 
#'   known in text analysis as a \emph{dictionary method}, even though
#'   technically this "dictionary" operates more like a thesarus.  If a thesaurus-like
#'   application is desired, set \code{exclusive = FALSE} to convert features 
#'   defined as values in a dictionary into their keys, while keeping all other
#'   features.
#' @return an object of the type passed with the value-matching features
#'   replaced by dictionary keys
#' @param x object to which dictionary or thesaurus will be supplied
#' @param dictionary the \link{dictionary}-class object that will be applied to
#'   \code{x}
#' @param exclusive if \code{TRUE}, remove all features not in dictionary, 
#'   otherwise, replace values in dictionary with keys while leaving other 
#'   features unaffected
#' @inheritParams valuetype
#' @param case_insensitive ignore the case of dictionary values if \code{TRUE}
#' @param capkeys if \code{TRUE}, convert dictionary keys to
#'   uppercase to distinguish them from other features
#' @param verbose print status messages if \code{TRUE}
#' @param ... not used
#' @keywords internal deprecated
#' @export
applyDictionary <- function(x, dictionary, ...) {
    UseMethod("applyDictionary")
}


#' @rdname applyDictionary
#' @examples 
#' toks <- tokens(data_corpus_inaugural)
#' head(kwic(toks, "united states"))
#' dict <- dictionary(list(country = "united states"))
#' toks2 <- applyDictionary(toks, dict, valuetype = "fixed")
#' toks2
#' @export 
applyDictionary.tokens <- function(x, dictionary, exclusive = TRUE, 
                                   valuetype = c("glob", "regex", "fixed"), 
                                   case_insensitive = TRUE,
                                   capkeys = !exclusive,
                                   verbose = TRUE, ...) {
    valuetype <- match.arg(valuetype)
    if (valuetype != "fixed")
        stop("valueype = ", valuetype, " not implemented yet")
    if (!exclusive)
        stop("exclusive = FALSE not implemented yet")
    
    # record some input data
    attr_orig <- attributes(x)
    names_docs <- names(x)
    names_keys <- names(dictionary)
    
    # if type glob but no wildcards, treat as fixed
    if (valuetype == "glob" && !any(stringi::stri_detect_charclass(dictionary, "[*?]")))
        valuetype <- "fixed"
    
    # handle case through conversion for fixed
    if (valuetype == "fixed" & case_insensitive) {
        dictionary <- dictionary(lapply(dictionary, toLower))
        x <- toLower(x)
    }
    
    # convert (individual) patterns to regex if glob
    if (valuetype == "glob") {
        dict_tokenized <- lapply(dict_tokenized, function(l) lapply(l, utils::glob2rx))
        valuetype <- "glob"
    }
    
    # split each dictionary value into a set of individual tokens;
    # this results in two-level list, where outer level is the dictionary keys
    # inner level is one split token set per value assigned to that key
    dict_tokenized <- lapply(dictionary, tokenize, what = "fasterword", ...)
    # match to integers by looking up against x's types
    dict_tokenized <- lapply(dict_tokenized, 
                             function(i) lapply(i, match, types(x)))
    
    # pad each document with a "blank", to prevent spanning documents
    x <- lapply(unclass(x), function(toks) c(toks, NA))
    # get the lengths, including dummy pad
    doclengths <- lengths(x)
    # unlist the tokens
    x <- unlist(x, use.names = FALSE)
    
    # set up values that will go straight into the counts of the dfm
    i <- integer() # empty vectors for now
    j <- integer() 
    xcount <- integer()
    
    # loop through keys and construct matches 
    #  works only for fixed for now
    for (key_int in 1:length(dict_tokenized)) {
        # find all matches
        this_key_match_index <- unlist(sapply(dict_tokenized[[key_int]], matchSequence, x))
        # add matches to running total
        xcount <- c(xcount, this_key_match_index)
        # add key index to running total
        j <- c(j, rep(key_int, length(this_key_match_index)))
    }
    
    # now convert the matches to document indexes
    # take one off the last 
    doclengths[length(doclengths)] <- doclengths[length(doclengths)] - 1
    # slice these up using the info about the document lengths
    i <- .bincode(xcount, breaks = cumsum(c(1, doclengths)), include.lowest = TRUE)
    
    # match up the key names
    keys_matched <- names_keys[j]
    
    # pad any document not included yet, for complete split
    if (length(emptydocs <- which(!(1:length(names_docs) %in% i)))) {
        i <- c(i, emptydocs)
        keys_matched <- c(keys_matched, rep(NA, length(emptydocs)))
    }
    
    newtokens <- split(keys_matched, i)
    attributes(newtokens) <- attr_orig
    attr(newtokens, "types") <- NULL  # get rid of types, so we can recompile
    class(newtokens) <- class(newtokens)[-1]  # remove tokens class tag
    # reverse the NA
    if (length(emptydocs))
        newtokens[emptydocs] <- list(rep(character(0), length(emptydocs)))
    newtokens <- as.tokens(newtokens)  # recompile as tokens
    
    # add some dictionary informatin
    attr(newtokens, "what") <- "dictionary"
    attr(newtokens, "dictionary") <- dictionary
    
    newtokens
}



#' @rdname applyDictionary
#' @export 
applyDictionary.tokenizedTexts <- function(x, dictionary, exclusive = TRUE, 
                                           valuetype = c("glob", "regex", "fixed"), 
                                           case_insensitive = TRUE,
                                           capkeys = !exclusive,
                                           verbose = TRUE, ...) {
    valuetype <- match.arg(valuetype)
    
    if (valuetype != "fixed")
        stop("valueype = ", valuetype, " not implemented yet")
    
    if (!exclusive)
        stop("exclusive = FALSE not implemented yet")
    
    ## if no multi-word entries in the dictionary values
    #    if (!any(stringi::stri_detect_charclass(x, "\\p{Z}"))) {
    #        return(x)
    #    }
    
    ## otherwise, process the multi-word dictionary values
    
    # record some input data
    attr_orig <- attributes(x)
    names_docs <- names(x)
    names_keys <- names(dictionary)
    
    # if type glob but no wildcards, treat as fixed
    if (valuetype == "glob" && !any(stringi::stri_detect_charclass(dictionary, "[*?]")))
        valuetype <- "fixed"
    
    # handle case through conversion for fixed
    if (valuetype == "fixed" & case_insensitive) {
        dictionary <- dictionary(lapply(dictionary, toLower))
        x <- toLower(x)
    }
    
    # pad each dictionary with a dummy value, so that length is preserved even for documents
    # with no matches, when splitting later
    # dictionary <- dictionary(c(dictionary, list(PAD = "")))
    
    # split each dictionary value into a set of individual tokens;
    # this results in two-level list, where outer level is the dictionary keys
    # inner level is one split token set per value assigned to that key
    dict_tokenized <- lapply(dictionary, tokenize, what = "fasterword", ...)
    
    # convert (individual) patterns to regex if glob
    if (valuetype == "glob") {
        dict_tokenized <- lapply(dict_tokenized, function(l) lapply(l, utils::glob2rx))
        valuetype <- "glob"
    }
    
    # pad each document with a blank, to prevent spanning documents
    x <- lapply(x, function(toks) c(toks, ""))
    # get the lengths, including dummy pad
    doclengths <- lengths(x)
    # unlist the tokens
    x <- unlist(x, use.names = FALSE)
    
    # set up values that will go straight into the counts of the dfm
    i <- integer() # empty vectors for now
    j <- integer() 
    xcount <- integer()
    
    # loop through keys and construct matches 
    #  works only for fixed for now
    for (key_int in 1:length(dict_tokenized)) {
        # find all matches
        this_key_match_index <- unlist(sapply(dict_tokenized[[key_int]], matchSequence, x))
        # add matches to running total
        xcount <- c(xcount, this_key_match_index)
        # add key index to running total
        j <- c(j, rep(key_int, length(this_key_match_index)))
    }
    
    # now convert the matches to document indexes
    # take one off the last 
    doclengths[length(doclengths)] <- doclengths[length(doclengths)] - 1
    # slice these up using the info about the document lengths
    i <- .bincode(xcount, breaks = cumsum(c(1, doclengths)), include.lowest = TRUE)
    
    # match up the key names
    keys_matched <- names_keys[j]
    
    # pad any document not included yet, for complete split
    if (length(emptydocs <- which(!(1:length(names_docs) %in% i)))) {
        i <- c(i, emptydocs)
        keys_matched <- c(keys_matched, rep(NA, length(emptydocs)))
    }
    
    newtokens <- split(keys_matched, i)
    attributes(newtokens) <- attr_orig
    # reverse the NA
    if (length(emptydocs))
        newtokens[emptydocs] <- list(rep(character(0), length(emptydocs)))
    
    # add some dictionary informatin
    attr(newtokens, "what") <- "dictionary"
    attr(newtokens, "dictionary") <- dictionary
    
    newtokens
}

#' @rdname applyDictionary
#' @details \code{applyDictionary.dfm} is the deprecated function name for
#' \code{\link{dfm_lookup}}.
#' @export
applyDictionary.dfm <- function(x, ...) {
    .Deprecated("dfm_lookup")
    dfm_lookup(x, ...)
}

