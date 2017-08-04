#' apply a dictionary to a dfm
#' 
#' Apply a dictionary to a dfm by looking up all dfm features for matches in a a
#' set of \link{dictionary} values, and replace those features with a count of
#' the dictionary's keys.  If \code{exclusive = FALSE} then the behaviour is to
#' apply a "thesaurus", where each value match is replaced by the dictionary
#' key, converted to capitals if \code{capkeys = TRUE} (so that the replacements
#' are easily distinguished from features that were terms found originally in
#' the document).
#' @param x the dfm to which the dictionary will be applied
#' @param dictionary a \link{dictionary} class object
#' @param levels levels of entries in a hierachical dictionary that will be 
#'   applied
#' @param exclusive if \code{TRUE}, remove all features not in dictionary, 
#'   otherwise, replace values in dictionary with keys while leaving other 
#'   features unaffected
#' @inheritParams valuetype
#' @param case_insensitive ignore the case of dictionary values if \code{TRUE}
#' @param capkeys if \code{TRUE}, convert dictionary keys to uppercase to
#'   distinguish them from other features
#' @param nomatch an optional character naming a new feature that will contain 
#'   the counts of features of \code{x} not matched to a dictionary key.  If 
#'   \code{NULL} (default), do not tabulate unmatched features.
#' @param verbose print status messages if \code{TRUE}
#' @export
#' @note If using \code{dfm_lookup} with dictionaries containing multi-word
#'   values, matches will only occur if the features themselves are multi-word
#'   or formed from ngrams. A better way to match dictionary values that include
#'   multi-word patterns is to apply \code{\link{tokens_lookup}} to the tokens,
#'   and then construct the dfm.
#' @keywords dfm
#' @examples
#' myDict <- dictionary(list(christmas = c("Christmas", "Santa", "holiday"),
#'                           opposition = c("Opposition", "reject", "notincorpus"),
#'                           taxglob = "tax*",
#'                           taxregex = "tax.+$",
#'                           country = c("United_States", "Sweden")))
#' myDfm <- dfm(c("My Christmas was ruined by your opposition tax plan.", 
#'                "Does the United_States or Sweden have more progressive taxation?"),
#'              remove = stopwords("english"), verbose = FALSE)
#' myDfm
#' 
#' # glob format
#' dfm_lookup(myDfm, myDict, valuetype = "glob")
#' dfm_lookup(myDfm, myDict, valuetype = "glob", case_insensitive = FALSE)
#' 
#' # regex v. glob format: note that "united_states" is a regex match for "tax*"
#' dfm_lookup(myDfm, myDict, valuetype = "glob")
#' dfm_lookup(myDfm, myDict, valuetype = "regex", case_insensitive = TRUE)
#' 
#' # fixed format: no pattern matching
#' dfm_lookup(myDfm, myDict, valuetype = "fixed")
#' dfm_lookup(myDfm, myDict, valuetype = "fixed", case_insensitive = FALSE)
#' 
#' # show unmatched tokens
#' dfm_lookup(myDfm, myDict, nomatch = "_UNMATCHED")
#' 
dfm_lookup <- function(x, dictionary, levels = 1:5,
                       exclusive = TRUE, valuetype = c("glob", "regex", "fixed"), 
                       case_insensitive = TRUE,
                       capkeys = !exclusive,
                       nomatch = NULL,
                       verbose = quanteda_options("verbose")) {
    UseMethod("dfm_lookup")
}
 
#' @noRd
#' @export
dfm_lookup.dfm <- function(x, dictionary, levels = 1:5,
                           exclusive = TRUE, valuetype = c("glob", "regex", "fixed"), 
                           case_insensitive = TRUE,
                           capkeys = !exclusive,
                           nomatch = NULL,
                           verbose = quanteda_options("verbose")) {
    if (!is.dictionary(dictionary))
        stop("dictionary must be a dictionary object")
    
    dictionary <- flatten_dictionary(dictionary, levels)
    valuetype <- match.arg(valuetype)
    attrs <- attributes(x)
    lengths <- ntoken(x)
    
    # Generate all combinations of type IDs
    values_id <- list()
    keys_id <- c()
    types <- featnames(x)
    
    if (verbose) 
        catm("applying a dictionary consisting of ", length(dictionary), " key", 
             if (length(dictionary) > 1L) "s" else "", "\n", sep="")
    
    for (h in seq_along(dictionary)) {
        values <- as.list(stri_replace_all_fixed(dictionary[[h]], ' ', attr(x, 'concatenator')))
        values_temp <- regex2id(values, types, valuetype, case_insensitive, FALSE)
        values_id <- c(values_id, values_temp)
        keys_id <- c(keys_id, rep(h, length(values_temp)))
    }
    if (length(values_id)) {
        if (capkeys) {
            keys <- char_toupper(names(dictionary))
        } else {
            keys <- names(dictionary)
        }
        temp <- x[,unlist(values_id, use.names = FALSE)]
        colnames(temp) <- keys[keys_id]
        temp <- dfm_compress(temp, margin = 'features')
        #temp <- dfm_select(temp, as.dfm(matrix(0, ncol = length(keys), 
        #                                       dimnames = list(docs = NULL, features = keys))))
        temp <- dfm_select(temp, as.dfm(rbind(structure(rep(0, length(keys)), names = keys))))
        if (exclusive) {
            result <- temp[, keys]
        } else {
            result <- cbind(x[,unlist(values_id) * -1], temp[,keys])
        }
    } else {
        if (exclusive) {
            result <- x[,0] # dfm without features
        } else {
            result <- x
        }
    }
    
    if (!is.null(nomatch)) {
        if (!exclusive) {
            warning("nomatch only applies if exclusive = TRUE")
        } else {
            result <- cbind(result, ntoken(dfm_remove(x, dictionary)))
            colnames(result)[ncol(result)] <- nomatch
        }
    }
    
    attr(result, "what") <- "dictionary"
    attr(result, "dictionary") <- dictionary
    attributes(result, FALSE) <- attrs
    docvars(result, '_length_original') <- lengths
    return(result)
}

