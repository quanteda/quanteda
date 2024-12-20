#' Apply a dictionary to a dfm
#'
#' Apply a dictionary to a dfm by looking up all dfm features for matches in a a
#' set of [dictionary] values, and replace those features with a count of
#' the dictionary's keys.  If `exclusive = FALSE` then the behaviour is to
#' apply a "thesaurus", where each value match is replaced by the dictionary
#' key, converted to capitals if `capkeys = TRUE` (so that the replacements
#' are easily distinguished from features that were terms found originally in
#' the document).
#' @param x the dfm to which the dictionary will be applied
#' @param dictionary a [dictionary]-class object
#' @param levels levels of entries in a hierarchical dictionary that will be
#'   applied
#' @param exclusive if `TRUE`, remove all features not in dictionary,
#'   otherwise, replace values in dictionary with keys while leaving other
#'   features unaffected
#' @inheritParams valuetype
#' @param capkeys if `TRUE`, convert dictionary keys to uppercase to
#'   distinguish them from other features
#' @param nomatch an optional character naming a new feature that will contain
#'   the counts of features of `x` not matched to a dictionary key.  If
#'   `NULL` (default), do not tabulate unmatched features.
#' @param verbose print status messages if `TRUE`
#' @export
#' @note If using `dfm_lookup` with dictionaries containing multi-word
#'   values, matches will only occur if the features themselves are multi-word
#'   or formed from n-grams. A better way to match dictionary values that include
#'   multi-word patterns is to apply [tokens_lookup()] to the tokens,
#'   and then construct the dfm.
#' @keywords dfm
#' @seealso dfm_replace
#' @examples
#' dict <- dictionary(list(christmas = c("Christmas", "Santa", "holiday"),
#'                         opposition = c("Opposition", "reject", "notincorpus"),
#'                         taxglob = "tax*",
#'                         taxregex = "tax.+$",
#'                         country = c("United_States", "Sweden")))
#' dfmat <- dfm(tokens(c("My Christmas was ruined by your opposition tax plan.",
#'                       "Does the United_States or Sweden have more progressive taxation?")))
#' dfmat
#'
#' # glob format
#' dfm_lookup(dfmat, dict, valuetype = "glob")
#' dfm_lookup(dfmat, dict, valuetype = "glob", case_insensitive = FALSE)
#'
#' # regex v. glob format: note that "united_states" is a regex match for "tax*"
#' dfm_lookup(dfmat, dict, valuetype = "glob")
#' dfm_lookup(dfmat, dict, valuetype = "regex", case_insensitive = TRUE)
#'
#' # fixed format: no pattern matching
#' dfm_lookup(dfmat, dict, valuetype = "fixed")
#' dfm_lookup(dfmat, dict, valuetype = "fixed", case_insensitive = FALSE)
#'
#' # show unmatched tokens
#' dfm_lookup(dfmat, dict, nomatch = "_UNMATCHED")
#'
dfm_lookup <- function(x, dictionary, levels = 1:5,
                       exclusive = TRUE,
                       valuetype = c("glob", "regex", "fixed"),
                       case_insensitive = TRUE,
                       capkeys = !exclusive,
                       nomatch = NULL,
                       verbose = quanteda_options("verbose")) {
    UseMethod("dfm_lookup")
}

#' @export
dfm_lookup.default <- function(x, dictionary, levels = 1:5,
                           exclusive = TRUE,
                           valuetype = c("glob", "regex", "fixed"),
                           case_insensitive = TRUE,
                           capkeys = !exclusive,
                           nomatch = NULL,
                           verbose = quanteda_options("verbose")) {
    check_class(class(x), "dfm_lookup")
}

#' @export
dfm_lookup.dfm <- function(x, dictionary, levels = 1:5,
                           exclusive = TRUE,
                           valuetype = c("glob", "regex", "fixed"),
                           case_insensitive = TRUE,
                           capkeys = !exclusive,
                           nomatch = NULL,
                           verbose = quanteda_options("verbose")) {
    
    x <- as.dfm(x)
    exclusive <- check_logical(exclusive)
    capkeys <- check_logical(capkeys)
    verbose <- check_logical(verbose)
    
    if (!nfeat(x) || !ndoc(x)) return(x)

    if (!is.dictionary(dictionary))
        stop("dictionary must be a dictionary object")

    valuetype <- match.arg(valuetype)
    type <- colnames(x)
    attrs <- attributes(x)

    if (verbose)
        catm("applying a dictionary consisting of ", length(dictionary), " key",
             if (length(dictionary) > 1L) "s" else "", "\n", sep = "")

    ids <- object2id(dictionary, type, valuetype, case_insensitive,
                     field_object(attrs, "concatenator"), levels)
    
    # flag nested patterns
    if (length(ids)) {
        m <- factor(names(ids), levels = unique(names(ids)))
        dup <- unlist(lapply(split(ids, m), duplicated), use.names = FALSE)
    } else {
        dup <- logical()
    }
    
    key <- attr(ids, "key")
    ids <- ids[lengths(ids) == 1 & !dup] # drop phrasal and nested patterns
    id_key <- match(names(ids), key)
    id <- unlist(ids, use.names = FALSE)
    if (capkeys)
        key <- char_toupper(key)
    if (length(id)) {
        if (exclusive) {
            if (!is.null(nomatch)) {
                id_nomatch <- setdiff(seq_len(nfeat(x)), id)
                id <- c(id, id_nomatch)
                id_key <- c(id_key, rep(length(key) + 1,
                                        length(id_nomatch)))
                key <- c(key, nomatch[1])
            }
            col_new <- key[id_key]
            x <- x[, id]
            set_dfm_featnames(x) <- col_new
            # merge identical keys and add non-existent keys
            result <- dfm_match(dfm_compress(x, margin = "features"), key)
        } else {
            if (!is.null(nomatch))
                warning("nomatch only applies if exclusive = TRUE")
            
            r <- order(id)
            id <- id[r]
            id_key <- id_key[r]
            
            # repeat columns for multiple keys
            id_rep <- seq_along(type)
            id_rep <- sort(c(id, id_rep[!id_rep %in% id]))
            
            col_new <- type[id_rep]
            # shift index for repeated columns
            col_new[id + cumsum(duplicated(id))] <- key[id_key]
            x <- x[,id_rep]
            
            set_dfm_featnames(x) <- col_new
            result <- dfm_compress(x, margin = "features")
        }

    } else {
        if (exclusive) {
            if (!is.null(nomatch)) {
                result <- as.dfm(matrix(ntoken(x), ncol = 1,
                                        dimnames = list(docnames(x), nomatch)))
            } else {
                result <- make_null_dfm(document = docnames(x), 
                                        feature = key)
            }
        } else {
            result <- x
        }
    }
    if (exclusive)
        field_object(attrs, "what") <- "dictionary"
    rebuild_dfm(result, attrs)
}
