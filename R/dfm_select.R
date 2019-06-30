#' Select features from a dfm or fcm
#'
#' This function selects or removes features from a \link{dfm} or \link{fcm},
#' based on feature name matches with \code{pattern}.  The most common usages
#' are to eliminate features from a dfm already constructed, such as stopwords,
#' or to select only terms of interest from a dictionary.
#'
#' @param x the \link{dfm} or \link{fcm} object whose features will be selected
#' @inheritParams pattern
#' @param selection whether to \code{keep} or \code{remove} the features
#' @param valuetype the type of pattern matching: \code{"glob"} for "glob"-style
#'   wildcard expressions; \code{"regex"} for regular expressions; or
#'   \code{"fixed"} for exact matching. See \link{valuetype} for details.
#'
#'   For \code{dfm_select}, \code{pattern} may also be a \link{dfm}; see Value
#'   below.
#' @param case_insensitive ignore the case of dictionary values if \code{TRUE}
#' @param min_nchar,max_nchar optional numerics specifying the minimum and
#'   maximum length in characters for tokens to be removed or kept; defaults are
#'   \code{NULL} for no limits.  These are applied after (and hence, in addition
#'   to) any selection based on pattern matches.
#' @param verbose if \code{TRUE} print message about how many pattern were
#'   removed
#' @details \code{dfm_remove} and \code{fcm_remove} are simply a convenience
#'   wrappers to calling \code{dfm_select} and \code{fcm_select} with
#'   \code{selection = "remove"}.
#'
#'   \code{dfm_keep} and \code{fcm_keep} are simply a convenience wrappers to
#'   calling \code{dfm_select} and \code{fcm_select} with \code{selection =
#'   "keep"}.
#' @note This function selects features based on their labels.  To select
#'   features based on the values of the document-feature matrix, use
#'   \code{\link{dfm_trim}}.
#' @return A \link{dfm} or \link{fcm} object, after the feature selection has
#'   been applied.
#'
#'   For compatibility with earlier versions, when \code{pattern} is a
#'   \link{dfm} object and \code{selection = "keep"}, then this will be
#'   equivalent to calling \code{\link{dfm_match}}.  In this case, the following
#'   settings are always used: \code{case_insensitive = FALSE}, and
#'   \code{valuetype = "fixed"}.  This functionality is deprecated, however, and
#'   you should use \code{\link{dfm_match}} instead.
#'   
#' @export
#' @keywords dfm
#' @seealso \code{\link{dfm_match}}
#' @examples
#' dfmat <- dfm(c("My Christmas was ruined by your opposition tax plan.",
#'                "Does the United_States or Sweden have more progressive taxation?"),
#'              tolower = FALSE)
#' dict <- dictionary(list(countries = c("United_States", "Sweden", "France"),
#'                           wordsEndingInY = c("by", "my"),
#'                           notintext = "blahblah"))
#' dfm_select(dfmat, pattern = dict)
#' dfm_select(dfmat, pattern = dict, case_insensitive = FALSE)
#' dfm_select(dfmat, pattern = c("s$", ".y"), selection = "keep", valuetype = "regex")
#' dfm_select(dfmat, pattern = c("s$", ".y"), selection = "remove", valuetype = "regex")
#' dfm_select(dfmat, pattern = stopwords("english"), selection = "keep", valuetype = "fixed")
#' dfm_select(dfmat, pattern = stopwords("english"), selection = "remove", valuetype = "fixed")
#'
#' # select based on character length
#' dfm_select(dfmat, min_nchar = 5)
#' 
dfm_select <- function(x, pattern = NULL, 
                       selection = c("keep", "remove"), 
                       valuetype = c("glob", "regex", "fixed"),
                       case_insensitive = TRUE,
                       min_nchar = NULL, max_nchar = NULL,
                       verbose = quanteda_options("verbose")) {
    UseMethod("dfm_select")
}

#' @export
dfm_select.default <-  function(x, pattern = NULL, 
                            selection = c("keep", "remove"), 
                            valuetype = c("glob", "regex", "fixed"),
                            case_insensitive = TRUE,
                            min_nchar = NULL, max_nchar = NULL,
                            verbose = quanteda_options("verbose")) {
    stop(friendly_class_undefined_message(class(x), "dfm_select"))
}

#' @export
dfm_select.dfm <-  function(x, pattern = NULL, 
                            selection = c("keep", "remove"), 
                            valuetype = c("glob", "regex", "fixed"),
                            case_insensitive = TRUE,
                            min_nchar = NULL, max_nchar = NULL,
                            verbose = quanteda_options("verbose")) {
    
    x <- as.dfm(x)
    selection <- match.arg(selection)
    valuetype <- match.arg(valuetype)
    attrs <- attributes(x)
    is_dfm <- FALSE
    feat <- featnames(x)
    
    id <- seq_len(nfeat(x))
    if (is.null(pattern)) {
        if (selection == "keep") {
            id_pat <- seq_len(nfeat(x))
        } else {
            id_pat <- integer()
        }
    } else {
        # special handling if pattern is a dfm
        if (is.dfm(pattern)) {
            #.Deprecated(msg = "pattern = dfm is deprecated; use dfm_match() instead")
            pattern <- featnames(pattern)
            valuetype <- "fixed"
            case_insensitive <- FALSE
            if (selection == "keep") {
                is_dfm <- TRUE
            }
        } else if (is.dictionary(pattern)) {
            pattern <- stri_replace_all_fixed(
                unlist(pattern, use.names = FALSE), 
                ' ', 
                attr(x, "concatenator")
            )
        }
        ids_pat <- pattern2id(pattern, feat, valuetype, case_insensitive)
        id_pat <- unlist_integer(ids_pat, unique = TRUE, use.names = FALSE)
        if ("" %in% feat) id_pat[id_pat == 0] <- 1L
    }
    if (selection == "keep") {
        id <- sort(id_pat)
    } else {
        id <- setdiff(id, id_pat)
    }
    
    # select features based on feature length
    if (is_dfm) {
        x <- dfm_match(x, pattern)
    } else {
        if (!is.null(min_nchar) | !is.null(max_nchar)) {
            len <- stri_length(feat)
            is_short <- is_long <- rep(FALSE, length(len))
            if (!is.null(min_nchar))
                is_short <- len < min_nchar
            if (!is.null(max_nchar))
                is_long <- max_nchar < len
            id_out <- which(is_short | is_long)
            id <- setdiff(id, id_out)
        }
        x <- x[, id] 
    } 
    if (verbose) {
        if ("keep" == selection) {
            message_select("keep", nfeat(x), 0)
        } else {
            message_select("remove", length(feat) - nfeat(x), 0)
        }
    }
    return(x)
}

#' @rdname dfm_select
#' @param ... used only for passing arguments from \code{dfm_remove} or
#'   \code{dfm_keep} to \code{dfm_select}. Cannot include
#'   \code{selection}.
#' @export
#' @examples 
#' dfmat <- dfm(c("This is a document with lots of stopwords.",
#'                "No if, and, or but about it: lots of stopwords."))
#' dfmat
#' dfm_remove(dfmat, stopwords("english"))
dfm_remove <- function(x, ...) {
    if ("selection" %in% names(list(...))) {
        stop("dfm_remove cannot include selection argument")
    }
    dfm_select(x, ..., selection = "remove")
}

#' @rdname dfm_select
#' @export
dfm_keep <- function(x, ...) {
    if ("selection" %in% names(list(...))) {
        stop("dfm_keep cannot include selection argument")
    }
    dfm_select(x, ..., selection = "keep")
}
