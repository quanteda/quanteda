#' Select features from a dfm or fcm
#'
#' This function selects or removes features from a [dfm] or [fcm],
#' based on feature name matches with `pattern`.  The most common usages
#' are to eliminate features from a dfm already constructed, such as stopwords,
#' or to select only terms of interest from a dictionary.
#'
#' @param x the [dfm] or [fcm] object whose features will be selected
#' @inheritParams pattern
#' @param selection whether to `keep` or `remove` the features
#' @inheritParams valuetype
#' @param min_nchar,max_nchar optional numerics specifying the minimum and
#'   maximum length in characters for tokens to be removed or kept; defaults are
#'   `NULL` for no limits.  These are applied after (and hence, in addition
#'   to) any selection based on pattern matches.
#' @param padding if `TRUE`, record the number of removed tokens in the first column.
#' @param verbose if `TRUE`, print message about how many pattern were
#'   removed
#' @details `dfm_remove` and `fcm_remove` are simply a convenience
#'   wrappers to calling `dfm_select` and `fcm_select` with
#'   `selection = "remove"`.
#'
#'   `dfm_keep` and `fcm_keep` are simply a convenience wrappers to
#'   calling `dfm_select` and `fcm_select` with `selection =
#'   "keep"`.
#' @note This function selects features based on their labels.  To select
#'   features based on the values of the document-feature matrix, use
#'   [dfm_trim()].
#' @return A [dfm] or [fcm] object, after the feature selection has
#'   been applied.
#'
#'   For compatibility with earlier versions, when `pattern` is a
#'   [dfm] object and `selection = "keep"`, then this will be
#'   equivalent to calling [dfm_match()].  In this case, the following
#'   settings are always used: `case_insensitive = FALSE`, and
#'   `valuetype = "fixed"`.  This functionality is deprecated, however, and
#'   you should use [dfm_match()] instead.
#'
#' @export
#' @keywords dfm
#' @seealso [dfm_match()]
#' @examples
#' dfmat <- tokens(c("My Christmas was ruined by your opposition tax plan.",
#'                "Does the United_States or Sweden have more progressive taxation?")) |>
#'     dfm(tolower = FALSE)
#' dict <- dictionary(list(countries = c("United_States", "Sweden", "France"),
#'                         wordsEndingInY = c("by", "my"),
#'                         notintext = "blahblah"))
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
                       padding = FALSE,
                       verbose = quanteda_options("verbose")) {
    UseMethod("dfm_select")
}

#' @export
dfm_select.default <-  function(x, pattern = NULL,
                            selection = c("keep", "remove"),
                            valuetype = c("glob", "regex", "fixed"),
                            case_insensitive = TRUE,
                            min_nchar = NULL, max_nchar = NULL,
                            padding = FALSE,
                            verbose = quanteda_options("verbose")) {
    check_class(class(x), "dfm_select")
}

#' @export
dfm_select.dfm <-  function(x, pattern = NULL,
                            selection = c("keep", "remove"),
                            valuetype = c("glob", "regex", "fixed"),
                            case_insensitive = TRUE,
                            min_nchar = NULL, max_nchar = NULL,
                            padding = FALSE,
                            verbose = quanteda_options("verbose")) {

    x <- as.dfm(x)
    selection <- match.arg(selection)
    valuetype <- match.arg(valuetype)
    
    if (is.dfm(pattern)) {
        lifecycle::deprecate_stop(
            when = "3.0", 
            what = "dfm_select(pattern = 'cannot be a dfm')",
            with = I('`dfm_match(x, pattern)`')
        )
    }

    attrs <- attributes(x)
    feat <- featnames(x)
        
    id <- seq_len(nfeat(x))
    if (is.null(pattern)) {
        if (selection == "keep") {
            id_pat <- seq_len(nfeat(x))
        } else {
            id_pat <- integer()
        }
    } else {
        if (is.dictionary(pattern)) {
            pattern <- stri_replace_all_fixed(
                unlist(pattern, use.names = FALSE),
                " ",
                field_object(attrs, "concatenator")
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
    if (verbose)
        before <- stats_dfm(x)
    if (padding) {
        n <- rowSums(x)
        x <- x[, id]
        m <- n - rowSums(x)
        if (sum(m)) {
            if (!nfeat(x) || "" != featnames(x)[1])
                x <- cbind(make_null_dfm("", docnames(x)), x)
            x[,1] <- x[,1] + m
            x <- rebuild_dfm(as.dfm(x), attrs)
            # x@padding <- TRUE TODO: add padding slot
        }
    } else {
        x <- x[, id]
    }
    if (verbose)
        message_dfm(ifelse(selection == "keep", "dfm_keep()", "dfm_remove()"), 
                    before, stats_dfm(x))
    return(x)
}

#' @rdname dfm_select
#' @param ... used only for passing arguments from `dfm_remove` or
#'   `dfm_keep` to `dfm_select`. Cannot include
#'   `selection`.
#' @export
#' @examples
#' dfmat <- dfm(tokens(c("This is a document with lots of stopwords.",
#'                       "No if, and, or but about it: lots of stopwords.")))
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
