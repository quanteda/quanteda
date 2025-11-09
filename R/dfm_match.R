#' Match the feature set of a dfm to given feature names
#' 
#' Match the feature set of a [dfm] to a specified vector of feature names.
#' For existing features in `x` for which there is an exact match for an
#' element of `features`, these will be included.  Any features in `x`
#' not `features` will be discarded, and any feature names specified in
#' `features` but not found in `x` will be added with all zero counts.
#'
#' Selecting on another [dfm]'s [featnames()] is useful when you
#' have trained a model on one dfm, and need to project this onto a test set
#' whose features must be identical. It is also used in
#' [bootstrap_dfm()].
#' @param x a [dfm]
#' @param features character; the feature names to be matched in the output dfm
#' @inheritParams messages
#' @return 
#' A [dfm] whose features are identical to those specified in
#' `features`. 
#' @note Unlike [dfm_select()], this function will add feature names
#'   not already present in `x`. It also provides only fixed,
#'   case-sensitive matches.  For more flexible feature selection, see
#'   [dfm_select()].
#' @seealso [dfm_select()]
#' @keywords dfm
#' @examples
#' # matching a dfm to a feature vector
#' dfm_match(dfm(tokens("")), letters[1:5])
#' dfm_match(data_dfm_lbgexample, c("A", "B", "Z"))
#' dfm_match(data_dfm_lbgexample, c("B", "newfeat1", "A", "newfeat2"))
#' 
#' # matching one dfm to another
#' txt <- c("This is text one", "The second text", "This is text three")
#' (dfmat1 <- dfm(tokens(txt[1:2])))
#' (dfmat2 <- dfm(tokens(txt[2:3])))
#' (dfmat3 <- dfm_match(dfmat1, featnames(dfmat2)))
#' setequal(featnames(dfmat2), featnames(dfmat3))
#' @export
dfm_match <- function(x, features,
                      verbose = quanteda_options("verbose")) {
    UseMethod("dfm_match")
}

#' @export
dfm_match.default <- function(x, features,
                              verbose = quanteda_options("verbose")) {
    check_class(class(x), "dfm_match")
}

#' @export
dfm_match.dfm <- function(x, features,
                          verbose = quanteda_options("verbose")) {
    x <- as.dfm(x)
    features <- check_character(features, min_len = 0, max_len = Inf)
    attrs <- attributes(x)
    if (verbose)
        before <- stats_dfm(x)
    x <- pad_dfm(x, features)
    result <- rebuild_dfm(x, attrs)
    if (verbose)
        message_dfm("dfm_match()", before, stats_dfm(result))
    return(result)
}
