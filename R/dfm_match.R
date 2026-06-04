#' Match the dfm columns with given features 
#' 
#' Match the columns of multiple [dfm] objects using a character vector. 
#' @param x the [dfm] object.
#' @param features character vector for the feature names to be matched in the 
#' resulting [dfm]. Columns not included in `features` are removed.
#' @details
#' Matching the dfm columns is necessary when you split a corpus into two: you fit 
#' a model on the test set and evaluation it on the test set whose features must be 
#' identical. It is also used in [bootstrap_dfm()].
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
#' txt <- c("This is text one", "The text two", "This is text three")
#' (dfmt1 <- dfm(tokens(txt[1:2])))
#' (dfmt2 <- dfm(tokens(txt[2:3])))
#' (dfmt3 <- dfm(dfm_match(dfmt1, featnames(dfmt2))))
#' identical(featnames(dfmt2), featnames(dfmt3))
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
