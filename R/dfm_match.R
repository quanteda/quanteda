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
#' dfm_match(dfm(""), letters[1:5])
#' dfm_match(data_dfm_lbgexample, c("A", "B", "Z"))
#' dfm_match(data_dfm_lbgexample, c("B", "newfeat1", "A", "newfeat2"))
#' 
#' # matching one dfm to another
#' txt <- c("This is text one", "The second text", "This is text three")
#' (dfmat1 <- dfm(txt[1:2]))
#' (dfmat2 <- dfm(txt[2:3]))
#' (dfmat3 <- dfm_match(dfmat1, featnames(dfmat2)))
#' setequal(featnames(dfmat2), featnames(dfmat3))
#' @export
dfm_match <- function(x, features) {
    x <- as.dfm(x)
    if (!is.character(features))
        stop("features must be a character vector")
    slots <- get_dfm_slots(x)
    x <- pad_dfm(x, features)
    set_dfm_slots(x) <- slots
    return(x)
}
