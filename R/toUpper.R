#' Convert texts to upper case
#' 
#' Convert texts or tokens to upper case
#' @rdname toUpper
#' @param x texts to be upper-cased
#' @param keepAcronyms if \code{TRUE}, do not uppercase any all-uppercase words
#' @param ... additional arguments passed to \pkg{stringi} functions, (e.g. 
#'   \code{\link{stri_trans_toupper}}), such as \code{locale}
#' @return Texts tranformed into their uppercased versions.  If \code{x} is a 
#'   character vector or a corpus, return a uppercased character vector.  If 
#'   \code{x} is a list of tokenized texts, then return a list of upper-cased 
#'   tokenized texts.
#' @export
#' @examples 
#' test1 <- c(text1 = "England and France are members of NATO and UNESCO", 
#'            text2 = "NASA sent a rocket into space.")
#' toUpper(test1)
#' toUpper(test1, keepAcronyms = TRUE)
#' 
#' test2 <- tokenize(test1, removePunct=TRUE)
#' toUpper(test2)
#' toUpper(test2, keepAcronyms = TRUE)
toUpper <- function(x, keepAcronyms=FALSE, ...) {
    UseMethod("toUpper")
}

#' @rdname toUpper
#' @export
toUpper.character <- function(x, keepAcronyms=FALSE, ...) {
    savedNames <- names(x)
    if (keepAcronyms)
        x <- stri_replace_all_regex(x, "\\b(\\p{Uppercase_Letter}{2,})\\b",  "_$1_", ...)
    x <- stri_trans_toupper(x, ...)
    if (keepAcronyms) {
        m1 <- unique(unlist(stri_extract_all_regex(x, "\\b_\\p{Uppercase_Letter}+_\\b", omit_no_match = TRUE, ...)))
        if (length(m1) > 0) {
            m2 <- stri_replace_all_fixed(stri_trans_toupper(m1, ...), "_", "", ...)
            x <- sapply(x, function(s) stri_replace_all_regex(s, m1,  m2, vectorize_all = FALSE, ...))
        }
    }
    names(x) <- savedNames
    return(x)
}

#' @rdname toUpper
#' @export
toUpper.NULL <- function(x, ...) NULL

#' @rdname toUpper
#' @export
toUpper.tokenizedTexts <- function(x, keepAcronyms = FALSE, ...) {
    attributes_saved <- attributes(x)
    typeTest <- all(sapply(x, is.character))
    if (!typeTest) {
        stop("Each element of the list must be a character vector.")
    }
    x <- lapply(x, toUpper, keepAcronyms = keepAcronyms, ...)
    class(x) <- c("tokenizedTexts", class(x))
    attributes(x) <- attributes_saved
    x
}


#' @rdname toUpper
#' @export
toUpper.corpus <- function(x, keepAcronyms=FALSE, ...) {
    toUpper(texts(x), keepAcronyms, ...)
}
