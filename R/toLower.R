#' Convert texts to lower (or upper) case
#' 
#' Convert texts or tokens to lower (or upper) case
#' @param x texts to be lower-cased (or upper-cased)
#' @param keepAcronyms if \code{TRUE}, do not lowercase any all-uppercase words.
#'   Only applies to \code{toLower}.
#' @param ... additional arguments passed to \pkg{stringi} functions, (e.g. 
#'   \code{\link{stri_trans_tolower}}), such as \code{locale}
#' @return Texts tranformed into their lower- (or upper-)cased versions.  If \code{x} is a 
#'   character vector or a corpus, return a character vector.  If 
#'   \code{x} is a list of tokenized texts, then return a list of 
#'   tokenized texts.
#' @export
#' @examples 
#' test1 <- c(text1 = "England and France are members of NATO and UNESCO", 
#'            text2 = "NASA sent a rocket into space.")
#' toLower(test1)
#' toLower(test1, keepAcronyms = TRUE)
#' 
#' test2 <- tokenize(test1, removePunct=TRUE)
#' toLower(test2)
#' toLower(test2, keepAcronyms = TRUE)
toLower <- function(x, keepAcronyms = FALSE, ...) {
    UseMethod("toLower")
}

#' @rdname toLower
#' @export
toLower.character <- function(x, keepAcronyms = FALSE, ...) {
    savedNames <- names(x)
    if (keepAcronyms)
        x <- stri_replace_all_regex(x, "\\b(\\p{Uppercase_Letter}{2,})\\b",  "_$1_", ...)
    x <- stri_trans_tolower(x, ...)
    if (keepAcronyms) {
        m1 <- unique(unlist(stri_extract_all_regex(x, "\\b_\\p{Lowercase_Letter}+_\\b", omit_no_match = TRUE, ...)))
        if (length(m1) > 0) {
            m2 <- stri_replace_all_fixed(stri_trans_toupper(m1, ...), "_", "", ...)
            x <- sapply(x, function(s) stri_replace_all_regex(s, m1,  m2, vectorize_all = FALSE, ...))
        }
    }
    names(x) <- savedNames
    return(x)
}

#' @rdname toLower
#' @export
toLower.NULL <- function(x, ...) NULL

#' @rdname toLower
#' @export
toLower.tokenizedTexts <- function(x, keepAcronyms = FALSE, ...) {
    attributes_saved <- attributes(x)
    typeTest <- all(sapply(x, is.character))
    if (!typeTest) {
        stop("Each element of the list must be a character vector.")
    }
    x <- lapply(x, toLower, keepAcronyms = keepAcronyms, ...)
    class(x) <- c("tokenizedTexts", class(x))
    attributes(x) <- attributes_saved
    x
}


#' @rdname toLower
#' @export
toLower.corpus <- function(x, keepAcronyms=FALSE, ...) {
    toLower(texts(x), keepAcronyms, ...)
}

#' @rdname toLower
#' @export
#' @examples 
#' test1 <- c(text1 = "England and France are members of NATO and UNESCO", 
#'            text2 = "NASA sent a rocket into space.")
#' toUpper(test1)
#' 
#' test2 <- tokenize(test1, removePunct = TRUE)
#' toUpper(test2)
toUpper <- function(x, ...) {
    UseMethod("toUpper")
}

#' @rdname toLower
#' @export
toUpper.character <- function(x, ...) {
    savedNames <- names(x)
    x <- stri_trans_toupper(x, ...)
    names(x) <- savedNames
    return(x)
}

#' @rdname toLower
#' @export
toUpper.NULL <- function(x, ...) NULL

#' @rdname toLower
#' @export
toUpper.tokenizedTexts <- function(x, ...) {
    attributes_saved <- attributes(x)
    typeTest <- all(sapply(x, is.character))
    if (!typeTest) {
        stop("Each element of the list must be a character vector.")
    }
    x <- lapply(x, toUpper, ...)
    class(x) <- c("tokenizedTexts", class(x))
    attributes(x) <- attributes_saved
    x
}


#' @rdname toLower
#' @export
toUpper.corpus <- function(x, ...) {
    toUpper(texts(x), ...)
}

