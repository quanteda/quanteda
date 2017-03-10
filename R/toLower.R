#' Convert texts to lower (or upper) case
#' 
#' Convert texts or tokens to lower (or upper) case
#' @param x texts to be lower-cased (or upper-cased)
#' @param keep_acronyms if \code{TRUE}, do not lowercase any all-uppercase words.
#'   Only applies to \code{toLower}.
#' @param ... additional arguments passed to \pkg{stringi} functions, (e.g. 
#'   \code{\link{stri_trans_tolower}}), such as \code{locale}
#' @return Texts tranformed into their lower- (or upper-)cased versions.  If \code{x} is a 
#'   character vector or a corpus, return a character vector.  If 
#'   \code{x} is a list of tokenized texts, then return a list of 
#'   tokenized texts.
#' @export
#' @keywords internal deprecated
#' @examples 
#' \donttest{
#' test1 <- c(text1 = "England and France are members of NATO and UNESCO", 
#'            text2 = "NASA sent a rocket into space.")
#' toLower(test1)
#' toLower(test1, keep_acronyms = TRUE)
#' 
#' test2 <- tokenize(test1, removePunct=TRUE)
#' toLower(test2)
#' toLower(test2, keep_acronyms = TRUE)
#' }
toLower <- function(x, keep_acronyms = FALSE, ...) {
    UseMethod("toLower")
}

#' @rdname toLower
#' @export
toLower.character <- function(x, keep_acronyms = FALSE, ...) {
    .Deprecated("char_tolower")
    char_tolower(x, keep_acronyms = keep_acronyms, ...)
}

#' @rdname toLower
#' @export
toLower.NULL <- function(x, ...) NULL

#' @rdname toLower
#' @export
toLower.tokenizedTexts <- function(x, keep_acronyms = FALSE, ...) {
    attributes_saved <- attributes(x)
    typeTest <- all(sapply(x, is.character))
    if (!typeTest) {
        stop("Each element of the list must be a character vector.")
    }
    x <- lapply(x, char_tolower, keep_acronyms = keep_acronyms, ...)
    class(x) <- c("tokenizedTexts", class(x))
    attributes(x) <- attributes_saved
    x
}

#' @rdname toLower
#' @export
toLower.tokens <- function(x, ...) {
    .Deprecated("tokens_tolower")
    tokens_tolower(x, ...)
}

#' @rdname toLower
#' @export
toUpper.tokens <- function(x, ...) {
    .Deprecated("tokens_toupper")
    tokens_toupper(x, ...)
}


#' @rdname toLower
toLower.corpus <- function(x, ...) {
    tolower(texts(x), ...)
}

#' @rdname toLower
#' @export
#' @examples 
#' \donttest{
#' test1 <- c(text1 = "England and France are members of NATO and UNESCO", 
#'            text2 = "NASA sent a rocket into space.")
#' toUpper(test1)
#' 
#' test2 <- tokenize(test1, removePunct = TRUE)
#' toUpper(test2)
#' }
toUpper <- function(x, ...) {
    UseMethod("toUpper")
}

#' @rdname toLower
#' @export
toUpper.character <- function(x, ...) {
    .Deprecated("char_toupper")
    char_toupper(x, ...)
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

