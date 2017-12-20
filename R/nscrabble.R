#' Count the Scrabble letter values of text
#' 
#' Tally the Scrabble letter values of text given a user-supplied function, such
#' as the sum (default) or mean of the character values.
#' @param x a character vector
#' @param FUN function to be applied to the character values in the text; 
#'   default is \code{sum}, but could also be \code{mean} or a user-supplied 
#'   function
#' @author Kenneth Benoit
#' @return a (named) integer vector of Scrabble letter values, computed using 
#'   \code{FUN}, corresponding to the input text(s)
#' @note Character values are only defined for non-accented Latin a-z, A-Z 
#'   letters.  Lower-casing is unnecessary.
#'   
#'   We would be happy to add more languages to this \emph{extremely useful
#'   function} if you send us the values for your language!
#' @examples
#' nscrabble(c("muzjiks", "excellency"))
#' nscrabble(data_corpus_inaugural[1:5], mean)
#' @export
nscrabble <- function(x, FUN = sum) {
    UseMethod("nscrabble")
}

#' @export
nscrabble.default <- function(x, FUN = sum) {
    stop(friendly_class_undefined_message(class(x), "nscrabble"))
}

#' @rdname nscrabble
#' @noRd
#' @export
nscrabble.character <- function(x, FUN = sum) {
    FUN <- match.fun(FUN)
    letter <- Char <- docIndex <- values <- V1 <- NULL
    
    letterVals <- data.table(letter = c(letters, LETTERS),
                             values = as.integer(rep(c(1, 3, 3, 2, 1, 4, 2, 4, 1, 8, 5, 1, 3, 1, 1, 3, 10, 1, 1, 1, 1, 4, 4, 8, 4, 10), 2)))
    setkey(letterVals, letter)
    
    textChars <- as.list(tokens(x, what = "character", remove_punct = TRUE))
    textDT <- data.table(docIndex = rep(seq_along(textChars), lengths(textChars)),
                         Char = unlist(textChars, use.names = FALSE))
    setkey(textDT, Char)
    
    textDT <- letterVals[textDT]
    textDT <- textDT[order(docIndex), FUN(values, na.rm = TRUE), by = docIndex]
    result <- textDT[, V1]
    if (!is.null(names(x))) names(result) <- names(x)
    result
}

#' Deprecated name for nscrabble
#' 
#' Use \code{\link{nscrabble}} instead.
#' @export
#' @keywords internal deprecated
scrabble <- function(x, ...) {
    .Deprecated("nscrabble")
    nscrabble(x, ...)
}

#### English scrabble values ----- 
# (1 point)-A, E, I, O, U, L, N, S, T, R.
# (2 points)-D, G.
# (3 points)-B, C, M, P.
# (4 points)-F, H, V, W, Y.
# (5 points)-K.
# (8 points)- J, X.
# (10 points)-Q, Z.
