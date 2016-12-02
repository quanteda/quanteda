#' compute the Scrabble letter values of text
#' 
#' count the Scrabble letter values of text given a user-supplied function, 
#' such as the sum (default) or mean of the character values.
#' @param x a character vector
#' @param FUN function to be applied to the character values in the text; 
#'   default is \code{sum}, but could also be \code{mean} or a user-supplied 
#'   function
#' @author Kenneth Benoit
#' @return a (named) integer vector of Scabble letter values, computed using
#'   \code{FUN}, corresponding to the input text(s)
#' @note Character values are only defined for non-accented Latin a-z, A-Z 
#'   letters.  Lower-casing is unnecessary.
#' @examples
#' nscrabble(c("muzjiks", "excellency"))
#' nscrabble(data_char_inaugural[1:5], mean)
#' @export
nscrabble <- function(x, FUN = sum) {
    UseMethod("nscrabble")
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
    
    textChars <- tokenize(x, what = "character", removePunct = TRUE)
    textDT <- data.table(docIndex = rep(1:length(textChars), lengths(textChars)),
                         Char = unlist(textChars, use.names = FALSE))
    setkey(textDT, Char)
    
    textDT <- letterVals[textDT]
    textDT <- textDT[order(docIndex), FUN(values, na.rm = TRUE), by = docIndex]
    result <- textDT[, V1]
    if (!is.null(names(x))) names(result) <- names(x)
    result
}

#' deprecated name for nscrabble
#' 
#' Use \code{\link{nscrabble}} instead.
#' @export
#' @keywords internal deprecated
scrabble <- function(x, ...) {
    .Deprecated("nscrabble")
    nscrabble(x, ...)
}
