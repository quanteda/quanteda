#' Constructor for tokenizedTextsHashed objects
#' 
#' Creates a hashed object of tokenizedTexts. 
#' @param x a source of tokenizedText 
#' @param ... additional arguments
#' @return A list the hashed tokens found in each text
#' @import fastmatch 
#' @details The hashTokens is designed to hash the tokenizedTexts object
#' and create the tokenizedTextsHashed object for functions like \code{\link{dfm}},
#' \code{\link{cfm}}, \code{\link{selectFeatures}} etc. to improve the performance
#' of these functions in terms of speed, especially for large dataset. 
#' @author Kenneth Benoit and Haiyan Wang
#' @export
#' @seealso \code{\link{tokenize}}
#' @examples 
#' txt <- c("The quick brown fox jumped over the lazy dog.",
#'          "The dog jumped and ate the fox.")
#' toks <- tokenize(toLower(txt), removePunct = TRUE)
#' toksHashed <- hashTokens(toks)
#' # returned as a list
#' as.list(toksHashed)
#' # returned as a tokenized Text
#' as.tokenizedTexts(toksHashed)

hashTokens <- function(x, ...) {
    UseMethod("hashTokens")
}

#' @rdname hashTokens
#' @export
hashTokens.tokenizedTexts <- function(x,...){
    if (!is.tokenizedTexts(x)) 
        stop("Input must be tokenizedTexts")
    types <- unique(unlist(x, use.names = FALSE))
   
    # order the features alphabetically
    types <- sort(types)
    # xNumeric <- mclapply(x, function(x,y) fmatch(x,y), types)
    xNumeric <- mclapply(x, function(x,y) fmatch(x,y), types)
    class(xNumeric) <- c("tokenizedTextsHashed", class(xNumeric))
    attr(xNumeric, "vocabulary") <- types
    return(xNumeric)
}

#' @rdname hashTokens
#' @details \code{as.tokenizedTexts} coerces tokenizedTextsHashed to a tokenizedText class object, 
#' making the methods available for this object type available to this object.
#' @export
as.tokenizedTexts.tokenizedTextsHashed <- function(x, ...) {
     if (!is.tokenizedTextsHashed(x))
        stop("input must be a tokenizedTextsHashed types")
    
      types <- attr(x, "vocabulary")
      xTt <- mclapply(x, function(x,y) y[x], types)
      xTt
}

#' @rdname hashTokens
#' @export
as.list.tokenizedTextsHashed <- function(x, ...){
    if (!is.tokenizedTextsHashed(x))
        stop("input must be a tokenizedTextsHashed types")
    attr(x,"vocabulary") <- NULL
    xList <- unclass(x)
    xList
}

#' @rdname hashTokens
#' @export
is.tokenizedTextsHashed <- function(x){
    ifelse("tokenizedTextsHashed" %in% class(x), TRUE, FALSE)
}

#' print a tokenizedTextsHashed objects
#' print method for a tokenizedTextsHashed object
#' @param x a tokenizedTextHashed object created by \link{hashTokens}
#' @param ... further arguments passed to base print method
#' @export
#' @method print tokenizedTextsHashed
print.tokenizedTextsHashed <- function(x, ...) {
    ndocuments <- ifelse(is.list(x), length(x), 1)
    cat("tokenizedTextHashed object from ", ndocuments, " document", 
        ifelse(ndocuments > 1, "s", ""), ".\n", sep = "")
    if (is.list(x)) { 
        class(x) <- "listof"
        print(x, ...)
    } else {
        x <- as.character(x)
        print(x, ...)
    }
}