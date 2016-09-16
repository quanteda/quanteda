#' constructor for tokenizedTextsHashed objects
#' 
#' Creates a hashed version of tokenizedText. 
#' @param x a source of tokenizedText 
#' @param ... additional arguments
#' @return 
#' @seealso \link{tokenize}
#' @details 
#' @export
hashingTokens <- function(x, ...) {
    UseMethod("hashingTokens")
}

#' @rdname hashingTokens
#' @export
hashingtokens.tokenizedTexts <- function(x){
    if (!is.tokenizedTexts(x)) 
        stop("Input must be tokenizedTexts")
    types <- unique(unlist(x, use.names = FALSE))
    xNumeric <- lapply(x, function(x,y) match(x,y), types)
    class(xNumeric) <- c("tokenizedTextsHashed", class(xNumeric))
    attr(xNumeric, "vocabulary") <- x
    return(xNumeric)
}

#' @rdname hashingTokens
#' @details \code{as.tokenizedTexts} coerces tokenizedTextsHashed to a tokenizedText class object, 
#' making the methods available for this object type available to this object.
#' @export
as.tokenizedTexts.tokenizedTextsHashed <- function(x) {
     if (!is.tokenizedTextsHashed(x))
        stop("input must be a tokenizedTextsHashed types")
      xtt <- attr(x, "vocabulary")
      xtt
}

 #' @rdname hashingTokens
#' @export
# as.list.tokenizedTextsHashed <- function(){
#     if (!is.tokenizedTextsHashed(x))
#         stop("input must be a tokenizedTextsHashed types")
#     xtt
# 
# }

#' @rdname hashingTokens
#' @export
is.tokenizedTextsHashed <- function(x){
    ifelse("tokenizedTextsHashed" %in% class(x), TRUE, FALSE)
}

#' print a tokenizedTextsHashed objects
#' 
#' print method for a tokenizedTextsHashed object
#' @param x a tokenizedTextHashed object created by \link{hashingTokens}
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