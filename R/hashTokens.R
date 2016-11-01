#' Constructor for tokenizedTextsHashed objects
#' 
#' Creates a hashed object of tokenizedTexts.
#' @param x a source of tokenizedText
#' @param vocabulary optional vocabulary for mapping of tokens
#' @param ... additional arguments
#' @return A list the hashed tokens found in each text
#' @import fastmatch
#' @details The hashTokens is designed to hash the tokenizedTexts object and
#'   create the tokenizedTextsHashed object for functions like
#'   \code{\link{dfm}}, \code{\link{fcm}}, \code{\link{selectFeatures}} etc. to
#'   improve the performance of these functions in terms of speed, especially
#'   for large dataset.
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
hashTokens.tokenizedTexts <- function(x, vocabulary, ...) {
    if (!is.tokenizedTexts(x)) 
        stop("Input must be tokenizedTexts")
    types <- unique(unlist(x, use.names = FALSE))

    # order the features alphabetically
    types <- sort(types)
    types <- types[types!=''] # make padding NA
    
    if (missing(vocabulary)) {
        vocabulary <- types
    } else {
        vocabulary <- c(vocabulary, types[!types %in% vocabulary]) 
    }
    xNumeric <- lapply(x, function(x,y) fmatch(x,y), vocabulary)

    class(xNumeric) <- c("tokenizedTextsHashed", class(xNumeric))
    attr(xNumeric, "vocabulary") <- vocabulary
    return(xNumeric)
}

#' @rdname hashTokens
#' @details \code{as.tokenizedTexts} coerces tokenizedTextsHashed to a
#'   tokenizedText class object, making the methods available for this object
#'   type available to this object.
#' @export
as.tokenizedTexts.tokenizedTextsHashed <- function(x, ...) {
    if (!is.tokenizedTextsHashed(x))
        stop("input must be a tokenizedTextsHashed types")
    
    types <- attr(x, "vocabulary")
    xTt <- lapply(x, function(x,y) y[x], types)
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

#' @details \code{tokenizeHashed} creates tokenizedTextsHashed object from characters vactors 
#' without creating a large intermediate tokenizedTexts object.
#' @param size_chunk size for batches of conversion of texts (number of documents)
#' @examples 
#' txt <- c('a b c d e', 'd e f g h', 'f g h i j', 'i j k l m')
#' tokenizeHashed(txt, size_chunk=2)
#'
#' \dontrun{data(SOTUCorpus, package = "quantedaData")
#' txt <- rep(unlist(tokenize(SOTUCorpus, what='sentence')), 20)
#' system.time(toks <- hashTokens(tokenize(txt)))
#' system.time(toks2 <- tokenizeHashed(txt))
#' }
#' @rdname hashTokens
#' @export
tokenizeHashed <- function(x, size_chunk = 1000, ...) {
  
  xSubs <- split(x, ceiling(seq_along(x)/size_chunk))
  xTokSubs <- list()
  for(i in 1:length(xSubs)){
    #cat('Tokenizing and hashing ...\n')
    if(i == 1){
      xTokSubs[[i]] <- hashTokens(tokenize(xSubs[[i]], ...))
    }else{
      xTokSubs[[i]] <- hashTokens(tokenize(xSubs[[i]], ...), attr(xTokSubs[[i-1]], "vocabulary"))
    }
  }
  xTok <- unlist(xTokSubs, recursive = FALSE)
  class(xTok) <- c("tokenizedTextsHashed", class(xTok))
  attr(xTok, "vocabulary") <- attr(xTokSubs[[length(xTokSubs)]], "vocabulary")
  return(xTok)
  
}


