#' Experimental C++ indexing dfm
#' 
#' Experimental dfm function that takes tokenizedTexts object as an input.  For
#' testing only.
#' @param x tokenizedTexts
#' @export
#' @examples 
#' \dontrun{## performance comparisons
#' data(SOTUCorpus, package = "quantedaData")
#' toks <- tokenize(tokenize(SOTUCorpus, what='sentence', simplify = TRUE), removePunct = TRUE)

#' microbenchmark::microbenchmark(dfm(toks2),
#'                                dfm2(toks2),
#'                                dfm3(toks2),
#'                                times=1, unit='relative')
#' 
#' toks2 <- rep(toks, 20)
#' attributes(toks2) <- attributes(toks)
#' microbenchmark::microbenchmark(dfm(toks2),
#'                                dfm2(toks2),
#'                                dfm3(toks2),
#'                                times=1, unit='relative')
#' }
dfm2 <- function(x) {
    docNames <- names(x)
    all <- unlist(x, use.names = FALSE)
    types <- unique(all)
    n <- length(all)
    index_document <- vector(mode="integer", length=n)
    index_feature <- vector(mode="integer", length=n)
    index_cpp(x, types, index_document, index_feature)
    #print(range(index_document))
    #print(range(index_feature))
    cat("Making sparseMatrix\n")
    mx <- Matrix::sparseMatrix(i = index_document, 
                               j = index_feature, 
                               x = 1L,
                               dimnames = list(docs = docNames, features = types))
    return(mx)
}

dfm3 <- function(x) {
  docNames <- names(x)
  all <- unlist(x, use.names = FALSE)
  types <- unique(all)
  n <- length(all)
  index <- index_cpp2(x, types, n)
  cat("Making sparseMatrix\n")
  mx <- Matrix::sparseMatrix(i = index$doc, 
                             j = index$feature, 
                             x = 1L,
                             dimnames = list(docs = docNames, features = types))
  return(mx)
}
