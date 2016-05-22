
#' Experimental c++ indexing
#' @param x tokenizedTexts
#' @export
#' @examples 
#' \dontrun{## performance comparisons
#' data(SOTUCorpus, package = "quantedaData")
#' toks <- tokenize(tokenize(SOTUCorpus, what='sentence', simplify = TRUE), removePunct = TRUE)
#' 
#' microbenchmark::microbenchmark(
#' dfm(toks),
#' dfm2(toks),
#' times=1, unit='relative')
#' 
dfm2 <- function(x){
  docNames <- names(x)
  all <- unlist(x, use.names = FALSE)
  types <- unique(all)
  n <- length(all)
  index_document <- vector(mode="integer", length=n)
  index_feature <- vector(mode="integer", length=n)
  index_cpp(x, types, index_document, index_feature)
  #print(range(index_document))
  #print(range(index_feature))
  cat("Making sparseMatrix\n");
  mx <- Matrix::sparseMatrix(i = index_document, 
                             j = index_feature, 
                             x = 1L,
                             dimnames = list(docs = docNames, features = types))
  return(mx)
}
