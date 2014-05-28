#' Convert a quanteda dfm (document feature matrix) into a tm term-document triplet matrix
#'
#'
#' tm uses a triplet matrix representation to represent term frequency per document. This 
#' function uses slam's coercion function to convert a standard feature matrix to this 
#' format for compatibility with #' packages that use this representation
#' 
#' @param d A matrix object with row names equal to the document names and column names
#'  equal to the feature labels, as returned by dfm()
#' @param weight Tm's coercion function accepts weightings such as tf idf, see tm's 
#'  as.termDocumentMatrix for list of possible args. The default is just tf (term frequency)
#' @return A triplet matrix of class TermDocumentMatrix
#' @export
#' @examples
#' data(movies)
#' d <- dfm(movies)
#' td <- toTdm(d)
toTdm <- function(d, weight=weightTf){
  sl <- as.simple_triplet_matrix(d)
  td <- as.TermDocumentMatrix(sl, weighting=weight)
  return(td)
}