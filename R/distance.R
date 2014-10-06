
#' compute similarities between documents and/or features
#' 
#' Compute similarities between documents and/or features from a 
#' \code{\link{dfm}}. Uses the similarity measures defined in 
#' \link[proxy]{simil}.  See \code{\link[proxy]{pr_DB}} for available distance 
#' measures, or how to create your own.
#' @param x a \link{dfm} object
#' @param selection character or character vector of document names or feature 
#'   labels from the dfm
#' @param n the top \code{n} most similar items will be returned, sorted in 
#'   descending order.  If n is \code{NULL}, return all items.
#' @param margin identifies the margin of the dfm on which similarity will be 
#'   computed: \code{features} for word/term features or \code{documents} for 
#'   documents.
#' @param method a valid method for computing similarity from 
#'   \code{\link[proxy]{pr_DB}}
#' @param sort sort results in descending order if \code{TRUE}
#' @param normalize if \code{TRUE}, normalize the dfm by term frequency within 
#'   document (so that the dfm values will be relative term frequency within 
#'   each document)
#' @return a named list of the selection labels, with a sorted named vector of 
#'   similarity measures.
#' @note The method for computing feature similarities can be quite slow when
#'   there are large numbers of feature types.  Future implementations will
#'   hopefully speed this up.
#' @examples
#' # create a dfm from inaugural addresses from Reagan onwards
#' presDfm <- dfm(subset(inaugCorpus, Year>1980), stopwords=TRUE, stem=TRUE)
#' 
#' # compute some document similarities
#' similarity(presDfm, "1985-Reagan", n=5, margin="documents")
#' similarity(presDfm, c("2009-Obama" , "2013-Obama"), n=5, margin="documents")
#' similarity(presDfm, c("2009-Obama" , "2013-Obama"), n=NULL, margin="documents")
#' similarity(presDfm, c("2009-Obama" , "2013-Obama"), n=NULL, margin="documents", method="cosine")
#' similarity(presDfm, "2005-Bush", n=NULL, margin="documents", method="eJaccard", sort=FALSE)
#' 
#' \dontrun{
#' # compute some term similarities
#' similarity(presDfm, c("fair", "health", "terror"), method="cosine")
#' 
#' # compare to tm
#' require(tm)
#' data("crude")
#' tdm <- TermDocumentMatrix(crude)
#' findAssocs(tdm, c("oil", "opec", "xyz"), c(0.7, 0.75, 0.1))
#' crudeDfm <- dfm(corpus(crude))
#' similarity(crudeDfm, c("oil", "opec", "xyz"), normalize=FALSE)
#' }
#' @export
similarity <- function(x, selection, n=10, 
                       margin=c("features", "documents"),
                       method="correlation", 
                       sort=TRUE, normalize=TRUE) {
    if (!is.dfm(x))
        stop("findAssociations only works for dfm objects.")
    margin = match.arg(margin)
    if (margin=="features") 
        items <- features(x) else
            items <- docnames(x)
    
    if (is.null(n) || n > length(items))
        n <- length(items)  # choose all features/docs if n is NULL
    
    # retain only existing features or documents
    selectIndex <- which(items %in% selection)
    if (length(selectIndex)==0)
        stop("no such documents or feature labels exist.")
    result <- as.list(selectIndex)
    names(result) <- items[selectIndex]
    # match(selection, items, nomatch=0L)
    
    if (normalize) x <- tf(x)  # normalize by term freq.
    
    similmatrix <- round(as.matrix(proxy::simil(x, method, by_rows=ifelse(margin=="features", FALSE, TRUE))), 3)
    
    for (item in selection) {
        result[[item]] <- similmatrix[, result[[item]]]
        if (sort==TRUE & !is.null(n)) 
            result[[item]] <- sort(result[[item]], decreasing=TRUE)[1:n] # sort
        result[[item]] <- result[[item]][!is.na(result[[item]])]  # remove NA items
    }
    # return a vector if list of length 1, otherwise return a named list
    if (length(result)==1) return(result[[1]]) else return(result)
}

