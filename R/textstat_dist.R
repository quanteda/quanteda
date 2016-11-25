#' @title Distance matrix between documents and/or features 
#' 
#' @description These functions compute distance matrix between documents and/or features from a 
#' \code{\link{dfm}} and return a standard \code{\link[stats]{dist}} object.  
#'     
#' @slot selection character or character vector of document names or feature 
#'   labels from the dfm
#' @slot n the top \code{n} most similar items will be returned.  If n is \code{NULL}, return all items.
#' @slot margin identifies the margin of the dfm on which similarity will be 
#'   computed:  \code{documents} for documents or \code{features} for word/term
#'   features.
#' @slot method a valid method for computing similarity from 
#'   \code{\link[proxy]{pr_DB}}, default "euclidean".
#' @slot normalize a deprecated argument retained (temporarily) for legacy 
#'   reasons.  If you want to compute similarity on a "normalized" dfm objects 
#'   (e.g. \code{x}), wrap it in \code{\link{weight}(x, "relFreq")}.
#' @slot digits decimal places to display similarity values
#' @slot tri whether the upper triangle of the symmetric \eqn{V \times V} matrix is recorded
#' @slot diag whether the diagonal of the distance matrix should be recorded
#' @seealso \link{dfm}
#' @export
#' @import methods
#' @docType class
#' @name dist-class
#' @keywords internal
setClass("dist",
         slots = c(selection = "character", n = "integer", margin = "character", 
                   method = "character", normalize = "logical", 
                   digits = "integer", tri = "logical", diag = "logical"),
         prototype = list (selection = character(0), n = NULL,
                           margin = c("documents", "features"),
                           method = "euclidean",
                           normalize = FALSE, tri = FALSE, diag = FALSE)
         #contains = "Matrix"
         )

#' @examples
#' # create a dfm from inaugural addresses from Reagan onwards
#' presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), ignoredFeatures = stopwords("english"),
#'                stem = TRUE)
#' 
#' # compute some document similarities
#' (tmp <- textstat_dist(presDfm, margin = "documents"))
#' # output as a list
#' #as.list(tmp)
#' # for specific comparisons
#' textstat_dist(presDfm, "1985-Reagan", n = 5, margin = "documents")
#' textstat_dist(presDfm, c("2009-Obama" , "2013-Obama"), n = 5, margin = "documents")
#' textstat_dist(presDfm, c("2009-Obama" , "2013-Obama"), margin = "documents")
#' #textstat_dist(presDfm, "2005-Bush", margin = "documents", method = "eJaccard")
#' 
#' @rdname dist-class
#' @export
setGeneric(name = "textstat_dist",
           signature = c("x", "selection", "n", "margin", "normalize"),
           def = function(x, selection = character(0), n = NULL,
                          margin = c("documents", "features"),
                          method = "euclidean", 
                          normalize = FALSE, tri = FALSE, diag = FALSE)
               {
                standardGeneric("textstat_dist")
               })


#' @rdname dist-class
#' @export
setMethod(f = "textstat_dist", 
          signature = signature("dfm", "ANY"),
          def = function(x, selection = character(0), n = NULL, 
                         margin = c("documents", "features"),
                         method = "euclidean",
                         normalize = FALSE, tri = TRUE, diag = FALSE ) {
              
              # value <- match.arg(value)
              
              if (normalize) {
                  warning("normalize is deprecated, not applied - use weight() instead")
                  # x <- weight(x, "relFreq")  # normalize by term freq.
              }
              
              margin <- match.arg(margin)
              if (margin == "features") {
                  items <- features(x)
              } else {
                  items <- docnames(x)
              }
              
              if (is.null(n) || n >= length(items))
                  #n <- length(items) - 1 # choose all features/docs if n is NULL
                  n <- length(items) 
              
              if (length(selection) != 0L) {
                  # retain only existing features or documents
                  selectIndex <- which(items %in% selection)
                  if (length(selectIndex)==0)
                      stop("no such documents or feature labels exist.")
                  
                  if (margin=="features") {
                      xSelect <- x[, selectIndex, drop=FALSE]
                  } else {
                      xSelect <- x[selectIndex, , drop=FALSE]
                  }
              } else xSelect <- NULL
              
              vecMethod <- c("euclidean")
              vecMethod_simil <- c("euclidean", "jaccard", "eJaccard","simple matching")
              
              if (method %in% vecMethod){
                  result <- get(paste(method,"Sparse", sep = ""))(x, xSelect, margin = ifelse(margin == "documents", 1, 2))
              } else if (method %in% vecMethod_simil) {
                  result <- 1 - get(paste(method,"Sparse", sep = ""))(x, xSelect, margin = ifelse(margin == "documents", 1, 2))
              } else{
                  # use proxy::dist() for all other methods
                  result <- as.matrix(proxy::dist(as.matrix(x), as.matrix(xSelect), method = method,
                                                  by_rows = ifelse(margin=="features", FALSE, TRUE)), diag = 1)
              }

              # convert NaNs to NA
              # similmatrix[is.nan(similmatrix)] <- NA
              
              # create a full square matrix if result is calculated only for selected features
              if (length(selection) != 0L) {
                  # adjust the order of the rows to put the selected features as the top rows
                  rname <- rownames(result)
                  cname <- colnames(result)
                  rname <- c(cname, rname[!rname %in% cname])
                  result <- result[rname,]
                  
                  # create a full square matrix 
                  nn <- if(length(selection) == 1L) length(result) else nrow(result)
                  rname <- if(length(selection) == 1L) names(result) else rownames(result)
                  x <- matrix(data = NA,nrow = nn,ncol = nn, dimnames = list(rname, rname))
                  if(length(selection) == 1L){
                      x[, 1] <- result
                  } else {
                      x[, 1:ncol(result)] <- result
                  }
                  result <- x
              }
              
              # truncate to n if n is not NULL
              if (!is.null(n))
                  result <- head(result, n)
             
              # discard the upper diagonal if tri == TRUE
              if (tri)
                  result[upper.tri(result, diag = !diag)]<-0

              # create a new dist object
              p <- nrow(result)
              if(ncol(result) != p) warning("non-square matrix")
              
              # only retain lower triangular elements for the dist object
              distM <- result[row(result) > col(result)]
              
              # set the attributes of the dist object
              attributes(distM) <- NULL
              attr(distM, "Size") <- nrow(result)
              if (!is.null(rownames(result))) attr(distM, "Labels") <- rownames(result)
              attr(distM, "Diag") <- diag
              attr(distM, "Upper") <- !tri
              attr(distM, "method") <- method
              attr(distM, "call") <- match.call()
              attr(distM, "dimnames") <- NULL
              class(distM) <- "dist"
              
              # This will call Stats::print.dist() and Stats::as.matrix.dist()
              distM
          })

# convert the dist class object to the sorted list used in tm::findAssocs()
#' @param sorted sort results in descending order if \code{TRUE}
#' @param n the top \code{n} most similar items will be returned, sorted in 
#'   descending order.  If n is \code{NULL}, return all items.
#' @rdname dist-class
#' @param ... unused
#' @export
as.list.dist <- function(x, sorted = TRUE, n = NULL, ...) {
    # convert the matrix to a list of similarities
    if (!is.null(attr(x, "Labels"))) xLabels <- attr(x, "Labels")
    result <- lapply(seq_len(ncol(as.matrix(x))), function(i) as.matrix(x)[, i])
    attributes(x) <- NULL
    names(result) <- if (!is.null(xLabels)) xLabels
    
    # remove the element of each similarity vector equal to the item itself
    tempseq <- seq_along(result)
    names(tempseq) <- names(result)
    result <- lapply( tempseq, function(i)
        result[[i]] <- result[[i]][-which(names(result[[i]]) == names(result)[i])] )
    
    # sort each element of the list and return only first n results if n not NULL
    if (sorted == TRUE)
        result <- lapply(result, sort, decreasing=TRUE, na.last = TRUE)
    
    # truncate to n if n is not NULL
    if (!is.null(n))
        result <- lapply(result, "[", 1:n)
    
    result
    
}

## used Matrix::crossprod and Matrix::tcrossprod for sparse Matrix handling

euclideanSparse <- function(x, y = NULL, sIndex = NULL, margin = 1){
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    marginSums <- if (margin == 2) colSums else rowSums
    cpFun <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod   
     n <- if (margin == 2) ncol(x) else nrow(x)
    
    if (!is.null(y)) {
        stopifnot(ifelse(margin == 2, nrow(x) == nrow(y), ncol(x) == ncol(y)))
        an <- marginSums(x^2)
        bn <- marginSums(y^2)
        
        # number of features
        kk <- y@Dim[1]
        tmp <- matrix(rep(an, kk), nrow = n) 
        tmp <-  tmp +  matrix(rep(bn, n), nrow = n, byrow=TRUE)
        eucmat <- sqrt( tmp - 2 * as.matrix(cpFun(x, y)) )
    } else {
        an <- marginSums(x^2)
        tmp <- matrix(rep(an, n), nrow = n) 
        tmp <-  tmp +  matrix(rep(an, n), nrow = n, byrow=TRUE)
        eucmat <- sqrt( tmp - 2 * as.matrix(cpFun(x)))
    }
    eucmat
}

