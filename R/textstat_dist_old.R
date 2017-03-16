#' Distance matrix between documents and/or features 
#' 
#' These functions compute distance matrix between documents and/or features from a 
#' \code{\link{dfm}} and return a standard \code{\link[stats]{dist}} object.  
#' @rdname textstat_simil
#' @seealso \link{dfm}
#' @export
#' @param p The power of the Minkowski distance.
#' @details \code{textstat_dist} options are: \code{"euclidean"} (default), 
#' \code{"Chisquared"}, \code{"Chisquared2"}, \code{"hamming"}, \code{"kullback"}. 
#' \code{"manhattan"}, \code{"maximum"}, \code{"canberra"}, and \code{"minkowski"}.
#' @importFrom RcppParallel RcppParallelLibs
#' @author Kenneth Benoit, Haiyan Wang
#' @examples
#' # create a dfm from inaugural addresses from Reagan onwards
#' presDfm <- dfm(corpus_subset(inaugCorpus, Year > 1980), 
#'                remove = stopwords("english"), stem = TRUE)
#'                
#' ## distance
#' 
#' # compute some document distances
#' (tmp <- textstat_dist(presDfm, margin = "documents"))
#' 
#' # for specific comparisons
#' identical(textstat_dist(presDfm, "1985-Reagan", n = 5, margin = "documents"),
#'           textstat_dist_old(presDfm, "1985-Reagan", n = 5, margin = "documents"))
#' 
#' 
#' 
textstat_dist_old <- function(x, selection = character(0), n = NULL, 
                          margin = c("documents", "features"),
                          method = "euclidean",
                          upper = TRUE, diag = FALSE, p = 2) {
    
    if (!is.dfm(x))
        stop("x must be a dfm object")
    
    # value <- match.arg(value)
    
    margin <- match.arg(margin)
    if (margin == "features") {
        items <- featnames(x)
        xsize <- dim(x)[2]
    } else {
        items <- docnames(x)
        xsize <- dim(x)[1]
    }
    
    if (is.null(n) || n >= xsize)
        n <- xsize # choose all features/docs if n is NULL
    
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
    
    vecMethod <- c("euclidean", "hamming", "Chisquared", "Chisquared2", "kullback", "manhattan", "maximum", "canberra")
    vecMethod_simil <- c("jaccard", "binary", "eJaccard", "simple matching")
    
    # # make all lower case
    # method <- tolower(method)
    # vecMethod <- tolower(vecMethod)
    # vecMethod_simil <- tolower(vecMethod_simil)

    if (method %in% vecMethod) {
        result <- get(paste(method,"_sparse", sep = ""))(x, xSelect, margin = ifelse(margin == "documents", 1, 2))
    } else if (method == "minkowski"){
        result <- get(paste(method,"_sparse", sep = ""))(x, xSelect, margin = ifelse(margin == "documents", 1, 2), p)
    } else if (method %in% vecMethod_simil) {
        if (method == "binary") method = "jaccard"
        result <- get(paste(method,"_sparse", sep = ""))(x, xSelect, margin = ifelse(margin == "documents", 1, 2))
    } else {
        stop("The metric is not currently supported by quanteda, please use other packages such as proxy::dist()/simil().")
    }
    #print(result)
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
        x <- Matrix::Matrix(data = 0,nrow = nn,ncol = nn, dimnames = list(rname, rname))
        if (length(selection) == 1L) {
            x[, 1] <- result
        } else {
            x[, 1:ncol(result)] <- result
        }
        result <- x
    }
    #print(result)
    if (!is.null(n))
        result <- result[1:n,]
    
    # create a new dist object
    distM <- stats::as.dist(result, diag = diag, upper = upper)
    attr(distM, "method") <- method
    attr(distM, "call") <- match.call()
    # This will call Stats::print.dist() and Stats::as.matrix.dist()
    distM
}