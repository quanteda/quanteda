#' @title Distance matrix between documents and/or features 
#' 
#' @description These functions compute distance matrix between documents and/or features from a 
#' \code{\link{dfm}} and return a standard \code{\link[stats]{dist}} object.  
#' @rdname textstat_simil
#' @param dist whether the distance matrix should be converted into an object of class "dist". 
#' @seealso \link{dfm}
#' @export
#' @details \code{textstat_dist} options are: \code{"euclidean"} (default), 
#' \code{"euclidean"}, \code{"hamming"}, \code{"Chisquared"}, 
#' \code{"Chisquared2"}, and \code{"kullback"}.
#' 
#' Distance matrix created from some methods, such as "kullback", is not symmetric. 
#' Hence the conversion to a dist object is not recommanded. 
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
#' textstat_dist(presDfm, "1985-Reagan", n = 5, margin = "documents")
#' textstat_dist(presDfm, c("2009-Obama" , "2013-Obama"), n = 5, margin = "documents")
#' textstat_dist(presDfm, c("2009-Obama" , "2013-Obama"), margin = "documents")
#' textstat_dist(presDfm, "2005-Bush", margin = "documents", method = "eJaccard")
#' 
textstat_dist <- function(x, selection = character(0), n = NULL, 
                         margin = c("documents", "features"),
                         method = "euclidean",
                         tri = TRUE, diag = FALSE, dist = TRUE) {

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
    
    vecMethod <- c("euclidean", "hamming", "Chisquared","Chisquared2", "kullback")
    vecMethod_simil <- c("jaccard", "binary", "eJaccard","simple matching")
    
    if (method %in% vecMethod){
        result <- get(paste(method,"Sparse", sep = ""))(x, xSelect, margin = ifelse(margin == "documents", 1, 2))
    } else if (method %in% vecMethod_simil) {
        if (method == "binary") method = "jaccardf"
        result <- 1 - get(paste(method,"Sparse", sep = ""))(x, xSelect, margin = ifelse(margin == "documents", 1, 2))
    } else{
        # use proxy::dist() for all other methods
        stop("The metric is not currently supported by quanteda, please use other packages such as proxy::dist()/simil().")
        #result <- as.matrix(proxy::dist(as.matrix(x), as.matrix(xSelect), method = method,
        #                                by_rows = ifelse(margin=="features", FALSE, TRUE)), diag = 1)
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
    
    if (!is.null(n))
        result <- result[1:n,]
    
    # discard the upper diagonal if tri == TRUE
    if (tri)
        result <- Matrix::tril(result)
    
    if (dist){
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
    } else {
        result
    }
}

# convert the dist class object to the sorted list used in tm::findAssocs()

#' coerce a dist object into a list
#' 
#' Coerce a dist matrix into a list of selected terms and tarhet terms in
#' descending order.  Can be used after calling \code{\link{textstat_simil}} or
#' \code{\link{textstat_dist}}.
#' @param x dist class object
#' @param sorted sort results in descending order if \code{TRUE}
#' @param n the top \code{n} highest-ranking items will be returned.  If n is 
#'   \code{NULL}, return all items.
#' @param ... unused
#' @method as.list dist
#' @export
#' @examples 
#' \dontrun{
#' ## compare to tm
#' 
#' # tm version
#' require(tm)
#' data("crude")
#' crude <- tm_map(crude, content_transformer(tolower))
#' crude <- tm_map(crude, removePunctuation)
#' crude <- tm_map(crude, removeNumbers)
#' crude <- tm_map(crude, stemDocument)
#' tdm <- TermDocumentMatrix(crude)
#' findAssocs(tdm, c("oil", "opec", "xyz"), c(0.75, 0.82, 0.1))
#' 
#' # in quanteda
#' quantedaDfm <- new("dfmSparse", Matrix::Matrix(t(as.matrix(tdm))))
#' as.list(textstat_simil(quantedaDfm, c("oil", "opec", "xyz"), margin = "features", n = 14))
#' 
#' # in base R
#' corMat <- as.matrix(proxy::simil(as.matrix(quantedaDfm), by_rows = FALSE))
#' round(head(sort(corMat[, "oil"], decreasing = TRUE), 14), 2)
#' round(head(sort(corMat[, "opec"], decreasing = TRUE), 9), 2)
#' } 
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
        kk <- y@Dim[margin]
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

# Hamming distance
# formula: hamming = sum(x .!= y)
hammingSparse <- function(x, y = NULL, margin = 1) {
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    
    # convert to binary matrix
    x <- tf(x, "boolean") 
    x0 <- 1 - x
    cpFun <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    marginSums <- if (margin == 2) nrow else ncol
    marginNames <- if (margin == 2) colnames else rownames
    # union 
    an <- marginSums(x)
    if (!is.null(y)) {
        y <- tf(y, "boolean")
        y0 <- 1 - y
        A <- cpFun(x, y)
        A0 <- cpFun(x0, y0)
        colNm <- marginNames(y)
    } else {
        A <- cpFun(x)
        A0 <- cpFun(x0)
        colNm <- marginNames(x)
    }
    rowNm <- marginNames(x)
    # common values
    A <- A + A0
    hammat <- an -A
    dimnames(hammat) <- list(rowNm,  colNm)
    hammat
}

#Chi-squared distance:divide by row sums and square root of column sums, and adjust for square root of matrix total (Legendre & Gallagher 2001, Bruce McCune & James b. Grace 2002). 
#http://adn.biol.umontreal.ca/~numericalecology/Reprints/Legendre_&_Gallagher.pdf
# https://www.pcord.com/book.htm
#formula: Chi = sum((x/rowsum(x_i) - y/rowsum(y_i))^2/(colsum(i)/total))
ChisquaredSparse <- function(x, y = NULL, sIndex = NULL, margin = 1){
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    marginSums <- if (margin == 2) colSums else rowSums
    marginNames <- if (margin == 2) colnames else rownames
    aveProfile <- if (margin == 2) sqrt(rowSums(x)/sum(x)) else sqrt(colSums(x)/sum(x))
    cpFun <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod   
    n <- if (margin == 2) ncol(x) else nrow(x)
    rowNm <- marginNames(x)
    colNm <- marginNames(x)
    if (margin == 1 ) {
        # convert into profiles
        x <- x/marginSums(x)
    
        # weighted by the average profiles
        x <- x %*% diag(1/aveProfile)
    } else {
        x <- x %*% diag(1/marginSums(x))
        x <- x / aveProfile
    }
    
    if (!is.null(y)) {
        stopifnot(ifelse(margin == 2, nrow(x) == nrow(y), ncol(x) == ncol(y)))
        colNm <- marginNames(y)
        # aveProfile is same as that for x 
        if (margin == 1 ) {
            # convert into profiles
            y <- y/marginSums(y)
            
            # weighted by the average profiles
            y <- y %*% diag(1/aveProfile)
        } else {
            y <- y %*% diag(1/marginSums(y))
            y <- y / aveProfile
        }
        an <- marginSums(x^2)
        bn <- marginSums(y^2)
        
        # number of features
        kk <- y@Dim[1]
        tmp <- matrix(rep(an, kk), nrow = n) 
        tmp <-  tmp +  matrix(rep(bn, n), nrow = n, byrow=TRUE)
        chimat <- tmp - 2 * as.matrix(cpFun(x, y))
        #chimat <-  sqrt(round(chimat, 2)) 
    } else {
        an <- marginSums(x^2)
        tmp <- matrix(rep(an, n), nrow = n) 
        tmp <-  tmp +  matrix(rep(an, n), nrow = n, byrow=TRUE)
        chimat <-  tmp - 2 * as.matrix(cpFun(x))
        #chimat <-  sqrt(round(chimat, 2)) 
    }
    dimnames(chimat) <- list(rowNm,  colNm)
    chimat
}

## This chi-squared method is used for histogram: sum((x-y)^2/((x+y)))/2
##http://www.ariel.ac.il/sites/ofirpele/publications/ECCV2010.pdf
Chisquared2Sparse <- function(x, y = NULL, sIndex = NULL, margin = 1){
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
        chimat <- sqrt( tmp - 2 * as.matrix(cpFun(x, y)) )
    } else {
        an <- marginSums(x^2)
        tmp <- matrix(rep(an, n), nrow = n) 
        tmp <-  tmp +  matrix(rep(an, n), nrow = n, byrow=TRUE)
        
        a1 <- marginSums(x)
        sumij <- matrix(rep(a1, n), nrow = n) + matrix(rep(a1, n), nrow = n, byrow=TRUE)
        
        chimat <- ( tmp - 2 * as.matrix(cpFun(x)))/sumij/2
    }
    chimat
}

# Kullback-Leibler divergence: is a measure of the difference between probability distributions
# This metric is not symmetric, it is better applied with setting of "dist = FALSE" and "tri= FALSE"
# to avoid eoercing the result to a dist object.
# assumption: p(x_i) = 0 implies p(y_i)=0 and in case both p(x_i) and p(y_i) equals to zero, 
# p(x_i)*log(p(x_i)/p(y_i)) is assumed to be zero as the limit value.
# formula: sum(p(x)*log(p(x)/p(y)))
kullbackSparse <- function(x, y = NULL, margin = 1) {
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    cpFun <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    marginSums <- if (margin == 2) colSums else rowSums
    marginNames <- if (margin == 2) colnames else rownames
    
    # probability
    x <- x/marginSums(x)
    logx <- log(x)
    logx[is.na(logx)] <- 0L
    logx[is.infinite(logx)] <- 0L
    if (!is.null(y)) {
        y <- y/marginSums(y)
        logy <- log(y)
        logy[is.na(logy)] <- 0L
        logy[is.infinite(logy)] <- 0L
        kullmat <- marginSums(x*logx - cpFun(x, logy))
        colNm <- marginNames(y)
    } else {
        kullmat <- marginSums(x*logx) - cpFun(x, logx)
        colNm <- marginNames(x)
    }
    rowNm <- marginNames(x)
    dimnames(kullmat) <- list(rowNm,  colNm)
    kullmat
}
