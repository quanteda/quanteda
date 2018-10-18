#' Similarity and distance computation between documents or features
#'
#' These functions compute matrixes of distances and similarities between
#' documents or features from a \code{\link{dfm}} and return a
#' \code{\link[stats]{dist}} object (or a matrix if specific targets are
#' selected).  They are fast and robust because they operate directly on the
#' sparse \link{dfm} objects, with the option of returning a sparse matrix
#' object.
#' @param x a \link{dfm} object
#' @param selection a valid index for document or feature names from \code{x},
#'   to be selected for comparison
#' @param margin identifies the margin of the dfm on which similarity or
#'   difference will be computed:  \code{"documents"} for documents or
#'   \code{"features"} for word/term features
#' @param method method the similarity or distance measure to be used; see
#'   Details
#' @param upper whether the upper triangle of the symmetric \eqn{V \times V}
#'   matrix is recorded. Only used when \code{value = "dist"}.
#' @param diag whether the diagonal of the distance matrix should be recorded. .
#'   Only used when \code{value = "dist"}.
#' @param min_simil minimum similarity value to be recoded.
#' @param rank an integer value specifying top-n most similar documents or
#'   features to be recorded.
#' @param value format of the returned object: if \code{"dist"}, a
#'   \code{\link{dist}} object; or if \code{"sparsematrix"}, a triangular
#'   compressed sparse column matrix format of class
#'   \link[Matrix]{dtCMatrix-class}.  See Value below.
#' @details \code{textstat_simil} options are: \code{"correlation"} (default),
#'   \code{"cosine"}, \code{"jaccard"}, \code{"ejaccard"}, \code{"dice"},
#'   \code{"edice"}, \code{"simple matching"}, \code{"hamman"}, and
#'   \code{"faith"}.
#' @note If you want to compute similarity on a "normalized" dfm object
#'   (controlling for variable document lengths, for methods such as correlation
#'   for which different document lengths matter), then wrap the input dfm in
#'   \code{\link{dfm_weight}(x, "prop")}.
#' @return By default, \code{textstat_simil} and \code{textstat_dist} return
#'   \code{\link{dist}} class objects if selection is \code{NULL}, otherwise, a
#'   matrix is returned matching distances to the documents or features
#'   identified in the selection.
#'   
#'   For dealing with large numbers of features or documents, it is more
#'   efficient (and far less memory intensive) to return sparse matrix objects
#'   where the values below \code{min_dist} or \code{min_simil} are dropped.
#'   This is the default when values for these arguments are supplied or when
#'   \code{value = "sparsematrix"}, in which case the returned object will be a
#'   compressed sparse column matrix format of class
#'   \link[Matrix]{dtCMatrix-class}.  Coercion methods are available for
#'   converting sparse return objects to the \code{dist} class via
#'   \code{\link{as.dist}}, or can be converted into other sparse matrix formats
#'   using \code{\link{as}}.
#' @export
#' @seealso \code{\link{textstat_dist}}, \code{\link{as.list.dist}},
#'   \code{\link{dist}}, \code{\link{as.dist}}
#' @examples
#' # similarities for documents
#' mt <- dfm(data_corpus_inaugural, remove_punct = TRUE, remove = stopwords("english"))
#' (s1 <- textstat_simil(mt, method = "cosine", margin = "documents"))
#' as.matrix(s1)
#' as.list(s1)
#'
#' # similarities for for specific documents
#' textstat_simil(mt, "2017-Trump", margin = "documents")
#' textstat_simil(mt, "2017-Trump", method = "cosine", margin = "documents")
#' textstat_simil(mt, c("2009-Obama" , "2013-Obama"), margin = "documents")
#'
#' # compute some term similarities
#' s2 <- textstat_simil(mt, c("fair", "health", "terror"), method = "cosine",
#'                       margin = "features")
#' head(as.matrix(s2), 10)
#' as.list(s2, n = 8)
#' 
textstat_simil <- function(x, selection = NULL,
                           margin = c("documents", "features"),
                           method = c("correlation", "cosine", "jaccard", "ejaccard",
                                      "dice", "edice", "hamman", "simple matching", "faith"), 
                           upper = FALSE, diag = FALSE, 
                           min_simil = NULL, rank = NULL,
                           value = if (is.null(min_simil) && is.null(rank)) "dist" else "sparsematrix") {
    UseMethod("textstat_simil")
}
    

#' @export    
textstat_simil.default <- function(x, selection = NULL,
                               margin = c("documents", "features"),
                               method = c("correlation", "cosine", "jaccard", "ejaccard",
                                          "dice", "edice", "hamman", "simple matching", "faith"), 
                               upper = FALSE, diag = FALSE, 
                               min_simil = NULL, rank = NULL,
                               value = if (is.null(min_simil) && is.null(rank)) "dist" else "sparsematrix") {
    stop(friendly_class_undefined_message(class(x), "textstat_simil"))
}
    
#' @export    
textstat_simil.dfm <- function(x, selection = NULL,
                               margin = c("documents", "features"),
                               method = c("correlation", "cosine", "jaccard", "ejaccard",
                                          "dice", "edice", "hamman", "simple matching", "faith"), 
                               upper = FALSE, diag = FALSE, 
                               min_simil = NULL, rank = NULL,
                               value = if (is.null(min_simil) && is.null(rank)) "dist" else "sparsematrix") {
    
    method <- match.arg(method)
    value <- match.arg(value, choices = c("dist", "sparsematrix"))
    result <- textstat_proxy(x, selection, margin, method, 1, min_simil, rank)
    if (value == "dist")
        result <- as_dist(result, method, match.call(), diag = diag, upper = upper)
    return(result)
}


#' @rdname textstat_simil
#' @export
#' @param p The power of the Minkowski distance.
#' @param min_dist minimum distance value to be recoded.
#' @details \code{textstat_dist} options are: \code{"euclidean"} (default), 
#'   \code{"kullback"}. \code{"manhattan"}, \code{"maximum"}, \code{"canberra"},
#'   and \code{"minkowski"}.
#' @references 
#'   \code{"kullback"} is the Kullback-Leibler distance, which assumes that
#'   \eqn{P(x_i) = 0} implies \eqn{P(y_i)=0}, and in case either \eqn{P(x_i)} or
#'   \eqn{P(y_i)} equals to zero, then \eqn{P(x_i) * log(p(x_i)/p(y_i))} is
#'   assumed to be zero as the limit value.  The formula is:
#'    \deqn{\sum{P(x)*log(P(x)/p(y))}}
#'    
#'   All other measures are described in the \pkg{proxy} package.
#' @importFrom RcppParallel RcppParallelLibs
#' @examples
#' # create a dfm from inaugural addresses from Reagan onwards
#' mt <- dfm(corpus_subset(data_corpus_inaugural, Year > 1990), 
#'                remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)
#'                
#' # distances for documents 
#' (d1 <- textstat_dist(mt, margin = "documents"))
#' as.matrix(d1)
#' 
#' # distances for specific documents
#' textstat_dist(mt, "2017-Trump", margin = "documents")
#' (d2 <- textstat_dist(mt, c("2009-Obama" , "2013-Obama"), margin = "documents"))
#' as.list(d1)
#' 
textstat_dist <- function(x, selection = NULL, 
                          margin = c("documents", "features"),
                          method = c("euclidean", "kullback",
                                     "manhattan", "maximum", "canberra", "minkowski"), 
                          upper = FALSE, diag = FALSE, 
                          p = 2, min_dist = NULL, rank = NULL, 
                          value = if (is.null(min_dist) && is.null(rank)) "dist" else "sparsematrix") {
    UseMethod("textstat_dist")
}

#' @export
textstat_dist.default <- function(x, selection = NULL, 
                                  margin = c("documents", "features"),
                                  method = c("euclidean", "kullback",
                                             "manhattan", "maximum", "canberra", "minkowski"), 
                                  upper = FALSE, diag = FALSE, 
                                  p = 2, min_dist = NULL, rank = NULL, 
                                  value = if (is.null(min_dist) && is.null(rank)) "dist" else "sparsematrix") {
    stop(friendly_class_undefined_message(class(x), "textstat_dist"))
}

#' @export
textstat_dist.dfm <- function(x, selection = NULL, 
                              margin = c("documents", "features"),
                              method = c("euclidean", "kullback",
                                         "manhattan", "maximum", "canberra", "minkowski"), 
                              upper = FALSE, diag = FALSE, 
                              p = 2, min_dist = NULL, rank = NULL, 
                              value = if (is.null(min_dist) && is.null(rank)) "dist" else "sparsematrix") {
    
    method <- match.arg(method)
    value <- match.arg(value, choices = c("dist", "sparsematrix"))
    result <- textstat_proxy(x, selection, margin, method, p, min_dist, rank)
    if (value == "dist")
        result <- as_dist(result, method, match.call(), diag = diag, upper = upper)
    return(result)
}

# internal function for textstat_dist and textstat_simil
textstat_proxy <- function(x, selection = NULL,
                           margin = c("documents", "features"),
                           method = c("cosine", "correlation", "jaccard", "ejaccard",
                                      "dice", "edice", "hamman", "simple matching", "faith",
                                      "euclidean", "chisquared", "hamming", "kullback",
                                      "manhattan", "maximum", "canberra", "minkowski"), 
                           p = 2, min_proxy = NULL, rank = NULL) {
    x <- as.dfm(x)
    if (!sum(x)) stop(message_error("dfm_empty"))
    margin <- match.arg(margin)
    method <- match.arg(method)
    if (margin == "documents") 
        x <- t(x)
    if (is.null(selection)) {
        i <- seq(ncol(x))
    } else {
        if (is.character(selection)) {
            i <- match(selection, colnames(x))
        } else {
            if (is.logical(selection))
                selection <- which(selection)
            i <- selection
            i[i < 1 | ncol(x) < i] <- NA
        }
        if (any(is.na(i)))
            stop(paste(selection[is.na(i)], collapse = ", "), " does not exist")
    }
    if (is.null(min_proxy)) 
        min_proxy <- -1.0
    if (is.null(rank))
        rank <- ncol(x)
    if (rank < 1)
        stop("rank must be great than or equal to 1")
    
    boolean <- FALSE
    weight <- 1
    if (method == "jaccard") {
        boolean <- TRUE
        method <- "ejaccard"
    } else if (method == "ejaccard") {
        weight <- 2
    } else if (method == "dice") {
        boolean <- TRUE
        method <- "edice"
    } else if (method == "edice") {
        weight <- 2
    } else if (method == "hamman") {
        boolean <- TRUE
    } else if (method == "faith") {
        boolean <- TRUE
    } else if (method == "simple matching") {
        boolean <- TRUE
    } else if (method == "minkowski") {
        if (p <= 0) 
            stop("p must be greater than zero")
        weight <- p
    }
    if (boolean)
        x <- dfm_weight(x, "boolean")
    if (method %in% c("cosine", "correlation")) {
        result <- qatd_cpp_similarity_linear(x, match(method, c("cosine", "correlation")),
                                             i, rank, min_proxy)
    } else {
        result <- qatd_cpp_similarity(x, match(method, c("ejaccard", "edice", "hamman", "simple matching", "faith", 
                                                         "euclidean", "chisquared", "kullback",
                                                         "manhattan", "maximum", "canberra", "minkowski")), 
                                      i, rank, min_proxy, weight)
    }
    label <- colnames(x)
    rownames(result) <- label
    if (is.null(selection)) {
        colnames(result) <- label
    } else {
        result <- result[,i, drop = FALSE]
        colnames(result) <- label[i]
    }
    return(as(result, "CsparseMatrix"))
}

# internal function to coerce to dist object
as_dist <- function(x, method, call, diag = diag, upper = upper) {
    # warning("dist object is deprecated as an output of textstat_dist/simil function. ",
    #         "Please coerce a sparse matrix to a dist object using as.dist(as.matrix(x)).")
    x <- as.matrix(x)
    if (ncol(x) == nrow(x)) {
        x <- as.dist(x, diag = diag, upper = upper)
    } else {
        attr(x, "Labels") <- colnames(x)
        attr(x, "Size") <- ncol(x)
        class(x) <- c("dist_selection")
    }
    attr(x, "method") <- method
    attr(x, "call") <- call
    return(x)
}
