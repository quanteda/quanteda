#' Similarity and distance computation between documents or features
#'
#' These functions compute matrixes of distances and similarities between
#' documents or features from a \code{\link{dfm}} and return a
#' \code{\link[stats]{dist}} object (or a matrix if specific targets are
#' selected).  They are fast and robust because they operate directly on the
#' sparse \link{dfm} objects.
#' @param x a \link{dfm} object
#' @param selection a valid index for document or feature names from \code{x},
#'   to be selected for comparison
#' @param margin identifies the margin of the dfm on which similarity or
#'   difference will be computed:  \code{"documents"} for documents or
#'   \code{"features"} for word/term features
#' @param method method the similarity or distance measure to be used; see
#'   Details
#' @param p The power of the Minkowski distance.
#' @param rank an integer value specifying top-n most similar documents or features
#'   to be recorded.
#' @param min_simil minimum similarity value to be recoded.
#' @param condition boolean flag used only in developemnt
#' @details \code{textstat_simil2} options are: \code{"correlation"} (default),
#'   \code{"cosine"}, \code{"jaccard"}, \code{"ejaccard"}, \code{"dice"},
#'   \code{"edice"}, \code{"simple matching"}, \code{"hamman"}, and
#'   \code{"faith"}.
#' @note If you want to compute similarity on a "normalized" dfm object
#'   (controlling for variable document lengths, for methods such as correlation
#'   for which different document lengths matter), then wrap the input dfm in
#'   \code{\link{dfm_weight}(x, "prop")}.
#' @return \code{textstat_simil2} and \code{textstat_dist} return
#'   \code{\link{dist}} class objects if selection is \code{NULL}, otherwise, a
#'   matrix is returned matching distances to the documents or features
#'   identified in the selection.
#' @export
#' @seealso \code{\link{textstat_dist}}, \code{\link{as.list.dist}},
#'   \code{\link{dist}}
#' @examples
#' # similarities for documents
#' pres_dfm <- dfm(data_corpus_inaugural, remove_punct = TRUE, remove = stopwords("english"))
#' (s1 <- textstat_simil2(pres_dfm, method = "cosine", margin = "documents"))
#' as.matrix(s1)
#' as.list(s1)
#'
#' # similarities for for specific documents
#' textstat_simil2(pres_dfm, "2017-Trump", margin = "documents")
#' textstat_simil2(pres_dfm, "2017-Trump", method = "cosine", margin = "documents")
#' textstat_simil2(pres_dfm, c("2009-Obama" , "2013-Obama"), margin = "documents")
#'
#' # compute some term similarities
#' s2 <- textstat_simil2(pres_dfm, c("fair", "health", "terror"), method = "cosine",
#'                       margin = "features")
#' head(as.matrix(s2), 10)
#' as.list(s2, n = 8)
#' 
textstat_simil2 <- function(x, selection = NULL,
                           margin = c("documents", "features"),
                           method = c("cosine", "correlation", "jaccard", "ejaccard",
                                      "dice", "edice", "hamman", "simple matching", "faith",
                                      "euclidean", "chisquared", "hamming", "kullback", 
                                      "manhattan", "maximum", "canberra", "minkowski"), 
                           rank = NULL,
                           min_simil = NULL, p = 2, condition = FALSE) {
    UseMethod("textstat_simil2")
}
    

#' @export    
textstat_simil2.default <- function(x, selection = NULL,
                               margin = c("documents", "features"),
                               method = c("cosine", "correlation", "jaccard", "ejaccard",
                                          "dice", "edice", "hamman", "simple matching", "faith",
                                          "euclidean", "chisquared", "hamming", "kullback",
                                          "manhattan", "maximum", "canberra", "minkowski"), 
                               rank = NULL,
                               min_simil = NULL, p = 2, condition = FALSE) {
    stop(friendly_class_undefined_message(class(x), "textstat_simil2"))
}
    
#' @export    
textstat_simil2.dfm <- function(x, selection = NULL,
                                margin = c("documents", "features"),
                                method = c("cosine", "correlation", "jaccard", "ejaccard",
                                           "dice", "edice", "hamman", "simple matching", "faith",
                                           "euclidean", "chisquared", "hamming", "kullback",
                                           "manhattan", "maximum", "canberra", "minkowski"), 
                                rank = NULL,
                                min_simil = NULL, p = 2, condition = FALSE) {
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
    if (is.null(min_simil)) 
        min_simil <- -1.0
    if (is.null(rank))
        rank <- ncol(x)
    
    boolean <- FALSE
    weight <- 1
    if (method == "jaccard") {
        boolean <- TRUE
        weight <- 1
        method <- "ejaccard"
    } else if (method == "ejaccard") {
        boolean <- FALSE
        weight <- 2
    } else if (method == "dice") {
        boolean <- TRUE
        weight <- 1
        method <- "edice"
    } else if (method == "edice") {
        boolean <- FALSE
        weight <- 2
    } else if (method == "hamman") {
        boolean <- TRUE
        weight <- 1
    } else if (method == "simple matching") {
        boolean <- TRUE
        weight <- 0
        method <- "hamman"
    } else if (method == "faith") {
        boolean <- TRUE
    } else if (method == "hamming") {
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
                                             i, rank, min_simil, condition)
    } else {
        result <- qatd_cpp_similarity(x, match(method, c("ejaccard", "edice", "hamman", "faith", 
                                                         "euclidean", "chisquared", "hamming", "kullback",
                                                         "manhattan", "maximum", "canberra", "minkowski")), 
                                      i, rank, min_simil, weight)
    }
    label <- colnames(x)
    rownames(result) <- label
    if (is.null(selection)) {
        colnames(result) <- label
    } else {
        result <- result[,i, drop = FALSE]
        colnames(result) <- label[i]
    }
    return(result)
}

