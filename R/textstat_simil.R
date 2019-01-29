#' Similarity and distance computation between documents or features
#'
#' These functions compute matrixes of distances and similarities between
#' documents or features from a \code{\link{dfm}} and return a
#' \code{\link[stats]{dist}} object (or a matrix if specific targets are
#' selected).  They are fast and robust because they operate directly on the
#' sparse \link{dfm} objects.
#' @param x a \link{dfm} object
#' @param selection a valid index for document or feature names (depending on
#'   \code{margin}) from \code{x}, to be selected for comparison
#' @param margin identifies the margin of the dfm on which similarity or
#'   difference will be computed:  \code{"documents"} for documents or
#'   \code{"features"} for word/term features.
#' @param method method the similarity or distance measure to be used; see
#'   Details.
#' @param upper whether the upper triangle of the symmetric \eqn{V \times V}
#'   matrix is recorded. Only used when \code{value = "dist"}.
#' @param diag whether the diagonal of the distance matrix should be recorded. .
#'   Only used when \code{value = "dist"}.
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
#'   These can be transformed into a list format using
#'   \code{\link{as.list.dist}}, if that format is preferred.
#' @export
#' @seealso \code{\link{textstat_dist}},
#'   \code{\link[quanteda]{as.matrix.simil}},
#'   \code{\link[quanteda]{as.list.dist}}, \code{\link[stats]{dist}},
#'   \code{\link[stats]{as.dist}}
#' @examples
#' # similarities for documents
#' dfmat <- dfm(corpus_subset(data_corpus_inaugural, Year > 1980), 
#'           remove_punct = TRUE, remove = stopwords("english"))
#' (tstat1 <- textstat_simil(dfmat, method = "cosine", margin = "documents"))
#' as.matrix(tstat1)
#' as.list(tstat1)
#'
#' # similarities for for specific documents
#' textstat_simil(dfmat, selection = "2017-Trump", margin = "documents")
#' textstat_simil(dfmat, selection = "2017-Trump", method = "cosine", margin = "documents")
#' textstat_simil(dfmat, selection = c("2009-Obama" , "2013-Obama"), margin = "documents")
#'
#' # compute some term similarities
#' tstat2 <- textstat_simil(dfmat, selection = c("fair", "health", "terror"), method = "cosine",
#'                       margin = "features")
#' head(as.matrix(tstat2), 10)
#' as.list(tstat2, n = 8)
#' 
textstat_simil <- function(x, selection = NULL,
                           margin = c("documents", "features"),
                           method = c("correlation", "cosine", "jaccard", "ejaccard",
                                      "dice", "edice", "hamman", "simple matching", "faith"),
                           upper = FALSE, diag = FALSE) {
    UseMethod("textstat_simil")
}
    

#' @export    
textstat_simil.default <- function(x, selection = NULL,
                               margin = c("documents", "features"),
                               method = c("correlation", "cosine", "jaccard", "ejaccard",
                                          "dice", "edice", "hamman", "simple matching", "faith"),
                               upper = FALSE, diag = FALSE) {
    stop(friendly_class_undefined_message(class(x), "textstat_simil"))
}
    
#' @export    
textstat_simil.dfm <- function(x, selection = NULL,
                               margin = c("documents", "features"),
                               method = c("correlation", "cosine", "jaccard", "ejaccard",
                                          "dice", "edice", "hamman", "simple matching", "faith"),
                               upper = FALSE, diag = FALSE) {
    x <- as.dfm(x)

    margin <- match.arg(margin)
    method <- match.arg(method)

    if (margin == "features") {
        name <- colnames(x)
    } else {
        name <- rownames(x)
    }
    if (is.null(selection)) {
        i <- seq_along(name)
    } else {
        if (is.character(selection)) {
            i <- match(selection, name)
        } else {
            if (is.logical(selection))
                selection <- which(selection)
            i <- selection
            i[i < 1 | length(name) < i] <- NA
        }
        if (any(is.na(i)))
            stop(paste(selection[is.na(i)], collapse = ", "), " does not exist")
    }
    if (margin == "features") {
        result <- textstat_proxy(x, x[, i], margin, method, 1, use_na = TRUE)
    } else {
        result <- textstat_proxy(x, x[i, ], margin, method, 1, use_na = TRUE)
    }
    result <- as_dist(result, method, match.call(), diag = diag, upper = upper)
    if (is.null(selection)) {
        class(result) <- c("simil", "dist")
    } else {
        class(result) <- c("simil_selection", "dist_selection")
    }
    result
}


#' @rdname textstat_simil
#' @export
#' @param p The power of the Minkowski distance.
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
#' dfmat <- dfm(corpus_subset(data_corpus_inaugural, Year > 1990), 
#'                remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)
#'                
#' # distances for documents 
#' (tstat1 <- textstat_dist(dfmat, margin = "documents"))
#' as.matrix(tstat1)
#' 
#' # distances for specific documents
#' textstat_dist(dfmat, "2017-Trump", margin = "documents")
#' (tstat2 <- textstat_dist(dfmat, c("2009-Obama" , "2013-Obama"), margin = "documents"))
#' as.list(tstat2)
#' 
textstat_dist <- function(x, selection = NULL,
                          margin = c("documents", "features"),
                          method = c("euclidean", "kullback",
                                     "manhattan", "maximum", "canberra", "minkowski"),
                          upper = FALSE, diag = FALSE, p = 2) {
    UseMethod("textstat_dist")
}

#' @export
textstat_dist.default <- function(x, selection = NULL,
                                  margin = c("documents", "features"),
                                  method = c("euclidean", "kullback",
                                             "manhattan", "maximum", "canberra", "minkowski"),
                                  upper = FALSE, diag = FALSE, p = 2) {
    stop(friendly_class_undefined_message(class(x), "textstat_dist"))
}

#' @export
textstat_dist.dfm <- function(x, selection = NULL,
                              margin = c("documents", "features"),
                              method = c("euclidean", "kullback",
                                         "manhattan", "maximum", "canberra", "minkowski"),
                              upper = FALSE, diag = FALSE, p = 2) {
    x <- as.dfm(x)

    margin <- match.arg(margin)
    method <- match.arg(method)

    if (margin == "features") {
        name <- colnames(x)
    } else {
        name <- rownames(x)
    }
    if (is.null(selection)) {
        i <- seq_along(name)
    } else {
        if (is.character(selection)) {
            i <- match(selection, name)
        } else {
            if (is.logical(selection))
                selection <- which(selection)
            i <- selection
            i[i < 1 | length(name) < i] <- NA
        }
        if (any(is.na(i)))
            stop(paste(selection[is.na(i)], collapse = ", "), " does not exist")
    }
    if (margin == "features") {
        result <- textstat_proxy(x, x[, i], margin, method, p, use_na = TRUE)
    } else {
        result <- textstat_proxy(x, x[i, ], margin, method, p, use_na = TRUE)
    }
    as_dist(result, method, match.call(), diag = diag, upper = upper)
}

#' [Experimental] Compute document/feature proximity
#'
#' This is an underlying function for \code{textstat_dist} and
#' \code{textstat_simil} but returns \code{TsparseMatrix}.
#' @keywords internal
#' @param y if a \link{dfm} object is provided, proximity between documents or
#'   features in \code{x} and \code{y} is computed.
#' @param use_na if \code{TRUE}, return \code{NA} for proximity to empty
#'   vectors. Note that use of \code{NA} makes the proximity matrices denser.
#' @inheritParams textstat_dist
#' @param min_proxy the minimum proximity value to be recoded.
#' @param rank an integer value specifying top-n most proximity values to be
#'   recorded.
#' @export
#' @seealso \code{\link{textstat_dist}}, \code{\link{textstat_simil}}
textstat_proxy <- function(x, y = NULL,
                           margin = c("documents", "features"),
                           method = c("cosine", "correlation", "jaccard", "ejaccard",
                                      "dice", "edice", "hamman", "simple matching", "faith",
                                      "euclidean", "chisquared", "hamming", "kullback",
                                      "manhattan", "maximum", "canberra", "minkowski"),
                           p = 2, min_proxy = NULL, rank = NULL, use_na = FALSE) {
    x <- as.dfm(x)
    if (is.null(y)) {
        y <- x
    } else {
        if (!is.dfm(y))
            stop("y must be a dfm")
        y <- as.dfm(y)
    }

    margin <- match.arg(margin)
    method <- match.arg(method)

    if (margin == "documents") {
        f <- union(featnames(x), featnames(y))
        x <- t(pad_dfm(x, f))
        y <- t(pad_dfm(y, f))
    } else {
        if (!identical(docnames(x), docnames(y)))
            stop("x and y must contain the same documents")
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
    if (boolean) {
        x <- dfm_weight(x, "boolean")
        y <- dfm_weight(y, "boolean")
    }
    if (method %in% c("cosine", "correlation", "euclidean")) {
        result <-
            qatd_cpp_similarity_linear(x, y,
                                       match(method, c("cosine", "correlation", "euclidean")),
                                       rank, min_proxy)
    } else {
        result <-
            qatd_cpp_similarity(x, y,
                                match(method, c("ejaccard", "edice", "hamman", "simple matching",
                                                "faith", "chisquared", "kullback", "manhattan",
                                                "maximum", "canberra", "minkowski")),
                                rank, min_proxy, weight)
    }
    dimnames(result) <- list(colnames(x), colnames(y))
    if (use_na) {
        na1 <- na2 <- logical()
        if (method == "cosine") {
            na1 <- qatd_cpp_nz(x) == 0
            na2 <- qatd_cpp_nz(y) == 0
        } else if (method == "correlation") {
            na1 <- qatd_cpp_sd(x) == 0
            na2 <- qatd_cpp_sd(y) == 0
        }
        if (any(na1))
            result[na1,,drop = FALSE] <- NA
        if (any(na2))
            result[,na2,drop = FALSE] <- NA
    }
    return(result)
    # return(as(result, "CsparseMatrix"))
}

# internal function to coerce to dist object
as_dist <- function(x, method, call, diag = FALSE, upper = FALSE) {
    result <- as.matrix(x)
    if (ncol(x) == nrow(x))
        result <- result[lower.tri(result)]
    attr(result, "Size") <- ncol(x)
    attr(result, "Labels") <- colnames(x)
    attr(result, "Diag") <- diag
    attr(result, "Upper") <- upper
    attr(result, "method") <- method
    attr(result, "call") <- call
    if (ncol(x) == nrow(x)) {
        class(result) <- "dist"
    } else {
        class(result) <- "dist_selection"
    }
    result
}

#' Coerce a simil object into a matrix
#' 
#' \code{as.matrix.simil} coerces an object returned from
#'   `textstat_simil()` into a matrix
#' @param diag  the value to use on the diagonal representing self-similarities
#' @note 
#'   Because for the similarity methods implemented in  \pkg{quanteda}, the
#'   similarity of an object with itself will be 1.0, \code{diag} defaults to
#'   this value. This differs the default \code{diag = NA} in
#'   \link[proxy]{as.matrix.simil} in the \pkg{proxy} package.
#' @param ... unused
#' @export
#' @method as.matrix simil
#' @keywords textstat internal
as.matrix.simil <- function(x, diag = 1.0, ...) {
    size <- attr(x, "Size")
    df <- matrix(0, size, size)
    df[row(df) > col(df)] <- x
    df <- df + t(df)
    label <- attr(x, "Labels")
     if (is.null(label)) {
        dimnames(df) <- list(seq_len(size), seq_len(size))
    } else {
        dimnames(df) <- list(label, label)
    }
    diag(df) <- diag
    df
}
