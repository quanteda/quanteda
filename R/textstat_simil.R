#' Similarity and distance computation between documents or features
#'
#' These functions compute matrixes of distances and similarities between
#' documents or features from a \code{\link{dfm}} and return a
#' \code{\link[stats]{dist}} object (or a matrix if specific targets are
#' selected).  They are fast and robust because they operate directly on the
#' sparse \link{dfm} objects.
#' @param x a \link{dfm} object
#' @param selection a valid index for document or feature names (depending on
#'   \code{margin}) from \code{x}, to be selected for comparison.  The selected
#'   document(s) or feature(s) will form the second of the pairs returned.  
#'   (See Value.)
#' @param margin identifies the margin of the dfm on which similarity or
#'   difference will be computed:  \code{"documents"} for documents or
#'   \code{"features"} for word/term features.
#' @param method character; the method identifying the similarity or distance
#'   measure to be used; see Details.
#' @param diag logical; whether to record the similarity of an item with itself.
#'   (similar to the "diagonal" were the output in matrix form).
#' @param upper logical; if \code{TRUE}, record both (A, B) and (B, A) pairs.  
#'   Similar to returning just the lower trinagle from a \link{dist} object, or the
#'   upper triangle when transforming the output into a matrix.
#' @param min_simil numeric; a threshold for the similarity values below which similarity
#'   values will not be returned
#' @details \code{textstat_simil} options are: \code{"correlation"} (default),
#'   \code{"cosine"}, \code{"jaccard"}, \code{"ejaccard"}, \code{"dice"},
#'   \code{"edice"}, \code{"simple matching"}, and \code{"hamman"}.
#' @note If you want to compute similarity on a "normalized" dfm object
#'   (controlling for variable document lengths, for methods such as correlation
#'   for which different document lengths matter), then wrap the input dfm in
#'   \code{\link{dfm_weight}(x, "prop")}.
#' @return A \link{data.frame} containing the pairs of documents or features
#'   whose similarity or distance will be compared.  
#'      
#'   These can be transformed easily into a list format using
#'   \code{as.list()}, which returns a list for each unique element of the
#'   second of the pairs.
#'   
#'   You can also transform the output in matrix object using
#'   \code{as.matrix()}, which could (for a \code{textstat_dist} object) be
#'   further transformed into a \link{dist} object.)
#' @export
#' @seealso \code{\link[stats]{as.dist}}
#' @examples
#' # similarities for documents
#' dfmat <- dfm(corpus_subset(data_corpus_inaugural, Year > 2000), 
#'              remove_punct = TRUE, remove = stopwords("english"))
#' (tstat1 <- textstat_simil(dfmat, method = "cosine", margin = "documents"))
#' as.matrix(tstat1)
#' as.list(tstat1)
#' 
#' textstat_simil(dfmat, method = "cosine", margin = "documents", diag = FALSE)
#' textstat_simil(dfmat, method = "cosine", margin = "documents", upper = FALSE)
#' textstat_simil(dfmat, method = "cosine", margin = "documents", diag = FALSE, upper = FALSE)
#'  
#' # min_simil
#' (tstat2 <- textstat_simil(dfmat, method = "cosine", margin = "documents", min_simil = 0.5))
#' as.matrix(tstat2)
#' 
#' # similarities for for specific documents
#' textstat_simil(dfmat, selection = "2017-Trump", margin = "documents")
#' textstat_simil(dfmat, selection = "2017-Trump", method = "cosine", margin = "documents")
#' textstat_simil(dfmat, selection = c("2009-Obama" , "2013-Obama"), margin = "documents")
#'
#' # compute some term similarities
#' tstat2 <- textstat_simil(dfmat, selection = c("fair", "health", "terror"), method = "cosine",
#'                          margin = "features")
#' head(as.matrix(tstat2), 10)
#' as.list(tstat2, n = 3)
#' 
textstat_simil <- function(x, selection = NULL,
                           margin = c("documents", "features"),
                           method = c("correlation", "cosine", "jaccard", "ejaccard",
                                      "dice", "edice", "hamman", "simple matching"),
                           upper = TRUE, 
                           diag = TRUE,
                           min_simil = 0) {
    UseMethod("textstat_simil")
}
    

#' @export    
textstat_simil.default <- function(x, selection = NULL,
                               margin = c("documents", "features"),
                               method = c("correlation", "cosine", "jaccard", "ejaccard",
                                          "dice", "edice", "hamman", "simple matching"),
                               upper = TRUE, 
                               diag = TRUE,
                               min_simil = 0) {
    stop(friendly_class_undefined_message(class(x), "textstat_simil"))
}
    
#' @export    
textstat_simil.dfm <- function(x, selection = NULL,
                               margin = c("documents", "features"),
                               method = c("correlation", "cosine", "jaccard", "ejaccard",
                                          "dice", "edice", "hamman", "simple matching"),
                               upper = TRUE, 
                               diag = TRUE,
                               min_simil = 0) {
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
        temp <- textstat_proxy(x, x[,i], margin, method, 1, 
                               min_proxy = if (min_simil == 0) NULL else min_simil, use_na = TRUE)
                               
    } else {
        temp <- textstat_proxy(x, x[i,], margin, method, 1, 
                               min_proxy = if (min_simil == 0) NULL else min_simil, use_na = TRUE)
    }
    
    if (upper) temp <- as(temp, "dgTMatrix")
    
    result <- data.frame(x = factor(temp@i + 1L, seq_len(nrow(temp)), rownames(temp)),
                         y = factor(temp@j + 1L, seq_len(ncol(temp)), colnames(temp)),
                         similarity = temp@x)
    
    # eliminate identical pairs when diag = FALSE
    if (!diag) {
        result <- result[result[1] != result[2], ]
    }
    
    # replace x and y with margin names
    names(result)[1:2] <- paste0(stri_sub(margin, 1, -2), 1:2)
    
    class(result) <- c("textstat_simil", "data.frame")
    attr(result, "selection") <- selection
    attr(result, "method") <- method
    attr(result, "min_simil") <- min_simil
    attr(result, "diag") <- diag
    attr(result, "upper") <- upper
    return(result)
}

#' @rdname textstat_simil
#' @export
#' @param upper whether the upper triangle of the symmetric \eqn{V \times V}
#'   matrix is recorded. Only used when \code{value = "dist"}.
#' @param diag whether the diagonal of the distance matrix should be recorded. .
#'   Only used when \code{value = "dist"}.
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
                                      "dice", "edice", "hamman", "simple matching",
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
        if (method == "correlation") {
            na1 <- qatd_cpp_sd(x) == 0
            na2 <- qatd_cpp_sd(y) == 0
        } else {
            na1 <- qatd_cpp_nz(x) == 0
            na2 <- qatd_cpp_nz(y) == 0
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
as.matrix.simil <- function(x, diag = 1.0, upper = TRUE, ...) {
    size <- attr(x, "Size")
    df <- matrix(0, size, size)
    df[row(df) > col(df)] <- x
    if (upper)
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

#' Coerce a textstat_simil or textstat_dist to a sparse Matrix format
#' 
#' Coerce a \link{textstat_simil} or \link{textstat_dist} to a sparse \pkg{Matrix} format.
#' @param x input textstat object
#' @keywords textstat internal
as.Matrix <- function(x) {
    UseMethod("as.Matrix")
}

#' @method as.Matrix textstat_simil
#' @keywords textstat internal
as.Matrix.textstat_simil <- function(x) {
    names(x)[1:2] <- c("x", "y")
    # if (!is.null(attr(x, "selection"))) x <- x[, c(2, 1, 3:ncol(x))]
    sparseMatrix(i = as.integer(x$x), j = as.integer(x$y),
                 x = x$similarity,
                 dims = c(nlevels(x$x), nlevels(x$y)),
                 dimnames = list(levels(x$x), levels(x$y)))
}

#' @export
#' @rdname textstat_simil
#' @method as.matrix textstat_simil
#' @keywords textstat internal
as.matrix.textstat_simil <- function(x, ...) {
    # pad the missing diagonal if did not exist
    if (!attr(x, "diag") & is.null(attr(x, "selection"))) {
        selected <- as.character(unique(unlist(x[c(1,2)])))
        samerows <- data.frame(selected, selected, 
                               similarity = rep(1.0, length(selected)), 
                               stringsAsFactors = FALSE)
        names(samerows) <- names(x)
        x <- rbind(x, samerows)
    }
    result <- as.Matrix(x)
    # return NA for missings, not 0
    if (attr(x, "min_simil") > 0) result[result == 0] <- NA
    # NA the lower triangle if upper = FALSE
    if (!attr(x, "upper")) {
        result <- t(result)
        result[upper.tri(result)] <- NA
    }
    as.matrix(result)
}

#' @rdname textstat_simil
#' @method as.list textstat_simil
#' @param sorted sort results in descending order if \code{TRUE}
#' @param n the top \code{n} highest-ranking items will be returned.  If n is 
#'   \code{NULL}, return all items.
#' @keywords textstat internal
#' @export
as.list.textstat_simil <- function(x, sorted = TRUE, n = NULL, ...) {
    if (!is.null(n) && n < 1) 
        stop("n must be 1 or greater")
    if (!is.null(n) && !sorted) {
        warning("ignoring n when sorted = FALSE")
        n <- NULL
    }
    
    # make symmetric if upper = FALSE
    if (!attr(x, "upper")) {
        reflected <- x[, c(2, 1, 3:ncol(x))]
        if (attr(x, "diag")) {
            reflected <- reflected[reflected[1] != reflected[2], ]
            attr(x, "diag") <- FALSE
        }
        names(reflected) <- names(x)
        x <- rbind(x, reflected)
    }

    if (!attr(x, "diag")) {
        selected <- unique(x[2])
        samerows <- data.frame(selected, selected, 
                               similarity = rep(1.0, length(selected)), 
                               stringsAsFactors = FALSE)
        names(samerows) <- names(x)
        rbind(x, samerows)
    }
    
    names(x)[1:2] <- c("x", "y")
    result <- split(structure(x$similarity, names = as.character(x$x)), x$y)
    if (sorted)
        result <- lapply(result, sort, decreasing = TRUE, na.last = TRUE)
    if (!is.null(n))
        result <- lapply(result, "[", seq_len(n))
    # remove any missing because n was greater than the length of the list item
    result <- lapply(result, function(y) y[!is.na(y)])
    result
}
