# class definition and class functions --------

#' @title textstat_simil/dist classes
#' @description Sparse classes for similarity and distance matrices created by
#'   \code{\link{textstat_simil}} and \code{\link{textstat_dist}}.
#' @rdname textstat_simildist-class
#' @export
#' @keywords internal textstat
#' @slot .Data a sparse \pkg{Matrix} object, symmetric if selection is
#'   \code{NULL}
#' @slot method the method used for computing similarity or distance
#' @slot min_simil numeric; a threshold for the similarity values below which similarity
#'   values are not computed
#' @slot margin identifies the margin of the dfm on which similarity or
#'   difference was computed:  \code{"documents"} for documents or
#'   \code{"features"} for word/term features.
#' @slot type either \code{"textstat_simil"} or \code{"textstat_dist"}
#' @seealso \code{\link{textstat_simil}}
setClass("textstat_simildist", contains = "dsparseMatrix",
         slots = c(method = "character",
                   margin = "character",
                   type = "character"))

#' @rdname textstat_simildist-class
setClass("textstat_dist", contains = c("textstat_simildist", "dsTMatrix"))

#' @rdname textstat_simildist-class
#' @slot selection target units, if any
setClass("textstat_dist_sel", contains = c("textstat_simildist", "dgTMatrix"),
         slots = c(selection = "character"))

#' @rdname textstat_simildist-class
setClass("textstat_simil", contains = c("textstat_simildist", "dsTMatrix"))

#' @rdname textstat_simildist-class
setClass("textstat_simil_sel", contains = c("textstat_simildist", "dgTMatrix"),
         slots = c(selection = "character"))

#' @rdname textstat_simildist-class
setClass("textstat_simil_sparse", contains = c("textstat_simildist", "dsCMatrix"),
         slots = c(min_simil = "numeric"))

#' @rdname textstat_simildist-class
setClass("textstat_simil_sel_sparse", contains = c("textstat_simildist", "dgCMatrix"),
         slots = c(selection = "character", min_simil = "numeric"))

validate_min_simil <- function(object) {
    if (object@min_simil < 0 || object@min_simil > 1.0)
        paste("min_simil must range from 0 to 1.0")
    else 
        TRUE
}

setValidity("textstat_simil_sparse", function(object) {
    validate_min_simil(object)
})

setValidity("textstat_simil_sel_sparse", function(object) {
    validate_min_simil(object)
})

setMethod("show", "textstat_simildist",
          function(object) {
              cat(object@type, " object; method = \"", object@method, "\"\n\n", sep = "")
              print(as.matrix(object))
          })

#' @rdname textstat_simildist-class
#' @inheritParams utils::head
#' @export
head.textstat_simildist <- function(x, n = 6L, ...) {
    head(as.matrix(x, n = n, ...))
}

#' @rdname textstat_simildist-class
#' @export
tail.textstat_simildist <- function(x, n = 6L, ...) {
    tail(as.matrix(x, n = n, ...))
}


# core functions ------

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
#' @param min_simil numeric; a threshold for the similarity values below which similarity
#'   values will not be returned
#' @details \code{textstat_simil} options are: \code{"correlation"} (default),
#'   \code{"cosine"}, \code{"jaccard"}, \code{"ejaccard"}, \code{"dice"},
#'   \code{"edice"}, \code{"simple matching"}, and \code{"hamman"}.
#' @note If you want to compute similarity on a "normalized" dfm object
#'   (controlling for variable document lengths, for methods such as correlation
#'   for which different document lengths matter), then wrap the input dfm in
#'   \code{\link{dfm_weight}(x, "prop")}.
#' @return A sparse matrix from the \pkg{Matrix} package that will be symmetric 
#'   if no target is selected (using \code{selection}).
#'      
#'   These can be transformed easily into a list format using \code{as.list()},
#'   which returns a list for each unique element of the second of the pairs,
#'   \code{as.dist} to be transformed into a \link[stats]{dist} object, or
#'   \code{as.matrix} to convert it into an ordinary matrix.
#' @export
#' @seealso \code{\link[stats]{as.dist}}
#' @examples
#' # similarities for documents
#' dfmat <- dfm(corpus_subset(data_corpus_inaugural, Year > 2000), 
#'              remove_punct = TRUE, remove = stopwords("english"))
#' (tstat1 <- textstat_simil(dfmat, method = "cosine", margin = "documents"))
#' as.matrix(tstat1)
#' as.list(tstat1)
#' as.list(tstat1, diag = TRUE)
#' 
#' # min_simil
#' (tstat2 <- textstat_simil(dfmat, method = "cosine", margin = "documents", min_simil = 0.6))
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
#' as.list(tstat2, n = 6)
#' 
textstat_simil <- function(x, selection = NULL,
                           margin = c("documents", "features"),
                           method = c("correlation", "cosine", "jaccard", "ejaccard",
                                      "dice", "edice", "hamman", "simple matching"),
                           min_simil = 0, ...) {
    UseMethod("textstat_simil")
}
    

#' @export    
textstat_simil.default <- function(x, selection = NULL,
                               margin = c("documents", "features"),
                               method = c("correlation", "cosine", "jaccard", "ejaccard",
                                          "dice", "edice", "hamman", "simple matching"),
                               min_simil = 0, ...) {
    stop(friendly_class_undefined_message(class(x), "textstat_simil"))
}
    
#' @export    
textstat_simil.dfm <- function(x, selection = NULL,
                               margin = c("documents", "features"),
                               method = c("correlation", "cosine", "jaccard", "ejaccard",
                                          "dice", "edice", "hamman", "simple matching"),
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
            selection <- featnames(x)[selection]
        }
        if (any(is.na(i)))
            stop(paste(selection[is.na(i)], collapse = ", "), " does not exist")
    }
    if (margin == "features") {
        temp <- textstat_proxy(x, x[, i], margin, method, 1, 
                               min_proxy = if (min_simil == 0) NULL else min_simil, use_na = TRUE)
                               
    } else {
        temp <- textstat_proxy(x, x[i, ], margin, method, 1, 
                               min_proxy = if (min_simil == 0) NULL else min_simil, use_na = TRUE)
    }
    
    if (min_simil == 0) {
        if (is.null(selection))
            return(new("textstat_simil", temp, 
                       method = method,  margin = margin,
                       type = "textstat_simil"))
        else
            return(new("textstat_simil_sel", temp, 
                       method = method, margin = margin,
                       type = "textstat_simil",
                       selection = selection))
    } else {
        if (is.null(selection)) {
            temp <- as(temp, "dsCMatrix")
            return(new("textstat_simil_sparse", temp, 
                       method = method, margin = margin,
                       type = "textstat_simil",
                       min_simil = min_simil))
        } else {
            temp <- as(temp, "dgCMatrix")
            return(new("textstat_simil_sel_sparse", temp, 
                       method = method,  margin = margin,
                       type = "textstat_simil", 
                       min_simil = min_simil, selection = selection))
        }
    }
}

#' @rdname textstat_simil
#' @export
#' @param p The power of the Minkowski distance.
#' @details \code{textstat_dist} options are: \code{"euclidean"} (default), 
#'   \code{"manhattan"}, \code{"maximum"}, \code{"canberra"},
#'   and \code{"minkowski"}.
#' @importFrom RcppParallel RcppParallelLibs
#' @examples
#'                
#' # distances for documents 
#' (tstat3 <- textstat_dist(dfmat, margin = "documents"))
#' as.matrix(tstat3)
#' as.list(tstat3)
#' as.dist(tstat3)
#' 
#' # distances for specific documents
#' textstat_dist(dfmat, "2017-Trump", margin = "documents")
#' (tstat4 <- textstat_dist(dfmat, c("2009-Obama" , "2013-Obama"), margin = "documents"))
#' as.matrix(tstat4)
#' as.list(tstat4)
#' 
#' \dontrun{
#' # plot a dendrogram after converting the object into distances
#' plot(hclust(as.dist(tstat3)))
#' }
textstat_dist <- function(x, selection = NULL,
                          margin = c("documents", "features"),
                          method = c("euclidean",
                                     "manhattan", "maximum", "canberra", "minkowski"),
                          p = 2) {
    UseMethod("textstat_dist")
}

#' @export
textstat_dist.default <- function(x, selection = NULL,
                                  margin = c("documents", "features"),
                                  method = c("euclidean",
                                             "manhattan", "maximum", "canberra", "minkowski"),
                                  p = 2) {
    stop(friendly_class_undefined_message(class(x), "textstat_dist"))
}

#' @export
textstat_dist.dfm <- function(x, selection = NULL,
                              margin = c("documents", "features"),
                              method = c("euclidean",
                                         "manhattan", "maximum", "canberra", "minkowski"),
                              p = 2) {
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
            selection <- featnames(x)[selection]
        }
        if (any(is.na(i)))
            stop(paste(selection[is.na(i)], collapse = ", "), " does not exist")
    }
    if (margin == "features") {
        temp <- textstat_proxy(x, x[, i], margin, method, p, use_na = TRUE)
    } else {
        temp <- textstat_proxy(x, x[i, ], margin, method, p, use_na = TRUE)
    }
    
    if (is.null(selection))
        return(new("textstat_dist", temp, 
                   method = method,  margin = margin,
                   type = "textstat_dist"))
    else
        return(new("textstat_dist_sel", temp, 
                   method = method,  margin = margin,
                   type = "textstat_dist", selection = selection))
}

# coercion methods ----------

#' @rdname textstat_simil
#' @method as.list textstat_simildist
#' @param sorted sort results in descending order if \code{TRUE}
#' @param n the top \code{n} highest-ranking items will be returned.  If n is 
#'   \code{NULL}, return all items.
#' @param diag logical; if \code{FALSE}, exclude the item's comparison with itself
#' @keywords textstat
#' @export
as.list.textstat_simildist <- function(x, sorted = TRUE, n = NULL, diag = FALSE, ...) {
    if (!is.null(n) && n < 1) 
        stop("n must be 1 or greater")
    if (!is.null(n) && !sorted) {
        warning("ignoring n when sorted = FALSE")
        n <- NULL
    }
    
    # NA the diagonal, if diag = FALSE
    if (!diag) x <- diag_to_NA(x)

    x <- as(x, "dgTMatrix")
    result <- split(structure(x@x, names = rownames(x)[x@i+1]), 
                    f = factor(colnames(x)[x@j+1], levels = colnames(x)))

    if (sorted)
        result <- lapply(result, sort, decreasing = TRUE, na.last = TRUE)
    if (!is.null(n))
        result <- lapply(result, "[", seq_len(n))
    # remove any missing
    result <- lapply(result, function(y) y[!is.na(y)])

    result
}

#' @rdname textstat_simil
#' @method as.data.frame textstat_simildist
#' @return \code{as.data.frame} for a \code{textstat_simil} or
#'   \code{textstat_dist} object returns a data.frame of pairwise combinations
#'   and the and their similarity or distance value.
#' @export
as.data.frame.textstat_simildist <- function(x, diag = FALSE, upper = FALSE) {
    method <- x@method
    margin <- x@margin
    
    if (!diag)
        x <- diag_to_NA(x)
    if (upper) {
        if (is(x, "symmetricMatrix")) {
            x <- as(x, "dgTMatrix")
        } else {
            warning("upper = TRUE has no effect when columns have been selected")
        }
    }

    result <- data.frame(x = factor(x@i + 1L, levels = seq_along(rownames(x)), labels = rownames(x)),
                         y = factor(match(colnames(x)[x@j + 1L], rownames(x)), 
                                    levels = seq_along(rownames(x)), labels = rownames(x)),
                         stat = x@x,
                         stringsAsFactors = FALSE)
    result <- subset(result, !is.na(stat))
    
    # eliminate same pairs when selection was used
    if (!diag && !setequal(rownames(x), colnames(x))) 
        result <- result[result[, 1] != result[, 2], ]

    # replace x and y with margin names
    names(result)[1:2] <- paste0(stri_sub(margin, 1, -2), 1:2)
    # replace stat with measure name
    names(result)[3] <- method
    
    result
} 

#' convert same-value pairs to NA in a textstat_simildist object
#' 
#' Converts the diagonal, or the same-pair equivalent in an object 
#' where the columns have been selected, to NA.
#' @param x the return from \code{\link{texstat_simil}} or \code{\link{texstat_dist}}
#' @return sparse Matrix format with same-pair values replaced with \code{NA}
#' @keywords textstat internal
diag_to_NA <- function(x) {
    # NA the diagonal, if diag = FALSE
    if (is(x, "symmetricMatrix")) {
        diag(x) <- NA
    } else {
        # NA same-item pairs
        toNA <- list(x = which(rownames(x) %in% colnames(x)), 
                     y = seq_along(colnames(x)))
        for (i in seq_len(lengths(toNA)[1]))
            x[toNA$x[i], toNA$y[i]] <- NA
    }
    x
}

# textstat_proxy ---------

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
