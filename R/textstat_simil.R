# class definition and class functions --------

#' @title textstat_simil/dist classes
#' @description Sparse classes for similarity and distance matrices created by
#'   [textstat_simil()] and [textstat_dist()].
#' @rdname textstat_proxy-class
#' @export
#' @keywords internal textstat
#' @slot .Data a sparse \pkg{Matrix} object, symmetric if selection is
#'   `NULL`
#' @slot method the method used for computing similarity or distance
#' @slot min_simil numeric; a threshold for the similarity values below which similarity
#'   values are not computed
#' @slot margin identifies the margin of the dfm on which similarity or
#'   difference was computed:  `"documents"` for documents or
#'   `"features"` for word/term features.
#' @slot type either `"textstat_simil"` or `"textstat_dist"`
#' @seealso [textstat_simil()]
setClass("textstat_proxy", contains = "Matrix",
         slots = c(method = "character",
                   margin = "character",
                   type = "character"))

#' @rdname textstat_proxy-class
#' @slot selection target units, if any
setClass("textstat_dist", contains = c("textstat_proxy", "dgeMatrix"))

#' @rdname textstat_proxy-class
setClass("textstat_dist_symm", contains = c("textstat_proxy", "dspMatrix"))

#' @rdname textstat_proxy-class
setClass("textstat_simil", contains = c("textstat_proxy", "dgeMatrix"))

#' @rdname textstat_proxy-class
setClass("textstat_simil_symm", contains = c("textstat_proxy", "dspMatrix"))

#' @rdname textstat_proxy-class
setClass("textstat_simil_sparse", contains = c("textstat_proxy", "dgTMatrix"),
         slots = c(min_simil = "numeric"))

#' @rdname textstat_proxy-class
setClass("textstat_simil_symm_sparse", contains = c("textstat_proxy", "dsTMatrix"),
         slots = c(min_simil = "numeric"))

validate_min_simil <- function(object) {
    if (object@min_simil < -1.0 || object@min_simil > 1.0) {
        paste("min_simil must range from -1.0 to 1.0")
    } else {
        return(TRUE)
    }
}

setValidity("textstat_simil_sparse", function(object) {
    validate_min_simil(object)
})

setValidity("textstat_simil_symm_sparse", function(object) {
    validate_min_simil(object)
})

#' Print a textstat_proxy object
#'
#' Print/show method for objects created by `textstat_simil` and
#' `textstat_dist`.
#' @param object the textstat_proxy object to be printed
#' @rdname textstat_proxy-class
#' @export
setMethod("show", "textstat_proxy",
          function(object) {
              cat(object@type, " object; method = \"", object@method, "\"\n", sep = "")
              Matrix::printSpMatrix(as(object, "sparseMatrix"),
                                    zero.print = if ("min_simil" %in% slotNames(object)) "." else 0,
                                    digits = min(getOption("digits"), 3),
                                    col.names = TRUE, align = "right")
          })

setMethod("head", signature(x = "textstat_proxy"), function(x, n = 6L, ...) {
    selectMethod(x, "Matrix")
})

setMethod("tail", signature(x = "textstat_proxy"), function(x, n = 6L, ...) {
    selectMethod(x, "Matrix")
})

#' Return the first or last part of a textstat_proxy object
#'
#' For a similarity or distance object computed via [textstat_simil] or
#' [textstat_dist], returns the first or last `n` rows.
#' @param x a textstat_simil/textstat_dist object
#' @param n a single, positive integer.  If positive, size for the resulting
#'   object: number of first/last documents for the dfm. If negative, all but
#'   the n last/first number of documents of x.
#' @param ... unused
#' @return A [matrix] corresponding to the subset defined
#'   by `n`.
#' @export
#' @name head.textstat_proxy
#' @method head textstat_proxy
#' @keywords textstat internal
head.textstat_proxy <- function(x, n = 6L, ...) {
    head(as.matrix(x), n)
}

#' @rdname head.textstat_proxy
#' @method tail textstat_proxy
#' @export
tail.textstat_proxy <- function(x, n = 6L, ...) {
    tail(as.matrix(x), n)
}

setMethod("head", signature(x = "textstat_proxy"), function(x, n = 6L, ...) {
    UseMethod("head")
})
setMethod("tail", signature(x = "textstat_proxy"), function(x, n = 6L, ...) {
    UseMethod("tail")
})

# core functions ------

#' Similarity and distance computation between documents or features
#'
#' These functions compute matrixes of distances and similarities between
#' documents or features from a [dfm()] and return a matrix of
#' similarities or distances in a sparse format.  These methods are fast
#' and robust because they operate directly on the sparse [dfm] objects.
#' The output can easily be coerced to an ordinary matrix, a data.frame of
#' pairwise comparisons, or a [dist][stats::dist] format.
#' @param x,y a [dfm] objects; `y` is an optional target matrix matching
#'   `x` in the margin on which the similarity or distance will be computed.
#' @param selection (deprecated - use `y` instead).
#' @param margin identifies the margin of the dfm on which similarity or
#'   difference will be computed:  `"documents"` for documents or
#'   `"features"` for word/term features.
#' @param method character; the method identifying the similarity or distance
#'   measure to be used; see Details.
#' @param min_simil numeric; a threshold for the similarity values below which similarity
#'   values will not be returned
#' @param ... unused
#' @details `textstat_simil` options are: `"correlation"` (default),
#'   `"cosine"`, `"jaccard"`, `"ejaccard"`, `"dice"`,
#'   `"edice"`, `"simple matching"`, and `"hamman"`.
#' @note If you want to compute similarity on a "normalized" dfm object
#'   (controlling for variable document lengths, for methods such as correlation
#'   for which different document lengths matter), then wrap the input dfm in
#'   `[dfm_weight](x, "prop")`.
#' @return A sparse matrix from the \pkg{Matrix} package that will be symmetric
#'   unless `y` is specified.
#'
#'   These can be transformed easily into a list format using `as.list()`, which
#'   returns a list for each unique element of the second of the pairs,
#'   `as.dist()` to be transformed into a \link[stats:dist]{dist} object, or
#'   `as.matrix()` to convert it into an ordinary matrix.
#' @export
#' @seealso \code{\link[stats:dist]{stats::as.dist()}}
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
#' textstat_simil(dfmat, dfmat["2017-Trump", ], margin = "documents")
#' textstat_simil(dfmat, dfmat["2017-Trump", ], method = "cosine", margin = "documents")
#' textstat_simil(dfmat, dfmat[c("2009-Obama", "2013-Obama"), ], margin = "documents")
#'
#' # compute some term similarities
#' tstat3 <- textstat_simil(dfmat, dfmat[, c("fair", "health", "terror")], method = "cosine",
#'                          margin = "features")
#' head(as.matrix(tstat3), 10)
#' as.list(tstat3, n = 6)
#'
textstat_simil <- function(x, y = NULL, selection = NULL,
                           margin = c("documents", "features"),
                           method = c("correlation", "cosine", "jaccard", "ejaccard",
                                      "dice", "edice", "hamman", "simple matching"),
                           min_simil = NULL, ...) {
    UseMethod("textstat_simil")
}

#' @export
textstat_simil.default <- function(x, y = NULL, selection = NULL,
                               margin = c("documents", "features"),
                               method = c("correlation", "cosine", "jaccard", "ejaccard",
                                          "dice", "edice", "hamman", "simple matching"),
                               min_simil = NULL, ...) {
    stop(friendly_class_undefined_message(class(x), "textstat_simil"))
}

#' @export
textstat_simil.dfm <- function(x, y = NULL, selection = NULL,
                               margin = c("documents", "features"),
                               method = c("correlation", "cosine", "jaccard", "ejaccard",
                                          "dice", "edice", "hamman", "simple matching"),
                               min_simil = NULL, ...) {

    if (!is.null(selection))
        .Deprecated(msg = "'selection' is deprecated. Use 'y' instead.")
    unused_dots(...)

    x <- as.dfm(x)
    margin <- match.arg(margin)
    method <- match.arg(method)

    if (margin == "features") {
        name <- colnames(x)
    } else {
        name <- rownames(x)
    }
    if (is.null(y)) {
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
            if (margin == "features") {
                y <- x[, i]
            } else {
                y <- x[i, ]
            }
        }
    } else {
        if (!is.dfm(y)) stop("y must be a dfm matching x in the margin specified")
        y <- as.dfm(y)
    }
    temp <- textstat_proxy(x, y, margin, method,
                           min_proxy = min_simil, use_na = TRUE)

    if (is.null(min_simil)) {
        if (is(temp, "dsTMatrix")) {
            temp <- as(temp, "dsyMatrix")
            return(new("textstat_simil_symm", as(temp, "dspMatrix"),
                       method = method, margin = margin,
                       type = "textstat_simil"))
        } else {
            return(new("textstat_simil", as(temp, "dgeMatrix"),
                       method = method, margin = margin,
                       type = "textstat_simil"))
        }
    } else {
        if (is(temp, "dsTMatrix")) {
            return(new("textstat_simil_symm_sparse", temp,
                       method = method, margin = margin,
                       type = "textstat_simil",
                       min_simil = min_simil))
        } else {
            return(new("textstat_simil_sparse", temp,
                       method = method, margin = margin,
                       type = "textstat_simil",
                       min_simil = min_simil))
        }
    }
}

#' @rdname textstat_simil
#' @export
#' @param p The power of the Minkowski distance.
#' @details `textstat_dist` options are: `"euclidean"` (default),
#'   `"manhattan"`, `"maximum"`, `"canberra"`,
#'   and `"minkowski"`.
#' @importFrom RcppParallel RcppParallelLibs
#' @examples
#'
#' # distances for documents
#' (tstat4 <- textstat_dist(dfmat, margin = "documents"))
#' as.matrix(tstat4)
#' as.list(tstat4)
#' as.dist(tstat4)
#'
#' # distances for specific documents
#' textstat_dist(dfmat, dfmat["2017-Trump", ], margin = "documents")
#' (tstat5 <- textstat_dist(dfmat, dfmat[c("2009-Obama" , "2013-Obama"), ], margin = "documents"))
#' as.matrix(tstat5)
#' as.list(tstat5)
#'
#' \dontrun{
#' # plot a dendrogram after converting the object into distances
#' plot(hclust(as.dist(tstat4)))
#' }
textstat_dist <- function(x, y = NULL, selection = NULL,
                          margin = c("documents", "features"),
                          method = c("euclidean",
                                     "manhattan", "maximum", "canberra", "minkowski"),
                          p = 2, ...) {
    UseMethod("textstat_dist")
}

#' @export
textstat_dist.default <- function(x, y = NULL, selection = NULL,
                                  margin = c("documents", "features"),
                                  method = c("euclidean",
                                             "manhattan", "maximum", "canberra", "minkowski"),
                                  p = 2, ...) {
    stop(friendly_class_undefined_message(class(x), "textstat_dist"))
}

#' @export
textstat_dist.dfm <- function(x, y = NULL, selection = NULL,
                              margin = c("documents", "features"),
                              method = c("euclidean",
                                         "manhattan", "maximum", "canberra", "minkowski"),
                              p = 2, ...) {
    if (!is.null(selection))
        .Deprecated(msg = "'selection' is deprecated. Use 'y' instead.")

    unused_dots(...)
    x <- as.dfm(x)

    margin <- match.arg(margin)
    method <- match.arg(method)

    if (margin == "features") {
        name <- colnames(x)
    } else {
        name <- rownames(x)
    }
    if (is.null(y)) {
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
            if (margin == "features") {
                y <- x[, i]
            } else {
                y <- x[i, ]
            }
        }
    } else {
        if (!is.dfm(y)) stop("y must be a dfm matching x in the margin specified")
        y <- as.dfm(y)
    }
    temp <- textstat_proxy(x, y, margin, method,
                           p = p, use_na = TRUE)

    if (is(temp, "dsTMatrix")) {
        temp <- as(temp, "dsyMatrix")
        return(new("textstat_dist_symm", as(temp, "dspMatrix"),
                   method = method, margin = margin,
                   type = "textstat_dist"))
    } else {
        return(new("textstat_dist", as(temp, "dgeMatrix"),
                   method = method, margin = margin,
                   type = "textstat_dist"))
    }
}

# coercion methods ----------

#' @rdname textstat_simil
#' @method as.list textstat_proxy
#' @param sorted sort results in descending order if `TRUE`
#' @param n the top `n` highest-ranking items will be returned.  If n is
#'   `NULL`, return all items.
#' @param diag logical; if `FALSE`, exclude the item's comparison with itself
#' @return `as.data.list` for a `textstat_simil` or
#'   `textstat_dist` object returns a list equal in length to the columns of the
#'   simil or dist object, with the rows and their values as named  elements.  By default,
#'   this list excludes same-time pairs (when `diag = FALSE`) and sorts the values
#'   in descending order (when `sorted = TRUE`).
#' @keywords textstat
#' @export
as.list.textstat_proxy <- function(x, sorted = TRUE, n = NULL, diag = FALSE, ...) {
    if (!is.null(n) && n < 1)
        stop("n must be 1 or greater")
    if (!is.null(n) && !sorted) {
        warning("ignoring n when sorted = FALSE")
        n <- NULL
    }

    x <- proxy2triplet(x, upper = TRUE)
    if (!diag)
        x <- diag2na(x)
    result <- split(structure(x@x, names = rownames(x)[x@i + 1L]),
                    f = factor(colnames(x)[x@j + 1], levels = colnames(x)))

    if (sorted)
        result <- lapply(result, sort, decreasing = TRUE, na.last = TRUE)
    if (!is.null(n))
        result <- lapply(result, head, n)
    # remove any missing
    result <- lapply(result, function(y) y[!is.na(y)])
    # remove any empty
    result <- result[lengths(result) > 0]
    return(result)
}

#' @rdname textstat_simil
#' @method as.data.frame textstat_proxy
#' @inheritParams base::as.data.frame
#' @param upper logical; if `TRUE`, return pairs as both (A, B) and (B, A)
#' @return `as.data.frame` for a `textstat_simil` or
#'   `textstat_dist` object returns a data.frame of pairwise combinations
#'   and the and their similarity or distance value.
#' @export
as.data.frame.textstat_proxy <- function(x, row.names = NULL, optional = FALSE,
                                            diag = FALSE, upper = FALSE,  ...) {
    method <- x@method
    margin <- x@margin

    if (!isSymmetric(x) && upper)
        warning("upper = TRUE has no effect when columns have been selected")
    x <- proxy2triplet(x, upper)
    if (!diag)
        x <- diag2na(x)
    
    all <- unique(c(colnames(x), rownames(x)))
    result <- data.frame(x = factor(rownames(x)[x@i + 1L], levels = all),
                         y = factor(colnames(x)[x@j + 1L], levels = all),
                         stat = x@x,
                         stringsAsFactors = FALSE)
    result <- subset(result, !is.na(stat))

    # replace x and y with margin names
    names(result)[1:2] <- paste0(stri_sub(margin, 1, -2), 1:2)
    # replace stat with measure name
    names(result)[3] <- method
    # drop row names
    row.names(result) <- NULL
    return(result)
}

#' convert same-value pairs to NA in a textstat_proxy object
#'
#' Converts the diagonal, or the same-pair equivalent in an object
#' where the columns have been selected, to NA.
#' @param x the return from [textstat_simil()] or [textstat_dist()]
#' @return sparse Matrix format with same-pair values replaced with `NA`
#' @keywords textstat internal
diag2na <- function(x) {
    if (is(x, "dsTMatrix")) {
        Matrix::diag(x) <- NA
    } else if (is(x, "dgTMatrix")) {
        name <- intersect(colnames(x), rownames(x))
        i <- match(name, rownames(x))
        j <- match(name, colnames(x))
        x <- x + Matrix::sparseMatrix(
            i = i, j = j, x = NA,
            dims = dim(x), dimnames = dimnames(x)
        )
        x <- as(x, "dgTMatrix")
    } else {
        stop("x must be a triplet matrix")
    }
    return(x)
}

proxy2triplet <- function(x, upper) {
    if (class(x) %in% c("textstat_dist", "textstat_simil")) {
        x <- as(x, "dgTMatrix")
    } else {
        if (class(x) %in% c("textstat_dist_symm", "textstat_simil_symm"))
            x <- as(as(x, "dsyMatrix"), "dsTMatrix")
        if (upper)
            x <- as(x, "dgTMatrix")
    }
    return(x)
}

#' as.matrix method for textstat_simil_sparse
#'
#' @param x an object returned by [textstat_simil] when `min_simil >
#'   0`
#' @param omitted value that will replace the omitted cells
#' @param ... unused
#' @return a [matrix] object
#' @export
#' @keywords textstat internal
#' @rdname as.matrix.textstat_simil_sparse
setMethod("as.matrix", "textstat_simil_sparse",
          function(x, omitted = NA, ...) {
              x[x == 0] <- omitted
              as.matrix(as(x, "dgeMatrix"))
          })

#' @export
#' @keywords textstat internal
#' @rdname as.matrix.textstat_simil_sparse
setMethod("as.matrix", "textstat_simil_symm_sparse",
          function(x, omitted = NA, ...) {
              x[x == 0] <- omitted
              as.matrix(as(x, "dgeMatrix"))
          })

# textstat_proxy ---------

#' \[Experimental\] Compute document/feature proximity
#'
#' This is an underlying function for `textstat_dist` and
#' `textstat_simil` but returns `TsparseMatrix`.
#' @keywords internal
#' @param y if a [dfm] object is provided, proximity between documents or
#'   features in `x` and `y` is computed.
#' @param use_na if `TRUE`, return `NA` for proximity to empty
#'   vectors. Note that use of `NA` makes the proximity matrices denser.
#' @inheritParams textstat_dist
#' @param min_proxy the minimum proximity value to be recoded.
#' @param rank an integer value specifying top-n most proximity values to be
#'   recorded.
#' @export
#' @seealso [textstat_dist()], [textstat_simil()]
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
    if (method %in% c("cosine", "correlation", "jaccard", "ejaccard", "dice", "edice",
                      "hamman", "simple matching", "faith")) {
        if (identical(x, y)) {
            result <- proxyC::simil(x, NULL, 2, method, min_simil = min_proxy, rank = rank)
        } else {
            result <- proxyC::simil(x, y, 2, method, min_simil = min_proxy, rank = rank)
        }
    } else {
        if (identical(x, y)) {
            result <- proxyC::dist(x, NULL, 2, method, p = p)
        } else {
            result <- proxyC::dist(x, y, 2, method, p = p)
        }
    }
    dimnames(result) <- list(colnames(x), colnames(y))
    if (use_na) {
        if (method == "correlation") {
            na1 <- proxyC::colSds(x) == 0
            na2 <- proxyC::colSds(y) == 0
        } else {
            na1 <- proxyC::colZeros(x) == nrow(x)
            na2 <- proxyC::colZeros(y) == nrow(y)
        }
        if (any(na1) || any(na2))
            result <- result + make_na_matrix(dim(result), which(na1), which(na2))
    }
    return(result)
}

make_na_matrix <- function(dims, row = NULL, col = NULL) {
    i <- j <- integer()
    if (is.integer(row)) {
        i <- c(i, rep(row, dims[2]))
        j <- c(j, rep(seq_len(dims[2]), each = length(row)))
    }
    if (is.integer(col)) {
        i <- c(i, rep(seq_len(dims[1]), each = length(col)))
        j <- c(j, rep(col, dims[1]))
    }
    Matrix::sparseMatrix(
        i = i, j = j, x = as.double(NA),
        dims = dims, giveCsparse = FALSE
    )
}
