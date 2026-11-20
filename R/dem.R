#' Create a document-feature matrix
#'
#' Construct a sparse document-feature matrix from a [tokens] or [dfm] object.
#' @param x a [tokens] or [dfm] object.
#' @param y a feature embedding object.
#' @param verbose display messages if `TRUE`.
#' @param ... not used.
#' @return a [dem-class] object
#' @import Matrix
#' @export
#' @rdname dem
#' @examples
dem <- function(x, y,
                verbose = quanteda_options("verbose"),
                ...) {
    UseMethod("dem")
}

#' @export
dem.default <- function(x, ...) {
    check_class(class(x), "dem")
}

#' @export
#' @export
dem.dfm <- function(x, y, 
                    verbose = quanteda_options("verbose"), 
                    ...) {
    
    verbose <- check_logical(verbose)
    
    # from https://github.com/koheiw/wordvector/blob/master/R/as.doc2vec.R
    y <- y[intersect(rownames(y), featnames(x)),,drop = FALSE]
    # if (group_data)
    #     x <- dfm_group(x)
    x <- dfm_match(x, rownames(y))
    # if (utils::packageVersion("proxyC") >= "0.6.0") {
    #     dov <- as.matrix(proxyC::tcrossprod(x, t(y), sparse = FALSE))
    # } else {
    mat <- as.matrix(Matrix::tcrossprod(x, t(y)))
    #}
    mat <- normalize(mat)
    
    build_dem(mat, 
              docvars = get_docvars(x, user = TRUE, system = TRUE), 
              meta = attrs[["meta"]], class = "dem")
}

normalize <- function(x) {
    s <- rowSums(abs(x))
    l <- s == 0
    x <- x / (s / ncol(x))
    x[l,] <- 0 # replace NA with zero
    return(x)
}
