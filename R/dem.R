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
dem.dfm <- function(x, y, 
                    verbose = quanteda_options("verbose"), 
                    ...) {
    
    x <- as.dem(x)
    verbose <- check_logical(verbose)
    attrs <- attributes(x)
    
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

as.dem <- function(x) {
    # call upgrade_dem()
    x
}

normalize <- function(x) {
    s <- rowSums(abs(x))
    l <- s == 0
    x <- x / (s / ncol(x))
    x[l,] <- 0 # replace NA with zero
    return(x)
}

# dem methods --------------------------

#' @noRd
#' @export
ndoc.dem <- function(x) {
    nrow(as.dem(x))
}

#' @noRd
#' @export
docnames.dem <- function(x) {
    rownames(as.dem(x))
}

#' @noRd
#' @export
"docnames<-.dem" <- function(x, unique_docnames = TRUE, value) {
    x <- as.dem(x)
    temp <- make_docvars(length(value), value, unique = unique_docnames)
    x@docvars[c("docname_", "docid_", "segid_")] <- temp
    x@Dimnames[["docs"]] <- temp[["docname_"]]
    return(x)
}

#' @noRd
#' @export
docvars.dem <- function(x, field = NULL) {
    x <- as.dem(x)
    select_docvars(x@docvars, field, user = TRUE, system = FALSE, drop = TRUE)
}

#' @export
"docvars<-.dem" <- function(x, field = NULL, value) {
    x <- as.dem(x)
    set_docvars(x@docvars, field) <- value
    return(x)
}

