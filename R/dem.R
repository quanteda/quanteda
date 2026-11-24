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

## TODO: add dem.tokens() with MWE compounding

#' @export
dem.dfm <- function(x, y, 
                    verbose = quanteda_options("verbose"), 
                    ...) {
    
    verbose <- check_logical(verbose)
    attrs <- attributes(x)
    
    if (verbose) {
        message_create("dfm", "dem")
        proc_time <- proc.time()
    }
    
    # from https://github.com/koheiw/wordvector/blob/master/R/as.doc2vec.R
    y <- y[intersect(rownames(y), featnames(x)),,drop = FALSE]
    # if (group_data)
    #     x <- dfm_group(x)
    x <- dfm_match(x, rownames(y), verbose = FALSE)
    # if (utils::packageVersion("proxyC") >= "0.6.0") {
    #     dov <- as.matrix(proxyC::tcrossprod(x, t(y), sparse = FALSE))
    # } else {
    mat <- as.matrix(Matrix::tcrossprod(x, t(y)))
    #}
    mat <- normalize(mat)
    result <- build_dem(mat, 
                        docvars = get_docvars(x, user = TRUE, system = TRUE), 
                        meta = attrs[["meta"]], class = "dem")
    if (verbose)
        message_finish(result, proc_time)
    return(result)
}

normalize <- function(x) {
    s <- rowSums(abs(x))
    l <- s == 0
    x <- x / (s / ncol(x))
    x[l,] <- 0 # replace NA with zero
    return(x)
}

#' Converts a Matrix to a dem
#' @param x a Matrix
#' @param meta a list of values to be assigned to slots
#' @keywords internal
matrix2dem <- function(x, docvars = NULL, meta = NULL) {
    
    docname <- rownames(x)
    if (nrow(x) > length(docname))
        docname <- paste0(quanteda_options("base_docname"), seq_len(nrow(x)))
    
    # featname <- colnames(x)
    # if (ncol(x) > length(featname))
    #     featname <- paste0(quanteda_options("base_featname"), seq_len(ncol(x)))
    
    if (is.null(docvars))
        docvars <- make_docvars(nrow(x), docname, unique = TRUE)
    if (is.null(meta))
        meta <- make_meta("dem")
    
    if (nrow(x) == 0 && ncol(x) == 0) {
        x <- make_null_dem()
    } else {
        x <- Matrix(x, sparse = FALSE)
    }
    
    build_dem(
        as(as(as(x, "dMatrix"), "generalMatrix"), "unpackedMatrix"),
        docvars = docvars,
        meta = meta
    )
}

# create an empty dem for given size and documents
make_null_dem <- function(size = NULL, document = NULL) { # NOTE: what the best name for embedding length?
    if (is.null(size)) size <- 0
    if (is.null(document)) document <- character()
    temp <- as(as(as(Matrix(
        nrow = length(document), ncol = size,
    ), "dMatrix"), "generalMatrix"), "unpackedMatrix")
    
    build_dem(temp, 
              docvars = make_docvars(length(document), document))
}

# dem methods --------------------------

#' Coercion and checking functions for dem objects
#' @export
as.dem <- function(x) {
    UseMethod("as.dem")
}

#' @export
as.dfm.default <- function(x) {
    check_class(class(x), "as.dem")
}

#' @method as.dem dem
#' @export
as.dem.dem <- function(x) {
    # call upgrade_dem()
    x
}

#' @method as.dem matrix
#' @export
as.dem.matrix <- function(x) {
    matrix2dem(x)
}

#' @method as.dem Matrix
#' @export
as.dem.Matrix <- function(x) {
    matrix2dem(x)
}


#' @rdname as.dem
#' @return
#' `is.dem` returns `TRUE` if and only if its argument is a [dem].
#' @export
is.dem <- function(x) {
    is(x, "dem") && isS4(x)
}

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

