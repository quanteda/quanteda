#' Create a document-feature matrix
#'
#' Construct a sparse document-feature matrix from a [tokens] or [dfm] object.
#' @param x a [tokens] or [dfm] object.
#' @param tolower convert all features to lowercase.
#' @param remove_padding logical; if `TRUE`, remove the "pads" left as empty tokens after
#' calling [tokens()] or [tokens_remove()] with `padding = TRUE`.
#' @param verbose display messages if `TRUE`.
#' @param ... not used.
#' @section Changes in version 3:
#' In \pkg{quanteda} v4, many convenience functions formerly available in
#' `dfm()` were removed. 
#' @return a [dfm-class] object
#' @import Matrix
#' @export
#' @rdname dfm
#' @keywords dfm
#' @seealso  [dfm_select()], [dfm-class]
#' @examples
#' ## for a corpus
#' toks <- data_corpus_inaugural %>%
#'   corpus_subset(Year > 1980) %>%
#'   tokens()
#' dfm(toks)
#'
#' # removal options
#' toks <- tokens(c("a b c", "A B C D")) %>%
#'     tokens_remove("b", padding = TRUE)
#' toks
#' dfm(toks)
#' dfm(toks) %>%
#'  dfm_remove(pattern = "") # remove "pads"
#'
#' # preserving case
#' dfm(toks, tolower = FALSE)
dfm <- function(x,
                tolower = TRUE,
                remove_padding = FALSE,
                verbose = quanteda_options("verbose"),
                ...) {
    UseMethod("dfm")
}

#' @export
dfm.default <- function(x, ...) {
    check_class(class(x), "dfm")
}

#' @export
dfm.tokens <- function(x,
                       tolower = TRUE,
                       remove_padding = FALSE,
                       verbose = quanteda_options("verbose"),
                       ...) {
    
    dfm(as.tokens_xptr(x), tolower = tolower, 
        remove_padding = remove_padding, verbose = verbose, ...)
    
}


#' @importFrom stringi stri_trans_totitle
#' @export
dfm.dfm <- function(x,
                    tolower = TRUE,
                    remove_padding = FALSE,
                    verbose = quanteda_options("verbose"),
                    ...) {
    x <- as.dfm(x)
    
    if (tolower) {
        if (verbose) catm(" ...lowercasing\n", sep = "")
        x <- dfm_tolower(x)
    }
    
    # if "remove" was matched to "remove_padding"
    if (!is.logical(remove_padding))
        remove_padding <- FALSE
    
    remove_padding <- check_logical(remove_padding)
    if (remove_padding)
        x <- dfm_remove(x, "", valuetype = "fixed")

    # remove any NA named columns
    is_na <- is.na(featnames(x))
    if (any(is_na))
        x <- x[, !is_na, drop = FALSE]

    return(x)
}


# create an empty dfm for given features and documents
make_null_dfm <- function(feature = NULL, document = NULL) {
    if (is.null(feature)) feature <- character()
    if (is.null(document)) document <- character()
    temp <- as(as(as(sparseMatrix(
        i = NULL,
        j = NULL,
        dims = c(length(document), length(feature))
    ), "CsparseMatrix"), "generalMatrix"), "dMatrix")

    build_dfm(temp, feature,
              docvars = make_docvars(length(document), document))
}

# pad dfm with zero-count features
pad_dfm <- function(x, feature) {
    feat_pad <- setdiff(feature, featnames(x))
    if (length(feat_pad)) {
        suppressWarnings(
            x <- cbind(x, make_null_dfm(feat_pad, docnames(x)))
        )
    }
    x <- x[, match(feature, featnames(x))]
    return(x)
}
