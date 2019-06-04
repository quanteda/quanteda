# featnames -----------

#' Get the feature labels from a dfm
#' 
#' Get the features from a document-feature matrix, which are stored as the
#' column names of the \link{dfm} object.
#' @param x the dfm whose features will be extracted
#' @return character vector of the feature labels
#' @examples
#' dfmat <- dfm(data_corpus_inaugural)
#' 
#' # first 50 features (in original text order)
#' head(featnames(dfmat), 50)
#' 
#' # first 50 features alphabetically
#' head(sort(featnames(dfmat)), 50)
#' 
#' # contrast with descending total frequency order from topfeatures()
#' names(topfeatures(dfmat, 50))
#' @export
featnames <- function(x) {
    UseMethod("featnames")
}

#' @export
#' @noRd
featnames.NULL <- function(x) {
    NULL
}

#' @export
#' @noRd
featnames.dfm <- function(x) {
    x <- as.dfm(x)
    if (is.null(colnames(x))) {
        character()
    } else {
        colnames(x)
    }
}

# docnames -----------

#' @noRd
#' @export
docnames.dfm <- function(x) {
    x <- as.dfm(x)
    if (is.null(rownames(x))) {
        paste0('text', seq_len(ndoc(x)))
    } else {
        rownames(x)
    }
}

#' @noRd
#' @export
docnames.NULL <- function(x) {
    NULL
}

# as.dfm -----------

#' Coercion and checking functions for dfm objects
#' 
#' Convert an eligible input object into a dfm, or check whether an object is a
#' dfm.  Current eligible inputs for coercion to a dfm are: \link{matrix},
#' (sparse) \link[Matrix]{Matrix}, \link[tm]{TermDocumentMatrix},
#' \link[tm]{DocumentTermMatrix}, \link{data.frame}, and other \link{dfm}
#' objects.
#' @param x a candidate object for checking or coercion to \link{dfm}
#' @return \code{as.dfm} converts an input object into a \link{dfm}.  Row names
#'   are used for docnames, and column names for featnames, of the resulting
#'   dfm.
#' @seealso \code{\link{as.data.frame.dfm}}, \code{\link{as.matrix.dfm}},
#'   \code{\link{convert}}
#' @export
as.dfm <- function(x) {
    UseMethod("as.dfm")
}

#' @export
as.dfm.default <- function(x) {
    stop(friendly_class_undefined_message(class(x), "as.dfm"))
}

#' @noRd
#' @method as.dfm dfm
#' @export
as.dfm.dfm <- function(x) {
    # make sure the dimension names are character
    set_dfm_dimnames(x) <- x@Dimnames
    # for compatibility with older dfm objects
    if (identical(dim(x@docvars), c(0L, 0L)))
        x@docvars <- data.frame(matrix(ncol = 0, nrow = nrow(x)))
    return(x)
}

#' @noRd
#' @method as.dfm matrix
#' @export
as.dfm.matrix <- function(x) {
    matrix2dfm(x)
}

#' @noRd
#' @method as.dfm Matrix
#' @export
as.dfm.Matrix <- function(x) {
    matrix2dfm(x)
}

#' @noRd
#' @method as.dfm data.frame
#' @export
as.dfm.data.frame <- function(x) {
    matrix2dfm(as.matrix(x, rownames.force = TRUE))
}

#' @noRd
#' @method as.dfm dfmSparse
#' @export
as.dfm.dfmSparse <- function(x) {
    as.dfm(as(x, 'dgCMatrix'))
}

#' @noRd
#' @method as.dfm DocumentTermMatrix
#' @export
as.dfm.DocumentTermMatrix <- function(x){
    as.dfm(
        sparseMatrix(i = x$i, j = x$j, x = x$v, 
                     dimnames = list(docs = rownames(x), 
                                     features = colnames(x))))
}

#' @noRd
#' @method as.dfm TermDocumentMatrix
#' @export
as.dfm.TermDocumentMatrix <- function(x){
    as.dfm(
        sparseMatrix(i = x$j, j = x$i, x = x$v, 
                     dimnames = list(colnames(x), rownames(x))))
}

#' Converts a Matrix to a dfm
#' @param x a Matrix
#' @param slots slots a list of values to be assigned to slots
#' @keywords internal
matrix2dfm <- function(x, slots = NULL) {
   
    rowname <- rownames(x)
    if (nrow(x) > length(rowname))
        rowname <- paste0(quanteda_options("base_docname"), seq_len(nrow(x)))
    
    colname <- colnames(x)
    if (ncol(x) > length(colname))
        colname <- paste0(quanteda_options("base_featname"), seq_len(ncol(x)))
    
    x <- Matrix(x, sparse = TRUE)
    # Force dfm to have docvars
    if (is.character(rownames(x))) {
        x <- new("dfm", as(x, 'dgCMatrix'), docvars = data.frame(row.names = make.unique(rownames(x))))
    } else {
        x <- new("dfm", as(x, 'dgCMatrix'))
    }
    set_dfm_dimnames(x) <- list(rowname, colname)
    set_dfm_slots(x, slots)
}

#' Set values to a dfm's S4 slots
#' @param x a dfm 
#' @param slots a list of values extracted using \code{attributes} and to be assigned to slots 
#' @param exceptions names of slots to be ignored
#' @keywords internal
set_dfm_slots <- function(x, slots = NULL, exceptions = NULL) {
    if (is.null(slots)) return(x)
    sname <- slotNames("dfm")
    sname <- setdiff(sname, c("Dim", "Dimnames", "i", "p", "x", "factors", exceptions))
    for (s in sname) {
        try({
            slot(x, s) <- slots[[s]]
        }, silent = TRUE)
    }
    return(x)
}

#' @rdname as.dfm
#' @return 
#' \code{is.dfm} returns \code{TRUE} if and only if its argument is a \link{dfm}.
#' @export
is.dfm <- function(x) {
    is(x, "dfm")
    # "dfm" %in% class(x)
}

# topfeatures -----------

#' Identify the most frequent features in a dfm
#' 
#' List the most (or least) frequently occurring features in a \link{dfm}, either
#' as a whole or separated by document.
#' @name topfeatures
#' @param x the object whose features will be returned
#' @param n how many top features should be returned
#' @param decreasing If \code{TRUE}, return the \code{n} most frequent features;
#'   otherwise return the \code{n} least frequent features
#' @param scheme one of \code{count} for total feature frequency (within
#'   \code{group} if applicable), or \code{docfreq} for the document frequencies
#'   of features
#' @inheritParams groups
#' @return A named numeric vector of feature counts, where the names are the 
#'   feature labels, or a list of these if \code{groups} is given.
#' @examples
#' dfmat1 <- corpus_subset(data_corpus_inaugural, Year > 1980) %>%
#'     dfm(remove_punct = TRUE)
#' dfmat2 <- dfm_remove(dfmat1, stopwords("english"))
#' 
#' # most frequent features
#' topfeatures(dfmat1)
#' topfeatures(dfmat2)
#' 
#' # least frequent features
#' topfeatures(dfmat2, decreasing = FALSE)
#' 
#' # top features of individual documents  
#' topfeatures(dfmat2, n = 5, groups = docnames(dfmat2))
#' 
#' # grouping by president last name
#' topfeatures(dfmat2, n = 5, groups = "President")
#'
#' # features by document frequencies
#' tail(topfeatures(dfmat1, scheme = "docfreq", n = 200))
#' @export
topfeatures <- function(x, n = 10, decreasing = TRUE, 
                        scheme = c("count", "docfreq"), groups = NULL) {
    UseMethod("topfeatures")
}

#' @export
topfeatures.default <- function(x, n = 10, decreasing = TRUE, 
                                scheme = c("count", "docfreq"), groups = NULL) {
    stop(friendly_class_undefined_message(class(x), "topfeatures"))
}

#' @export
#' @noRd
#' @importFrom stats quantile
topfeatures.dfm <- function(x, n = 10, decreasing = TRUE,  
                            scheme = c("count", "docfreq"), groups = NULL) {
    
    x <- as.dfm(x)
    if (!nfeat(x) || !ndoc(x)) return(numeric())
    scheme <- match.arg(scheme)
    
    if (!is.null(groups)) {
        rownames(x) <- generate_groups(x, groups)
        result <- list()
        for (i in unique(docnames(x))) {
            result[[i]] <- topfeatures(x[which(rownames(x)==i), ], 
                                       n = n, scheme = scheme, 
                                       decreasing = decreasing, groups = NULL)
        }
        return(result)
    }
    
    if (n > nfeat(x)) n <- nfeat(x)
    
    if (scheme == "count") {
        wght <- colSums(x)
    } else if (scheme == "docfreq") {
        wght <- docfreq(x)
    }
    
    result <- sort(wght, decreasing)
    return(result[1:n])
    
    # Under development by Ken
    # if (is.resampled(x)) {
    #     subdfm <- x[, order(colSums(x[,,1]), decreasing = decreasing), ]
    #     subdfm <- subdfm[, 1:n, ]   # only top n need to be computed
    #     return(data.frame(#features=colnames(subdfm),
    #         freq=colSums(subdfm[,,1]),
    #         cilo = apply(colSums(subdfm), 1, stats::quantile, (1 - ci) / 2),
    #         cihi = apply(colSums(subdfm), 1, stats::quantile, 1 - (1 - ci) / 2)))
    # } else {
    #    subdfm <- sort(colSums(x), decreasing)
    #    return(subdfm[1:n])
    #}
}

# sparsity -----------

#' Compute the sparsity of a document-feature matrix
#'
#' Return the proportion of sparseness of a document-feature matrix, equal
#' to the proportion of cells that have zero counts.
#' @param x the document-feature matrix
#' @examples 
#' dfmat <- dfm(data_corpus_inaugural)
#' sparsity(dfmat)
#' sparsity(dfm_trim(dfmat, min_termfreq = 5))
#' @export
sparsity <- function(x) {
    UseMethod("sparsity")
}

#' @export
sparsity.default <- function(x) {
    stop(friendly_class_undefined_message(class(x), "sparsity"))
}

#' @export
sparsity.dfm <- function(x) {
    (1 - length(x@x) / prod(dim(x)))
}


#  Internal --------

#' Internal functions for dfm objects
#' 
#' Internal function documentation for \link{dfm} objects.
#' @name dfm-internal
#' @keywords dfm internal
NULL

#' The \code{Compare} methods enable relational operators to be use with dfm. 
#' Relational operations on a dfm with a numeric will return a
#' \link[Matrix]{dgCMatrix-class} object.
#' @rdname dfm-internal
#' @param e1 a \link{dfm}
#' @param e2 a numeric value to compare with values in a dfm
#' @export
#' @seealso \link{Comparison} operators
setMethod("Compare", c("dfm", "numeric"), function(e1, e2) {
    as(callGeneric(as(e1, "dgCMatrix"), e2), "lgCMatrix")
})
