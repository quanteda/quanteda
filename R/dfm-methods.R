# featnames -----------

#' Get the feature labels from a dfm
#' 
#' Get the features from a document-feature matrix, which are stored as the
#' column names of the \link{dfm} object.
#' @param x the dfm whose features will be extracted
#' @return character vector of the feature labels
#' @examples
#' inaugDfm <- dfm(data_corpus_inaugural, verbose = FALSE)
#' 
#' # first 50 features (in original text order)
#' head(featnames(inaugDfm), 50)
#' 
#' # first 50 features alphabetically
#' head(sort(featnames(inaugDfm)), 50)
#' 
#' # contrast with descending total frequency order from topfeatures()
#' names(topfeatures(inaugDfm, 50))
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
    x
}

#' @noRd
#' @method as.dfm matrix
#' @export
as.dfm.matrix <- function(x) {
    as_dfm_constructor(x)
}

#' @noRd
#' @method as.dfm Matrix
#' @export
as.dfm.Matrix <- function(x) {
    as_dfm_constructor(x)
}

#' @noRd
#' @method as.dfm data.frame
#' @export
as.dfm.data.frame <- function(x) {
    as_dfm_constructor(as.matrix(x, rownames.force = TRUE))
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
                     dimnames = list(rownames(x), colnames(x)))
    )
}

#' @noRd
#' @method as.dfm TermDocumentMatrix
#' @export
as.dfm.TermDocumentMatrix <- function(x){
    as.dfm(
        sparseMatrix(i = x$j, j = x$i, x = x$v, 
                     dimnames = list(colnames(x), rownames(x)))
    )
}

as_dfm_constructor <- function(x) {
    x <- Matrix(x, sparse = TRUE) # dimnames argument is not working
    names(dimnames(x)) <- c("docs", "features")
    if (nrow(x) > 0 && is.null(rownames(x))) 
        rownames(x) <- paste0(quanteda_options("base_docname"), seq_len(nrow(x)))
    if (ncol(x) > 0 && is.null(colnames(x)))
        colnames(x) <- paste0(quanteda_options("base_featname"), seq_len(ncol(x)))
    new("dfm", x, docvars = data.frame(row.names = rownames(x)))
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
#' mydfm <- corpus_subset(data_corpus_inaugural, Year > 1980) %>%
#'     dfm(remove_punct = TRUE)
#' mydfm_nostopw <- dfm_remove(mydfm, stopwords("english"))
#' 
#' # most frequent features
#' topfeatures(mydfm)
#' topfeatures(mydfm_nostopw)
#' 
#' # least frequent features
#' topfeatures(mydfm_nostopw, decreasing = FALSE)
#' 
#' # top features of individual documents  
#' topfeatures(mydfm_nostopw, n = 5, groups = docnames(mydfm_nostopw))
#' 
#' # grouping by president last name
#' topfeatures(mydfm_nostopw, n = 5, groups = "President")
#'
#' # features by document frequencies
#' tail(topfeatures(mydfm, scheme = "docfreq", n = 200))
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
#' inaug_dfm <- dfm(data_corpus_inaugural, verbose = FALSE)
#' sparsity(inaug_dfm)
#' sparsity(dfm_trim(inaug_dfm, min_count = 5))
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
