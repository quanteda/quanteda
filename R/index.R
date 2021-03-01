#' Locate a pattern in a tokens object
#' 
#' Locates a [pattern] within a tokens object, returning the index positions of
#' the beginning and ending tokens in the pattern.
#' @param x an input [tokens] object
#' @inheritParams pattern
#' @inheritParams valuetype
#' @return a data.frame consisting of one row per pattern match, with columns
#'   for the document name, index positions `from` and `to`, and the pattern
#'   matched.
#' @export
#' @examples
#' toks <- tokens(data_corpus_inaugural[1:8])
#' index(toks, pattern = "secure*")
#' index(toks, pattern = c("secure*", phrase("united states"))) %>% head()
index <- function(x, pattern, 
                   valuetype = c("glob", "regex", "fixed"),
                   case_insensitive = TRUE) {
    UseMethod("index")
}

#' @export
index.default <- function(x, ...) {
    check_class(class(x), "index")
}

#' @export
index.tokens <- function(x, pattern, 
                           valuetype = c("glob", "regex", "fixed"),
                           case_insensitive = TRUE) {
    x <- as.tokens(x)
    valuetype <- match.arg(valuetype)
    
    attrs <- attributes(x)
    type <- types(x)
    if (is.list(pattern) && is.null(names(pattern)))
        names(pattern) <- pattern
    ids <- object2id(pattern, attrs[["types"]], valuetype,
                     case_insensitive, field_object(attrs, "concatenator"))
    result <- qatd_cpp_index(x, type, ids)
    result$pattern <- factor(result$pattern, levels = unique(names(ids)))
    if (nrow(result)) {
        r <- order(match(result$docname, docnames(x)),
                   result$from, result$to, result$pattern)
        result <- result[r,]
    }
    rownames(result) <- NULL
    class(result) <- c("index", "data.frame")
    return(result)
}

#' @rdname index
#' @return `is.index` returns `TRUE` if the object was created by
#'   [index()]; `FALSE` otherwise.
#' @export
is.index <- function(x) {
    "index" %in% class(x)
}
