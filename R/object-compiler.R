#' Object compilers
#' @rdname object-compiler
#' @param source character that indicating source object
#' @param feature character for feature of resulting `dfm`
#' @param docvars data.frame for docment level variables 
#' @param meta list for meta fields
#' @param ... added to object meta fields  
#' @keywords internal
compile_dfm <- function(x, features,
                        docvars = data.frame(), meta = list(), ...) {
    result <- new("dfm", x)
    result@Dimnames <- list(
        docs = as.character(docvars[["docname_"]]), 
        features = as.character(features)
    )
    result@docvars <- docvars
    result@meta <- make_meta("dfm", inherit = meta, ...)
    return(result)
}

#' @rdname object-compiler
#' @param types character for types of resulting `tokens`` object
#' @param padding logical indicating if the `tokens` object contains paddings
compile_tokens <- function(x, types, 
                           docvars = data.frame(), meta = list(), ...) {
    
    structure(x,
              names = docvars[["docname_"]],
              class = "tokens",
              types = types,
              padding = FALSE,
              docvars = docvars,
              meta = make_meta("tokens", inherit = meta, ...))
}

#' @rdname object-compiler
compile_corpus <- function(x, 
                           docvars = data.frame(), 
                           meta = list(), ...) {
    structure(x,
              names = docvars[["docname_"]],
              class = "corpus",
              docvars = docvars,
              meta = make_meta("corpus", inherit = meta, ...))
}

