
#' @rdname kwic
#' @noRd
#' @examples 
#' toks <- tokens(data_char_inaugural)
#' toks <- tokens_remove(toks, stopwords())
#' cont <- contexts(toks, "clinton", window = 10, valuetype = "glob")
#' 
#' x <- dfm(cont)
#' y <- dfm(toks)
#' z <- dfm_select(x, y, padding = TRUE)
#' head(textstat_keyness(new("dfmSparse", Matrix::rbind2(z, y)), target = 1:nrow(x)), 20)
#' 
#' @export 
contexts <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ...) {
    UseMethod("contexts")
}

#' @rdname kwic
#' @noRd
#' @export 
contexts.tokens <- function(x, keywords, window = 5, valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE, ...) {
    
    if (!is.tokens(x))
        stop("x must be a tokens object")
    
    valuetype <- match.arg(valuetype)
    keywords <- vector2list(keywords)
    attr_org <- attributes(x)
    
    # add document names if none
    if (is.null(names(x))) {
        names(x) <- paste("text", 1:length(x), sep="")
    }
    
    types <- types(x)
    keywords_id <- regex2id(keywords, types, valuetype, case_insensitive, FALSE)
    x <- qatd_cpp_tokens_contexts(x, types, keywords_id, window, FALSE)
    attributes(x, FALSE) <- attr_org
    
    return(x)
}
