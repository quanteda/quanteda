
#' @rdname kwic
#' @noRd
#' @examples 
#' 
#' toks <- tokens(data_char_inaugural)
#' coxs <- contexts(toks, "economy", window = 5, case_insensitive = FALSE, valuetype = "fixed")
#'
#' # compare with fcm 
#' fcm <- fcm(toks, 'window', window = 5, tri = FALSE)
#' v1 <- colSums(fcm['economy',])
#' v2 <- colSums(dfm(coxs))
#' merge(v1, v2, by='row.names')
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
    result <- qatd_cpp_tokens_contexts(x, types, keywords_id, window)
    attributes(result, FALSE) <- attr_org
    
    return(result)
}
