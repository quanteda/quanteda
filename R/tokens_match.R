#' Match the tokens IDs with given types
#'
#' Match the token types and IDs of multiple [tokens] objects using a character vector. 
#' @param x the [tokens] object.
#' @param types character vector for the token types to be matched in the resulting 
#'  [tokens]. Tokens not included in `types` are all removed.
#' @export
#' @seealso dfm_match
#' @examples
#' txt <- c("This is text one", "The text two", "This is text three")
#' (toks1 <- tokens(txt[1:2]))
#' (toks2 <- tokens(txt[2:3]))
#' (toks3 <- tokens_match(toks1, types(toks2)))
#' identical(types(toks2), types(toks3))
tokens_match <- function(x, types,
                         verbose = quanteda_options("verbose")) {
    UseMethod("tokens_match")
}

#' @export
tokens_match.default <- function(x, types, 
                                 verbose = quanteda_options("verbose")) {
    check_class(class(x), "tokens_match")
}

#' @export
tokens_match.tokens_xptr <- function(x, types, 
                                     verbose = quanteda_options("verbose")) {
    
    
    types <- check_character(types, min_len = 0, max_len = Inf, 
                             normalize = TRUE)
    
    attrs <- attributes(x)
    if (verbose)
        before <- stats_tokens(x)
    
    type <- get_types(x)
    set_types(x) <- types
    
    index <- match(type, types)
    index[is.na(index)] <- -1L
    
    result <-  cpp_tokens_match(x, index, get_threads())
    
    result <- rebuild_tokens(result, attrs)
    if (verbose)
        message_tokens("tokens_match()", before, stats_tokens(result))
    return(result)
}

#' @export
tokens_match.tokens <- function(x, ...) {
    as.tokens(tokens_match(as.tokens_xptr(x), ...))
}
