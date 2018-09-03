#' Return the set difference of tokens
#' 
#' Returns the tokens not found in a character vector.  This is used for
#' instance in counting the "unfamiliar" words not found in a list, for instance
#' with the Dale-Chall readability index.
#' 
#' @inheritParams pattern
#' @inheritParams valuetype
#' @param case_insensitive ignore case when matching, if \code{TRUE}
#' @param unique if \code{TRUE}, do not return duplicates
#' @return tokens not found in \code{pattern}
#' @seealso \code{\link{textstat_readability}}, \code{\link{data_char_wordlist}}
#' @keywords tokens internal
#' @export
#' @examples 
#' toks <- tokens(c(d1 = "This sentence is extremely brief.", 
#'                  d2 = "Two heads are superior to one."), remove_punct = TRUE)
#' tokens_setdiff(tokens_tolower(toks), pattern = data_char_wordlists$dalechall)
#' 
#' txt <- c("a a b c d", "x y c c")
#' tokens_setdiff(tokens(txt), c("c", "d"))
#' tokens_setdiff(tokens(txt), c("c", "d"), unique = TRUE)
tokens_setdiff <- function(x, pattern, valuetype = c("fixed", "glob", "regex"),
                           case_insensitive = FALSE, unique = FALSE) {
    UseMethod("tokens_setdiff")
}

#' @export
tokens_setdiff.default <- function(x, pattern, valuetype = c("fixed", "glob", "regex"),
                                   case_insensitive = FALSE, unique = FALSE) {
    stop(friendly_class_undefined_message(class(x), "tokens_setdiff"))
}
    
#' @export
tokens_setdiff.tokens <- function(x, pattern, valuetype = c("fixed", "glob", "regex"),
                                  case_insensitive = FALSE, unique = FALSE) {
    
    valuetype <- match.arg(valuetype)
    pattern <- as.character(pattern)
    
    if (valuetype == "fixed") {
        if (case_insensitive) {
            x <- tokens_tolower(x)
            pattern <- char_tolower(pattern)
        }
        ret <- lapply(tokens_tolower(x), function(y) y[!(y %in% pattern)])
        
    } else {
        stop("glob and regex patterns are not yet implemented for this function")
    }
    
    if (unique) ret <- lapply(ret, unique)
    as.tokens(ret)
}

